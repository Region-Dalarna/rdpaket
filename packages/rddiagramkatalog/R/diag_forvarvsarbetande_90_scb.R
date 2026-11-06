#' Diagram: förvärvsarbetande per bransch sedan 1990
#'
#' Diagram för antalet förvärvsarbetande inom olika branscher från 1990
#' till senaste observation. Finns i två varianter: ett stapeldiagram där
#' varje stapel motsvarar ett år, och ett liggande stapeldiagram med
#' förändring från första till sista år.
#'
#' @param region_vekt Vilken region vill man ha. Enbart 1 får väljas.
#' @param output_mapp_data Mapp där data sparas, om man vill spara data. Används primärt i Rmarkdown-rapporter.
#' @param utmapp Mapp där figuren sparas. Måste finnas om `skriv_diagramfil = TRUE`.
#' @param skriv_diagramfil Sparar figuren till `utmapp`.
#' @param filnamn_data Filnamn på sparad data.
#' @param diag_antal Diagram som visar antal för varje bransch i valda år.
#' @param diag_forandring Förändring från första till sista år (i antal) för varje bransch.
#' @param kon_klartext `c("män", "kvinnor")` ger totalt. Det går även att välja ett av könen. Jämförelse mellan kön är inte möjlig.
#' @param valda_ar Vilka år skall jämföras i `diag_antal` (får inte vara fler än antalet färger i `diag_fargvekt`). `"9999"` ger senaste år.
#' @param diag_fargvekt Val av diagramfärger.
#' @param returnera_figur Skall figuren returneras som ett ggplot-objekt.
#' @param returnera_data TRUE om användaren vill returnera data från funktionen.
#'
#' @return En lista med `ggplot`-objekt.
#' @export
diag_forvarvsarbetande_90_scb <- function(region_vekt = "20",
                                              output_mapp_data = NA,
                                              utmapp = rdverktyg::utskriftsmapp(),
                                              skriv_diagramfil = TRUE,
                                              filnamn_data = "forvarvsarbetande_bransch.xlsx",
                                              diag_antal = TRUE,
                                              diag_forandring = TRUE,
                                              kon_klartext = c("män","kvinnor"),
                                              valda_ar = c("1990","2000","2010","9999"),
                                              diag_fargvekt = rddiagram::diagramfarger("rus_sex"),
                                              returnera_figur = TRUE,
                                              returnera_data = FALSE){


  # =================================================================================================================
  # Diagram för antalet förvärvsarbetande inom olika branscher från 1990 till senaste observation
  # Finns för tillfället i två varianter, det ena är ett stapeldiagram där varje stapel motsvarar ett år,
  # det andra är ett liggande stapeldiagram med förändring från första till sista år
  # =================================================================================================================

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  gg_list <- list() # Skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  objektnamn <- c() # Används för att namnge objekt i lista
  list_data <- list() # Skapa tom lista som används för att spara till Excel.

  # =============================================== API-uttag ===============================================
  # Hämtar samma data som tidigare kom via
  # hamta_data/hamta_forvarvsarbetande_bransch_1990_senastear_SCB.R, men
  # direkt mot SCB:s PxWeb-API v2 med pxweb2r, från fem tabeller (samma fem
  # källor som förut, tre klassificeringsbyten SNI92 -> SNI2002 -> SNI2007
  # samt källbyte RAMS -> BAS 2020):
  # - TAB341  = "AMPAK3" (SNI92), 1990-2003. Redan på den grova 11-kategori-
  #   indelningen som används rakt av som "Näringsgren", ingen ombenämning.
  # - TAB904  = "DagSNIKonK" (SNI2002), 2004-2007. Samma sak - redan på
  #   samma 11-kategoriindelning.
  # - TAB900  = "DagSNI07KonK" (SNI2007), 2008-2018. Finare indelning (16
  #   kategorier) - döps om till samma gamla RAMS-fraser som ovan.
  # - TAB5837 = "DagSni07KonKN" (SNI2007), bara 2019 hämtas. Samma
  #   kategorier/ombenämning som TAB900.
  # - TAB3785 = BAS-tabellen (samma familj som i de andra sysselsatta-
  #   diagrammen), 2020-. Egna klartexter igen (använder "företag inom...",
  #   "...väsendet" osv.) - döps om till samma gamla RAMS-fraser för att
  #   kunna slås ihop med de fyra äldre tabellerna.
  # Ingen av dessa fem har en CKM-efterföljare - BAS/RAMS-registren verkar
  # inte omfattas av CKM (till skillnad från befolkningsstatistiken).
  #
  # Upptäckt under migreringen: original-koden letade efter kategorin
  # "utbildning " (med ett mellanslag på slutet) i SNI2007-tabellerna, men
  # den riktiga etiketten är "utbildning" (utan mellanslag) - så den
  # kategorin hamnade fel/borta 2008-2019 i originalet. Rättat här.

  sni2007_till_ramsfras <- function(x) {
    dplyr::case_when(
      x == "jordbruk, skogsbruk och fiske" ~ "jordbruk, skogsbruk, jakt, fiske",
      x == "tillverkning och utvinning" ~ "utvinning av mineral, tillverkningsindustri",
      x == "energiförsörjning; miljöverksamhet" ~ "energi- o vattenförsörjning, avfallshantering",
      x == "byggverksamhet" ~ "byggindustri",
      x == "handel" ~ "handel; transport, magasinering; kommunikation",
      x == "transport och magasinering" ~ "handel; transport, magasinering; kommunikation",
      x == "hotell- och restaurangverksamhet" ~ "personliga och kulturella tjänster",
      x == "information och kommunikation" ~ "kreditinstitut, fastighetsförvaltn, företagstjänster",
      x == "finans- och försäkringsverksamhet" ~ "kreditinstitut, fastighetsförvaltn, företagstjänster",
      x == "fastighetsverksamhet" ~ "kreditinstitut, fastighetsförvaltn, företagstjänster",
      x == "företagstjänster" ~ "kreditinstitut, fastighetsförvaltn, företagstjänster",
      x == "offentlig förvaltning och försvar" ~ "civila myndigheter, försvar; internat. organisationer",
      x == "utbildning" ~ "forskning o utveckling; utbildning",
      x == "vård och omsorg; sociala tjänster" ~ "enh för hälso- och sjukvård, socialtjänst; veterinärer",
      x == "kulturella och personliga tjänster m.m." ~ "personliga och kulturella tjänster",
      x == "okänd verksamhet" ~ "näringsgren okänd"
    )
  }

  bas_till_ramsfras <- function(x) {
    dplyr::case_when(
      x == "företag inom jordbruk, skogsbruk och fiske" ~ "jordbruk, skogsbruk, jakt, fiske",
      x == "tillverkningsindustri; gruvor och mineralutvinningsindustri" ~ "utvinning av mineral, tillverkningsindustri",
      x == "företag inom energi och miljö" ~ "energi- o vattenförsörjning, avfallshantering",
      x == "byggindustri" ~ "byggindustri",
      x == "handel; serviceverkstäder för motorfordon och motorcyklar" ~ "handel; transport, magasinering; kommunikation",
      x == "transport- och magasineringsföretag" ~ "handel; transport, magasinering; kommunikation",
      x == "hotell och restauranger" ~ "personliga och kulturella tjänster",
      x == "informations- och kommunikationsföretag" ~ "kreditinstitut, fastighetsförvaltn, företagstjänster",
      x == "kreditinstitut och försäkringsbolag m.m." ~ "kreditinstitut, fastighetsförvaltn, företagstjänster",
      x == "fastighetsbolag och fastighetsförvaltare" ~ "kreditinstitut, fastighetsförvaltn, företagstjänster",
      x == "företag inom juridik, ekonomi, vetenskap och teknik; företag inom uthyrning, fastighetsservice, resetjänster och andra stödtjänster" ~ "kreditinstitut, fastighetsförvaltn, företagstjänster",
      x == "civila myndigheter och försvaret" ~ "civila myndigheter, försvar; internat. organisationer",
      x == "utbildningsväsendet" ~ "forskning o utveckling; utbildning",
      x == "enheter för vård och omsorg, socialtjänst" ~ "enh för hälso- och sjukvård, socialtjänst; veterinärer",
      x == "enheter för kultur, nöje och fritid; andra serviceföretag m.m." ~ "personliga och kulturella tjänster",
      x == "uppgift saknas" ~ "näringsgren okänd"
    )
  }

  d1 <- pxweb2r::pxweb2_get_data("TAB341", query = list(Region = region_vekt, SNI92 = "*", Kon = kon_klartext, Tid = "*"), quiet = TRUE) |>
    dplyr::rename(regionkod = region_kod, Näringsgren = `näringsgren SNI92`, `Förvärvsarbetande 16+ år (dagbef)` = value) |>
    dplyr::select(regionkod, region, kön, Näringsgren, år, `Förvärvsarbetande 16+ år (dagbef)`)

  d2 <- pxweb2r::pxweb2_get_data("TAB904", query = list(Region = region_vekt, SNI2002 = "*", Kon = kon_klartext, Tid = "*"), quiet = TRUE) |>
    dplyr::rename(regionkod = region_kod, Näringsgren = `näringsgren SNI 2002`, `Förvärvsarbetande 16+ år (dagbef)` = value) |>
    dplyr::select(regionkod, region, kön, Näringsgren, år, `Förvärvsarbetande 16+ år (dagbef)`)

  d3 <- pxweb2r::pxweb2_get_data("TAB900", query = list(Region = region_vekt, SNI2007 = "*", Kon = kon_klartext, Tid = "*"), quiet = TRUE) |>
    dplyr::rename(regionkod = region_kod, `Förvärvsarbetande 16+ år (dagbef)` = value) |>
    dplyr::mutate(Näringsgren = sni2007_till_ramsfras(`näringsgren SNI 2007`)) |>
    dplyr::select(regionkod, region, kön, Näringsgren, år, `Förvärvsarbetande 16+ år (dagbef)`)

  d4 <- pxweb2r::pxweb2_get_data("TAB5837", query = list(Region = region_vekt, SNI2007 = "*", Kon = kon_klartext, Tid = "2019"), quiet = TRUE) |>
    dplyr::rename(regionkod = region_kod, `Förvärvsarbetande 16+ år (dagbef)` = value) |>
    dplyr::mutate(Näringsgren = sni2007_till_ramsfras(`näringsgren SNI 2007`)) |>
    dplyr::select(regionkod, region, kön, Näringsgren, år, `Förvärvsarbetande 16+ år (dagbef)`)

  d5 <- pxweb2r::pxweb2_get_data(
      "TAB3785",
      query = list(Region = region_vekt, SNI2007 = "*", Kon = kon_klartext, Fodelseregion = "totalt",
                   ContentsCode = "sysselsatta efter arbetsställets belägenhet", Tid = "*"),
    quiet = TRUE) |>
    dplyr::filter(`näringsgren SNI 2007` != "Total") |>
    dplyr::rename(regionkod = region_kod, `Förvärvsarbetande 16+ år (dagbef)` = value) |>
    dplyr::mutate(Näringsgren = bas_till_ramsfras(`näringsgren SNI 2007`)) |>
    dplyr::select(regionkod, region, kön, Näringsgren, år, `Förvärvsarbetande 16+ år (dagbef)`)

  df_utskrift <- dplyr::bind_rows(d1, d2, d3, d4, d5) |>
    dplyr::group_by(region,kön,Näringsgren,år) |>
    dplyr::summarize(antal = sum(`Förvärvsarbetande 16+ år (dagbef)`), .groups = "drop")

  df <- df_utskrift

  if(diag_antal == TRUE){

    # Ersätter "9999" med senaste år
    valda_ar <- stringr::str_replace(as.character(valda_ar), "9999", as.character(max(df$år)))

    if("kvinnor" %in% unique(df$kön) & "män" %in% unique(df$kön)) {
      variabellista = c("region","Näringsgren","år")
      diagram_titel <- paste0("Förvärvsarbetande 16-74 år i ",rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(region_vekt)[2]))
      objektnamn <- paste0("forvarvsarbetande_90_totalt_",rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(region_vekt)[2]))
    }else {
        variabellista = c("region","kön","Näringsgren","år")
        diagram_titel <- paste0("Förvärvsarbetande ",unique(df$kön) ," 16-74 år i ",rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(region_vekt)[2]))
        objektnamn <- paste0("forvarvsarbetande_90_",unique(df$kön),"_",rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(region_vekt)[2]))
        }

    df_sum = df |>
      dplyr::group_by(dplyr::across(dplyr::any_of(variabellista))) |>
        dplyr::summarize(antal = sum(antal), .groups = "drop")

    # Ändrar namn på branscher (så att de bättre överensstämmer med de som finns idag)

    df_sum <- df_sum |>
      dplyr::mutate(Näringsgren = dplyr::case_when(
        Näringsgren == "byggindustri" ~ "Bygg",
        Näringsgren == "civila myndigheter, försvar; internat. organisationer" ~ "Myndigheter mm" ,
        Näringsgren == "energi- o vattenförsörjning, avfallshantering" ~ "Energi och miljö",
        Näringsgren == "enh för hälso- och sjukvård, socialtjänst; veterinärer" ~ "Hälso- och sjukvård mm" ,
        Näringsgren == "forskning o utveckling; utbildning" ~ "Utbildning",
        Näringsgren == "handel; transport, magasinering; kommunikation" ~ "Handel, transport mm" ,
        Näringsgren == "jordbruk, skogsbruk, jakt, fiske" ~ "Jordbruk och skogsbruk",
        Näringsgren == "kreditinstitut, fastighetsförvaltn, företagstjänster" ~ "Företagstjänster, finans mm",
        Näringsgren == "näringsgren okänd" ~ "Okänd verksamhet" ,
        Näringsgren == "personliga och kulturella tjänster" ~ "Hotell, restaurang och kultur mm",
        Näringsgren == "utvinning av mineral, tillverkningsindustri" ~ "Tillverkning och utvinning"))

    # Om användaren vill returnera data görs detta här
    if(returnera_data == TRUE){
      assign("forvarvsarbetande_90_senastear", df_sum, envir = .GlobalEnv)
    }

    # Om användaren vill spara data görs detta här. Sker enbart om både outputmapp och filnamn har valts
    if (!is.na(output_mapp_data) & !is.na(filnamn_data)){
      list_data <- c(list_data,list("Antal" = df_sum))
    }

    diagram_capt <- "Källa: RAMS och BAS i SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Branschgruppering baserad på SNI2002 och SNI92.\nByte från RAMS till BAS som datakälla från och med 2020."
    diagramfil <- paste0(objektnamn,".png")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = dplyr::filter(df_sum, år %in% valda_ar),
      skickad_x_var = "Näringsgren",
      skickad_y_var = "antal",
      skickad_x_grupp = "år",
      manual_x_axis_text_vjust=1,
      manual_x_axis_text_hjust=1,
      manual_color = diag_fargvekt,
      x_axis_sort_value = TRUE,
      vand_sortering = TRUE,
      stodlinjer_avrunda_fem = TRUE,
      x_axis_sort_grp = length(valda_ar),
      x_axis_lutning = 45,
      diagram_titel = diagram_titel,
      diagram_capt = diagram_capt,
      manual_y_axis_title = "",
      output_mapp = utmapp,
      filnamn_diagram = diagramfil,
      skriv_till_diagramfil = skriv_diagramfil
    )

    gg_list <- c(gg_list, list(gg_obj))

  }

  if(diag_forandring == TRUE){
    diagram_capt <- "Källa: RAMS i SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Branschgruppering baserad på SNI2002 och SNI92"

    df_sum = df |>
      dplyr::group_by(år,region,Näringsgren) |>
        dplyr::summarize(antal = sum(antal), .groups = "drop")

    # Beräknar förändring in antalet anställda från 1990 till senaste år
    df_for <- df_sum |>
      dplyr::filter(år %in% c(min(år),max(år))) |>
      dplyr::group_by(region,Näringsgren) |>
      dplyr::mutate(skillnad = dplyr::last(antal)-dplyr::first(antal)) |>
      dplyr::mutate(Näringsgren = dplyr::case_when(
        Näringsgren == "byggindustri" ~ "Bygg",
        Näringsgren == "civila myndigheter, försvar; internat. organisationer" ~ "Myndigheter mm" ,
        Näringsgren == "energi- o vattenförsörjning, avfallshantering" ~ "Energi och miljö",
        Näringsgren == "enh för hälso- och sjukvård, socialtjänst; veterinärer" ~ "Hälso- och sjukvård mm" ,
        Näringsgren == "forskning o utveckling; utbildning" ~ "Utbildning",
        Näringsgren == "handel; transport, magasinering; kommunikation" ~ "Handel, transport mm" ,
        Näringsgren == "jordbruk, skogsbruk, jakt, fiske" ~ "Jordbruk och skogsbruk",
        Näringsgren == "kreditinstitut, fastighetsförvaltn, företagstjänster" ~ "Företagstjänster, finans mm",
        Näringsgren == "näringsgren okänd" ~ "Okänd verksamhet" ,
        Näringsgren == "personliga och kulturella tjänster" ~ "Hotell, restaurang och kultur mm",
        Näringsgren == "utvinning av mineral, tillverkningsindustri" ~ "Tillverkning och utvinning"))

    # Om användaren vill returnera data görs detta här
    if(returnera_data == TRUE){
      assign("forvarvsarbetande_90_forandring", df_for, envir = .GlobalEnv)
    }

    # Om användaren vill spara data görs detta här. Sker enbart om både outputmapp och filnamn har valts
    if (!is.na(output_mapp_data) & !is.na(filnamn_data)){
      list_data <- c(list_data,list("Förändring" = df_for))
    }

    diagram_titel <- paste0("Förändring av antalet förvärvsarbetande 16-74 år från år ", min(df_for$år), " till ", max(df_for$år))
    diagramfil <- "forvarvsarbetande_90_forandring.png"
    objektnamn <- c(objektnamn,paste0("forvarvsarbetande_90_forandring_",rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(region_vekt)[2])))

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = dplyr::filter(df_for, år == max(år), Näringsgren != "Okänd verksamhet"),
      skickad_x_var = "Näringsgren",
      skickad_y_var = "skillnad",
      manual_color = diag_fargvekt[1],
      diagram_titel = diagram_titel,
      x_axis_sort_value = TRUE,
      x_axis_lutning = 45,
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      diagram_capt = diagram_capt,
      diagram_liggande = TRUE,
      stodlinjer_avrunda_fem = TRUE,
      geom_position_stack = TRUE,
      output_mapp = utmapp,
      filnamn_diagram = diagramfil,
      skriv_till_diagramfil = skriv_diagramfil
    )

    gg_list <- c(gg_list, list(gg_obj))
  }

  names(gg_list) <- c(objektnamn)

  # OBS: originalet returnerade (vid returnera_figur = TRUE, standardvärdet)
  # INNAN Excel-exporten nedan, vilket gjorde att output_mapp_data/
  # filnamn_data aldrig skrevs i praktiken. Exporten görs nu alltid innan
  # returneringen.
  if (!is.na(output_mapp_data) & !is.na(filnamn_data)){
    openxlsx::write.xlsx(list_data,paste0(output_mapp_data,filnamn_data))
  }

  if(returnera_figur == TRUE) return(gg_list)

}
