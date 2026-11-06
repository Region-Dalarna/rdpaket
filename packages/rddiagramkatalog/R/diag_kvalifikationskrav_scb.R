#' Diagram: kvalifikationskrav för anställda
#'
#' Diagram som jämför kvalifikationskrav inom olika områden (kommun eller
#' län) och bransch i valt län/kommun. Funkar enbart för senaste år.
#'
#' @param region_vekt Vilken region vill man titta på. Enbart en får väljas.
#' @param output_mapp_data Mapp där data sparas, om man vill spara data. Används primärt i Rmarkdown-rapporter.
#' @param utmapp Mapp där figuren sparas. Måste finnas om `skriv_diagramfil = TRUE`.
#' @param diag_jmf_omrade Jämför antingen kommuner eller län (beroende på val under `jmf_omrade`).
#' @param diag_jmf_bransch Jämför bransch i valt län/kommun.
#' @param jmf_omrade Om `"lan"`, jämförs Sveriges alla län. Om `"kommun"`, jämförs kommuner i valt län (`region_vekt`) eller i samma län som vald kommun (`region_vekt`).
#' @param skriv_diagramfil Sparar figuren till `utmapp`.
#' @param stodlinjer_avrunda_fem Stödlinjer på y-axeln avrundas till närmaste fem.
#' @param filnamn_data Filnamn på sparad data.
#' @param kon_klartext Män och kvinnor ger totalt. Det går även att välja ett av könen. Jämförelse mellan kön är inte möjlig.
#' @param diag_fargvekt Val av diagramfärger.
#' @param returnera_figur Skall figuren returneras som ett ggplot-objekt.
#' @param returnera_data Skall data returneras.
#'
#' @return En namngiven lista med `ggplot`-objekt.
#' @export
diag_kvalifikationskrav_scb <- function(region_vekt = "20",
                                       output_mapp_data = NA,
                                       utmapp = rdverktyg::utskriftsmapp(),
                                       diag_jmf_omrade = TRUE,
                                       diag_jmf_bransch = TRUE,
                                       jmf_omrade = "lan",
                                       skriv_diagramfil = TRUE,
                                       stodlinjer_avrunda_fem = TRUE,
                                       filnamn_data = "kvalifikationskrav.xlsx",
                                       kon_klartext = c("män","kvinnor"),
                                       diag_fargvekt = rddiagram::diagramfarger("rus_sex"),
                                       returnera_figur = TRUE,
                                       returnera_data = FALSE){


  # =================================================================================================================
  # Diagram som jämför kvalifikationskrav inom olika områden (kommun eller län) och bransch i valt län/kommun
  # Funkar enbart för senaste år
  #
  # Enligt SCB är åldersgruppen ändrad från 16-64 till 16-69 år från och med årgång 2023:
  # "Från årgång 2023 redovisas ålder 16-69 år, för tidigare år redovisas 16-64 år. Från år 2023 ingår modellimputerade yrken. Jämförelse av statistik för 2023 mot tidigare år bör därför göras med försiktighet."
  # =================================================================================================================

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  gg_list <- list() # Skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  objektnamn <- c() # Används för att namnge objekt i lista
  list_data <- list() # Skapa tom lista som används för att spara till Excel.

  valt_lan = rdverktyg::hamtaregion_kod_namn(region_vekt)[[2]]

  if(jmf_omrade == "lan"){
    region_vekt = rdverktyg::hamtaAllaLan()
  }
  if(jmf_omrade == "kommun"){
    region_vekt = rdverktyg::hamtakommuner(substr(region_vekt,1,2))
  }

  # Skriptet source:ade tidigare hamta_data-repots
  # hamta_yrke_rams_bas_region_yrke2012_sni2007_kon_tid_YREG56_YREG56N_YREG56BAS_scb.R,
  # som (med tid="9999") slog ihop tre tabeller (RAMS 2014-2018, RAMS
  # 2019-2021, BAS 2020-2024) och sedan filtrerade på det övergripande
  # senaste året. Eftersom BAS-tabellen (TAB4436) alltid har det senaste
  # året av de tre lämnar RAMS-tabellerna aldrig något bidrag när enbart
  # senaste år efterfrågas (vilket är det enda den här funktionen någonsin
  # gör - se "Funkar enbart för senaste år" ovan) - kontrollerat både i
  # källkoden och mot skarp data. Migreringen hämtar därför direkt och
  # enbart från BAS-tabellen TAB4436, vilket ger identiskt resultat.
  px_df <- pxweb2r::pxweb2_get_data(
    table = "TAB4436",
    query = list(
      Region = region_vekt,
      Yrke2012 = "*",
      SNI2007 = "*",
      Kon = kon_klartext,
      ContentsCode = "Antal",
      Tid = "9999"
    ), quiet = TRUE) |>
    dplyr::rename(regionkod = region_kod,
                  yrkeskod = `yrke (ssyk 2012)_kod`,
                  Antal = value) |>
    dplyr::select(-`näringsgren sni 2007_kod`, -tabellinnehåll) |>
    dplyr::filter(år == max(år))

  # Ändra namn på vissa branscher
  px_df$Branschgrupp <- dplyr::case_when(
    stringr::str_squish(px_df$`näringsgren SNI 2007`) == "jordbruk, skogsbruk och fiske" ~ "Jordbruk och skogsbruk",
    stringr::str_squish(px_df$`näringsgren SNI 2007`) == "tillverkning och utvinning" ~ "Tillverkning och utvinning",
    stringr::str_squish(px_df$`näringsgren SNI 2007`) == "energiförsörjning; miljöverksamhet" ~ "Energi och miljö",
    stringr::str_squish(px_df$`näringsgren SNI 2007`) == "byggverksamhet" ~ "Bygg",
    stringr::str_squish(px_df$`näringsgren SNI 2007`) == "handel" ~ "Handel",
    stringr::str_squish(px_df$`näringsgren SNI 2007`) == "transport och magasinering" ~ "Transport",
    stringr::str_squish(px_df$`näringsgren SNI 2007`) == "hotell- och restaurangverksamhet" ~ "Hotell och restaurang",
    stringr::str_squish(px_df$`näringsgren SNI 2007`) == "information och kommunikation" ~ "IT och kommunikation",
    stringr::str_squish(px_df$`näringsgren SNI 2007`) == "finans- och försäkringsverksamhet" ~ "Finans och försäkring",
    stringr::str_squish(px_df$`näringsgren SNI 2007`) == "fastighetsverksamhet" ~ "Fastighet",
    stringr::str_squish(px_df$`näringsgren SNI 2007`) == "företagstjänster" ~ "Företagstjänster",
    stringr::str_squish(px_df$`näringsgren SNI 2007`) == "offentlig förvaltning och försvar" ~ "Offentlig förvaltning",
    stringr::str_squish(px_df$`näringsgren SNI 2007`) == "utbildning" ~ "Utbildning",
    stringr::str_squish(px_df$`näringsgren SNI 2007`) == "vård och omsorg; sociala tjänster" ~ "Vård och omsorg",
    stringr::str_squish(px_df$`näringsgren SNI 2007`) == "kulturella och personliga tjänster m.m." ~ "Kultur m.m.",
    stringr::str_squish(px_df$`näringsgren SNI 2007`) == "okänd verksamhet" ~ "Okänd verksamhet"
  )

  # Klassificerar yrken utifrån hur avancerade de är
  px_df$kompetensniva <- dplyr::case_when(
    substr(px_df$yrkeskod,1,1)=="1" ~ "Chefsyrken",
    dplyr::between(as.integer(substr(px_df$yrkeskod,1,1)),2,2) ~ "Motsvarande fördjupad högskolekompetens",
    dplyr::between(as.integer(substr(px_df$yrkeskod,1,1)),3,3) ~ "Motsvarande högskolekompetens",
    dplyr::between(as.integer(substr(px_df$yrkeskod,1,1)),4,8) ~ "Motsvarande gymnasial kompetens",
    substr(px_df$yrkeskod,1,1) =="0" & px_df$yrkeskod !="0002" ~ "Motsvarande gymnasial kompetens",
    px_df$yrkeskod =="0002" ~ "Okänt",
    substr(px_df$yrkeskod,1,1) =="9" ~ "Enklare yrken")

  if(diag_jmf_omrade == TRUE){
    if("kvinnor" %in% unique(px_df$kön) & "män" %in% unique(px_df$kön)) {
      variabellista = c("år","region","kompetensniva")
      diagram_titel <- paste0("Kvalifikationskrav för anställda 16-69 år ",max(px_df$år))
      objektnamn <- paste0("kvalifikationskrav_jmf_",jmf_omrade)
    }else {
      variabellista = c("år","kön","region","kompetensniva")
      diagram_titel <- paste0("Kvalifikationskrav för anställda ",unique(px_df$kön) ," 16-64 år i ",max(px_df$år))
      objektnamn <- paste0("kvalifikationskrav_jmf_",unique(px_df$kön),"_",jmf_omrade)
    }

    diagram_capt <- "Källa: Yrkesregistret i SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"

    px_df_sum <- px_df |>
      dplyr::group_by(dplyr::across(dplyr::any_of(variabellista))) |>
      dplyr::summarize(Antal = sum(Antal), .groups = "drop_last") |>
      dplyr::mutate(Andel = ((Antal/sum(Antal))*100)-0.001) |>
      dplyr::ungroup()

    if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
      list_data <- c(list_data,list("Jmf" = px_df_sum))
    }

    if(returnera_data == TRUE){
      assign("kvalifikationskrav_jmf", px_df_sum, envir = .GlobalEnv)
    }

    diagramfil <- paste0("kompetenskrav_jmf_",jmf_omrade,".png")


    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = px_df_sum |>
        dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region),
                      kompetensniva = factor(kompetensniva, levels = c("Enklare yrken","Motsvarande gymnasial kompetens","Motsvarande högskolekompetens","Motsvarande fördjupad högskolekompetens","Chefsyrken","Okänt")[6:1])),
      skickad_x_var = "region",
      skickad_y_var = "Andel",
      skickad_x_grupp = "kompetensniva",
      manual_color = c(rgb(211,211,211, maxColorValue = 255),diag_fargvekt),
      diagram_titel = diagram_titel,
      x_axis_lutning = 0,
      diagram_capt = diagram_capt,
      legend_vand_ordning = TRUE,
      legend_byrow = TRUE,
      legend_kolumner = 2,
      diagram_liggande = TRUE,
      geom_position_stack = TRUE,
      manual_y_axis_title = "procent",
      stodlinjer_avrunda_fem = stodlinjer_avrunda_fem,
      x_axis_sort_value = TRUE,
      x_axis_sort_grp = 6,
      vand_sortering = TRUE,
      output_mapp = utmapp,
      filnamn_diagram = diagramfil,
      skriv_till_diagramfil = skriv_diagramfil)
    gg_list <- c(gg_list, list(gg_obj))

  }

  if(diag_jmf_bransch == TRUE){
    if("kvinnor" %in% unique(px_df$kön) & "män" %in% unique(px_df$kön)) {
      variabellista = c("år","region","Branschgrupp","kompetensniva")
      diagram_titel <- paste0("Kvalifikationskrav för anställda 16-69 år i ",valt_lan," ",max(px_df$år))
      objektnamn <- c(objektnamn,paste0("kvalifikationskrav_bransch_",valt_lan))
    }else {
      variabellista = c("år","kön","region","Branschgrupp","kompetensniva")
      diagram_titel <- paste0("Kvalifikationskrav för anställda ",unique(px_df$kön) ," 16-69 år i ",valt_lan," ",max(px_df$år))
      objektnamn <- c(objektnamn,paste0("kvalifikationskrav_bransch_",unique(px_df$kön),"_",valt_lan))
    }

    diagram_capt <- "Källa: Yrkesregistret i SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"

    px_df_sum <- px_df |>
      dplyr::filter(år == max(år),region == valt_lan) |>
      dplyr::group_by(dplyr::across(dplyr::any_of(variabellista))) |>
      dplyr::summarize(Antal = sum(Antal), .groups = "drop_last") |>
      dplyr::mutate(Andel=((Antal/sum(Antal))*100),
                    Andel = ifelse(Andel<0.001,Andel,Andel-0.001)) |>
      dplyr::ungroup()

    if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
      list_data <- c(list_data,list("Bransch" = px_df_sum))
    }

    if(returnera_data == TRUE){
      assign("kvalifikationskrav_jmf_bransch", px_df_sum, envir = .GlobalEnv)
    }

    diagramfil <- "kompetenskrav_bransch.png"

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = px_df_sum |>
        dplyr::filter(Branschgrupp != "Okänd verksamhet") |>
        dplyr::mutate(kompetensniva = factor(kompetensniva, levels = c("Enklare yrken","Motsvarande gymnasial kompetens","Motsvarande högskolekompetens","Motsvarande fördjupad högskolekompetens","Chefsyrken","Okänt")[6:1])),
      skickad_x_var = "Branschgrupp",
      skickad_y_var = "Andel",
      skickad_x_grupp = "kompetensniva",
      manual_color = c(rgb(211,211,211, maxColorValue = 255),rddiagram::diagramfarger("rus_sex")),
      diagram_titel = diagram_titel,
      legend_vand_ordning = TRUE,
      legend_byrow = TRUE,
      legend_kolumner = 2,
      diagram_capt = diagram_capt,
      x_axis_lutning = 0,
      diagram_liggande = TRUE,
      x_axis_sort_value = TRUE,
      x_axis_sort_grp = 6,
      vand_sortering = TRUE,
      geom_position_stack = TRUE,
      manual_y_axis_title = "procent",
      stodlinjer_avrunda_fem = stodlinjer_avrunda_fem,
      output_mapp = utmapp,
      filnamn_diagram = diagramfil,
      skriv_till_diagramfil = skriv_diagramfil)

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
