#' Diagram: andel förvärvsarbetande per bransch
#'
#' Skapar diagram för andelen förvärvsarbetande inom olika branscher, dels
#' på länsnivå, dels på kommunnivå. Enbart senaste år och ingen
#' uppdelning på kön.
#'
#' @param region_vekt Region vi är intresserade av.
#' @param utmapp Mapp där figuren sparas. Måste finnas om `skriv_diagramfil = TRUE`.
#' @param output_mapp_data Mapp där data sparas.
#' @param filnamn_data Filnamn på sparad data.
#' @param diag_fargvekt Vilka färger skall användas i diagram.
#' @param skriv_diagramfil Om TRUE sparas figuren till `utmapp`.
#' @param diagram_capt Diagrammets källtext.
#' @param diag_lan Skapar ett diagram där län jämförs med riket.
#' @param diag_kommun Motsvarande diagram där kommuner jämförs med länet.
#' @param diag_lan_antal Antal för länet, uppdelat på kvinnor och män.
#' @param returnera_figur Skall figur returneras (i en lista).
#' @param returnera_data Skall data returneras (till R-studios globala miljö).
#'
#' @return En namngiven lista med `ggplot`-objekt.
#' @export
diag_sysselsatta_andel_scb <- function(region_vekt = "20",
                                   utmapp = rdverktyg::utskriftsmapp(),
                                   output_mapp_data = NA,
                                   filnamn_data = "andel_forvarvsarbetande.xlsx",
                                   diag_fargvekt = rddiagram::diagramfarger("rus_sex"),
                                   skriv_diagramfil = TRUE,
                                   diagram_capt = "Källa: BAS i SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Branschens andel av totalt antal förvärvsarbetande",
                                   diag_lan = TRUE,
                                   diag_kommun = TRUE,
                                   diag_lan_antal = FALSE,
                                   returnera_figur = TRUE,
                                   returnera_data = FALSE){

  # ========================================== Allmän info ============================================

  # 1: Skapar diagram för andelen förvärvsarbetande inom olika branscher, dels på länsnivå, dels på kommunnivå. Enbart senaste år och ingen uppdelning på kön
  # 1: Antal förvärvsarbetande senaste observation uppdelat på kön
  # ========================================== Inställningar ============================================

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  # Skapar en tom vektor som skall innehålla objektnamn
  objektnamn <- c()
  # Lista som används för att lägg till dataset till Excelfil (som sparas)
  list_data <- list()

  vald_region = rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(region_vekt)$region)

# =============================================== API-uttag ===============================================

  # hämta branschnyckel från Region Dalarnas github-repo depot
  branschtabell <- intern_hamta_branschnyckel()

  df <- pxweb2r::pxweb2_get_data(
    table = "TAB3784",
    query = list(
      Region = rdverktyg::hamtakommuner(region_vekt,tamedlan = TRUE,tamedriket = TRUE),
      Kon = c("kvinnor","män"),
      SNI2007 = "*",
      Fodelseregion = "totalt",
      ContentsCode = "sysselsatta efter arbetsställets belägenhet",
      Tid = "9999"
    ), quiet = TRUE) |>
    dplyr::mutate(`näringsgren sni 2007_kod` = ifelse(`näringsgren sni 2007_kod` == "US", "00", `näringsgren sni 2007_kod`)) |>
      dplyr::filter(`näringsgren sni 2007_kod` != "A-U+US") |>
        dplyr::rename(branschkod = `näringsgren sni 2007_kod`,
               regionkod = region_kod,
               `sysselsatta efter arbetsställets belägenhet` = value) |>
          dplyr::left_join(dplyr::select(branschtabell, Br15kod, bransch = Bransch), by = c("branschkod" = "Br15kod")) |>
      dplyr::select(-`näringsgren SNI 2007`,-tabellinnehåll,-födelseregion) |>
        dplyr::relocate(branschkod, .after = region) |>
          dplyr::relocate(bransch, .after = branschkod) |>
            rdverktyg::manader_bearbeta_scbtabeller()

  # Summerar på region och sektor
  df_sum <- df |>
    dplyr::group_by(år, månad_år, tid, region, bransch) |>
        dplyr::summarize("Antal" = sum(`sysselsatta efter arbetsställets belägenhet`), .groups = "drop_last") |>
      dplyr::mutate(andel = (Antal/sum(Antal))*100,
             region = rdverktyg::skapa_kortnamn_lan(region,byt_ut_riket_mot_sverige = TRUE),
             manad_txt = format(as.Date(paste0(stringr::str_sub(tid, 6,7), "-01"), format = "%m-%d"), "%B"))

  if(diag_lan == TRUE | diag_kommun == TRUE){
    if(returnera_data == TRUE){
      assign("andel_forvarvsarbetande_bransch", df_sum, envir = .GlobalEnv)
    }

    if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
      list_data <- c(list_data,list("Andel per bransch" = df_sum))
    }

  }

  if(diag_lan==TRUE){

    diagram_titel <- paste0("Andel förvärvsarbetande 16-74 år\nper bransch i ",unique(df_sum$manad_txt), " ", unique(df_sum$år))
    diagramfil <- "andel_per_bransch.png"
    objektnamn <- c(objektnamn,"andel_per_bransch")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = df_sum |>
        dplyr::filter(region %in% c("Sverige",vald_region),bransch != "Okänt") |>
        dplyr::mutate(bransch = stringr::str_wrap(bransch,20)),
      skickad_x_var = "bransch",
      skickad_y_var = "andel",
      skickad_x_grupp = "region",
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      manual_color = diag_fargvekt,
      x_axis_sort_value = TRUE,
      manual_y_axis_title = "procent",
      stodlinjer_avrunda_fem = TRUE,
      diagram_titel = diagram_titel,
      diagram_capt = diagram_capt,
      output_mapp = utmapp,
      filnamn_diagram = diagramfil,
      skriv_till_diagramfil = skriv_diagramfil)

    gg_list <- c(gg_list, list(gg_obj))

  }

  # Vektor som används för att skapa figurer för samtliga kommuner (mha en loop)
  kommun_vektor <- rdverktyg::hamtaregion_kod_namn(rdverktyg::hamtakommuner(region_vekt,tamedlan=FALSE,tamedriket=FALSE))[2]

  # Loop som skapar diagram för samtliga Dalarnas kommuner
  j=1
  if(diag_kommun==TRUE){
    while(j <= length(kommun_vektor$region)){
      diagram_titel <- paste0("Andel förvärvsarbetande 16-74 år per bransch i ",unique(df_sum$manad_txt), " ", unique(df_sum$år))
      diagram_typ <- paste0("andel_per_bransch","_",kommun_vektor$region[j])
      diagramfil <- paste0(diagram_typ,".png")
      objektnamn <- c(objektnamn,diagram_typ)

      # För att ort respektive län skall skrivas i rätt ordning (Dalarna först) så skapas en faktorvariabel med mutate nedan
      gg_obj <- rddiagram::SkapaStapelDiagram(
        skickad_df = df_sum |>
          dplyr::filter(region %in% c(vald_region,kommun_vektor$region[j]),bransch != "Okänt") |>
          dplyr::mutate(region = factor(region, levels = c(vald_region,kommun_vektor$region[j])),
                        bransch = stringr::str_wrap(bransch,20)),
        skickad_x_var = "bransch",
        skickad_y_var = "andel",
        skickad_x_grupp = "region",
        manual_x_axis_text_vjust = 1,
        manual_x_axis_text_hjust = 1,
        manual_color = diag_fargvekt,
        x_axis_sort_value = TRUE,
        x_axis_sort_grp = 2,
        vand_sortering = TRUE,
        manual_y_axis_title = "procent",
        stodlinjer_avrunda_fem = TRUE,
        diagram_titel = diagram_titel,
        diagram_capt = diagram_capt,
        output_mapp = utmapp,
        filnamn_diagram = diagramfil,
        skriv_till_diagramfil = skriv_diagramfil)

      gg_list <- c(gg_list, list(gg_obj))
      j=j+1
    }

  }

  if(diag_lan_antal==TRUE){

    # Summerar på region och sektor
    df_kon <- df |>
      dplyr::rename("Antal" = `sysselsatta efter arbetsställets belägenhet`) |>
        dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region,byt_ut_riket_mot_sverige = TRUE),
               manad_txt = format(as.Date(paste0(stringr::str_sub(tid, 6,7), "-01"), format = "%m-%d"), "%B")) |>
          dplyr::select(år, månad_år, manad_txt, region, kön, bransch, Antal) |>
            dplyr::filter(region == vald_region)

    if(returnera_data == TRUE){
      assign("antal_forvarvsarbetande_bransch", df_kon, envir = .GlobalEnv)
    }

    if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
      list_data <- c(list_data,list("Antal per bransch" = df_kon))
    }

    diagram_titel <- paste0("Antal förvärvsarbetande 16-74 år per bransch\ni ",vald_region," i ",unique(df_sum$manad_txt), " ", unique(df_sum$år))
    diagramfil <- "antal_per_bransch.png"
    objektnamn <- c(objektnamn,"antal_per_bransch")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = df_kon |>
        dplyr::filter(bransch != "Okänt") |>
        dplyr::mutate(bransch = stringr::str_wrap(bransch,20)),
      skickad_x_var = "bransch",
      skickad_y_var = "Antal",
      skickad_x_grupp = "kön",
      manual_x_axis_text_vjust=1,
      manual_x_axis_text_hjust=1,
      manual_color = rddiagram::diagramfarger("kon"),
      x_axis_sort_value = TRUE,
      manual_y_axis_title = "",
      stodlinjer_avrunda_fem = TRUE,
      diagram_titel = diagram_titel,
      diagram_capt = diagram_capt,
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
  if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
    openxlsx::write.xlsx(list_data,paste0(output_mapp_data,filnamn_data))
  }

  if(returnera_figur == TRUE) return(gg_list)

}
