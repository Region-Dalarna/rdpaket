#' Diagram: småhuspriser
#'
#' Två diagram över priser på småhus: ett linjediagram över den indexerade
#' prisutvecklingen för valt län jämfört med riket, och ett stapeldiagram som
#' jämför medelpriset mellan Sveriges län för senaste period. Data laddas
#' automatiskt ned från SCB:s öppna statistikdatabas.
#'
#' @param region_vekt Vilken region (län) som ska väljas som fokus.
#' @param utmapp Mapp där figurerna sparas. Måste finnas om
#'   `skriv_diagramfil = TRUE`.
#' @param skriv_diagramfil Skall figurerna sparas.
#' @param returnera_data Skall data läggas i R-studios globala miljö.
#' @param returnera_figur Skall en lista med figurer returneras.
#' @param stodlinjer_avrunda_fem TRUE om stödlinjerna ska avrundas till fem.
#' @param diag_fargvekt Vilka färger som ska användas i linjediagrammet.
#' @param diag_fargvekt_lan Vilka färger som ska användas i länsjämförelsen.
#' @param diag_tidsserie Skapa diagram: prisutveckling över tid.
#' @param diag_lan_jmf Skapa diagram: länsjämförelse.
#' @param startar Startår för diagrammet. Finns från 1997.
#' @param data_namm Namn på dataobjektet som läggs i globala miljön om
#'   `returnera_data = TRUE`.
#'
#' @return En namngiven lista med `ggplot`-objekt.
#' @export
diag_smahuspriser_scb <- function(region_vekt = "20",
                                  utmapp = rdverktyg::utskriftsmapp(),
                                  skriv_diagramfil = TRUE,
                                  returnera_data = FALSE,
                                  returnera_figur = TRUE,
                                  stodlinjer_avrunda_fem = TRUE,
                                  diag_fargvekt = rddiagram::diagramfarger("rus_sex"),
                                  diag_fargvekt_lan = rddiagram::diagramfarger("rus_tre_fokus"),
                                  diag_tidsserie = TRUE,
                                  diag_lan_jmf = TRUE,
                                  startar = "2010",
                                  data_namm = "priser_df") {
  
  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)
  gg_list <- list()
  objektnamn <- c()
  
  smahuspriser_df <- pxweb2r::pxweb2_get_data(
    table = "TAB3656",
    query = list(
      Region       = rdverktyg::hamtaAllaLan(),
      ContentsCode = "Medelpris, 1000-tals kronor",
      Tid          = "*"
    ), quiet = TRUE) |>
    dplyr::rename(`Medelpris, 1000-tals kronor` = value,
                  regionkod = region_kod) |>
    dplyr::select(-tabellinnehåll)
  
  priser_df <- smahuspriser_df |>
    dplyr::mutate(ar = substr(månad, 1, 4),
                  manad_long = format(as.Date(paste(ar, stringr::str_sub(månad, 6, 7), "1", sep = "-")), "%B"),
                  Period = paste(ar, stringr::str_sub(månad, 6, 7), sep = "-"),
                  manad_ar = paste0(manad_long, " ", ar)) |>
    dplyr::select(-månad) |>
    dplyr::rename("Medelpris" = `Medelpris, 1000-tals kronor`) |>
    dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region, byt_ut_riket_mot_sverige = TRUE)) |>
    dplyr::filter(ar >= startar)
  
  if (returnera_data == TRUE) {
    assign(data_namm, priser_df, envir = .GlobalEnv)
  }
  
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."
  
  jmf_vekt <- "00"
  ValdGeografi <- rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(region_vekt)$region, byt_ut_riket_mot_sverige = TRUE)
  jmf_geografi <- rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(jmf_vekt)$region, byt_ut_riket_mot_sverige = TRUE)
  
  if (diag_tidsserie == TRUE) {
    
    diagramtitel <- paste0("Förändring i priser på småhus mellan ",
                           dplyr::first(priser_df$manad_ar), " och ", dplyr::last(priser_df$manad_ar))
    diagramtitel <- stringr::str_wrap(diagramtitel, 40)
    diagramfilnamn <- "smahuspriser_tidserie.png"
    objektnamn <- c(objektnamn, diagramfilnamn |> stringr::str_remove(".png"))
    
    gg_obj <- rddiagram::SkapaLinjeDiagram(
      skickad_df = priser_df |>
        dplyr::filter(ar >= startar, region %in% c(jmf_geografi, ValdGeografi)) |>
        dplyr::mutate(manad_ar = factor(manad_ar, levels = unique(manad_ar), ordered = TRUE)) |>
        dplyr::group_by(region) |>
        dplyr::mutate("Index (startvärde 100)" = (Medelpris / dplyr::first(Medelpris)) * 100),
      skickad_x_var = "manad_ar",
      skickad_y_var = "Index (startvärde 100)",
      skickad_x_grupp = "region",
      berakna_index = FALSE,
      stodlinjer_avrunda_fem = stodlinjer_avrunda_fem,
      manual_color = diag_fargvekt,
      diagram_titel = diagramtitel,
      diagram_capt = diagram_capt,
      output_mapp = utmapp,
      x_axis_visa_var_xe_etikett = 12,
      inkludera_sista_vardet_var_xe_etikett = FALSE,
      x_axis_var_xe_etikett_ta_bort_nast_sista_vardet = TRUE,
      filnamn_diagram = diagramfilnamn,
      skriv_till_diagramfil = skriv_diagramfil)
    
    gg_list <- c(gg_list, list(gg_obj))
  }
  
  if (diag_lan_jmf == TRUE) {
    diagramtitel <- paste0("Småhuspriser i ", dplyr::last(priser_df$manad_long), " ", max(priser_df$ar))
    diagramfilnamn <- "smahuspriser_lan.png"
    objektnamn <- c(objektnamn, diagramfilnamn |> stringr::str_remove(".png"))
    
    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = priser_df |>
        dplyr::filter(Period == dplyr::last(Period)) |>
        dplyr::mutate(fokus = ifelse(region == ValdGeografi, 1,
                                     ifelse(region == jmf_geografi, 2, 0))),
      skickad_x_var = "region",
      skickad_y_var = "Medelpris",
      x_var_fokus = "fokus",
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      stodlinjer_avrunda_fem = stodlinjer_avrunda_fem,
      manual_color = diag_fargvekt_lan,
      diagram_titel = diagramtitel,
      manual_y_axis_title = "Medelpris (tkr)",
      x_axis_sort_value = TRUE,
      diagram_capt = diagram_capt,
      output_mapp = utmapp,
      filnamn_diagram = diagramfilnamn,
      skriv_till_diagramfil = skriv_diagramfil)
    
    gg_list <- c(gg_list, list(gg_obj))
  }
  
  names(gg_list) <- objektnamn
  
  if (returnera_figur == TRUE) return(gg_list)
}