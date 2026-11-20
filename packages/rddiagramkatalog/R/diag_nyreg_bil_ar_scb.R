#' Diagram: nyregistrerade personbilar per år (Trafikanalys)
#'
#' Stapeldiagram per år över antal nyregistrerade personbilar för vald
#' region. Används primärt i rapporten "Det samhällsekonomiska läget i
#' Dalarna". Data laddas automatiskt ned från SCB (Trafikanalys).
#'
#' @param region_vekt Vilken region som ska väljas.
#' @param utmapp Mapp där figuren sparas. Måste finnas om
#'   `skriv_diagramfil = TRUE`.
#' @param skriv_diagramfil Skall figuren sparas.
#' @param returnera_data Skall data läggas i R-studios globala miljö.
#' @param returnera_figur Skall en lista med figuren returneras.
#' @param data_namm Namn på dataobjektet som läggs i globala miljön om
#'   `returnera_data = TRUE`.
#'
#' @return En namngiven lista med ett `ggplot`-objekt.
#' @export
diag_nyreg_bil_ar_scb <- function(region_vekt = "20",
                                  utmapp = rdverktyg::utskriftsmapp(),
                                  skriv_diagramfil = FALSE,
                                  returnera_data = FALSE,
                                  returnera_figur = TRUE,
                                  data_namm = "fordon_df_ar") {
  
  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)
  gg_list <- list()
  
  # Nyregistrerade personbilar, månad: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__TK__TK1001__TK1001A/PersBilarDrivMedel/
  fordon_df <- pxweb2r::pxweb2_get_data(
    table = "TAB3277",
    query = list(
      Region       = region_vekt,
      Drivmedel    = NA,
      ContentsCode = "*",
      Tid          = "*"
    ), quiet = TRUE) |>
    dplyr::rename(tid = månad,
                  antal = value)
  
  # Bearbetar data på månadsbasis för att kunna lägga till senaste månad i caption nedan.
  fordon_df <- fordon_df |>
    dplyr::mutate(år = stringr::str_sub(tid, 1, 4),
                  månad = format(as.Date(paste(år, stringr::str_sub(tid, 6, 7), "1", sep = "-")), "%b"),
                  manad_long = format(as.Date(paste(år, stringr::str_sub(tid, 6, 7), "1", sep = "-")), "%B"),
                  år_månad = paste0(år, " - ", månad),
                  månad_år = paste0(månad, " ", år)) |>
    dplyr::select(-tid)
  
  senaste_ar <- max(fordon_df$år)
  senaste_manad <- dplyr::last(fordon_df$manad_long)
  
  # Grupperar på årsbasis
  fordon_df_ar <- fordon_df |>
    dplyr::mutate(år = as.integer(år)) |>
    dplyr::group_by(region_kod, region, år) |>
    dplyr::summarise(
      antal = sum(antal, na.rm = TRUE),
      .groups = "drop") |>
    dplyr::arrange(region_kod, år)
  
  if (returnera_data == TRUE) {
    assign(data_namm, fordon_df_ar, envir = .GlobalEnv)
  }
  
  diagram_titel <- paste0("Nyregistrerade personbilar i ", rdverktyg::skapa_kortnamn_lan(unique(fordon_df_ar$region)))
  diagramfilnamn <- paste0("antal_nyregistrerade_personbilar_ar_", rdverktyg::skapa_kortnamn_lan(unique(fordon_df_ar$region)), ".png")
  diagram_capt <- paste0("Källa: SCB (Trafikanalys)\nBearbetning: Samhällsanalys, Region Dalarna\nData för ",
                         senaste_ar, " till och med ", senaste_manad)
  
  gg_obj <- rddiagram::SkapaStapelDiagram(
    skickad_df = fordon_df_ar |> dplyr::mutate(år = as.character(år)),
    skickad_x_var = "år",
    skickad_y_var = "antal",
    manual_color = rddiagram::diagramfarger("rus_sex"),
    diagram_titel = diagram_titel,
    diagram_capt = diagram_capt,
    output_mapp = utmapp,
    stodlinjer_avrunda_fem = TRUE,
    manual_x_axis_text_vjust = 1,
    manual_x_axis_text_hjust = 1,
    manual_y_axis_title = "Antal",
    filnamn_diagram = diagramfilnamn,
    skriv_till_diagramfil = skriv_diagramfil)
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list) <- stringr::str_remove(diagramfilnamn, ".png")
  
  if (returnera_figur == TRUE) {
    return(gg_list)
  }
}