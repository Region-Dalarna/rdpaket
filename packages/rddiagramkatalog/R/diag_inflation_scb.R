#' Diagram: inflation (KPIF)
#'
#' Ett linjediagram över inflationen i Sverige enligt KPIF
#' (12-månadsförändring) från valt startvärde till senaste månad, med en
#' referenslinje för Riksbankens inflationsmål (2 procent). Data laddas
#' automatiskt ned från SCB:s öppna statistikdatabas.
#'
#' @param utmapp Mapp där figuren sparas. Måste finnas om
#'   `skriv_diagramfil = TRUE`.
#' @param skriv_diagramfil Skall figuren sparas.
#' @param returnera_data Skall data läggas i R-studios globala miljö.
#' @param returnera_figur Skall en lista med figuren returneras.
#' @param diag_fargvekt Vilka färger som ska användas i diagrammet.
#' @param antal_etiketter Intervall mellan visade x-etiketter (i månader).
#' @param startvarde Startvärde (år-månad) för diagrammet. Finns från 1987-01.
#' @param data_namm Namn på dataobjektet som läggs i globala miljön om
#'   `returnera_data = TRUE`.
#'
#' @return En namngiven lista med ett `ggplot`-objekt.
#' @export
diag_inflation_scb <- function(utmapp = rdverktyg::utskriftsmapp(),
                               skriv_diagramfil = TRUE,
                               returnera_data = FALSE,
                               returnera_figur = TRUE,
                               diag_fargvekt = rddiagram::diagramfarger("rus_sex"),
                               antal_etiketter = 24,
                               startvarde = "1993-01",
                               data_namm = "KPI_df") {
  
  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)
  gg_list <- list()
  
  KPIF_df <- pxweb2r::pxweb2_get_data(
    table = "TAB6590",
    query = list(
      ContentsCode = "KPIF, 12-månadsförändring, 2020=100",
      Tid          = "*"
    ), quiet = TRUE) |>
    dplyr::rename(`KPIF, 12-månadsförändring, 2020=100` = value) |>
    dplyr::select(-tabellinnehåll)
  
  KPIF_df <- KPIF_df |>
    dplyr::mutate(ar = substr(månad, 1, 4),
                  manad_long = format(as.Date(paste(ar, stringr::str_sub(månad, 6, 7), "1", sep = "-")), "%B"),
                  Period = paste(ar, stringr::str_sub(månad, 6, 7), sep = "-"),
                  manad_ar = paste0(manad_long, " ", ar)) |>
    dplyr::rename("KPIF" = `KPIF, 12-månadsförändring, 2020=100`) |>
    dplyr::mutate(variabel = "Inflation") |>
    dplyr::filter(Period >= startvarde)
  
  if (returnera_data == TRUE) {
    assign(data_namm, KPIF_df, envir = .GlobalEnv)
  }
  
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
  
  KPI_df_mal <- KPIF_df |>
    dplyr::select(KPIF, manad_ar, Period, variabel) |>
    dplyr::mutate("KPIF" = 2,
                  "variabel" = "Riksbankens inflationsmål") |>
    dplyr::filter(Period >= startvarde)
  
  KPI_utskrift <- rbind(KPIF_df |> dplyr::select(KPIF, manad_ar, Period, variabel), KPI_df_mal)
  
  diagramtitel <- paste0("Inflation i Sverige från ", dplyr::first(KPIF_df$manad_ar),
                         " till ", dplyr::last(KPIF_df$manad_ar))
  diagramfilnamn <- "inflation.png"
  
  gg_obj <- rddiagram::SkapaLinjeDiagram(
    skickad_df = KPI_utskrift |>
      dplyr::mutate(manad_ar = factor(manad_ar, levels = unique(manad_ar), ordered = TRUE)),
    skickad_x_var = "manad_ar",
    skickad_y_var = "KPIF",
    skickad_x_grupp = "variabel",
    berakna_index = FALSE,
    stodlinjer_avrunda_fem = FALSE,
    manual_color = c(diag_fargvekt[1], "#000000"),
    x_axis_var_xe_etikett_ta_bort_nast_sista_vardet = TRUE,
    diagram_titel = diagramtitel,
    diagram_capt = diagram_capt,
    output_mapp = utmapp,
    manual_y_axis_title = "procent",
    x_axis_visa_var_xe_etikett = antal_etiketter,
    filnamn_diagram = diagramfilnamn,
    skriv_till_diagramfil = skriv_diagramfil)
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list) <- diagramfilnamn |> stringr::str_remove(".png")
  
  if (returnera_figur == TRUE) return(gg_list)
}