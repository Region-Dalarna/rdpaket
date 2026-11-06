#' Diagram: långtidsarbetslöshet över tid, uppdelat på kön
#'
#' Hämtar långtidsarbetslöshet från Kolada och bygger ett stapeldiagram
#' uppdelat på kön.
#'
#' @param region_vekt Regionkod(er), t.ex. `"20"` för Dalarnas län.
#' @param tid_koder Vilka år som ska hämtas, som en sträng `"start:slut"`
#'   (t.ex. `"2011:9999"`, där `"9999"` betyder "till och med senaste
#'   tillgängliga år") eller en vanlig heltalsvektor, t.ex. `2015:2024`.
#' @param output_mapp_data Outputmapp för data.
#' @param utmapp Mapp där diagram ska sparas. Måste finnas om `skriv_diagramfil = TRUE`.
#' @param skriv_diagramfil TRUE om diagram ska skrivas till fil.
#' @param returnera_figur Returnerar en figur.
#' @param diag_fargvekt Valfri färgvektor, standard: `rddiagram::diagramfarger("kon")`.
#' @param returnera_data Skall data returneras.
#'
#' @return En namngiven lista med ett `ggplot`-objekt.
#' @export
diag_langtidsarbetslohet_tidsserie_kolada <- function(region_vekt = "20",
                                                   tid_koder = "2011:9999",
                                                   output_mapp_data = NA,
                                                   utmapp = rdverktyg::utskriftsmapp(),
                                                   skriv_diagramfil = TRUE,
                                                   returnera_figur = TRUE,
                                                   diag_fargvekt = NA,
                                                   returnera_data = FALSE) {

  if (all(is.na(diag_fargvekt))) diag_fargvekt <- rddiagram::diagramfarger("kon")

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  gg_list <- list()

  långtidsarbetslöshet <- rdverktyg::hamta_kolada_df(kpi_id = c("N03926"),
                                          valda_kommuner = region_vekt,
                                          valda_ar = intern_tolka_ar_intervall(tid_koder),
                                          konsuppdelat = TRUE) |>
    dplyr::mutate(kon = tolower(kon))

  if (returnera_data == TRUE) {
    assign("långtidsarbetslöshet", långtidsarbetslöshet, envir = .GlobalEnv)
  }

  diagram_capt <- "Källa: Arbetsförmedlingen (via Kolada)\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Antal invånare 25-64 år (årsmedelvärde år T) som varit öppet arbetslösa eller i program med aktivitetsstöd i minst sex månader,\ndividerat med antal invånare 25-64 år den 31/12 år T-1."

  diagramtitel <- "Långtidsarbetslöshet 25-64 år i Dalarna"
  diagramfilnamn <- "langtidsarbetsloshet_kolada.png"

  gg_obj <- rddiagram::SkapaStapelDiagram(
    skickad_df = långtidsarbetslöshet,
    skickad_x_var = "ar",
    skickad_y_var = "varde",
    skickad_x_grupp = "kon",
    diagram_titel = diagramtitel,
    diagram_capt = diagram_capt,
    manual_y_axis_title = "procent",
    x_axis_lutning = 0,
    manual_color = diag_fargvekt,
    lagg_pa_logga = FALSE,
    output_mapp = utmapp,
    filnamn_diagram = diagramfilnamn,
    skriv_till_diagramfil = skriv_diagramfil)

  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, "\\.png")

  if (returnera_figur == TRUE) {
    return(gg_list)
  }
}
