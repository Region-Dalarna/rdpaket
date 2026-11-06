#' Diagram: långtidsarbetslöshet över tid, uppdelat på kön
#'
#' Hämtar långtidsarbetslöshet från Kolada och bygger ett stapeldiagram
#' uppdelat på kön.
#'
#' @param region_vekt Regionkod(er), t.ex. `"20"` för Dalarnas län.
#' @param output_mapp_data Outputmapp för data.
#' @param output_mapp_figur Outputmapp för figur.
#' @param spara_figur Sparar figuren till `output_mapp_figur`.
#' @param returnera_figur Returnerar en figur.
#' @param valda_farger Valfri färgvektor, standard: `rddiagram::diagramfarger("kon")`.
#' @param returnera_data Skall data returneras.
#'
#' @return En namngiven lista med ett `ggplot`-objekt.
#' @export
diagram_langtidsarbetslohet_tidsserie <- function(region_vekt = "20",
                                                   output_mapp_data = NA,
                                                   output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                                   spara_figur = TRUE,
                                                   returnera_figur = TRUE,
                                                   valda_farger = NA,
                                                   returnera_data = FALSE) {

  if (all(is.na(valda_farger))) valda_farger <- rddiagram::diagramfarger("kon")

  gg_list <- list()

  långtidsarbetslöshet <- rdverktyg::hamta_kolada_df(kpi_id = c("N03926"),
                                          valda_kommuner = region_vekt,
                                          valda_ar = 2011:2100,
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
    manual_color = valda_farger,
    lagg_pa_logga = FALSE,
    output_mapp = output_mapp_figur,
    filnamn_diagram = diagramfilnamn,
    skriv_till_diagramfil = spara_figur)

  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, "\\.png")

  if (returnera_figur == TRUE) {
    return(gg_list)
  }
}
