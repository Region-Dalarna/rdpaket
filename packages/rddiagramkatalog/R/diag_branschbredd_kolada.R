#' Diagram: branschbredd (Kolada)
#'
#' Ett stapeldiagram över branschbredd för valda regioner, första och senaste
#' tillgängliga år. Branschbredd är antalet unika branscher i området
#' dividerat med totalt antal branscher enligt SNI2007 på femsiffernivå.
#' Riket visas inte i diagrammet. Data laddas automatiskt ned från Kolada.
#'
#' @param utmapp Mapp där figuren sparas. Måste finnas om
#'   `skriv_diagramfil = TRUE`.
#' @param region_vekt Vilka regioner (regionkoder) som ska väljas. Standard är
#'   Dalarnas kommuner, länet och riket (riket visas inte i diagrammet).
#' @param skriv_diagramfil Skall figuren sparas.
#' @param returnera_data Skall data läggas i R-studios globala miljö.
#' @param returnera_figur Skall en lista med figuren returneras.
#' @param diag_fargvekt Vilka färger som ska användas i diagrammet.
#' @param output_mapp_data Mapp där data sparas som Excelfil. `NA` ger ingen
#'   Excelfil.
#' @param filnamn_data Filnamn för Excelfilen.
#' @param data_namm Namn på dataobjektet som läggs i globala miljön om
#'   `returnera_data = TRUE`.
#'
#' @return En namngiven lista med ett `ggplot`-objekt.
#' @export
diag_branschbredd_kolada <- function(utmapp = rdverktyg::utskriftsmapp(),
                                     region_vekt = rdverktyg::hamtakommuner("20", tamedlan = TRUE, tamedriket = TRUE),
                                     skriv_diagramfil = TRUE,
                                     returnera_data = FALSE,
                                     returnera_figur = TRUE,
                                     diag_fargvekt = rddiagram::diagramfarger("rus_sex"),
                                     output_mapp_data = NA,
                                     filnamn_data = "branschbredd.xlsx",
                                     data_namm = "branschbredd") {

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_data_branschbredd_kolada.R")

  gg_list <- list()

  df <- hamta_data_branschbredd(region = region_vekt,
                                output_mapp = output_mapp_data,
                                filnamn = filnamn_data,
                                tid = 1900:2100,
                                returnera_data = TRUE)

  if (returnera_data == TRUE) {
    assign(data_namm, df, envir = .GlobalEnv)
  }

  diagram_capt <- "Källa: Kolada (RKA)\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Unika branscher i området dividerat med totalt antal branscher enligt SNI2007 på 5-siffernivå"
  diagram_titel <- "Branschbredd"
  diagramfilnamn <- "branschbredd.png"

  gg_obj <- rddiagram::SkapaStapelDiagram(
    skickad_df = df |>
      dplyr::filter(year %in% c(min(year), max(year)),
                    municipality != "Riket") |>
      dplyr::mutate(municipality = rdverktyg::skapa_kortnamn_lan(municipality) |>
                      stringr::str_remove("^Region "),
                    year = as.character(year)),
    skickad_x_var = "municipality",
    skickad_y_var = "Branschbredd",
    skickad_x_grupp = "year",
    manual_x_axis_text_vjust = 1,
    manual_x_axis_text_hjust = 1,
    manual_color = diag_fargvekt,
    stodlinjer_avrunda_fem = TRUE,
    x_axis_sort_value = TRUE,
    x_axis_sort_grp = 2,
    vand_sortering = TRUE,
    manual_y_axis_title = "procent",
    diagram_titel = diagram_titel,
    diagram_capt = diagram_capt,
    output_mapp = utmapp,
    filnamn_diagram = diagramfilnamn,
    skriv_till_diagramfil = skriv_diagramfil)

  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list) <- diagramfilnamn |> stringr::str_remove(".png")

  if (returnera_figur == TRUE) return(gg_list)
}
