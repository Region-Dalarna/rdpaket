#' Diagram: nystartade företag och konkurser (Kolada)
#'
#' Två stapeldiagram över antal nystartade företag respektive antal konkurser
#' per 1000 invånare (16-64 år) för valda regioner. Diagrammen visar de två
#' senaste åren samt medelvärdet för femårsperioden dessförinnan. Data laddas
#' automatiskt ned från Kolada (källa SCB).
#'
#' @param utmapp Mapp där figuren sparas. Måste finnas om
#'   `skriv_diagramfil = TRUE`.
#' @param region_vekt Vilka regioner (regionkoder) som ska väljas. Standard är
#'   Dalarnas kommuner, länet och riket.
#' @param skriv_diagramfil Skall figurerna sparas.
#' @param returnera_data Skall data läggas i R-studios globala miljö.
#' @param returnera_figur Skall en lista med figurer returneras.
#' @param diag_fargvekt Vilka färger som ska användas i diagrammen.
#' @param cont_cod Vilka nyckeltal som ska hämtas: "N00999" för nystartade
#'   företag, "N00926" för konkurser. Ett diagram skapas per nyckeltal.
#' @param output_mapp_data Mapp där data sparas som Excelfil. `NA` ger ingen
#'   Excelfil.
#' @param filnamn_data Filnamn för Excelfilen.
#' @param data_namm_nystartade Namn på dataobjektet för nystartade företag som
#'   läggs i globala miljön om `returnera_data = TRUE`.
#' @param data_namm_konkurser Namn på dataobjektet för konkurser som läggs i
#'   globala miljön om `returnera_data = TRUE`.
#'
#' @return En namngiven lista med `ggplot`-objekt.
#' @export
diag_nystartade_konkurser_kolada <- function(utmapp = rdverktyg::utskriftsmapp(),
                                             region_vekt = rdverktyg::hamtakommuner("20", tamedlan = TRUE, tamedriket = TRUE),
                                             skriv_diagramfil = TRUE,
                                             returnera_data = FALSE,
                                             returnera_figur = TRUE,
                                             diag_fargvekt = rddiagram::diagramfarger("rus_sex"),
                                             cont_cod = c("N00999", "N00926"),
                                             output_mapp_data = NA,
                                             filnamn_data = "nystartade_konkurser.xlsx",
                                             data_namm_nystartade = "Nystartade",
                                             data_namm_konkurser = "Konkurser") {

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_nystart_konkurser_kolada.R")

  gg_list <- list()
  lista_data <- list() # Används för att spara till Excel
  spara_data <- !is.na(output_mapp_data) && !is.na(filnamn_data)

  ny_konk_list <- hamta_data_nystartade_konkurser(region = region_vekt,
                                                  output_mapp = NA, # Data sparas i ett senare skede
                                                  cont_cod = cont_cod,
                                                  tid = 1900:2100,
                                                  returnera_data = TRUE)

  # Kortnamn för län ("Region Dalarna" -> "Dalarna") och riket ("Riket" -> "Sverige")
  justera_regionnamn <- function(df) {
    df |>
      dplyr::mutate(municipality = stringr::str_remove(municipality, "^Region "),
                    municipality = dplyr::if_else(municipality == "Riket", "Sverige", municipality))
  }

  if ("N00999" %in% cont_cod) {

    senaste_ar_nystartade <- max(ny_konk_list[["Nystartade"]]$year)
    medel_ar_nystartade <- (senaste_ar_nystartade - 6):(senaste_ar_nystartade - 2)
    valda_ar_nystartade <- paste0("medelvärde ", min(medel_ar_nystartade), "-", max(medel_ar_nystartade))

    # Medelvärde för de fem år som föregår näst senaste år
    nystartade_medel <- ny_konk_list[["Nystartade"]] |>
      dplyr::filter(year %in% medel_ar_nystartade) |>
      dplyr::group_by(municipality) |>
      dplyr::summarise(nystartade_ftg = mean(nystartade_ftg), .groups = "drop") |>
      dplyr::mutate(year = valda_ar_nystartade)

    nystartade_utskrift <- dplyr::bind_rows(
      ny_konk_list[["Nystartade"]] |>
        dplyr::filter(year %in% c(senaste_ar_nystartade, senaste_ar_nystartade - 1)) |>
        dplyr::mutate(year = as.character(year)),
      nystartade_medel)

    if (spara_data) {
      lista_data[["Nystartade"]] <- nystartade_utskrift
    }

    if (returnera_data == TRUE) {
      assign(data_namm_nystartade, nystartade_utskrift, envir = .GlobalEnv)
    }

    diagram_capt <- "Källa: SCB (via Kolada/RKA)\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Ett nystartat företag definieras enligt Eurostats rekommendation, dvs. helt nya företag exklusive\nombildningar av befintliga företag. Enskilda näringsidkare utan registrerat firmanamn hos Bolagsverket ingår."
    diagram_titel <- "Antal nystartade företag per 1000 invånare (16-64 år)"
    diagramfilnamn <- "nystartade.png"

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = nystartade_utskrift |>
        justera_regionnamn() |>
        dplyr::mutate(year = factor(year, levels = c(valda_ar_nystartade,
                                                     as.character(senaste_ar_nystartade - 1),
                                                     as.character(senaste_ar_nystartade)))),
      skickad_x_var = "municipality",
      skickad_y_var = "nystartade_ftg",
      skickad_x_grupp = "year",
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      manual_color = diag_fargvekt,
      x_axis_sort_value = TRUE,
      x_axis_sort_grp = 3,
      vand_sortering = TRUE,
      dataetiketter = FALSE,
      stodlinjer_avrunda_fem = TRUE,
      dataetiketter_antal_dec = 0,
      manual_y_axis_title = "Nystartade företag/1000 invånare",
      diagram_titel = diagram_titel,
      diagram_capt = diagram_capt,
      output_mapp = utmapp,
      filnamn_diagram = diagramfilnamn,
      skriv_till_diagramfil = skriv_diagramfil)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn |> stringr::str_remove(".png")
  }

  if ("N00926" %in% cont_cod) {

    senaste_ar_konk <- max(ny_konk_list[["Konkurser"]]$year)
    medel_ar_konk <- (senaste_ar_konk - 6):(senaste_ar_konk - 2)
    valda_ar_konk <- paste0("medelvärde ", min(medel_ar_konk), "-", max(medel_ar_konk))

    # Medelvärde för de fem år som föregår näst senaste år
    konkurser_medel <- ny_konk_list[["Konkurser"]] |>
      dplyr::filter(year %in% medel_ar_konk) |>
      dplyr::group_by(municipality) |>
      dplyr::summarise(konkurser = mean(konkurser), .groups = "drop") |>
      dplyr::mutate(year = valda_ar_konk)

    konkurser_utskrift <- dplyr::bind_rows(
      ny_konk_list[["Konkurser"]] |>
        dplyr::filter(year %in% c(senaste_ar_konk, senaste_ar_konk - 1)) |>
        dplyr::mutate(year = as.character(year)),
      konkurser_medel)

    if (spara_data) {
      lista_data[["Konkurser"]] <- konkurser_utskrift
    }

    if (returnera_data == TRUE) {
      assign(data_namm_konkurser, konkurser_utskrift, envir = .GlobalEnv)
    }

    diagram_capt <- "Källa: SCB (via Kolada/RKA)\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Statistiken innehåller samtliga inledda konkurser för företag och privatpersoner."
    diagram_titel <- "Antal konkurser per 1000 invånare (16-64 år)"
    diagramfilnamn <- "konkurser.png"

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = konkurser_utskrift |>
        justera_regionnamn() |>
        dplyr::mutate(year = factor(year, levels = c(valda_ar_konk,
                                                     as.character(senaste_ar_konk - 1),
                                                     as.character(senaste_ar_konk)))),
      skickad_x_var = "municipality",
      skickad_y_var = "konkurser",
      skickad_x_grupp = "year",
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      manual_color = diag_fargvekt,
      x_axis_sort_value = TRUE,
      x_axis_sort_grp = 3,
      vand_sortering = TRUE,
      dataetiketter = FALSE,
      stodlinjer_avrunda_fem = TRUE,
      dataetiketter_antal_dec = 0,
      manual_y_axis_title = "Konkurser/1000 invånare",
      diagram_titel = diagram_titel,
      diagram_capt = diagram_capt,
      output_mapp = utmapp,
      filnamn_diagram = diagramfilnamn,
      skriv_till_diagramfil = skriv_diagramfil)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn |> stringr::str_remove(".png")
  }

  if (spara_data && length(lista_data) > 0) {
    openxlsx::write.xlsx(lista_data, file.path(output_mapp_data, filnamn_data))
  }

  if (returnera_figur == TRUE) return(gg_list)
}
