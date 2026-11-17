#' Diagram: yrken med högst andel utrikes födda som invandrat de senaste 10 åren
#'
#' Liggande stapeldiagram över de yrken (SSYK3) som har högst andel utrikes födda som invandrat under de senaste
#' tio åren, för en eller flera regioner. Med flera regioner kan diagrammet göras som ett facetdiagram (en panel
#' per region) eller som ett diagram per region. Data är en manuellt underhållen ögonblicksbild ur Regionala
#' utvecklingsdatabasen (SCB), vyn NMS, en Excel-fil på G:, och hämtas alltså inte live från SCB. Filen innehåller
#' alla kommuner i Sverige.
#'
#' Excel-filen ska ha kolumnerna `SSYK3`, `SSYK3_namn`, `regionkod`, `region`, `ar` och `andel`. Rader med
#' `SSYK3 == "***"` (okänt yrke) tas bort. Senaste året i filen används för varje region.
#'
#' @param datafil Sökväg till Excel-filen.
#' @param region_vekt En eller flera regionkoder (län eller kommuner). Standard `"20"` (Dalarna).
#' @param antal_storsta_yrken Hur många yrken som visas per region. Standard 15.
#' @param diag_facet `TRUE` (standard) ger ett facetdiagram med en panel per region när flera regioner valts, `FALSE`
#'   ger ett diagram per region. Med en region spelar parametern ingen roll.
#' @param visa_logga_i_diagram `TRUE` om logga ska visas i diagrammet.
#' @param logga_sokvag Sökväg till logga. `NA` använder standardloggan.
#' @param diagram_capt Diagrammets källtext.
#' @param utmapp Mapp där diagrammen sparas. Måste finnas om `skriv_diagramfil = TRUE`.
#' @param skriv_diagramfil `TRUE` om diagrammen ska skrivas till fil.
#' @param returnera_data `TRUE` lägger data i `.GlobalEnv` som `hogst_andel_inv_yrke`.
#' @param returnera_figur `TRUE` (standard) returnerar diagrammen.
#'
#' @return En namngiven lista med `ggplot`-objekt, ett eller ett per region.
#' @export
diag_hogst_andel_utr_fodda_inv_sen_10_aren_ssyk3_mikrodata_scb <- function(
    datafil = "G:/Samhällsanalys/Statistik/data_fran_nms_db/andel_nyanlanda_per_yrke.xlsx",
    region_vekt = "20",
    antal_storsta_yrken = 15,
    diag_facet = TRUE,
    visa_logga_i_diagram = TRUE,
    logga_sokvag = NA,
    diagram_capt = "Källa: Regionala utvecklingsdatabasen (SCB), NMS\nBearbetning: Samhällsanalys, Region Dalarna\nEndast yrken med minst 20 förvärvsarbetande har tagits med i diagrammet.",
    utmapp = rdverktyg::utskriftsmapp(),
    skriv_diagramfil = TRUE,
    returnera_data = FALSE,
    returnera_figur = TRUE
) {

  # ========================================== Inställningar ============================================

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)
  region_vekt <- unique(as.character(region_vekt))
  if (length(region_vekt) == 1) diag_facet <- FALSE

  # ========================================== Läser in data ============================================
  # Filen på G: är en manuellt underhållen ögonblicksbild.

  rdverktyg::sakerstall_indatafil(datafil)
  yrke_df <- readxl::read_xlsx(datafil)

  saknas <- setdiff(c("SSYK3", "SSYK3_namn", "regionkod", "region", "ar", "andel"), names(yrke_df))
  if (length(saknas) > 0) stop("Kolumnerna saknas i ", datafil, ": ", paste(saknas, collapse = ", "), ".", call. = FALSE)

  yrke_df <- yrke_df |>
    dplyr::filter(SSYK3 != "***", regionkod %in% region_vekt)
  okanda <- setdiff(region_vekt, yrke_df$regionkod)
  if (length(okanda) > 0) stop("Inga data för regionkod: ", paste(okanda, collapse = ", "), " i ", datafil, ".", call. = FALSE)

  # de yrken med högst andel i varje regions senaste år
  chart_df <- purrr::map_dfr(region_vekt, \(kod) {
    yrke_df |>
      dplyr::filter(regionkod == kod, ar == max(ar)) |>
      dplyr::arrange(dplyr::desc(andel)) |>
      dplyr::slice_head(n = antal_storsta_yrken)
  })

  if (returnera_data) assign("hogst_andel_inv_yrke", chart_df, envir = .GlobalEnv)

  # ========================================== Diagram ==================================================

  skapa <- function(kod) {
    df <- chart_df[chart_df$regionkod %in% kod, ]
    ar_txt <- if (length(unique(df$ar)) == 1) paste0(" år ", unique(df$ar)) else ""
    region_txt <- if (diag_facet) "" else paste0(" i ", unique(df$region))
    filnamn <- paste0("hogst_andel_nyanlanda_per_yrke_", paste(kod, collapse = "_"), "_ar",
                      paste(unique(df$ar), collapse = "_"), ".png")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = df,
      skickad_x_var = "SSYK3_namn",
      skickad_y_var = "andel",
      skickad_x_grupp = if (length(unique(df$ar)) == 1) NA else "ar",
      diagram_titel = paste0("Högst andel utrikes födda som invandrat de senaste 10 åren per yrke", region_txt, ar_txt),
      diagram_capt = diagram_capt,
      x_axis_sort_value = TRUE,
      x_axis_lutning = 0,
      manual_y_axis_title = "procent",
      manual_color = rddiagram::diagramfarger("rd_gron")[1],
      stodlinjer_avrunda_fem = TRUE,
      diagram_liggande = TRUE,
      lagg_pa_logga = visa_logga_i_diagram,
      logga_path = logga_sokvag,
      facet_grp = if (diag_facet) "region" else NULL,
      facet_sort = TRUE,
      facet_scale = "free_y",
      facet_kolumner = 2,
      output_mapp = utmapp,
      filnamn_diagram = filnamn,
      skriv_till_diagramfil = skriv_diagramfil)

    stats::setNames(list(gg_obj), stringr::str_remove(filnamn, "\\.[^.]+$"))
  }

  gg_list <- if (diag_facet) skapa(region_vekt) else purrr::flatten(purrr::map(region_vekt, skapa))

  if (returnera_figur) return(gg_list)
  invisible(NULL)
}
