#' Diagram: jobbflöden per utbildningsgrupp (E2)
#'
#' Stapeldiagram ur SCB:s regionala matchningsindikatorer, tabellen Jobbflöden (E2, TAB6927): nya
#' förvärvsarbetande och jobbytare bland de förvärvsarbetande 20–69 år (dagbefolkning) i en utbildningsgrupp,
#' staplade per årsintervall. Med flera utbildningsgrupper blir det ett facetdiagram (en panel per grupp) eller ett
#' diagram per grupp.
#'
#' Andelarna beräknas som antalet nya förvärvsarbetande respektive jobbytare i procent av antalet förvärvsarbetande
#' det andra året i årsintervallet. Jobbytare är det totala antalet jobbytare mellan två näringsgrenar eller inom
#' samma näringsgren. SCB:s äldre tabell (E2a utan CKM, TAB6363) uppdateras inte längre och gäller 20–64 år, så den
#' används inte och serierna är inte direkt jämförbara. Tabellen finns för län och kommuner, men inte för FA-regioner.
#'
#' @param region_vekt Länskod eller kommunkod, bara en åt gången.
#' @param utb_klartext Utbildningsgrupper som klartext. Standard `"samtliga utbildningsgrupper"`.
#' @param kon_bakgr_klartext Urval av befolkningen, bara ett åt gången: `"totalt"` (standard), `"män"`,
#'   `"kvinnor"`, `"inrikes född"`, `"utrikes född"` eller en åldersgrupp (`"20–24 år"`, `"25–29 år"` ... `"65–69 år"`).
#' @param matt `"andel"` (standard, procent av de förvärvsarbetande) eller `"antal"` (antal personer).
#' @param diag_facet `TRUE` (standard) ger ett facetdiagram när flera utbildningsgrupper valts, `FALSE` ger ett
#'   diagram per utbildningsgrupp.
#' @param visa_logga_i_diagram `TRUE` om logga ska visas i diagrammet.
#' @param logga_sokvag Sökväg till logga. `NA` använder standardloggan.
#' @param diagram_capt Diagrammets källtext.
#' @param utmapp Mapp där diagrammen sparas. Måste finnas om `skriv_diagramfil = TRUE`.
#' @param skriv_diagramfil `TRUE` om diagrammen ska skrivas till fil.
#' @param returnera_data `TRUE` lägger data i `.GlobalEnv` som `matchning_e2`.
#' @param returnera_figur `TRUE` (standard) returnerar diagrammen.
#'
#' @return En namngiven lista med `ggplot`-objekt, ett eller ett per utbildningsgrupp.
#' @export
diag_matchning_e2_jobbfloden_utbgrp_scb <- function(
    region_vekt = "20",
    utb_klartext = "samtliga utbildningsgrupper",
    kon_bakgr_klartext = "totalt",
    matt = c("andel", "antal"),
    diag_facet = TRUE,
    visa_logga_i_diagram = TRUE,
    logga_sokvag = NA,
    diagram_capt = "Källa: SCB:s öppna statistikdatabas, Regionala matchningsindikatorer\nBearbetning: Samhällsanalys, Region Dalarna",
    utmapp = rdverktyg::utskriftsmapp(),
    skriv_diagramfil = TRUE,
    returnera_data = FALSE,
    returnera_figur = TRUE
) {

  # ========================================== Inställningar ============================================

  matt <- match.arg(matt)
  if (length(region_vekt) != 1) stop("region_vekt ska vara en läns- eller kommunkod (ett värde).", call. = FALSE)
  if (length(kon_bakgr_klartext) != 1) stop("kon_bakgr_klartext ska vara ett enda värde.", call. = FALSE)
  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  # ========================================== Hämta data ===============================================
  # Länk till tabell: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM9906__AM9906A/RegionInd19E2AN/
  # (motsvarande nya tabell TAB6927). Näringsgrenen sätts till samtliga (A-U), annars kommer alla näringsgrenar med.

  innehall <- c(forvarvsarbetande = "Förvärvsarbetande år 2", `nya förvärvsarbetande` = "- varav nya förvärvsarbetande",
                jobbytare = "Totalt antal jobbytare mellan år 1 och år 2")
  jobb_df <- pxweb2r::pxweb2_get_data(
    table = "TAB6927",
    query = list(Region = region_vekt, SNI2007 = "A-U",
                 Utbildning = intern_matchning_kod("TAB6927", "Utbildning", utb_klartext),
                 KonAlderFodelseland = intern_matchning_kod("TAB6927", "KonAlderFodelseland", kon_bakgr_klartext),
                 ContentsCode = intern_matchning_kod("TAB6927", "ContentsCode", unname(innehall)),
                 Tid = "*"),
    quiet = TRUE) |>
    dplyr::rename(regionkod = region_kod, kon_bakgr = `kön/ålder/födelseland`) |>
    dplyr::select(regionkod, region, utbildning_kod, utbildning, kon_bakgr, tabellinnehåll, årsintervall, value) |>
    dplyr::mutate(tabellinnehåll = names(innehall)[match(tabellinnehåll, innehall)]) |>
    tidyr::pivot_wider(names_from = tabellinnehåll, values_from = value) |>
    tidyr::pivot_longer(c(`nya förvärvsarbetande`, jobbytare), names_to = "tabellinnehåll", values_to = "antal") |>
    dplyr::mutate(tabellinnehåll = factor(tabellinnehåll, levels = c("nya förvärvsarbetande", "jobbytare")),
                  andel = antal / forvarvsarbetande * 100,
                  varde = if (matt == "andel") andel else antal,
                  region = rdverktyg::skapa_kortnamn_lan(region)) |>
    dplyr::filter(!is.na(varde))

  if (returnera_data) assign("matchning_e2", jobb_df, envir = .GlobalEnv)

  # ========================================== Diagram ==================================================

  region_txt <- unique(jobb_df$region)
  region_fil <- gsub("[^a-z0-9]+", "_", chartr("åäö", "aao", tolower(region_txt)))

  skapa <- function(df, titel_tillagg, fil_tillagg, facet) {
    diagramfil <- paste0("jobbfloden_", matt, "_", region_fil, "_", fil_tillagg, ".png")
    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = df,
      skickad_x_var = "årsintervall",
      skickad_y_var = "varde",
      skickad_x_grupp = "tabellinnehåll",
      geom_position_stack = TRUE,
      manual_color = rddiagram::diagramfarger("rd_gron"),
      diagram_titel = paste0("Jobbflöden i ", region_txt, titel_tillagg),
      diagram_capt = diagram_capt,
      manual_y_axis_title = if (matt == "andel") "procent av de förvärvsarbetande" else "antal personer",
      x_axis_lutning = 45,
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      stodlinjer_avrunda_fem = TRUE,
      facet_grp = if (facet) "utbildning" else NULL,
      facet_legend_bottom = TRUE,
      lagg_pa_logga = visa_logga_i_diagram,
      logga_path = logga_sokvag,
      output_mapp = utmapp,
      filnamn_diagram = diagramfil,
      skriv_till_diagramfil = skriv_diagramfil)
    stats::setNames(list(gg_obj), stringr::str_remove(diagramfil, "\\.[^.]+$"))
  }

  flera <- length(unique(jobb_df$utbildning)) > 1
  if (flera && diag_facet) {
    jobb_df$utbildning <- stringr::str_wrap(jobb_df$utbildning, 35)
    gg_list <- skapa(jobb_df, "", paste0(length(unique(jobb_df$utbildning)), "_utbgrp"), TRUE)
  } else {
    gg_list <- purrr::flatten(purrr::map(unique(jobb_df$utbildning_kod), \(kod) {
      df <- jobb_df[jobb_df$utbildning_kod == kod, ]
      skapa(df, paste0("\n", unique(df$utbildning)), kod, FALSE)
    }))
  }

  if (returnera_figur) return(gg_list)
  invisible(NULL)
}
