#' Diagram: matchning mellan utbildning och yrke per utbildningsgrupp (E3)
#'
#' Stapeldiagram ur SCB:s regionala matchningsindikatorer, tabellen Matchning – utbildning (E3, TAB6929): hur stor
#' andel av de förvärvsarbetande i varje utbildningsgrupp som arbetar i yrken som matchar utbildningen. Funktionen
#' ger ett diagram per anrop, av två slag (`diagramtyp`):
#' * `"utbildningsgrupper"` (standard): alla utbildningsgrupper jämförda ett år, sorterade efter värdet. Med
#'   `utb_klartext` markeras valda utbildningsgrupper med en annan färg.
#' * `"tidsserie"`: utvecklingen över tid för en eller flera utbildningsgrupper (flera ger en panel per
#'   utbildningsgrupp).
#'
#' Tabellen gäller befolkningen 20–69 år (nattbefolkning) och finns för län och kommuner, men inte för
#' FA-regioner. Äldre versioner (E3 utan CKM, TAB6356) uppdateras inte längre och har samma tidsperiod (2019–2024)
#' som den nya, så de används inte.
#'
#' @param region_vekt Länskod eller kommunkod, bara en åt gången.
#' @param diagramtyp `"utbildningsgrupper"` (standard) eller `"tidsserie"`, se ovan.
#' @param kon_bakgr_klartext Urval av befolkningen, bara ett åt gången: `"20–69 år"` (standard), `"totalt"`,
#'   `"män"`, `"kvinnor"`, `"inrikes född"`, `"utrikes född"` eller en åldersgrupp (`"20–24 år"`, `"25–29 år"` ...
#'   `"65–69 år"`).
#' @param matt Vad som visas: `"andel i matchade yrken"` (standard, andelen helt matchade av de förvärvsarbetande
#'   som har tillräckliga uppgifter, A/(A+B+C)) eller `"matchad förvärvsgrad"` (SCB:s mått A/E, helt matchade av hela
#'   befolkningen).
#' @param utb_klartext Utbildningsgrupper som klartext, t.ex. `"förskollärarutbildning"`. För `"utbildningsgrupper"`
#'   är det de som markeras (standard `NA` markerar ingen). För `"tidsserie"` är det de som visas (standard
#'   `NA` ger `"samtliga utbildningsgrupper"`).
#' @param ar Vilket år som visas för `"utbildningsgrupper"`. `NA` (standard) tar senaste året.
#' @param visa_logga_i_diagram `TRUE` om logga ska visas i diagrammet.
#' @param logga_sokvag Sökväg till logga. `NA` använder standardloggan.
#' @param diagram_capt Diagrammets källtext.
#' @param utmapp Mapp där diagrammet sparas. Måste finnas om `skriv_diagramfil = TRUE`.
#' @param skriv_diagramfil `TRUE` om diagrammet ska skrivas till fil.
#' @param returnera_data `TRUE` lägger data i `.GlobalEnv` som `matchning_e3`.
#' @param returnera_figur `TRUE` (standard) returnerar diagrammet.
#'
#' @return En namngiven lista med ett `ggplot`-objekt.
#' @export
diag_matchning_e3_utbgrp_scb <- function(
    region_vekt = "20",
    diagramtyp = c("utbildningsgrupper", "tidsserie"),
    kon_bakgr_klartext = "20–69 år",
    matt = c("andel i matchade yrken", "matchad förvärvsgrad"),
    utb_klartext = NA,
    ar = NA,
    visa_logga_i_diagram = TRUE,
    logga_sokvag = NA,
    diagram_capt = "Källa: SCB:s öppna statistikdatabas, Regionala matchningsindikatorer\nBearbetning: Samhällsanalys, Region Dalarna",
    utmapp = rdverktyg::utskriftsmapp(),
    skriv_diagramfil = TRUE,
    returnera_data = FALSE,
    returnera_figur = TRUE
) {

  # ========================================== Inställningar ============================================

  diagramtyp <- match.arg(diagramtyp)
  matt <- match.arg(matt)
  if (length(region_vekt) != 1) stop("region_vekt ska vara en läns- eller kommunkod (ett värde).", call. = FALSE)
  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  kon_bakgr_koder <- c("totalt" = "totalt", "kvinnor" = "2", "män" = "1", "inrikes född" = "INR", "utrikes född" = "UTR",
                       "20–69 år" = "20-69", "20–24 år" = "20-24", "25–29 år" = "25-29", "30–34 år" = "30-34",
                       "35–39 år" = "35-39", "40–44 år" = "40-44", "45–49 år" = "45-49", "50–54 år" = "50-54",
                       "55–59 år" = "55-59", "60–64 år" = "60-64", "65–69 år" = "65-69")
  if (length(kon_bakgr_klartext) != 1 || !kon_bakgr_klartext %in% names(kon_bakgr_koder)) {
    stop("kon_bakgr_klartext måste vara ett av: ", paste0("\"", names(kon_bakgr_koder), "\"", collapse = ", "), call. = FALSE)
  }

  # ========================================== Hämta data ===============================================
  # Länk till tabell: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM9906__AM9906A/RegionInd19E3N1/
  # (motsvarande nya tabell TAB6929). Näringsgrenen sätts till samtliga (A-U), annars kommer alla näringsgrenar med.

  matchning_df <- pxweb2r::pxweb2_get_data(
    table = "TAB6929",
    query = list(Region = region_vekt, Utbildning = "*", SNI2007 = "A-U",
                 KonAlderFodelseland = kon_bakgr_koder[[kon_bakgr_klartext]],
                 ContentsCode = "*", Tid = "*"),
    quiet = TRUE) |>
    dplyr::rename(regionkod = region_kod, kon_bakgr = `kön/ålder/födelseland`) |>
    dplyr::select(regionkod, region, utbildning_kod, utbildning, kon_bakgr, tabellinnehåll, ar = år, value)

  # A = helt matchade, B = delvis, C = inte matchade, E = hela befolkningen (justerat)
  innehall <- c(A = "Anställda, helt matchade, antal (A)", B = "Anställda, delvis matchade, antal (B)",
                C = "Anställda, inte matchade, antal (C)", matchad_forvarvsgrad = "Matchad förvärvsgrad, procent (A/E)")
  matchning_df <- matchning_df |>
    dplyr::filter(tabellinnehåll %in% innehall) |>
    dplyr::mutate(tabellinnehåll = names(innehall)[match(tabellinnehåll, innehall)]) |>
    tidyr::pivot_wider(names_from = tabellinnehåll, values_from = value) |>
    dplyr::mutate(
      andel_matchade = A / (A + B + C) * 100,
      varde = if (matt == "andel i matchade yrken") andel_matchade else matchad_forvarvsgrad,
      region = rdverktyg::skapa_kortnamn_lan(region)) |>
    dplyr::filter(!is.na(varde))

  if (returnera_data) assign("matchning_e3", matchning_df, envir = .GlobalEnv)

  # ========================================== Texter ===================================================

  region_txt <- unique(matchning_df$region)
  filnamn_region <- gsub("[^a-z0-9]+", "_", chartr("åäö", "aao", tolower(region_txt)))
  grupp_txt <- switch(kon_bakgr_klartext, "totalt" = "", paste0(" (", kon_bakgr_klartext, ")"))
  matt_fil <- gsub(" ", "_", chartr("åäö", "aao", matt))
  kon_fil <- gsub("[^a-z0-9]+", "_", chartr("åäö", "aao", tolower(kon_bakgr_klartext)))

  # ========================================== Diagram ==================================================

  if (diagramtyp == "utbildningsgrupper") {
    # inte nivåer (00N), inriktningar (00I) eller nivåkoder med två tecken eller färre, och inte okänd utbildning
    akt_ar <- if (is.na(ar)) max(matchning_df$ar) else as.character(ar)
    chart_df <- matchning_df |>
      dplyr::filter(ar == akt_ar, !utbildning_kod %in% c("00N", "00I"), nchar(utbildning_kod) > 2,
                    utbildning != "okänd utbildningsnivå") |>
      dplyr::mutate(fokus = as.integer(utbildning %in% utb_klartext[!is.na(utb_klartext)]))
    if (nrow(chart_df) == 0) stop("Inga värden för år ", akt_ar, ".", call. = FALSE)

    diagramtitel <- if (matt == "andel i matchade yrken") {
      paste0("Andel av de förvärvsarbetande", grupp_txt, " per utbildningsgrupp som arbetar i matchade yrken i ",
             region_txt, " år ", akt_ar)
    } else {
      paste0("Matchad förvärvsgrad", grupp_txt, " per utbildningsgrupp i ", region_txt, " år ", akt_ar)
    }
    diagramfil <- paste0("matchning_e3_", matt_fil, "_", kon_fil, "_", filnamn_region, "_", akt_ar, ".png")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = chart_df,
      skickad_x_var = "utbildning",
      skickad_y_var = "varde",
      x_var_fokus = "fokus",
      manual_color = rddiagram::diagramfarger("rd_gron_tva_fokus"),
      diagram_titel = diagramtitel,
      diagram_capt = diagram_capt,
      manual_y_axis_title = "procent",
      procent_0_100_10intervaller = TRUE,
      x_axis_sort_value = TRUE,
      x_axis_lutning = 0,
      y_axis_storlek = 6,
      diagram_liggande = TRUE,
      lagg_pa_logga = visa_logga_i_diagram,
      logga_path = logga_sokvag,
      output_mapp = utmapp,
      filnamn_diagram = diagramfil,
      diagramfil_hojd = 10,
      skriv_till_diagramfil = skriv_diagramfil)
  } else {
    valda <- if (all(is.na(utb_klartext))) "samtliga utbildningsgrupper" else utb_klartext
    chart_df <- matchning_df |> dplyr::filter(utbildning %in% valda)
    saknas <- setdiff(valda, chart_df$utbildning)
    if (length(saknas) > 0) stop("Hittade inte utbildningsgrupperna: ", paste(saknas, collapse = ", "), call. = FALSE)
    flera <- length(unique(chart_df$utbildning)) > 1
    if (flera) chart_df$utbildning <- stringr::str_wrap(chart_df$utbildning, 40)

    utb_titel <- if (flera) "per utbildningsgrupp" else paste0("med ", unique(chart_df$utbildning))
    diagramtitel <- if (matt == "andel i matchade yrken") {
      paste0("Andel av de förvärvsarbetande", grupp_txt, " ", utb_titel, " som arbetar i matchade yrken i ", region_txt)
    } else {
      paste0("Matchad förvärvsgrad", grupp_txt, " ", utb_titel, " i ", region_txt)
    }
    diagramfil <- paste0("matchning_e3_tid_", matt_fil, "_", kon_fil, "_", filnamn_region, "_",
                         if (flera) paste0(length(unique(chart_df$utbildning)), "_utbgrp") else
                           unique(chart_df$utbildning_kod), ".png")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = chart_df,
      skickad_x_var = "ar",
      skickad_y_var = "varde",
      diagram_titel = stringr::str_wrap(diagramtitel, 80),
      diagram_capt = diagram_capt,
      manual_color = rddiagram::diagramfarger("rd_gron")[1],
      manual_y_axis_title = "procent",
      procent_0_100_10intervaller = TRUE,
      x_axis_lutning = 0,
      facet_grp = if (flera) "utbildning" else NULL,
      facet_scale = "fixed",
      lagg_pa_logga = visa_logga_i_diagram,
      logga_path = logga_sokvag,
      output_mapp = utmapp,
      filnamn_diagram = diagramfil,
      skriv_till_diagramfil = skriv_diagramfil)
  }

  gg_list <- stats::setNames(list(gg_obj), stringr::str_remove(diagramfil, "\\.[^.]+$"))
  if (returnera_figur) return(gg_list)
  invisible(NULL)
}
