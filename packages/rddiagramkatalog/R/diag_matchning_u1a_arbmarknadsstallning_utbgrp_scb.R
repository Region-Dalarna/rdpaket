#' Diagram: arbetsmarknadsställning per utbildningsgrupp (U1a)
#'
#' Stapeldiagram ur SCB:s regionala matchningsindikatorer, tabellen Arbetsmarknadsställning (U1a, TAB6367):
#' befolkningen 20–64 år (nattbefolkning) uppdelad på förvärvsarbetande, inskrivna arbetslösa och inte i
#' arbetskraften, per utbildningsgrupp. Funktionen ger ett diagram per anrop, av fyra slag (`diagramtyp`):
#' * `"utbildningsgrupper"` (standard): alla utbildningsgrupper jämförda ett år, sorterade efter värdet. Med
#'   `utb_klartext` markeras valda utbildningsgrupper med en annan färg.
#' * `"tidsserie"`: utvecklingen över tid för en eller flera utbildningsgrupper (flera ger en panel per grupp).
#' * `"fordelning"`: hur befolkningen fördelar sig på förvärvsarbetande, inskrivna arbetslösa och inte i
#'   arbetskraften över tid, för en eller flera utbildningsgrupper.
#' * `"jamforelse"`: regionerna i `region_vekt` jämförda för en utbildningsgrupp ett år. Anges bara en region läggs
#'   riket till som jämförelse. Den första regionen markeras.
#'
#' Tabellen finns för riket, län och FA-regioner (`"FA01"` ... `"FA60"`, men inte för kommuner) och gäller 2019–2024. Det finns ingen nyare
#' version, och inga äldre versioner finns kvar i SCB:s API. Observera tidsseriebrott 2019 (ny datakälla och
#' metod) och 2022 (BAS används för förvärvsarbetande).
#'
#' @param region_vekt Region(er): läns-, FA-regions- eller rikskod (`"00"`). Kommuner finns inte i tabellen. Bara en region för alla
#'   diagramtyper utom `"jamforelse"`.
#' @param diagramtyp `"utbildningsgrupper"` (standard), `"tidsserie"`, `"fordelning"` eller `"jamforelse"`, se ovan.
#' @param matt Vad som visas (inte för `"fordelning"`): `"förvärvsfrekvens"` (standard, förvärvsarbetande av alla
#'   20–64 år), `"arbetslöshet"` (inskrivna arbetslösa av de förvärvsarbetande och arbetslösa),
#'   `"arbetskraftsdeltagande"` (förvärvsarbetande och arbetslösa av alla) eller `"andel studerande"` (studerande som
#'   inte är i arbetskraften, av alla).
#' @param utb_klartext Utbildningsgrupper som klartext. För `"utbildningsgrupper"` är det de som markeras (standard
#'   `NA` markerar ingen). För `"tidsserie"`, `"fordelning"` och `"jamforelse"` är det de som visas (standard `NA`
#'   ger `"samtliga utbildningsgrupper"`; `"jamforelse"` använder den första).
#' @param ar Vilket år som visas för `"utbildningsgrupper"` och `"jamforelse"`. `NA` (standard) tar senaste året.
#' @param minsta_antal Utbildningsgrupper med färre personer än så (i nämnaren för måttet) tas inte med i
#'   `"utbildningsgrupper"` och `"jamforelse"`. Standard 30.
#' @param visa_logga_i_diagram `TRUE` om logga ska visas i diagrammet.
#' @param logga_sokvag Sökväg till logga. `NA` använder standardloggan.
#' @param diagram_capt Diagrammets källtext.
#' @param utmapp Mapp där diagrammet sparas. Måste finnas om `skriv_diagramfil = TRUE`.
#' @param skriv_diagramfil `TRUE` om diagrammet ska skrivas till fil.
#' @param returnera_data `TRUE` lägger data i `.GlobalEnv` som `matchning_u1a`.
#' @param returnera_figur `TRUE` (standard) returnerar diagrammet.
#'
#' @return En namngiven lista med ett `ggplot`-objekt.
#' @export
diag_matchning_u1a_arbmarknadsstallning_utbgrp_scb <- function(
    region_vekt = "20",
    diagramtyp = c("utbildningsgrupper", "tidsserie", "fordelning", "jamforelse"),
    matt = c("förvärvsfrekvens", "arbetslöshet", "arbetskraftsdeltagande", "andel studerande"),
    utb_klartext = NA,
    ar = NA,
    minsta_antal = 30,
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
  region_vekt <- unique(as.character(region_vekt))
  if (diagramtyp == "jamforelse") {
    if (length(region_vekt) == 1 && region_vekt != "00") region_vekt <- c(region_vekt, "00")
  } else if (length(region_vekt) != 1) {
    stop("region_vekt ska vara en region för diagramtypen \"", diagramtyp, "\".", call. = FALSE)
  }
  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)
  intern_matchning_kontrollera_regioner("TAB6367", region_vekt)

  # ========================================== Hämta data ===============================================
  # Länk till tabell: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM9906__AM9906A/RegionInd19U1aN/
  # (motsvarande tabell TAB6367)

  utb_koder <- if (diagramtyp == "utbildningsgrupper") "*" else {
    intern_matchning_kod("TAB6367", "Utbildngrupp",
                         if (all(is.na(utb_klartext))) "samtliga utbildningsgrupper" else utb_klartext)
  }
  if (diagramtyp == "jamforelse") utb_koder <- utb_koder[1]

  innehall <- c(A = "Förvärvsarbetande (A)", B = "Inskrivna arbetslösa på Arbetsförmedlingen (B)",
                C = "Inte i arbetskraften (C)", studerande = "- Varav studerande",
                totalt = "Totalt antal personer (A+B+C)")
  koder <- c(A = "000007I4", B = "000007I8", C = "000007I9", studerande = "000007IA", totalt = "000007IC")

  u1a_df <- pxweb2r::pxweb2_get_data(
    table = "TAB6367",
    query = list(Region = region_vekt, Utbildngrupp = utb_koder, ContentsCode = unname(koder), Tid = "*"),
    quiet = TRUE) |>
    dplyr::rename(regionkod = region_kod, ar = år) |>
    dplyr::select(regionkod, region, utbildning_kod, utbildning, ar, tabellinnehåll, value) |>
    dplyr::mutate(tabellinnehåll = names(innehall)[match(tabellinnehåll, innehall)])
  u1a_df <- u1a_df |>
    dplyr::filter(!is.na(tabellinnehåll)) |>
    tidyr::pivot_wider(names_from = tabellinnehåll, values_from = value) |>
    dplyr::mutate(region_txt = intern_matchning_regiontext(regionkod, region))

  if (returnera_data) assign("matchning_u1a", u1a_df, envir = .GlobalEnv)

  # ========================================== Diagram ==================================================

  if (diagramtyp == "fordelning") {
    valda_utb <- unique(u1a_df$utbildning)
    chart_df <- u1a_df |>
      dplyr::filter(regionkod == region_vekt[1]) |>
      tidyr::pivot_longer(c(A, B, C), names_to = "kategori", values_to = "antal") |>
      dplyr::mutate(
        kategori = factor(c(A = "förvärvsarbetande", B = "inskrivna arbetslösa", C = "inte i arbetskraften")[kategori],
                          levels = c("förvärvsarbetande", "inskrivna arbetslösa", "inte i arbetskraften")),
        andel = antal / totalt * 100)
    flera <- length(valda_utb) > 1
    if (flera) chart_df$utbildning <- stringr::str_wrap(chart_df$utbildning, 35)
    region_txt <- unique(chart_df$region_txt)
    diagramfil <- paste0("arbmarknadsstallning_u1a_fordelning_", intern_matchning_filtext(region_txt), "_",
                         if (flera) paste0(length(valda_utb), "_utbgrp") else unique(chart_df$utbildning_kod), ".png")
    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = chart_df, skickad_x_var = "ar", skickad_y_var = "andel", skickad_x_grupp = "kategori",
      geom_position_stack = TRUE, manual_color = rddiagram::diagramfarger("rd_gron"),
      diagram_titel = stringr::str_wrap(paste0("Arbetsmarknadsställning för befolkningen 20–64 år ",
                                               if (flera) "per utbildningsgrupp" else paste0("med ", unique(chart_df$utbildning)),
                                               " i ", region_txt), 80),
      diagram_capt = diagram_capt, manual_y_axis_title = "procent", procent_0_100_10intervaller = TRUE,
      x_axis_lutning = 0, facet_grp = if (flera) "utbildning" else NULL, facet_scale = "fixed",
      facet_legend_bottom = TRUE, lagg_pa_logga = visa_logga_i_diagram, logga_path = logga_sokvag,
      output_mapp = utmapp, filnamn_diagram = diagramfil, skriv_till_diagramfil = skriv_diagramfil)
    gg_list <- stats::setNames(list(gg_obj), stringr::str_remove(diagramfil, "\\.[^.]+$"))
  } else {
    matt_def <- list(
      "förvärvsfrekvens" = list(varde = quote(A / totalt * 100), namnare = quote(totalt),
                                titel = "Förvärvsfrekvens (förvärvsarbetande av befolkningen 20–64 år)", proc = TRUE),
      "arbetslöshet" = list(varde = quote(B / (A + B) * 100), namnare = quote(A + B),
                            titel = "Arbetslöshet (inskrivna arbetslösa av förvärvsarbetande och arbetslösa 20–64 år)", proc = FALSE),
      "arbetskraftsdeltagande" = list(varde = quote((A + B) / totalt * 100), namnare = quote(totalt),
                                      titel = "Arbetskraftsdeltagande (20–64 år)", proc = TRUE),
      "andel studerande" = list(varde = quote(studerande / totalt * 100), namnare = quote(totalt),
                                titel = "Andel studerande utanför arbetskraften (20–64 år)", proc = TRUE))[[matt]]
    chart_df <- u1a_df |>
      dplyr::mutate(varde = eval(matt_def$varde), namnare = eval(matt_def$namnare)) |>
      dplyr::filter(!is.na(varde), namnare >= minsta_antal)
    gg_list <- intern_matchning_stapel(
      chart_df, diagramtyp, matt_def$titel, paste0("arbmarknadsstallning_u1a_", intern_matchning_filtext(matt)),
      "procent", matt_def$proc, region_vekt, utb_klartext, ar, NULL, visa_logga_i_diagram, logga_sokvag,
      diagram_capt, utmapp, skriv_diagramfil)
  }

  if (returnera_figur) return(gg_list)
  invisible(NULL)
}
