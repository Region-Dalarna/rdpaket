#' Diagram: in- och utpendling per utbildningsgrupp (U2)
#'
#' Stapeldiagram ur SCB:s regionala matchningsindikatorer, tabellen In- och utpendling (U2, TAB6369 och äldre TAB5279):
#' förvärvsarbetande 20–64 år som pendlar över regiongräns, per utbildningsgrupp. Funktionen ger ett diagram per
#' anrop, av tre slag (`diagramtyp`):
#' * `"utbildningsgrupper"` (standard): alla utbildningsgrupper jämförda ett år, sorterade efter värdet. Med
#'   `utb_klartext` markeras valda utbildningsgrupper med en annan färg.
#' * `"tidsserie"`: utvecklingen över tid för en eller flera utbildningsgrupper (flera ger en panel per grupp).
#' * `"jamforelse"`: regionerna i `region_vekt` jämförda för en utbildningsgrupp ett år. Anges bara en region läggs
#'   riket till som jämförelse. Den första regionen markeras.
#'
#' Tabellen finns för riket, län och FA-regioner (`"FA01"` ... `"FA60"`, men inte för kommuner) och gäller 2019–2024.
#' Den äldre versionen (TAB5279, 2006–2018, uppdateras inte) kan sättas ihop med den (`inkludera_aldre_tabell`).
#' Observera tidsseriebrott 2019 (ny datakälla och metod, AGI i stället för KU) och 2022 (BAS används för
#' förvärvsarbetande), och att utbildningsgrupperna är något färre i den äldre tabellen. Gränspendling till Norge och
#' Danmark är bara med i äldre data till och med 2017.
#'
#' @param region_vekt Region(er): läns-, FA-regions- eller rikskod (`"00"`). Kommuner finns inte i tabellen. Bara en region för alla
#'   diagramtyper utom `"jamforelse"`.
#' @param diagramtyp `"utbildningsgrupper"` (standard), `"tidsserie"` eller `"jamforelse"`, se ovan.
#' @param matt Vad som visas: `"andel utpendlare"` (standard, utpendlare av nattbefolkningen, alltså de
#'   förvärvsarbetande som bor i regionen), `"andel inpendlare"` (inpendlare av dagbefolkningen, alltså de som
#'   arbetar i regionen), `"nettopendling antal"` (inpendlare minus utpendlare) eller `"nettopendling andel"`
#'   (nettopendlingen i procent av nattbefolkningen).
#' @param kon_klartext `"totalt"` (standard) eller `c("kvinnor", "män")` för ett diagram uppdelat på kön.
#' @param utb_klartext Utbildningsgrupper som klartext. För `"utbildningsgrupper"` är det de som markeras (standard
#'   `NA` markerar ingen). För `"tidsserie"` och `"jamforelse"` är det de som visas (standard `NA` ger
#'   `"samtliga utbildningsgrupper"`; `"jamforelse"` använder den första).
#' @param ar Vilket år som visas för `"utbildningsgrupper"` och `"jamforelse"`. `NA` (standard) tar senaste året.
#' @param inkludera_aldre_tabell `TRUE` (standard) sätter ihop den nya tabellen (2019–) med SCB:s äldre U2 (TAB5279,
#'   2006–2018, uppdateras inte längre) till en tidsserie 2006–2024. Utbildningsgrupper som bara finns i en av
#'   tabellerna får bara den delen. Ett år till och med 2018 i `ar` hämtas ur den äldre tabellen. `FALSE` använder
#'   bara den nya.
#' @param minsta_antal Utbildningsgrupper med färre förvärvsarbetande än så (nattbefolkning, eller dagbefolkning för
#'   inpendling) tas inte med i `"utbildningsgrupper"` och `"jamforelse"`. Standard 30.
#' @param visa_logga_i_diagram `TRUE` om logga ska visas i diagrammet.
#' @param logga_sokvag Sökväg till logga. `NA` använder standardloggan.
#' @param diagram_capt Diagrammets källtext.
#' @param utmapp Mapp där diagrammet sparas. Måste finnas om `skriv_diagramfil = TRUE`.
#' @param skriv_diagramfil `TRUE` om diagrammet ska skrivas till fil.
#' @param returnera_data `TRUE` lägger data i `.GlobalEnv` som `matchning_u2`.
#' @param returnera_figur `TRUE` (standard) returnerar diagrammet.
#'
#' @return En namngiven lista med ett `ggplot`-objekt.
#' @export
diag_matchning_u2_pendling_utbgrp_scb <- function(
    region_vekt = "20",
    diagramtyp = c("utbildningsgrupper", "tidsserie", "jamforelse"),
    matt = c("andel utpendlare", "andel inpendlare", "nettopendling antal", "nettopendling andel"),
    kon_klartext = "totalt",
    utb_klartext = NA,
    ar = NA,
    inkludera_aldre_tabell = TRUE,
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
  if (!(identical(kon_klartext, "totalt") || setequal(kon_klartext, c("kvinnor", "män")))) {
    stop("kon_klartext ska vara \"totalt\" eller c(\"kvinnor\", \"män\").", call. = FALSE)
  }
  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)
  intern_matchning_kontrollera_regioner("TAB6369", region_vekt)

  # ========================================== Hämta data ===============================================
  # Länk till tabell: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM9906__AM9906A/RegionInd19U2N/
  # (motsvarande tabell TAB6369)

  koder <- list(
    ny = c(inpendlare = "000007IH", utpendlare = "000007II", dag = "000007IJ", natt = "000007IK"),
    gammal = c(inpendlare = "0000036V", utpendlare = "0000036W", dag = "0000036X", natt = "0000036Y"))

  enskilt_ar <- diagramtyp %in% c("utbildningsgrupper", "jamforelse")
  if (enskilt_ar && is.na(ar)) {
    ar <- max(pxweb2r::pxweb2_get_values("TAB6369", "Tid", quiet = TRUE)$code)
  }
  utb_val <- if (diagramtyp == "utbildningsgrupper") NULL else {
    if (all(is.na(utb_klartext))) "samtliga utbildningsgrupper" else if (diagramtyp == "jamforelse") utb_klartext[1] else utb_klartext
  }

  u2_df <- intern_matchning_hamta_tabellpar(
    c(ny = "TAB6369", gammal = "TAB5279"), koder, region_vekt, utb_val, list(Kon = kon_klartext),
    ar = if (enskilt_ar) ar else NULL, med_gammal = inkludera_aldre_tabell) |>
    tidyr::pivot_wider(names_from = post, values_from = value) |>
    dplyr::mutate(region_txt = intern_matchning_regiontext(regionkod, region),
                  kon = stringr::str_to_title(kon))

  if (returnera_data) assign("matchning_u2", u2_df, envir = .GlobalEnv)

  # ========================================== Diagram ==================================================

  matt_def <- list(
    "andel utpendlare" = list(varde = quote(utpendlare / natt * 100), namnare = quote(natt),
                              titel = "Andel utpendlare av de förvärvsarbetande boende 20–64 år", proc = FALSE, y = "procent"),
    "andel inpendlare" = list(varde = quote(inpendlare / dag * 100), namnare = quote(dag),
                              titel = "Andel inpendlare av de förvärvsarbetande på arbetsplatser i regionen 20–64 år", proc = FALSE, y = "procent"),
    "nettopendling antal" = list(varde = quote(inpendlare - utpendlare), namnare = quote(natt),
                                 titel = "Nettopendling (inpendlare minus utpendlare) 20–64 år", proc = FALSE, y = "antal personer"),
    "nettopendling andel" = list(varde = quote((inpendlare - utpendlare) / natt * 100), namnare = quote(natt),
                                 titel = "Nettopendling i procent av de förvärvsarbetande boende 20–64 år", proc = FALSE, y = "procent"))[[matt]]

  chart_df <- u2_df |>
    dplyr::mutate(varde = eval(matt_def$varde), namnare = eval(matt_def$namnare)) |>
    dplyr::filter(!is.na(varde), namnare >= minsta_antal)
  har_kon <- length(kon_klartext) > 1
  if (!har_kon) chart_df$kon <- NULL

  gg_list <- intern_matchning_stapel(
    chart_df, diagramtyp, matt_def$titel, paste0("pendling_u2_", intern_matchning_filtext(matt)),
    matt_def$y, matt_def$proc, region_vekt, utb_klartext, ar,
    if (har_kon) stats::setNames(rddiagram::diagramfarger("kon"), c("Kvinnor", "Män")) else NULL,
    visa_logga_i_diagram, logga_sokvag, diagram_capt, utmapp, skriv_diagramfil)

  if (returnera_figur) return(gg_list)
  invisible(NULL)
}
