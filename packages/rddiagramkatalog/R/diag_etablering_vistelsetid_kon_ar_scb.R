#' Diagram: etablering på arbetsmarknaden efter vistelsetid, uppdelat på kön och år
#'
#' Stapeldiagram över utrikes födda efter vistelsetid i Sverige (0–1, 2–3, 4–9 och 10– år) och, som jämförelse,
#' inrikes födda. Diagrammet delas upp i en panel för kvinnor och en för män, och staplarna för varje grupp är ett
#' år vardera: äldsta året är ljusast och senaste året mörkast. Kvinnor är gula och män gröna (färgerna i
#' `rddiagram::diagramfarger("kon")`). Startår och slutår står i rubriken, och teckenförklaringen är avstängd som
#' standard eftersom nyanserna följer årsordningen.
#'
#' Data hämtas direkt med `pxweb2r`: länstabellerna TAB389 (till och med 2021) och TAB6384 (från 2022), och
#' kommuntabellerna TAB4881 (till och med 2021) och TAB6383 (från 2022). Före 2022 heter måtten
#' "förvärvsarbetande" i stället för "sysselsatta" (ny definition från 2019). Serierna sätts ihop som en serie.
#'
#' Finns även ett diagram med fler varianter av samma tabeller: [diag_etablering_diverse_scb()].
#'
#' @param region_vekt Ett län eller en kommun (kod), bara en åt gången.
#' @param cont_klartext Vad som visas, bara en åt gången: `"sysselsättningsgrad"` (standard, andel sysselsatta),
#'   `"företagare"`, `"inskrivna arbetslösa"`, `"öppet arbetslösa"`, `"aktivitetsstöd"`, `"långtidsarbetslösa"`,
#'   `"kvar i sysselsättning efter ett år"`, `"kvar som företagare efter ett år"`, `"chefsposition"`,
#'   `"eftergymnasial i yrkesområde 2-3"` eller `"eftergymnasial på kvalifikationsnivå 3-4"`.
#' @param startar Första år som visas. Standard 2012.
#' @param slutar Sista år som visas. `NA` (standard) tar senaste året med data.
#' @param visa_teckenforklaring `TRUE` visar teckenförklaring (en post per kön och år). Standard `FALSE`.
#' @param visa_logga_i_diagram `TRUE` om logga ska visas i diagrammet.
#' @param diagram_capt Diagrammets källtext.
#' @param utmapp Mapp där diagrammet sparas. Måste finnas om `skriv_diagramfil = TRUE`.
#' @param skriv_diagramfil `TRUE` om diagrammet ska skrivas till fil.
#' @param filformat Bildformat, `"png"` (standard) eller `"svg"`.
#' @param returnera_data `TRUE` lägger data i `.GlobalEnv` som `etablering_vistelsetid`.
#' @param returnera_figur `TRUE` (standard) returnerar diagrammet.
#'
#' @return En namngiven lista med ett `ggplot`-objekt.
#' @export
diag_etablering_vistelsetid_kon_ar_scb <- function(
    region_vekt = "20",
    cont_klartext = "sysselsättningsgrad",
    startar = 2012,
    slutar = NA,
    visa_teckenforklaring = FALSE,
    visa_logga_i_diagram = TRUE,
    diagram_capt = "Källa: SCB:s öppna statistikdatabas, Befolkningens arbetsmarknadsstatus (BAS)\nBearbetning: Samhällsanalys, Region Dalarna",
    utmapp = rdverktyg::utskriftsmapp(),
    skriv_diagramfil = TRUE,
    filformat = "png",
    returnera_data = FALSE,
    returnera_figur = TRUE
) {

  # ========================================== Inställningar ============================================

  # klartext -> SCB:s måttnamn (används i rubriken). Ordningen är densamma som i SCB:s fyra tabeller, så måttet
  # hittas via sin position i respektive tabell (etiketterna skiljer sig åt mellan de gamla och nya tabellerna).
  matt <- c(
    "sysselsättningsgrad" = "Andel sysselsatta",
    "företagare" = "Andel företagare av de sysselsatta",
    "inskrivna arbetslösa" = "Andel inskrivna arbetslösa",
    "öppet arbetslösa" = "Andel öppet arbetslösa",
    "aktivitetsstöd" = "Andel sökande i program med aktivitetsstöd",
    "långtidsarbetslösa" = "Andel långtidsarbetslösa",
    "kvar i sysselsättning efter ett år" = "Andel fortfarande sysselsatta efter ett år",
    "kvar som företagare efter ett år" = "Andel fortfarande företagare efter ett år",
    "chefsposition" = "Andel i chefsposition",
    "eftergymnasial i yrkesområde 2-3" = "Andel med eftergymnasial utbildning med arbete inom yrkesområde 2-3 (SSYK)",
    "eftergymnasial på kvalifikationsnivå 3-4" = "Andel med eftergymnasial utbildning med arbete på kvalifikationsnivå 3-4 (SSYK)")

  if (length(cont_klartext) != 1 || !cont_klartext %in% names(matt)) {
    stop("cont_klartext måste vara ett av: ", paste0("\"", names(matt), "\"", collapse = ", "), call. = FALSE)
  }
  if (length(region_vekt) != 1 || !nchar(region_vekt) %in% c(2, 4)) {
    stop("region_vekt ska vara en läns- eller kommunkod (ett värde).", call. = FALSE)
  }
  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  # ========================================== Hämta data ===============================================

  # (tabell, första/sista år i tabellen avgörs av tabellens egna tidskoder)
  tabeller <- if (nchar(region_vekt) == 2) c("TAB389", "TAB6384") else c("TAB4881", "TAB6383")

  bakgrundsvariabler <- c("INT010", "INT020", "INT030", "INT040", "SE")

  hamta_tabell <- function(tabell) {
    tid_giltiga <- pxweb2r::pxweb2_get_values(tabell, "Tid", quiet = TRUE)$code
    tid_alla <- as.integer(tid_giltiga)
    tid_val <- tid_giltiga[tid_alla >= startar & (is.na(slutar) | tid_alla <= slutar)]
    if (length(tid_val) == 0) return(NULL)

    cont_kod <- pxweb2r::pxweb2_get_values(tabell, "ContentsCode", quiet = TRUE)$code[match(cont_klartext, names(matt))]

    pxweb2r::pxweb2_get_data(
      table = tabell,
      query = list(Region = region_vekt, Kon = c("män", "kvinnor"), UtbNiv = "000",
                   BakgrVar = bakgrundsvariabler, ContentsCode = cont_kod, Tid = tid_val),
      quiet = TRUE)
  }

  etablering_df <- purrr::map(tabeller, hamta_tabell) |>
    dplyr::bind_rows()
  if (nrow(etablering_df) == 0) stop("SCB gav inga värden för det valda urvalet.", call. = FALSE)

  etablering_df <- etablering_df |>
    dplyr::filter(!is.na(value)) |>
    dplyr::mutate(
      region = rdverktyg::skapa_kortnamn_lan(region),
      bakgrundsvariabel = dplyr::if_else(bakgrundsvariabel == "födelseregion: Sverige", "Inrikes födda",
                                         sub("^vistelsetid ", "", bakgrundsvariabel)),
      kön = stringr::str_to_title(kön),
      år = as.character(år))

  # vistelsetiderna ska ligga i "rätt" ordning, med inrikes födda sist
  etablering_df$bakgrundsvariabel <- factor(
    etablering_df$bakgrundsvariabel,
    levels = c("0–1 år", "2–3 år", "4–9 år", "10– år", "Inrikes födda"))

  if (returnera_data) assign("etablering_vistelsetid", etablering_df, envir = .GlobalEnv)

  # ========================================== Färger ===================================================

  # en färgskala per kön, från ljus (äldsta året) till kvinnornas/männens egen färg (senaste året)
  ar_alla <- sort(unique(etablering_df$år))
  kon_farger <- rddiagram::diagramfarger("kon")
  ljusare <- \(farg) grDevices::rgb(t(grDevices::col2rgb(farg) * 0.25 + 255 * 0.75), maxColorValue = 255)
  skala <- \(farg) if (length(ar_alla) == 1) farg else grDevices::colorRampPalette(c(ljusare(farg), farg))(length(ar_alla))

  fargvektor <- c(
    stats::setNames(skala(kon_farger[1]), paste0("Kvinnor.", ar_alla)),
    stats::setNames(skala(kon_farger[2]), paste0("Män.", ar_alla)))

  # ========================================== Diagram ==================================================

  region_txt <- unique(etablering_df$region)
  ar_txt <- paste0(min(ar_alla), "–", max(ar_alla))
  diagramtitel <- paste0(matt[[cont_klartext]], ": utrikes födda efter vistelsetid i Sverige och inrikes födda i ",
                         region_txt, " ", ar_txt)
  diagramfil <- paste0("etablering_vistelsetid_kon_", gsub("[^a-z0-9]+", "_",
                         chartr("åäö", "aao", tolower(cont_klartext))), "_", gsub("[^a-z0-9]+", "_",
                         chartr("åäö", "aao", tolower(region_txt))), "_", min(ar_alla), "_", max(ar_alla), ".", filformat)

  gg_obj <- rddiagram::SkapaStapelDiagram(
    skickad_df = etablering_df,
    skickad_x_var = "bakgrundsvariabel",
    skickad_y_var = "value",
    skickad_x_grupp = "år",
    skickad_namngiven_fargvektor = fargvektor,
    farg_variabler = list(kön = c("Kvinnor", "Män")),
    facet_grp = "kön",
    facet_scale = "fixed",
    manual_x_axis_title = "Vistelsetid i Sverige respektive inrikes födda",
    manual_y_axis_title = "procent",
    diagram_titel = diagramtitel,
    diagram_capt = diagram_capt,
    x_axis_sort_value = FALSE,
    x_axis_lutning = 0,
    procent_0_100_10intervaller = TRUE,
    legend_tabort = !visa_teckenforklaring,
    facet_legend_bottom = TRUE,
    legend_rader = 2,
    legend_byrow = TRUE,
    lagg_pa_logga = visa_logga_i_diagram,
    output_mapp = utmapp,
    filnamn_diagram = diagramfil,
    skriv_till_diagramfil = skriv_diagramfil)

  gg_list <- list(gg_obj)
  names(gg_list) <- stringr::str_remove(diagramfil, "\\.[^.]+$")

  if (returnera_figur) return(gg_list)
  invisible(NULL)
}
