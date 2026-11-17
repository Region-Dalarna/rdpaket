#' Diagram: etablering på arbetsmarknaden efter vistelsetid, uppdelat på kön och år
#'
#' Stapeldiagram över utrikes födda efter vistelsetid i Sverige (0–1, 2–3, 4–9 och 10– år) och, som jämförelse,
#' inrikes födda. Diagrammet delas upp i en panel för kvinnor och en för män, och staplarna för varje grupp är ett
#' år vardera: äldsta året är ljusast och senaste året mörkast. Kvinnor är gula och män gröna (färgerna i
#' `rddiagram::diagramfarger("kon")`). Startår och slutår står i rubriken, och teckenförklaringen är avstängd som
#' standard eftersom nyanserna följer årsordningen. Rubriken är t.ex. "Sysselsatta utrikes respektive inrikes födda
#' i arbetsför ålder i Dalarna år 1997-2024".
#'
#' Data kommer från SCB:s statistik "Registerdata för integration" (AA0003) i statistikdatabasen och hämtas direkt med `pxweb2r`: länstabellerna TAB389 (till och med 2021) och TAB6384 (från 2022), och
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
#' @param startar Första år som visas. `NA` (standard) tar alla år i tabellerna (från 1997).
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
    startar = NA,
    slutar = NA,
    visa_teckenforklaring = FALSE,
    visa_logga_i_diagram = TRUE,
    diagram_capt = "Källa: SCB:s öppna statistikdatabas, Registerdata för integration\nBearbetning: Samhällsanalys, Region Dalarna",
    utmapp = rdverktyg::utskriftsmapp(),
    skriv_diagramfil = TRUE,
    filformat = "png",
    returnera_data = FALSE,
    returnera_figur = TRUE
) {

  # ========================================== Inställningar ============================================

  # Måtten ligger i samma ordning i SCB:s fyra tabeller, så måttet hittas via sin position i respektive tabell
  # (etiketterna skiljer sig åt mellan de gamla och nya tabellerna). Texten är måttets ord i rubriken.
  rubrik_matt <- c(
    "sysselsättningsgrad" = "Sysselsatta",
    "företagare" = "Företagare",
    "inskrivna arbetslösa" = "Inskrivna arbetslösa",
    "öppet arbetslösa" = "Öppet arbetslösa",
    "aktivitetsstöd" = "Personer i program med aktivitetsstöd",
    "långtidsarbetslösa" = "Långtidsarbetslösa",
    "kvar i sysselsättning efter ett år" = "Fortfarande sysselsatta efter ett år",
    "kvar som företagare efter ett år" = "Fortfarande företagare efter ett år",
    "chefsposition" = "Personer i chefsposition",
    "eftergymnasial i yrkesområde 2-3" = "Eftergymnasialt utbildade med arbete inom yrkesområde 2-3",
    "eftergymnasial på kvalifikationsnivå 3-4" = "Eftergymnasialt utbildade med arbete på kvalifikationsnivå 3-4")

  if (length(cont_klartext) != 1 || !cont_klartext %in% names(rubrik_matt)) {
    stop("cont_klartext måste vara ett av: ", paste0("\"", names(rubrik_matt), "\"", collapse = ", "), call. = FALSE)
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
    tid_val <- tid_giltiga[(is.na(startar) | tid_alla >= startar) & (is.na(slutar) | tid_alla <= slutar)]
    if (length(tid_val) == 0) return(NULL)

    cont_kod <- pxweb2r::pxweb2_get_values(tabell, "ContentsCode", quiet = TRUE)$code[match(cont_klartext, names(rubrik_matt))]

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
  skala <- \(farg, ljus) if (length(ar_alla) == 1) farg else grDevices::colorRampPalette(c(ljus, farg))(length(ar_alla))

  fargvektor <- c(
    stats::setNames(skala(kon_farger[1], "#FFF5CC"), paste0("Kvinnor.", ar_alla)),
    stats::setNames(skala(kon_farger[2], "#D5EAE6"), paste0("Män.", ar_alla)))

  # ========================================== Diagram ==================================================

  region_txt <- unique(etablering_df$region)
  ar_txt <- paste0(min(ar_alla), "-", max(ar_alla))
  # rubriken ritas som en textruta (ggtext) där <br> ger radbrytning: första raden beskriver gruppen, andra raden var och när
  diagramtitel <- paste0(rubrik_matt[[cont_klartext]], " utrikes respektive inrikes födda i arbetsför ålder<br>i ",
                         region_txt, " år ", ar_txt)
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
