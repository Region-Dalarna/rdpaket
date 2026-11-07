#' Diagram: total befolkningsförändring per region, enligt prognos
#'
#' Visar total befolkningsförändring (antal eller andel) mellan idag och
#' `jmfrtid` år framåt, en stapel per region, enligt SCB:s
#' befolkningsprognos eller Region Dalarnas egna Profet-prognos.
#'
#' @param region_vekt Regioner att jämföra.
#' @param fokus_regionkod `"region"` (standard) fokuserar på de regioner i
#'   `region_vekt` som är läns-/rikskoder (inte kommuner), `NA` för ingen
#'   fokusering, eller en specifik regionkod.
#' @param befprognostabell_url Antingen en https-url till SCB:s API, eller
#'   sökvägen till en mapp med lokala Profet-/Halland-prognosfiler. Se
#'   [rdhamtadata::hamta_befprognos_diff_data()].
#' @param jmfrtid Antal år i jämförelsen, räknat från startåret.
#' @param stodlinjer_avrunda_fem TRUE om stödlinjerna ska avrundas till
#'   närmaste femtal.
#' @param visa_logga_i_diagram TRUE om logga ska visas i diagrammet.
#' @param logga_sokvag Sökväg till logga.
#' @param skriv_diagramfil TRUE om diagram ska skrivas till fil.
#' @param dataetiketter TRUE om dataetiketter ska visas i diagrammet.
#' @param diag_fargvekt Valfri färgvektor.
#' @param utmapp Mapp där diagram sparas. Måste finnas om
#'   `skriv_diagramfil = TRUE`.
#' @param diagram_capt Diagrammets källtext. `"auto"` (standard) väljer text
#'   utifrån vilken tabell som används.
#' @param andel_istallet_for_antal Om `TRUE` visas andel i stället för
#'   antal.
#'
#' @return En namngiven lista med ett `ggplot`-objekt.
#' @export
diag_befprognos_diff_tot_per_region_scb <- function(
    region_vekt = c("20", "2021", "2023", "2026", "2029", "2031", "2034", "2039", "2061", "2062", "2080", "2081", "2082", "2083", "2084", "2085"),
    fokus_regionkod = "region",
    befprognostabell_url = "G:/Samhällsanalys/Statistik/Befolkningsprognoser/Profet/datafiler/",
    jmfrtid = 10,
    stodlinjer_avrunda_fem = TRUE,
    visa_logga_i_diagram = TRUE,
    logga_sokvag = NA,
    skriv_diagramfil = TRUE,
    dataetiketter = FALSE,
    diag_fargvekt = NA,
    utmapp = rdverktyg::utskriftsmapp(),
    diagram_capt = "auto",
    andel_istallet_for_antal = FALSE
) {

  # ========================================== Allmän info ============================================
  # Total befolkningsförändring per region (inte uppdelat på åldersgrupp) enligt befolkningsprognos -
  # bygger direkt ovanpå rdhamtadata::hamta_befprognos_diff_data(), som gör både datahämtning och
  # beräkning av förändringen.
  # ========================================== Inställningar ============================================

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  if (all(is.na(diag_fargvekt))) diag_fargvekt <- rddiagram::diagramfarger("rus_sex")

  prognos_diff_df <- rdhamtadata::hamta_befprognos_diff_data(
    region_vekt = region_vekt,
    tabeller_url = befprognostabell_url,
    jmfrtid = jmfrtid
  )
  prognos_diff_df <- dplyr::mutate(prognos_diff_df, region = rdverktyg::skapa_kortnamn_lan(region))

  if (diagram_capt == "auto") {
    if (stringr::str_detect(befprognostabell_url, "api.scb.se")) {
      diagram_capt <- paste0("Källa: SCB:s befolkningsframskrivning från juni år ", unique(prognos_diff_df$prognos_ar), "\nBearbetning: Samhällsanalys, Region Dalarna")
    } else if (befprognostabell_url == "G:/Samhällsanalys/Statistik/Befolkningsprognoser/Profet/datafiler/") {
      diagram_capt <- "Källa: Region Dalarnas egna befolkningsprognos, bearbetning av Samhällsanalys, Region Dalarna\nPrognosen för Ludvika kommun baseras på ett scenario som i allt väsentligt liknar den som Ludvika kommun\nsjälva tagit fram i deras scenario med medelstark tillväxt."
    }
  }

  y_lbl_axel <- "förändring antal invånare"
  y_lbl_diagram_titel <- "Förändring folkmängd"

  # om regioner är alla kommuner i ett län eller alla län i Sverige görs revidering, annars inte
  region_start <- rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(unique(prognos_diff_df$region)))
  region_txt <- rdverktyg::ar_alla_kommuner_i_ett_lan(unique(prognos_diff_df$regionkod), returnera_text = TRUE, returtext = region_start)
  region_txt <- rdverktyg::ar_alla_lan_i_sverige(unique(prognos_diff_df$regionkod), returnera_text = TRUE, returtext = region_txt)

  if (!is.na(fokus_regionkod)) {
    if (fokus_regionkod == "region") fokus_regionkod <- unique(prognos_diff_df$regionkod)[nchar(unique(prognos_diff_df$regionkod)) == 2]
  }

  if (!is.na(fokus_regionkod)) {
    prognos_diff_df <- dplyr::mutate(prognos_diff_df, fokus = ifelse(regionkod %in% fokus_regionkod, 1, 0))
  }

  if (length(unique(prognos_diff_df$prognos_ar)) < 2) {
    diagramtitel <- paste0(y_lbl_diagram_titel, " i ", region_txt, " ",
                           unique(prognos_diff_df$start_ar), "-", unique(prognos_diff_df$slut_ar), "\n(enligt befolkningsprognos våren ",
                           unique(prognos_diff_df$prognos_ar), ")")
  } else {
    # Bugfix (confirmed via källkoden): "skickad_jmfrtid" var odefinierad i originalet (flaggat med en
    # egen TODO-kommentar i källan: "troligen ska det vara jmfrtid") - denna gren körs aldrig i
    # praktiken i dagsläget (hamta_befprognos_diff_data() returnerar alltid ett enda prognos_ar), men
    # fixas ändå för att inte krascha om/när flera prognosår stöds i framtiden.
    diagramtitel <- paste0(y_lbl_diagram_titel, " i ", region_txt, " på ", jmfrtid,
                           " års sikt")
  }

  enhet <- ifelse(andel_istallet_for_antal, "andel", "antal")
  filnamn <- paste0("befprogn_", tolower(region_txt), "_", enhet, "_alla_tot.png")

  chart_df <- if (andel_istallet_for_antal) prognos_diff_df else dplyr::filter(prognos_diff_df, regionkod != fokus_regionkod)

  gg_obj <- rddiagram::SkapaStapelDiagram(
    skickad_df = chart_df,
    skickad_y_var = ifelse(andel_istallet_for_antal, "andel", "antal"),
    skickad_x_var = "region",
    x_axis_sort_value = TRUE,
    x_var_fokus = if (!is.na(fokus_regionkod)) "fokus" else NA,
    diagram_titel = diagramtitel,
    output_mapp = utmapp,
    diagram_capt = diagram_capt,
    stodlinjer_avrunda_fem = stodlinjer_avrunda_fem,
    manual_color = diag_fargvekt[c(1, 2)],
    manual_y_axis_title = ifelse(andel_istallet_for_antal, "procent", y_lbl_axel),
    x_axis_lutning = 45,
    manual_x_axis_text_vjust = 1,
    manual_x_axis_text_hjust = 1,
    lagg_pa_logga = visa_logga_i_diagram,
    logga_path = logga_sokvag,
    dataetiketter = dataetiketter,
    filnamn_diagram = filnamn,
    skriv_till_diagramfil = skriv_diagramfil)

  gg_list <- list(gg_obj)
  names(gg_list)[[length(gg_list)]] <- stringr::str_remove(filnamn, ".png")
  return(gg_list)
}
