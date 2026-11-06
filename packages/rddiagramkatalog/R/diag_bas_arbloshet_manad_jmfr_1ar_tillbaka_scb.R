#' Diagram: förändring i arbetslöshet jämfört med samma månad föregående år
#'
#' Stapeldiagram över skillnaden i arbetslöshet (procentenheter) mellan en
#' given månad och samma månad ett antal månader tillbaka, uppdelat på
#' region och, valfritt, inrikes/utrikes födda.
#'
#' \if{html}{\figure{diag_bas_arbloshet_manad_jmfr_1ar_tillbaka_scb.png}{options: width="100%" alt="Exempel: diag_bas_arbloshet_manad_jmfr_1ar_tillbaka_scb() med standardinställningar"}}
#'
#' @param region_vekt NA = jämförelse mellan länen, annars jämförs kommuner i de regioner vars länskoder man skickar med.
#' @param diagram_capt Diagrammets källtext.
#' @param utmapp Mapp där diagram ska sparas. Måste finnas om `skriv_diagramfil = TRUE`.
#' @param tid_koder Vilken månad som ska jämföras (slutpunkten), `"9999"` ger senaste. OBS: till skillnad från flera andra funktioner i paketet anger denna EN period att jämföra mot, inte ett intervall - `"start:9999"`-mönstret stöds inte här.
#' @param dela_upp_utrikes Sätts till FALSE om man vill ha förändring i arbetslöshet totalt, dvs. inte uppdelat på inrikes/utrikes-födda.
#' @param returnera_data Sätts till TRUE om man vill returnera data till R-studios global environment.
#' @param jamfor_antal_manader_bakat Antal månader tillbaka som jämförelsen görs mot.
#' @param visa_dataetiketter Visa dataetiketter.
#' @param skriv_diagramfil TRUE om diagram ska skrivas till fil.
#' @param demo Sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren.
#'
#' @return En namngiven lista med ett `ggplot`-objekt.
#' @export
diag_bas_arbloshet_manad_jmfr_1ar_tillbaka_scb <- function(
    region_vekt = NA,
    diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
    utmapp = rdverktyg::utskriftsmapp(),
    tid_koder = "9999",
    dela_upp_utrikes = TRUE,
    returnera_data = FALSE,
    jamfor_antal_manader_bakat = 12,
    visa_dataetiketter = FALSE,
    skriv_diagramfil = FALSE,
    demo = FALSE
    ) {

# Ändrat så att man även kan jämföra förändring i arbetslöshet totalt, dvs. inte uppdelat på utrikes/inrikes födda Jon 2026-06-24
# Det går dessutom att returnera data till global environment
# SCB har lagt till ett längre streck mellan åren i åldersgrupper. Har ändrat nedan. Jon 2026-08-25

  # om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
  # demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
  if (demo){
    demo_url <-
      c("https://region-dalarna.github.io/utskrivna_diagram/bas_arbloshet_jmfr_Sveriges län_juli_ar2023-2024.png")
    purrr::walk(demo_url, ~browseURL(.x))
    if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
    rdverktyg::stop_tyst()
  }

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  gg_list <- list()

  hamta_region <- if (all(is.na(region_vekt))) rdverktyg::hamtaAllaLan(FALSE) else rdverktyg::hamtakommuner(lan = region_vekt, tamedlan = TRUE, tamedriket = TRUE)

  # Om man vill dela upp utrikes hanteras det här. "*" ger tre facetter
  # (inrikes född/totalt/utrikes född) - till skillnad från Kon nedan är
  # Fodelseregion aldrig utelämnad ur frågan, så pxweb2r:s "*"-fyllnad för
  # utelämnade variabler (se diagram_befolkningsforandring.R) är inte
  # aktuell här och ger ingen dubbelräkning.
  fodelseregion_val <- if (dela_upp_utrikes == TRUE) "*" else "totalt"

  # Länk till tabell: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0210__AM0210A/ArbStatusM/
  # Räknar ut vilken månad som ska jämföras mot (N månader bakåt), på samma
  # sätt som den gamla hamta_data-funktionen gjorde: hitta positionen för
  # begärd månad i tabellens giltiga (kronologiskt sorterade) månadskoder,
  # och plocka koden N steg tidigare.
  giltiga_manader <- pxweb2r::pxweb2_get_values("TAB6260", "Tid", quiet = TRUE)$code
  tid_nu <- if (tid_koder == "9999") max(giltiga_manader) else tid_koder
  idx_nu <- which(giltiga_manader == tid_nu)
  idx_da <- idx_nu - jamfor_antal_manader_bakat
  tid_hamta <- unique(c(tid_nu, giltiga_manader[idx_da]))

  bas_syss_df <- pxweb2r::pxweb2_get_data(
    table = "TAB6260",
    query = list(
      Region = hamta_region,
      Kon = "totalt",
      Alder = "20–64 år",
      Fodelseregion = fodelseregion_val,
      ContentsCode = "arbetslöshet",
      Tid = tid_hamta
    ), quiet = TRUE) |>
    dplyr::rename(regionkod = region_kod, arbetslöshet = value) |>
    dplyr::select(-tabellinnehåll, -dplyr::any_of("kön")) |>
    rdverktyg::manader_bearbeta_scbtabeller() |>
    dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region, byt_ut_riket_mot_sverige = TRUE))

  # Bugfix (confirmed genom test): bas_syss_df:s radordning följer frågeordningen i tid_hamta ovan
  # (tid_nu, dvs. senaste månaden, hämtas/står först - jämförelsemånaden N månader bakåt sist) och är
  # INTE kronologiskt sorterad. manad_nu/manad_da (och ar_start/ar_slut nedan) använde first()/last() åt
  # fel håll och pekade därför på varsin fel månad/år - diff blev då "äldre månad minus senaste månaden"
  # i stället för tvärtom, och diagramtiteln "i {månad} år {ar_slut} jämfört med {månad} år {ar_start}"
  # visade åren i fel ordning (t.ex. "juni 2025 jämfört med juni 2026" i stället för "juni 2026 jämfört
  # med juni 2025"). Rättat genom att byta first()/last().
  manad_nu <- as.character(dplyr::first(bas_syss_df$tid))
  manad_da <- as.character(dplyr::last(bas_syss_df$tid))

  chart_df <- bas_syss_df |>
    dplyr::select(-c(år, månad, månad_år, år_månad)) |>
    tidyr::pivot_wider(names_from = tid, values_from = arbetslöshet) |>
    dplyr::mutate(diff = .data[[manad_nu]] - .data[[manad_da]])

  # Returnerar data till R global environment
  if(returnera_data == TRUE){
    assign("forandring_arbetsloshet_df", chart_df, envir = .GlobalEnv)
  }

  # om regioner är alla kommuner i ett län eller alla län i Sverige görs revidering, annars inte
  region_start <- rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(unique(bas_syss_df$region)))
  region_txt <- rdverktyg::ar_alla_kommuner_i_ett_lan(unique(bas_syss_df$regionkod), returnera_text = TRUE, returtext = region_start)
  region_txt <- rdverktyg::ar_alla_lan_i_sverige(unique(bas_syss_df$regionkod), returnera_text = TRUE, returtext = region_txt)
  regionfil_txt <- region_txt
  regionkod_txt <- if (region_start == region_txt) paste0(unique(bas_syss_df$regionkod), collapse = "_") else region_txt

  manad_txt <- as.character(dplyr::pull(dplyr::distinct(bas_syss_df, månad)))

  ar_start <- as.character(dplyr::last(dplyr::pull(dplyr::distinct(bas_syss_df, år))))

  ar_slut <- as.character(dplyr::first(dplyr::pull(dplyr::distinct(bas_syss_df, år))))

  diagramtitel <- glue::glue("Skillnad i arbetslöshet för invånare 20-64 år i {region_txt}\ni {manad_txt} år {ar_slut} jämfört med {manad_txt} år {ar_start}")

  # Ändrar namn på figur baserat på uppdelning
  if(dela_upp_utrikes == TRUE){
    diagramfil <- glue::glue("bas_arbloshet_jmfr_{regionfil_txt}_{manad_txt}_ar{ar_start}-{ar_slut}.png")}else{
      diagramfil <- glue::glue("bas_arbloshet_jmfr_totalt_{regionfil_txt}_{manad_txt}_ar{ar_start}-{ar_slut}.png")}


  gg_obj <- rddiagram::SkapaStapelDiagram(
    skickad_df = chart_df,
    skickad_x_var = "region",
    skickad_y_var = "diff",
    skickad_x_grupp = if(dela_upp_utrikes == TRUE){"födelseregion"} else {NA},
    x_axis_sort_value = TRUE,
    diagram_titel = diagramtitel,
    diagram_capt = diagram_capt,
    stodlinjer_avrunda_fem = TRUE,
    filnamn_diagram = diagramfil,
    dataetiketter = visa_dataetiketter,
    manual_y_axis_title = "procentenheter",
    manual_x_axis_text_vjust = 1,
    manual_x_axis_text_hjust = 1,
    manual_color = if(dela_upp_utrikes == TRUE){rddiagram::diagramfarger("rus_sex")} else {rddiagram::diagramfarger("rus_sex")[1]},
    output_mapp = utmapp,
    facet_sort = TRUE,
    facet_grp = if(dela_upp_utrikes == TRUE){"födelseregion"} else {NULL},
    facet_scale = "free_x",
    facet_legend_bottom = FALSE,
    skriv_till_diagramfil = skriv_diagramfil
  )

  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfil, ".png")

  return(gg_list)
}
