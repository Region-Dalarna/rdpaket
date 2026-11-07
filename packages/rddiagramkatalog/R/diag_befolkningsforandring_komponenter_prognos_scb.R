#' Diagram: befolkningsförändring per komponent, enligt prognos
#'
#' Visar befolkningsförändringen uppdelat på komponent (naturlig
#' befolkningstillväxt, flyttnetto inrikes, flyttnetto utrikes) mellan ett
#' startår (sista året med faktisk befolkningsstatistik) och ett valfritt
#' antal år framåt, enligt SCB:s befolkningsprognos eller Region Dalarnas
#' egna Profet-prognos. Två varianter: total förändring per komponent
#' (`diag_forandr_tot`) eller förändring år för år (`diag_forandr_per_ar`).
#'
#' @param region_vekt Region(er) att hämta prognos för.
#' @param diag_forandr_tot Skapa diagram med total förändring per komponent
#'   mellan start- och jämförelseår.
#' @param diag_forandr_per_ar Skapa diagram med förändring per år för
#'   respektive komponent.
#' @param eget_regionnamn Namn att använda i diagramrubriken. `NA`
#'   (standard) = hämtas automatiskt utifrån `region_vekt`.
#' @param jmfrtid Antal år i jämförelsen, räknat från startåret.
#' @param tabeller_url Antingen en eller flera https-url:er till SCB:s API,
#'   eller sökvägen till en mapp med lokala Profet-/Halland-prognosfiler.
#'   Se [rdhamtadata::hamta_befprognos_data()].
#' @param facet_variabel `"region"` (standard) om regionerna ska läggas som
#'   facet i ett gemensamt diagram, annars `NA` för ett diagram per region.
#' @param facet_x_axis_storlek Textstorlek på x-axeln i facet-diagram.
#' @param utmapp Mapp där diagram sparas. Måste finnas om
#'   `skriv_diagramfil = TRUE`.
#' @param gruppera_namn Namn att gruppera ihop de medskickade regionerna
#'   till en gemensam rad under, eller `NA` (standard) för ingen gruppering.
#' @param logga_sokvag Sökväg till logga.
#' @param logga_storlek Loggans storlek i diagrammet.
#' @param facet_scale `"free"` (standard) eller `"fixed"` y-axel i
#'   facet-diagram.
#' @param visa_logga_i_diagram TRUE om logga ska visas i diagrammet.
#' @param skriv_diagramfil TRUE om diagram ska skrivas till fil.
#' @param filformat Bildformat på diagrammet, t.ex. `"png"` (standard).
#' @param konsuppdelat Om `TRUE` delas diagrammet upp på kön. Stöds ännu
#'   inte fullt ut - lämna som `FALSE` (standard).
#' @param ta_bort_diagramtitel TRUE om diagramtitel ska tas bort.
#' @param andel_istallet_for_antal Om `TRUE` visas andel i stället för
#'   antal.
#' @param stodlinjer_avrunda_fem TRUE om stödlinjerna ska avrundas till
#'   närmaste femtal.
#' @param dataetiketter TRUE om dataetiketter ska visas i diagrammet.
#' @param spara_excelfil TRUE om underliggande data ska sparas som Excel-fil
#'   i `utmapp`.
#' @param diag_fargvekt Valfri färgvektor. `NA` (standard) ger
#'   `rddiagram::diagramfarger("rd_gron")`.
#' @param diagram_capt Diagrammets källtext.
#' @param returnera_figur Skall figur returneras (i en lista).
#'
#' @return En namngiven lista med `ggplot`-objekt.
#' @export
diag_befolkningsforandring_komponenter_prognos_scb <- function(region_vekt = "20",
                                              diag_forandr_tot = TRUE,
                                              diag_forandr_per_ar = FALSE,
                                              eget_regionnamn = NA,
                                              jmfrtid = 10,
                                              tabeller_url = "G:/Samhällsanalys/Statistik/Befolkningsprognoser/Profet/datafiler/",
                                              facet_variabel = "region",
                                              facet_x_axis_storlek = 5,
                                              utmapp = rdverktyg::utskriftsmapp(),
                                              gruppera_namn = NA,
                                              logga_sokvag = NA,
                                              logga_storlek = 20,
                                              facet_scale = "free",
                                              visa_logga_i_diagram = TRUE,
                                              skriv_diagramfil = TRUE,
                                              filformat = "png",
                                              konsuppdelat = FALSE,
                                              ta_bort_diagramtitel = FALSE,
                                              andel_istallet_for_antal = FALSE,
                                              stodlinjer_avrunda_fem = TRUE,
                                              dataetiketter = FALSE,
                                              spara_excelfil = FALSE,
                                              diag_fargvekt = NA,
                                              diagram_capt = "Källa: SCB:s befolkningsprognos\nBearbetning: Samhällsanalys, Region Dalarna",
                                              returnera_figur = TRUE
                                              ) {

  # ========================================== Allmän info ============================================
  # Befolkningsförändring per komponent (naturlig tillväxt, flyttnetto inrikes/utrikes) enligt
  # befolkningsprognos. Data hämtas via rdhamtadata::hamta_befprognos_data() - samma funktion som
  # diag_befolkningsprognos_profet_scb() använder, men ingen riket-jämförelse här (fanns inte i
  # originalet).
  # ========================================== Inställningar ============================================

  options(dplyr.summarise.inform = FALSE)
  options(scipen = 999)

  if (stringr::str_sub(filformat, 1, 1) != ".") filformat <- paste0(".", filformat)

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  if (is.na(diag_fargvekt[1])) diag_fargvekt <- rddiagram::diagramfarger("rd_gron")

  # ========== Hämta befolkningsprognos för tabell(er) i vektor tabeller_url  ====================

  # jmfrtid räknas från "startår" (prognosår - 1). "+N" i hamta_befprognos_data() betyder
  # "prognosår + N" - vi ber därför om "+0" till "+(jmfrtid-1)" för att få årsflödena från
  # startår+1 till slutår (jmfrtid år totalt).
  befprogn_df <- rdhamtadata::hamta_befprognos_data(region_vekt = region_vekt,
                                       url_prognos_vektor = tabeller_url,
                                       kon_klartext = c("kvinnor", "män"),
                                       tid_vekt = paste0("+", c(0:(jmfrtid - 1))),
                                       cont_klartext = c("Folkmängd", "Födda", "Döda", "Inrikes inflyttning",
                                                         "Inrikes utflyttning", "Invandring", "Utvandring"),
                                       prognos_ar = "9999"
  )

  # Här skapar vi en rad med total folkmängd i dfmalar ==========================================
  total_df <- befprogn_df |>
    dplyr::group_by(regionkod, region, kön, år, prognos_ar) |>
    dplyr::summarize(dplyr::across(dplyr::where(is.numeric), ~ sum(.x, na.rm = TRUE))) |>
    dplyr::ungroup() |>
    dplyr::mutate(ålder = "totalt ålder")

  # Lägg på total_df som rad på dfmalar
  progn_bef <- dplyr::bind_rows(befprogn_df, total_df)

  prognosar <- unique(befprogn_df$prognos_ar)                              # lägg prognosår(en) i en vektor
  startar <- as.character(as.numeric(prognosar) - 1)                       # lägg startår(en) i en vektor
  slutar <- as.character(as.numeric(startar) + jmfrtid)                    # lägg slutår(en) i en vektor

  suppressWarnings(progn_bef <- dplyr::mutate(progn_bef, aldernum = readr::parse_number(ålder)))

  # Lägg ihop i åldersgrupper
  progn_bef_alla <- progn_bef |>
    dplyr::mutate(aldergrp = dplyr::case_when(aldernum < 20 ~ "0-19 år",
                                dplyr::between(aldernum, 20, 64) ~ "20-64 år",
                                dplyr::between(aldernum, 65, 79) ~ "65-79 år",
                                aldernum > 79 ~ "80+ år",
                                TRUE ~ "totalt"),
           aldergrp = factor(aldergrp, levels = c("totalt", "0-19 år", "20-64 år", "65-79 år", "80+ år")),
           start_ar = as.character(as.numeric(prognos_ar) - 1),
           slut_ar = as.character(as.numeric(start_ar) + jmfrtid),
           ar_beskr = paste0("Förändring ", start_ar, "-", slut_ar, " (prognos våren ",
                             prognos_ar, ")"),
           Folkmängd = round(Folkmängd),
           `Naturlig befolkningstillväxt` = round(Födda - Döda),
           `Flyttnetto, inrikes` = round(`Inrikes inflyttning` - `Inrikes utflyttning`),
           `Flyttnetto, utrikes` = round(Invandring - Utvandring)) |>
    dplyr::select(-aldernum) |>
    tidyr::pivot_longer(dplyr::where(is.numeric), names_to = "befforandr", values_to = "antal") |>
    dplyr::group_by(regionkod, region, kön, aldergrp, år, prognos_ar, start_ar, slut_ar, ar_beskr, befforandr) |>
    dplyr::summarise(antal = sum(antal, na.rm = TRUE), .groups = "drop")

  if (!konsuppdelat) {
    progn_bef_alla <- progn_bef_alla |>
      dplyr::group_by(dplyr::across(-c(kön, antal))) |>
      dplyr::summarise(antal = sum(antal, na.rm = TRUE)) |>
      dplyr::ungroup()
  }

  # om vi vill gruppera ihop regionerna till en så gör vi det här
  if (!is.na(gruppera_namn)) progn_bef_alla <- progn_bef_alla |>
    dplyr::group_by(dplyr::across(c(-region, -regionkod, -antal))) |>
    dplyr::summarise(antal = sum(antal, na.rm = TRUE)) |>
    dplyr::mutate(regionkod = "grp", region = gruppera_namn) |>
    dplyr::ungroup()

  # Vi tar bara den samlade "totalt"-åldersgruppen (ingen åldersuppdelning görs här) och alla
  # "förändrings"-kategorier utom stockvariabeln Folkmängd (som inte är en flödesvariabel och
  # därför inte kan summeras över flera år på ett meningsfullt sätt).
  # - diag_forandr_tot summerar denna över alla år (startår+1 till slutår) till EN totalsiffra per
  #   kategori och period.
  # - diag_forandr_per_ar använder samma dataset rakt av, år för år (ingen summering över år).
  prognos_diff_df <- dplyr::filter(progn_bef_alla, aldergrp == "totalt", befforandr != "Folkmängd")

  gg_list <- list()

  if (diag_forandr_tot) {

    prognos_diff_chart <- prognos_diff_df |>
      dplyr::mutate(befforandr = factor(befforandr, levels = c("Naturlig befolkningstillväxt",
                                                        "Flyttnetto, inrikes", "Flyttnetto, utrikes"))) |>
      dplyr::filter(!is.na(befforandr)) |>
      dplyr::group_by(regionkod, region, prognos_ar, start_ar, slut_ar, ar_beskr, befforandr) |>
      dplyr::summarise(antal = sum(antal, na.rm = TRUE)) |>
      dplyr::ungroup()

    if (!is.na(facet_variabel)) {

      gg_list <- intern_skrivut_befprognos_diagram(skickad_df = prognos_diff_chart,
                                            regionkoder = unique(prognos_diff_chart$regionkod),
                                            skickad_jmfrtid = jmfrtid,
                                            facet_var = facet_variabel,
                                            facet_scale = facet_scale,
                                            facet_x_axis_storlek = facet_x_axis_storlek,
                                            eget_regionnamn = eget_regionnamn,
                                            farger_diagram = diag_fargvekt,
                                            konsuppdelat = konsuppdelat,
                                            dataetiketter = dataetiketter,
                                            output_fold = utmapp,
                                            skapa_fil = skriv_diagramfil,
                                            filformat = filformat,
                                            spara_excelfil = spara_excelfil,
                                            logga_storlek = logga_storlek,
                                            ta_med_logga = visa_logga_i_diagram,
                                            logga_path = logga_sokvag,
                                            stodlinjer_avrunda_fem = stodlinjer_avrunda_fem,
                                            diagram_capt = diagram_capt,
                                            utan_diagramtitel = ta_bort_diagramtitel,
                                            skickad_andel = andel_istallet_for_antal,
                                            x_var = "befforandr",
                                            filnamn_typ = "befprogn_beffor_")
    } else {
      gg_list <- purrr::flatten(purrr::map(unique(prognos_diff_chart$regionkod),
                     ~ intern_skrivut_befprognos_diagram(skickad_df = prognos_diff_chart,
                                                  regionkoder = .x,
                                                  skickad_jmfrtid = jmfrtid,
                                                  facet_var = NA,
                                                  facet_scale = facet_scale,
                                                  facet_x_axis_storlek = facet_x_axis_storlek,
                                                  eget_regionnamn = eget_regionnamn,
                                                  farger_diagram = diag_fargvekt,
                                                  konsuppdelat = konsuppdelat,
                                                  dataetiketter = dataetiketter,
                                                  output_fold = utmapp,
                                                  skapa_fil = skriv_diagramfil,
                                                  filformat = filformat,
                                                  spara_excelfil = spara_excelfil,
                                                  logga_storlek = logga_storlek,
                                                  ta_med_logga = visa_logga_i_diagram,
                                                  logga_path = logga_sokvag,
                                                  stodlinjer_avrunda_fem = stodlinjer_avrunda_fem,
                                                  diagram_capt = diagram_capt,
                                                  utan_diagramtitel = ta_bort_diagramtitel,
                                                  skickad_andel = andel_istallet_for_antal,
                                                  x_var = "befforandr",
                                                  filnamn_typ = "befprogn_beffor_")
      ))
    }
  } # slut på if-sats om man vill skriva ut diag_forandr_tot

  if (diag_forandr_per_ar) {

    if (is.na(facet_variabel)) facet_variabel <- "befforandr"

    if (facet_variabel == "region") {

      gg_list <- purrr::flatten(purrr::map(unique(prognos_diff_df$befforandr),
                     ~ intern_skrivut_befprognos_diagram(skickad_df = tidyr::complete(
                                                    dplyr::filter(prognos_diff_df, befforandr == .x, aldergrp == "totalt"),
                                                    regionkod, region, år, prognos_ar, ar_beskr,
                                                    fill = list(antal = 0, andel = 0)),
                                                  regionkoder = unique(prognos_diff_df$regionkod),
                                                  skickad_jmfrtid = jmfrtid,
                                                  facet_var = "region",
                                                  facet_scale = facet_scale,
                                                  facet_x_axis_storlek = facet_x_axis_storlek,
                                                  eget_regionnamn = eget_regionnamn,
                                                  farger_diagram = diag_fargvekt,
                                                  konsuppdelat = konsuppdelat,
                                                  dataetiketter = dataetiketter,
                                                  output_fold = utmapp,
                                                  skapa_fil = skriv_diagramfil,
                                                  filformat = filformat,
                                                  spara_excelfil = spara_excelfil,
                                                  logga_storlek = logga_storlek,
                                                  ta_med_logga = visa_logga_i_diagram,
                                                  logga_path = logga_sokvag,
                                                  stodlinjer_avrunda_fem = stodlinjer_avrunda_fem,
                                                  diagram_capt = diagram_capt,
                                                  utan_diagramtitel = ta_bort_diagramtitel,
                                                  skickad_andel = andel_istallet_for_antal,
                                                  x_var = "år",
                                                  y_lbl = .x,
                                                  filnamn_typ = "befprogn_beffor_per_ar_")))

    } else {
      gg_list <- purrr::flatten(purrr::map(unique(prognos_diff_df$regionkod),
                     ~ intern_skrivut_befprognos_diagram(skickad_df = tidyr::complete(
                                                    dplyr::filter(prognos_diff_df, regionkod == .x, aldergrp == "totalt"),
                                                    regionkod, region, år, prognos_ar, ar_beskr,
                                                    befforandr, fill = list(antal = 0, andel = 0)),
                                                  regionkoder = .x,
                                                  skickad_jmfrtid = jmfrtid,
                                                  facet_var = facet_variabel,
                                                  facet_scale = facet_scale,
                                                  facet_x_axis_storlek = facet_x_axis_storlek,
                                                  eget_regionnamn = eget_regionnamn,
                                                  farger_diagram = diag_fargvekt,
                                                  konsuppdelat = konsuppdelat,
                                                  dataetiketter = dataetiketter,
                                                  output_fold = utmapp,
                                                  skapa_fil = skriv_diagramfil,
                                                  filformat = filformat,
                                                  spara_excelfil = spara_excelfil,
                                                  logga_storlek = logga_storlek,
                                                  ta_med_logga = visa_logga_i_diagram,
                                                  logga_path = logga_sokvag,
                                                  stodlinjer_avrunda_fem = stodlinjer_avrunda_fem,
                                                  diagram_capt = diagram_capt,
                                                  utan_diagramtitel = ta_bort_diagramtitel,
                                                  skickad_andel = andel_istallet_for_antal,
                                                  x_var = "år",
                                                  filnamn_typ = "befprogn_beffor_per_ar_")))
    }
  } # slut if-sats för diag_forandr_per_ar

  if (returnera_figur == TRUE) return(gg_list)
}
