#' Diagram: arbetspendling över tid
#'
#' Fem diagram över pendlingen till och från kommuner:
#' 1. In- och utpendling samt de som bor och jobbar i samma kommun, per år och kommun från 1993 och framåt.
#' 2. Samma som ovan men utan de som bor och jobbar i samma kommun.
#' 3. Nettopendlingen per år och kommun från 1993 och framåt.
#' 4. Största inpendlingskommuner för den kommun man valt, per år och kommun från 1993 och framåt.
#' 5. Största utpendlingskommuner för den kommun man valt, per år och kommun från 1993 och framåt.
#'
#' @param region_vekt Region(er).
#' @param tid_koder Vilka år som ska hämtas, `"*"` = alla. OBS: till skillnad
#'   från flera andra funktioner i paketet stöds inte `"start:9999"`-mönstret
#'   här, eftersom fyra olika tabeller med olika giltiga årsintervall slås
#'   ihop - ange i stället en explicit årsvektor vid behov.
#' @param skriv_ut_dataetiketter_diagram Visa dataetiketter.
#' @param skriv_diagramfil TRUE om diagram ska skrivas till fil.
#' @param skriv_till_excelfil TRUE om data ska skrivas till en excelfil.
#' @param utmapp Mapp där diagram (och ev. excelfil) ska sparas. Måste finnas om `skriv_diagramfil = TRUE` eller `skriv_till_excelfil = TRUE`.
#' @param diag_fargvekt_tre Valfri färgvektor (tre färger), standard: `rddiagram::diagramfarger("bla_gra_tre")`.
#' @param diag_fargvekt_atta Valfri färgvektor (åtta färger), standard: `RColorBrewer::brewer.pal(9, "Set3")`.
#' @param visa_logga_i_diagram TRUE om logga ska visas i diagrammet.
#' @param logga_sokvag Sökväg till logga.
#' @param diagram_capt Diagrammets källtext.
#' @param diag_in_ut_sammma Skapa diagram för in- och utpendling inklusive de som bor och arbetar i samma kommun.
#' @param diag_in_ut Skapa diagram för in- och utpendling, utan de som bor och arbetar i samma kommun.
#' @param diag_nettopendling Skapa diagram för nettopendling.
#' @param diag_storsta_inpendlrelationer Skapa diagram för de största inpendlingsrelationerna.
#' @param diag_storsta_utpendlrelationer Skapa diagram för de största utpendlingsrelationerna.
#'
#' @return En namngiven lista med `ggplot`-objekt.
#' @export
diag_arbetspendling_over_tid_scb <- function(
    region_vekt = "20",
    tid_koder = "*",
    skriv_ut_dataetiketter_diagram = FALSE,
    skriv_diagramfil = TRUE,
    skriv_till_excelfil = FALSE,
    utmapp = rdverktyg::utskriftsmapp(),
    diag_fargvekt_tre = NA,
    diag_fargvekt_atta = NA,
    visa_logga_i_diagram = TRUE,
    logga_sokvag = NA,
    diagram_capt = "Källa: SCB:s öppna statistikdatabas, bearbetning av: Samhällsanalys, Region Dalarna\nPendlingsstatistiken har tidsseriebrott år 2003-2004, 2018-2019 och 2019-2020, så jämförelser mellan dessa perioder bör göras med viss försiktighet.",
    diag_in_ut_sammma = TRUE,
    diag_in_ut = TRUE,
    diag_nettopendling = TRUE,
    diag_storsta_inpendlrelationer = TRUE,
    diag_storsta_utpendlrelationer = TRUE
  ) {

  # =======================================================================================================
  #
  # Migrerad till pxweb2r/rddiagram/rdverktyg. Pendlingsdatan (fyra SCB-tabeller, RAMS och BAS) hämtas
  # med rdhamtadata::hamta_pendling_rams_bas_scb(), som delas med karta_gg_nettopendling_kommun_scb()
  # och karta_lf_pendlingsfloden_scb().
  # =======================================================================================================

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil || skriv_till_excelfil)

  # om ingen färgvektor är medskickad, använd rddiagram::diagramfarger("bla_gra_tre")
  if (all(is.na(diag_fargvekt_tre))) diag_fargvekt_tre <- rddiagram::diagramfarger("bla_gra_tre")

  # detsamma görs för fargvektor med åtta färger
  if (all(is.na(diag_fargvekt_atta))) diag_fargvekt_atta <- RColorBrewer::brewer.pal(9, "Set3")[c(1,3:7)]

  gg_list <- list()               # skapa lista för att lägga diagrammen i
  flikar_list <- list()

  dataetiketter_txt <- ifelse(skriv_ut_dataetiketter_diagram, "_dataetiketter", "")

  # =============================================== API-uttag ===============================================

  px_df <- rdverktyg::funktion_upprepa_forsok_om_fel(function()
    suppressMessages(rdhamtadata::hamta_pendling_rams_bas_scb(region_vekt = region_vekt, tid_vekt = tid_koder))
  )

  skapa_diagram <- function(regionkod) {
    # förbered regionkoder för att också kunna aggregera till län (tabellerna innehåller bara kommuner)
    regiontyp <- ifelse(nchar(regionkod) == 2, "län", "kommun")
    regiontyp_titel <- ifelse(nchar(regionkod) == 2, "län", "kommuner")
    region_txt <- rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(regionkod)$region))

    # förbered df för att skapa diagram
    pendling_df <- px_df |>
      dplyr::filter(regionkod_bo %in% regionkod | regionkod_arb %in% regionkod) |>
      dplyr::mutate(pendlingstyp = dplyr::case_when(
        regionkod_bo == regionkod_arb ~ paste0("bor och arbetar i samma ", regiontyp),
        regionkod_bo %in% regionkod ~ "utpendlare",
        regionkod_arb %in% regionkod ~ "inpendlare")) |>
      dplyr::rename(antal_pendlare = pendlare) |>
      dplyr::relocate(år, pendlingstyp, .before = 1)

    if (skriv_till_excelfil) {
      flikar_list <- c(flikar_list, list(pendling_df))
      names(flikar_list)[length(flikar_list)] <- "pendling_in_ut"
    }

    # =================== diagram för in- och utpendlng över kommungräns samt även de som bor och arbetar i samma kommun =========================

    if (diag_in_ut_sammma){

      diagram_titel <- paste0("In- och utpendling i ", region_txt, " ", min(pendling_df$år), "-", max(pendling_df$år))
      diagramfil <- paste0("pendling_", region_txt, "_", min(pendling_df$år), "-", max(pendling_df$år), dataetiketter_txt, ".png")

      gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = pendling_df,
                         skickad_x_var = "år",
                         skickad_x_grupp = "pendlingstyp",
                         skickad_y_var = "antal_pendlare",
                         diagram_titel = diagram_titel,
                         diagram_capt = diagram_capt,
                         manual_y_axis_title = "",
                         manual_x_axis_text_vjust = 1,
                         manual_x_axis_text_hjust = 1,
                         stodlinjer_avrunda_fem = TRUE,
                         manual_color = diag_fargvekt_tre,
                         dataetiketter = skriv_ut_dataetiketter_diagram,
                         lagg_pa_logga = visa_logga_i_diagram,
                         logga_path = logga_sokvag,
                         logga_scaling = 22,
                         output_mapp = utmapp,
                         skriv_till_diagramfil = skriv_diagramfil,
                         filnamn_diagram = diagramfil)

      gg_list <- c(gg_list, list(gg_obj))
      names(gg_list)[length(gg_list)] <- "diag_in_ut_sammma"

    } # slut if-sats för diag_in_ut_sammma


    if (diag_in_ut){
      diagram_titel <- paste0("In- och utpendling i ", region_txt, " ", min(pendling_df$år), "-", max(pendling_df$år))
      diagramfil <- paste0("pendling_bara_", region_txt, "_", min(pendling_df$år), "-", max(pendling_df$år), dataetiketter_txt, ".png")

      gg_obj <- rddiagram::SkapaStapelDiagram(
                         skickad_df = dplyr::filter(pendling_df, pendlingstyp != paste0("bor och arbetar i samma ", regiontyp)),
                         skickad_x_var = "år",
                         skickad_x_grupp = "pendlingstyp",
                         skickad_y_var = "antal_pendlare",
                         diagram_titel = diagram_titel,
                         diagram_capt = diagram_capt,
                         manual_y_axis_title = "",
                         manual_x_axis_text_vjust = 1,
                         manual_x_axis_text_hjust = 1,
                         stodlinjer_avrunda_fem = TRUE,
                         dataetiketter = skriv_ut_dataetiketter_diagram,
                         lagg_pa_logga = visa_logga_i_diagram,
                         logga_path = logga_sokvag,
                         manual_color = diag_fargvekt_tre[c(2,3)],
                         logga_scaling = 22,
                         skriv_till_diagramfil = skriv_diagramfil,
                         output_mapp = utmapp,
                         filnamn_diagram = diagramfil)

      gg_list <- c(gg_list, list(gg_obj))
      names(gg_list)[length(gg_list)] <- "diag_in_ut"

    } # slut if-sats för diag_in_ut


    if (diag_nettopendling){

      nettopendling_df <- pendling_df |>
        dplyr::mutate(regionkod = dplyr::if_else(pendlingstyp == "inpendlare", regionkod_arb, regionkod_bo),
               region = dplyr::if_else(pendlingstyp == "inpendlare", arbetsställeregion, bostadsregion)) |>
        dplyr::group_by(år, regionkod, region, kön, pendlingstyp) |>
        dplyr::summarise(antal_pendlare = sum(antal_pendlare, na.rm = TRUE), .groups = "drop") |>
        tidyr::pivot_wider(names_from = pendlingstyp,
                    values_from = antal_pendlare) |>
        dplyr::mutate(nettopendling = inpendlare - utpendlare) |>
        dplyr::rename(ar = år)

      # skriv till excelfil om man valt det
      if (skriv_till_excelfil) {
        flikar_list <- c(flikar_list, list(nettopendling_df))
        names(flikar_list)[length(flikar_list)] <- "nettopendling"
      }

      diagram_titel <- paste0("Nettopendling i ", region_txt, " ", min(pendling_df$år), "-", max(pendling_df$år))
      diagramfil <- paste0("nettopendling_", region_txt, "_", min(pendling_df$år), "-", max(pendling_df$år), dataetiketter_txt, ".png")

      gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = nettopendling_df,
                         skickad_x_var = "ar",
                         skickad_y_var = "nettopendling",
                         diagram_titel = diagram_titel,
                         diagram_capt = diagram_capt,
                         dataetiketter = skriv_ut_dataetiketter_diagram,
                         manual_x_axis_text_vjust = 1,
                         manual_x_axis_text_hjust = 1,
                         stodlinjer_avrunda_fem = TRUE,
                         manual_color = diag_fargvekt_tre[1],
                         lagg_pa_logga = visa_logga_i_diagram,
                         logga_path = logga_sokvag,
                         logga_scaling = 22,
                         skriv_till_diagramfil = skriv_diagramfil,
                         output_mapp = utmapp,
                         filnamn_diagram = diagramfil)

      gg_list <- c(gg_list, list(gg_obj))
      names(gg_list)[length(gg_list)] <- "diag_nettopendling"

    } # slut if-sats för diag_nettopendling

    # =========================================== diagram med pendlingsrelationsregioner =====================================

    if (diag_storsta_inpendlrelationer | diag_storsta_utpendlrelationer) {


      bara_pendlare_df <- dplyr::filter(pendling_df, pendlingstyp != paste0("bor och arbetar i samma ", regiontyp))

      # skriv till excelfil om man valt det
      if (skriv_till_excelfil) {
        flikar_list <- c(flikar_list, list(bara_pendlare_df))
        names(flikar_list)[length(flikar_list)] <- "pendling_relationer"
      }

      # ta ut de sex största inpendlingskommunerna och lägg i en vektor
      storsta_inpendl_regioner <- bara_pendlare_df |>
        dplyr::filter(pendlingstyp == "inpendlare") |>
        dplyr::group_by(regionkod_bo) |>
        dplyr::summarise(antal_pendlare = sum(antal_pendlare, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(dplyr::desc(antal_pendlare)) |>
        dplyr::slice(1:6) |>
        dplyr::pull(regionkod_bo)


      # ta ut de sex största utpendlingskommunerna och lägg i en vektor
      storsta_utpendl_regioner <- bara_pendlare_df |>
        dplyr::filter(pendlingstyp == "utpendlare") |>
        dplyr::group_by(regionkod_arb) |>
        dplyr::summarise(antal_pendlare = sum(antal_pendlare, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(dplyr::desc(antal_pendlare)) |>
        dplyr::slice(1:6) |>
        dplyr::pull(regionkod_arb)

      # ================= diagram - inpendling till de sex största inpendlingsrelationsregionerna över hela perioden =================

      if (diag_storsta_inpendlrelationer) {
        diagram_titel <- paste0("Största inpendlings", regiontyp_titel, " till ", region_txt, " ", min(pendling_df$år), "-", max(pendling_df$år))
        diagramfil <- paste0("inpendlingsrelationer_", region_txt, "_", min(pendling_df$år), "-", max(pendling_df$år), dataetiketter_txt, ".png")

        gg_obj <- rddiagram::SkapaStapelDiagram(
                             skickad_df = dplyr::filter(bara_pendlare_df, pendlingstyp == "inpendlare",
                                    regionkod_bo %in% storsta_inpendl_regioner),
                           skickad_x_var = "år",
                           skickad_x_grupp = "bostadsregion",
                           skickad_y_var = "antal_pendlare",
                           diagram_titel = diagram_titel,
                           diagram_capt = diagram_capt,
                           manual_y_axis_title = "antal pendlare",
                           manual_x_axis_text_vjust = 1,
                           manual_x_axis_text_hjust = 1,
                           stodlinjer_avrunda_fem = TRUE,
                           manual_color = diag_fargvekt_atta,
                           lagg_pa_logga = visa_logga_i_diagram,
                           logga_scaling = 22,
                           logga_path = logga_sokvag,
                           dataetiketter = skriv_ut_dataetiketter_diagram,
                           skriv_till_diagramfil = skriv_diagramfil,
                           output_mapp = utmapp,
                           filnamn_diagram = diagramfil)

        gg_list <- c(gg_list, list(gg_obj))
        names(gg_list)[length(gg_list)] <- "diag_storsta_inpendlrelationer"

      } # slut if-sats om diagram med största inpendlingskommuner är vald

      # ================= diagram - utpendling till de sex största utpendlingsrelationsregionerna över hela perioden =================

      if (diag_storsta_utpendlrelationer) {
        diagram_titel <- paste0("Största utpendlings", regiontyp_titel, " i ", region_txt, " ", min(pendling_df$år), "-", max(pendling_df$år))
        diagramfil <- paste0("utpendlingsrelationer_", region_txt, "_", min(pendling_df$år), "-", max(pendling_df$år), dataetiketter_txt, ".png")

        gg_obj <- rddiagram::SkapaStapelDiagram(
                             skickad_df = dplyr::filter(bara_pendlare_df, pendlingstyp == "utpendlare",
                                    regionkod_arb %in% storsta_utpendl_regioner),
                           skickad_x_var = "år",
                           skickad_x_grupp = "arbetsställeregion",
                           skickad_y_var = "antal_pendlare",
                           diagram_titel = diagram_titel,
                           diagram_capt = diagram_capt,
                           manual_y_axis_title = "antal pendlare",
                           manual_x_axis_text_vjust = 1,
                           manual_x_axis_text_hjust = 1,
                           stodlinjer_avrunda_fem = TRUE,
                           manual_color = diag_fargvekt_atta,
                           logga_scaling = 22,
                           logga_path = logga_sokvag,
                           lagg_pa_logga = visa_logga_i_diagram,
                           dataetiketter = skriv_ut_dataetiketter_diagram,
                           skriv_till_diagramfil = skriv_diagramfil,
                           output_mapp = utmapp,
                           filnamn_diagram = diagramfil)

        gg_list <- c(gg_list, list(gg_obj))
        names(gg_list)[length(gg_list)] <- "diag_storsta_utpendlrelationer"

      } # slut if-sats om diagram med största utpendlingskommuner är vald

    } # if-sats om någon av största in- eller utpendlingsrelationskommuner är valda

    if (skriv_till_excelfil) {
      start_ar <- min(pendling_df$år)
      slut_ar <- max(pendling_df$år)
      pendling_excelflikar <- purrr::compact(flikar_list)
      writexl::write_xlsx(pendling_excelflikar, paste0(utmapp, "arbetspendling_", start_ar, "-", slut_ar, "_", regionkod, ".xlsx"))
    }

    return(gg_list)              # här returnerar vi listan med ggplot-diagram
    } # slut funktion för att skapa själva diagrammen

    retur_list <- purrr::flatten(purrr::map(region_vekt, ~ skapa_diagram(.x)))
    return(retur_list)
} # slut funktion
