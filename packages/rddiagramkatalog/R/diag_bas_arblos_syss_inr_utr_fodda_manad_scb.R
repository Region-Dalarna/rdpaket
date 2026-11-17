#' Diagram: arbetslöshet och sysselsättning per månad, inrikes och utrikes födda (BAS)
#'
#' Stapeldiagram ur SCB:s Befolkningens arbetsmarknadsstatus (BAS), månadsvis, uppdelat på inrikes och utrikes
#' födda. Funktionen ger ett diagram per anrop, av två slag (`diagramtyp`):
#' * `"jamforelse"`: de valda regionerna jämförda i den senaste månaden med data, sorterade efter värdet. Med
#'   standardvärdet för `region_vekt` blir det Dalarna, länets kommuner och riket.
#' * `"tidsserie"`: utvecklingen månad för månad. Med en region ett diagram, med flera regioner en panel per region.
#'
#' Väljer man ett län och en av länets kommuner i `region_vekt` får man alltså en jämförelse mellan dem.
#'
#' @param region_vekt En eller flera regioner: läns-, kommun- eller rikskod (`"00"`). Standard är Dalarna, länets
#'   kommuner och riket, `rdverktyg::hamtakommuner("20")`.
#' @param diagramtyp `"jamforelse"` (standard) eller `"tidsserie"`, se ovan.
#' @param alder_klartext Åldersgrupp, bara en åt gången. Standard `"20–64 år"`. Finns `"15–19 år"`, `"16–19 år"`,
#'   `"20–24 år"` ... `"60–64 år"`, `"65–69 år"`, `"70–74 år"`, `"15–74 år"`, `"16–64 år"`, `"16–65 år"`,
#'   `"16–66 år"`, `"20–64 år"`, `"20–65 år"` och `"20–66 år"`.
#' @param kon_klartext Kön: `"totalt"` (standard), `"män"` eller `"kvinnor"`. Diagrammet delas inte upp på kön.
#' @param fodelseregion_klartext `c("inrikes född", "utrikes född")` (standard) ger en stapel per grupp, `"totalt"`
#'   ger en stapel för alla.
#' @param cont_klartext Vad som visas, bara en åt gången: `"arbetslöshet"` (standard), `"antal sysselsatta"`,
#'   `"antal arbetslösa"`, `"antal sysselsatta och arbetslösa (arbetskraften)"`, `"antal studerande"`,
#'   `"antal pensionärer"`, `"antal sjuka"`, `"antal övriga"`, `"antal totalt"`, `"arbetskraftsdeltagande"` eller
#'   `"sysselsättningsgrad"`.
#' @param tid_koder Vilka månader som hämtas, t.ex. `"2024M01:9999"`. `NULL` (standard) ger senaste månaden med data
#'   för `"jamforelse"` och alla månader för `"tidsserie"`. En jämförelse visar alltid bara den senaste av de
#'   hämtade månaderna.
#' @param dataetiketter `TRUE` ger värden på staplarna.
#' @param x_axis_visa_var_xe_etikett Visa bara var n:te etikett på x-axeln (t.ex. 3 = var tredje). `NA`
#'   (standard) visar alla.
#' @param inkludera_sista_vardet_var_xe_etikett `TRUE` tar alltid med sista värdet när bara var n:te etikett visas.
#' @param diag_fargvekt Färgvektor, `NULL` (standard) ger `rd_gron`.
#' @param diagram_capt Diagrammets källtext.
#' @param utmapp Mapp där diagrammet sparas. Måste finnas om `skriv_diagramfil = TRUE`.
#' @param skriv_diagramfil `TRUE` om diagrammet ska skrivas till fil.
#' @param filformat Bildformat, `"png"` (standard) eller `"svg"`.
#' @param returnera_data `TRUE` lägger data i `.GlobalEnv` som `arblos_syss_manad`.
#' @param returnera_figur `TRUE` (standard) returnerar diagrammet.
#'
#' @return En namngiven lista med ett `ggplot`-objekt.
#' @export
diag_bas_arblos_syss_inr_utr_fodda_manad_scb <- function(
    region_vekt = rdverktyg::hamtakommuner("20"),
    diagramtyp = c("jamforelse", "tidsserie"),
    alder_klartext = "20–64 år",
    kon_klartext = "totalt",
    fodelseregion_klartext = c("inrikes född", "utrikes född"),
    cont_klartext = "arbetslöshet",
    tid_koder = NULL,
    dataetiketter = FALSE,
    x_axis_visa_var_xe_etikett = NA,
    inkludera_sista_vardet_var_xe_etikett = TRUE,
    diag_fargvekt = NULL,
    diagram_capt = "Källa: SCB:s öppna statistikdatabas, Befolkningens arbetsmarknadsstatus (BAS)\nBearbetning: Samhällsanalys, Region Dalarna",
    utmapp = rdverktyg::utskriftsmapp(),
    skriv_diagramfil = TRUE,
    filformat = "png",
    returnera_data = FALSE,
    returnera_figur = TRUE
) {

  # ========================================== Inställningar ============================================

  diagramtyp <- match.arg(diagramtyp)
  for (p in c("alder_klartext", "kon_klartext", "cont_klartext")) {
    if (length(get(p)) != 1) stop(p, " ska vara ett enda värde.", call. = FALSE)
  }
  region_vekt <- unique(as.character(region_vekt))
  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)
  if (is.null(tid_koder)) tid_koder <- if (diagramtyp == "jamforelse") "9999" else "*"

  # ========================================== Hämta data ===============================================
  # Länk till tabell: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0210__AM0210A/ArbStatusM/

  tid <- intern_tid_for_tabell("TAB6260", tid_koder, ger_senaste = TRUE)
  intern_stoppa_om_ingen_tid(tid, "arbetsmarknadsstatus (TAB6260)")
  data_df <- pxweb2r::pxweb2_get_data(
    table = "TAB6260",
    query = list(Region = region_vekt, Kon = kon_klartext, Alder = alder_klartext,
                 Fodelseregion = fodelseregion_klartext, ContentsCode = cont_klartext, Tid = tid),
    quiet = TRUE) |>
    dplyr::rename(regionkod = region_kod, varde = value) |>
    dplyr::filter(!is.na(varde))
  if (nrow(data_df) == 0) stop("SCB gav inga värden för det valda urvalet.", call. = FALSE)

  manadsnamn <- c("januari", "februari", "mars", "april", "maj", "juni", "juli", "augusti", "september",
                  "oktober", "november", "december")
  data_df <- data_df |>
    intern_behall_senaste_tid(tid_koder, tidkol = "månad") |>
    dplyr::mutate(
      ar = substr(månad, 1, 4),
      manad = manadsnamn[as.integer(substr(månad, 6, 7))],
      manad_ar = paste(substr(manad, 1, 3), ar),
      region = rdverktyg::skapa_kortnamn_lan(region, byt_ut_riket_mot_sverige = TRUE))

  # etiketterna ska ligga i tidsordning på x-axeln, inte alfabetiskt
  data_df$manad_ar <- factor(data_df$manad_ar, levels = unique(data_df$manad_ar[order(data_df$månad)]))

  if (returnera_data) assign("arblos_syss_manad", data_df, envir = .GlobalEnv)

  # ========================================== Texter ===================================================

  antal_var <- grepl("antal", cont_klartext)
  cont_txt <- if (cont_klartext == "arbetslöshet") "Andel arbetslösa" else stringr::str_to_sentence(cont_klartext)
  cont_filnamn <- cont_txt |> tolower() |> chartr(old = "åäö", new = "aao") |> stringr::str_replace_all("[^a-z0-9]+", "_")
  alder_txt <- unique(data_df$ålder)
  alder_filnamn <- gsub("[^0-9]+", "_", alder_txt) |> sub(pattern = "_$", replacement = "")
  grupper <- length(unique(data_df$födelseregion))
  bakgr_txt <- if (grupper == 2) "_bakgr" else ""
  etikett_txt <- if (dataetiketter) "_etiketter" else ""
  regioner_txt <- if (length(unique(data_df$region)) == 1) {
    unique(data_df$region)
  } else paste0(length(unique(data_df$region)), "_regioner")
  regioner_fil <- gsub("[^a-z0-9]+", "_", tolower(chartr("åäöÅÄÖ", "aaoaao", regioner_txt)))
  farger <- if (is.null(diag_fargvekt)) {
    if (grupper == 2) rddiagram::diagramfarger("rd_gron")[c(1, 4)] else rddiagram::diagramfarger("rd_gron")[1]
  } else diag_fargvekt
  y_titel <- if (antal_var) "antal" else "procent"
  grupp_var <- if (grupper == 2) "födelseregion" else NA

  # ========================================== Diagram ==================================================

  if (diagramtyp == "jamforelse") {
    senaste <- data_df |> dplyr::filter(månad == max(månad))
    ar_manad <- paste(senaste$manad[1], senaste$ar[1])
    diagram_titel <- paste0(cont_txt, " ", alder_txt, " i ", ar_manad)
    diagramfil <- paste0(cont_filnamn, bakgr_txt, "_", alder_filnamn, "_", regioner_fil, "_",
                         senaste$manad[1], "_", senaste$ar[1], etikett_txt, ".", filformat)
    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = senaste,
      skickad_x_var = "region",
      skickad_y_var = "varde",
      skickad_x_grupp = grupp_var,
      x_axis_sort_value = TRUE,
      x_axis_sort_grp = if (grupper == 2) 2 else NA,
      vand_sortering = grupper == 2,
      diagram_titel = diagram_titel,
      diagram_capt = diagram_capt,
      stodlinjer_avrunda_fem = TRUE,
      x_axis_visa_var_xe_etikett = x_axis_visa_var_xe_etikett,
      inkludera_sista_vardet_var_xe_etikett = inkludera_sista_vardet_var_xe_etikett,
      dataetiketter = dataetiketter,
      dataetiketter_justering_hojdled = if (grupper == 2) -1.7 else 0,
      manual_y_axis_title = y_titel,
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      manual_color = farger,
      output_mapp = utmapp,
      filnamn_diagram = diagramfil,
      skriv_till_diagramfil = skriv_diagramfil)
  } else {
    flera <- length(unique(data_df$region)) > 1
    diagram_titel <- paste0(cont_txt, " ", alder_txt, if (!flera) paste0(" i ", regioner_txt))
    diagramfil <- paste0(cont_filnamn, bakgr_txt, "_tid_", alder_filnamn, "_", regioner_fil, etikett_txt, ".", filformat)
    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = data_df,
      skickad_x_var = "manad_ar",
      skickad_y_var = "varde",
      skickad_x_grupp = grupp_var,
      x_axis_storlek = if (flera) NULL else 8,
      diagram_titel = diagram_titel,
      diagram_capt = diagram_capt,
      stodlinjer_avrunda_fem = TRUE,
      dataetiketter = dataetiketter && !flera,
      dataetiketter_justering_hojdled = if (grupper == 2) -1.7 else -17,
      x_axis_visa_var_xe_etikett = x_axis_visa_var_xe_etikett,
      inkludera_sista_vardet_var_xe_etikett = inkludera_sista_vardet_var_xe_etikett,
      facet_grp = if (flera) "region" else NULL,
      facet_scale = "fixed",
      facet_x_axis_storlek = 4,
      manual_y_axis_title = y_titel,
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      manual_color = farger,
      output_mapp = utmapp,
      filnamn_diagram = diagramfil,
      skriv_till_diagramfil = skriv_diagramfil)
  }

  gg_list <- stats::setNames(list(gg_obj), tools::file_path_sans_ext(diagramfil))
  if (returnera_figur) return(gg_list)
  invisible(NULL)
}
