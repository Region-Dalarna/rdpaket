#' Diagram: befolkning inrikes/utrikes födda, en åldersgrupp
#'
#' Stapeldiagram över befolkning uppdelat på inrikes/utrikes födda, för en
#' vald åldersgrupp, per år. Med `forandring = TRUE` visas i stället hur
#' befolkningen förändrats från året innan, uppdelat på inrikes och utrikes
#' födda, med ett svart streck i varje stapel för förändringen totalt.
#'
#' @param region_vekt Läns- och kommunkoder, det blir ett diagram (och en fil om man skriver bildfiler) per region.
#' @param aldersintervall Två värden: från och med åldern, till och med ålder. `c(20,65)` blir åldersgruppen 20-65 år.
#' @param forandring `TRUE` visar förändringen från året innan (första året i urvalet har ingen förändring och
#'   visas inte) i stället för antalet. Staplarna för inrikes och utrikes födda läggs på varandra, och ett svart
#'   streck i varje stapel visar förändringen totalt (summan av de två grupperna).
#' @param visa_totalvarden Bara med `forandring = TRUE`: `TRUE` (standard) ritar totalstrecket, `FALSE` döljer det.
#' @param diagram_capt Diagrammets källtext.
#' @param utmapp Mapp där diagram (och ev. excelfil) ska sparas. Måste finnas om `skriv_diagramfil = TRUE` eller `skriv_till_excelfil = TRUE`.
#' @param diag_fargvekt Valfri färgvektor, standard: `rddiagram::diagramfarger("rus_sex")`.
#' @param visa_logga_i_diagram TRUE om logga ska visas i diagrammet.
#' @param logga_sokvag Sökväg till logga.
#' @param stodlinjer_avrunda_fem Avrunda stödlinjerna till närmaste femtal.
#' @param tid_koder Vilka år som ska hämtas, `"*"` = alla.
#' @param returnera_data Skall data returneras till global environment.
#' @param ta_bort_diagramtitel FALSE så skrivs ingen diagramtitel ut.
#' @param visa_dataetiketter Visa dataetiketter.
#' @param skriv_diagramfil TRUE om diagram ska skrivas till fil.
#' @param skriv_till_excelfil TRUE om data ska skrivas till en excelfil.
#'
#' @return En namngiven lista med `ggplot`-objekt.
#' @export
diag_bef_inr_utr_en_aldersgrupp_scb <- function(
    region_vekt = "20",
    aldersintervall = c(20, 65),
    forandring = FALSE,
    visa_totalvarden = TRUE,
    diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
    utmapp = rdverktyg::utskriftsmapp(),
    diag_fargvekt = NA,
    visa_logga_i_diagram = TRUE,
    logga_sokvag = NA,
    stodlinjer_avrunda_fem = TRUE,
    tid_koder = "*",
    returnera_data = FALSE,
    ta_bort_diagramtitel = FALSE,
    visa_dataetiketter = FALSE,
    skriv_diagramfil = TRUE,
    skriv_till_excelfil = FALSE
) {

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil || skriv_till_excelfil)

  gg_list <- list()

  # om ingen färgvektor är medskickad, använd rddiagram::diagramfarger("rus_sex")
  if (all(is.na(diag_fargvekt))) {
    diag_fargvekt <- rddiagram::diagramfarger("rus_sex")[c(1,2)]
  }

  if (length(aldersintervall) != 2) stop("Parametern aldersintervall måste innehålla två värden. Det ena är från och med åldern, det andra till och med ålder. aldersintervall = c(20,65) blir alltså åldersgruppen 20-65 år.")

  aldersgrupp_txt <- glue::glue("{min(aldersintervall)}-{max(aldersintervall)} år")

  # hamta_data-repots hamta_bef_region_alder_kon_fodelseregion_tid_
  # InrUtrFoddaRegAlKon_scb.R (v1: BE0101E/InrUtrFoddaRegAlKon) hämtas här
  # direkt via v2-motsvarigheterna TAB4823 (historik 2000-2024) och TAB6645
  # (CKM 2025-), samma tabellpar som redan verifierats i
  # diag_bef_inrikes_utrikes_antal_forandring_prognos_IntRap.R. Individuella
  # åldrar (inte "totalt") begärs alltid explicit eftersom historiktabellen
  # saknar en "totalt"-kod för Alder - ren digitkod (t.ex. "20") funkar för
  # båda tabellerna så länge intervallet inte når 100+ år (skulle det
  # hända krävs samma "hamta_individuella_aldrar()"-hjälpfunktion som i
  # systerskriptet).
  alder_koder_hamta <- as.character(min(aldersintervall):max(aldersintervall))

  # Vilka år som hämtas ur respektive tabell, se intern_tid_for_tabell().
  tid_tab4823 <- intern_tid_for_tabell("TAB4823", tid_koder, ger_senaste = TRUE)
  tid_tab6645 <- intern_tid_for_tabell("TAB6645", tid_koder, ger_senaste = TRUE)
  intern_stoppa_om_ingen_tid(list(tid_tab4823, tid_tab6645), "folkmängd efter födelseregion")

  bef_folkmangd_historik <- if (length(tid_tab4823) > 0) pxweb2r::pxweb2_get_data(
    table = "TAB4823",
    query = list(
      Region = region_vekt,
      Alder = alder_koder_hamta,
      Kon = c("män","kvinnor"),
      Fodelseregion = c("född i Sverige","utrikes född"),
      ContentsCode = "Antal",
      Tid = tid_tab4823
    ),
    on_all_values_invalid = "null", quiet = TRUE)

  bef_folkmangd_ckm <- if (length(tid_tab6645) > 0) pxweb2r::pxweb2_get_data(
    table = "TAB6645",
    query = list(
      Region = region_vekt,
      Alder = alder_koder_hamta,
      Kon = c("män","kvinnor"),
      Fodelseregion = c("född i Sverige","utrikes född"),
      ContentsCode = "Antal",
      Tid = tid_tab6645
    ),
    on_all_values_invalid = "null", quiet = TRUE)

  bef_folkmangd <- rdverktyg::funktion_upprepa_forsok_om_fel(function()
    dplyr::bind_rows(bef_folkmangd_historik, bef_folkmangd_ckm) |>
      intern_behall_senaste_tid(tid_koder) |>
      dplyr::rename(regionkod = region_kod, Antal = value) |>
      dplyr::select(-tabellinnehåll))

  diagram_df <- bef_folkmangd |>
      dplyr::group_by(år, regionkod, region, bakgrund = födelseregion) |>
      dplyr::summarise(antal = sum(Antal, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(aldersgrupp = aldersgrupp_txt,
             bakgrund = ifelse(bakgrund == "utrikes född", "Utrikes födda", "Inrikes födda"),
             bakgrund = factor(bakgrund, levels = c("Utrikes födda", "Inrikes födda")),
             region = rdverktyg::skapa_kortnamn_lan(region))

  # förändring från året innan, per region och bakgrund. Första året saknar föregående år och tas bort.
  if (forandring) {
    diagram_df <- diagram_df |>
      dplyr::arrange(regionkod, bakgrund, år) |>
      dplyr::group_by(regionkod, region, bakgrund) |>
      dplyr::mutate(antal = antal - dplyr::lag(antal)) |>
      dplyr::ungroup() |>
      dplyr::filter(!is.na(antal)) |>
      dplyr::arrange(regionkod, år, bakgrund)
    if (nrow(diagram_df) == 0) stop("För att visa förändring behövs minst två år. Ange fler år i tid_koder.", call. = FALSE)
  }

  # returnera datasetet till global environment, bl.a. bra när man skapar Rmarkdown-rapporter
  if(returnera_data == TRUE){
    assign("bef_inr_utr_en_aldersgrupp", diagram_df, envir = .GlobalEnv)
  }

  if (aldersgrupp_txt %in% c("20-64 år", "20-65 år")) {
    aldersgrupp_txt <- paste0("i arbetsför ålder (", aldersgrupp_txt, ")")
  }

  skapa_diagram <- function(skickad_regionkod) {

    diagram_region_df <- dplyr::filter(diagram_df, regionkod %in% skickad_regionkod)

    region_txt <- rdverktyg::skapa_kortnamn_lan(unique(diagram_region_df$region))

    diagramtitel <- if (forandring) glue::glue("Befolkningsförändring {aldersgrupp_txt} i {region_txt}") else glue::glue("Befolkning {aldersgrupp_txt} i {region_txt}")
    diagramfil <- glue::glue("bef_inr_utr_{if (forandring) 'forandring_' else ''}{aldersgrupp_txt}_{region_txt}_ar{min(diagram_region_df$år)}-{max(diagram_region_df$år)}.png")

    # totalstrecket: ett kort svart streck i varje stapel vid summan av inrikes och utrikes födda, som i
    # diag_inr_flyttnetto_bakgrund_scb(). x-positionen är årets plats på den diskreta x-axeln.
    visa_total <- forandring && visa_totalvarden
    total_list <- NA
    if (visa_total) {
      totaler <- dplyr::summarise(dplyr::group_by(diagram_region_df, år), summa = sum(antal, na.rm = TRUE), .groups = "drop")
      totaler <- dplyr::arrange(totaler, år)
      halv_tjocklek <- max(diff(range(c(0, totaler$summa))), 1) * 0.006
      total_list <- purrr::map(seq_len(nrow(totaler)), \(i) list(
        geom = "rect", ymin = totaler$summa[i] - halv_tjocklek, ymax = totaler$summa[i] + halv_tjocklek,
        xmin = i - 0.45, xmax = i + 0.45, alpha = 1, fill = "black"))
    }

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = diagram_region_df,
      skickad_x_var = "år",
      skickad_y_var = "antal",
      skickad_x_grupp = "bakgrund",
      diagram_titel = if (ta_bort_diagramtitel) NULL else diagramtitel,
      diagram_capt = diagram_capt,
      stodlinjer_avrunda_fem = stodlinjer_avrunda_fem,
      filnamn_diagram = diagramfil,
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      manual_color = diag_fargvekt,
      output_mapp = utmapp,
      lagg_pa_logga = visa_logga_i_diagram,
      logga_path = logga_sokvag,
      dataetiketter = visa_dataetiketter,
      geom_position_stack = TRUE,
      fokusera_varden = total_list,
      legend_vand_ordning = TRUE,
      facet_legend_bottom = TRUE,
      skriv_till_diagramfil = skriv_diagramfil && !visa_total
    ) # slut skriv ggplot_objekt

    if (visa_total) {
      # totalstrecket ritas av fokusera_varden ovan; det här osynliga linjelagret ger bara en förklaring i
      # teckenförklaringen (samma lösning som i diag_inr_flyttnetto_bakgrund_scb()). Diagrammet måste sparas
      # här eftersom lagret läggs till efter SkapaStapelDiagram().
      gg_obj <- gg_obj +
        ggplot2::geom_line(ggplot2::aes(group = 1, color = "line"), alpha = 0) +
        ggplot2::scale_color_manual(name = "", values = c("line" = "black"), labels = "förändring totalt") +
        # override.aes gör strecket i teckenförklaringen synligt trots att själva linjelagret är osynligt
        ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(alpha = 1, linewidth = 1.5))) +
        ggplot2::theme(legend.key = ggplot2::element_rect(fill = "white"), legend.box.just = "bottom")
      if (skriv_diagramfil) {
        rddiagram::skriv_till_diagramfil(gg_obj, output_mapp = utmapp, filnamn_diagram = diagramfil,
                                         logga = if (visa_logga_i_diagram) (if (is.na(logga_sokvag)) TRUE else logga_sokvag) else FALSE)
      }
    }

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfil, ".png")

    return(gg_list)

  } # slut funktion som skapar diagrammet

  retur_list <- purrr::flatten(purrr::map(region_vekt, ~skapa_diagram(skickad_regionkod = .x)))

  if (skriv_till_excelfil) {
    region_xlsx <- paste0(rdverktyg::skapa_kortnamn_lan(unique(diagram_df$region)), collapse = "_")
    excefilnamn <- glue::glue("bef_inr_utr_{aldersgrupp_txt}_{region_xlsx}_ar{min(diagram_df$år)}-{max(diagram_df$år)}.xlsx")
    openxlsx::write.xlsx(diagram_df, paste0(utmapp, excefilnamn))
  }

  return(retur_list)

} # slut funktion
