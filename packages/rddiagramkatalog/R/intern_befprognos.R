# Delad hjälpfunktion för befolkningsprognosdiagrammen (diag_befolkningsprognos_profet_scb(),
# diag_befolkningsforandring_komponenter_prognos_scb()) - bygger och sparar ett enskilt
# stapeldiagram ur redan färdigbearbetad prognos-/förändringsdata. Utbruten ur
# skrivut_befprognos_diagram() i källrepots diag_befolkningsprognos_scb_api_profet.R.
intern_skrivut_befprognos_diagram <- function(skickad_df,
                                       regionkoder,
                                       skickad_jmfrtid,
                                       facet_var,
                                       facet_scale,
                                       facet_x_axis_storlek,
                                       skickad_andel,
                                       konsuppdelat,
                                       x_var,
                                       y_lbl = "",
                                       logga_storlek,
                                       ta_med_logga,
                                       logga_path,
                                       eget_regionnamn,
                                       stodlinjer_avrunda_fem,
                                       farger_diagram,
                                       dataetiketter,
                                       output_fold,
                                       utan_diagramtitel,
                                       skapa_fil,
                                       filformat,
                                       spara_excelfil,
                                       diagram_capt,
                                       filnamn_typ) {

  skickad_facetinst <- !is.na(facet_var) && length(unique(skickad_df[[facet_var]])) > 1

  region_txt <- rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(regionkoder)$region))

  etiketter_txt <- if (dataetiketter) "_lbl" else ""
  # om alla regionkoder kommer från samma län så döps regiontexten om till <län>s kommuner
  if (rdverktyg::ar_alla_kommuner_i_ett_lan(regionkoder)) {
    lan_txt <- rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(stringr::str_sub(regionkoder[1], 1, 2))$region)
    region_txt <- paste0(lan_txt, "s kommuner")
  }

  if (!is.na(eget_regionnamn)) region_txt <- eget_regionnamn              # om eget_regionnamn skickas med trumfar de alla andra inställningar
  # Om det bara är en befolkningsprognos som plottas så används inte legend (se längre ner) och information
  # om vilka år som avses + när prognosen är ifrån läggs i diagramtiteln, är det fler prognoser som plottas
  # läggs inte den informationen i diagramtiteln utan istället i legenden.
  y_lbl_diagram_titel <- if (y_lbl == "") "Befolkningsförändring" else y_lbl
  y_lbl_axel <- if (y_lbl == "") "förändring antal invånare" else y_lbl

  if (length(unique(skickad_df$prognos_ar)) < 2) {
    diagramtitel <- paste0(y_lbl_diagram_titel, " i ", region_txt, " ",
                           unique(skickad_df$start_ar), "-", unique(skickad_df$slut_ar), "\n(enligt befolkningsprognos våren ",
                           unique(skickad_df$prognos_ar), ")")
  } else {
    diagramtitel <- paste0(y_lbl_diagram_titel, " i ", region_txt, " på ", skickad_jmfrtid,
                           " års sikt")
  }

  # gör grupper
  if (skickad_facetinst) {
    pre_facet_scale <- if (facet_scale == "free") "_free" else "_fixed"
  } else pre_facet_scale <- ""
  enhet <- if (skickad_andel) "_andel" else "_antal"
  prognos_ar_txt <- paste0(unique(skickad_df$prognos_ar), collapse = "_")
  filnamn_pre <- paste0(filnamn_typ, region_txt, "_", prognos_ar_txt, "_", skickad_jmfrtid, "ars_sikt", ifelse(y_lbl == "", "", paste0("_", y_lbl)), etiketter_txt, enhet, pre_facet_scale)
  if (konsuppdelat) filnamn_pre <- paste0(filnamn_pre, "_kon")
  filnamn <- paste0(filnamn_pre, filformat)

  # lägg till fokus på åldersgruppen totalt om det finns i datasetet
  if ("aldergrp" %in% names(skickad_df)) {
    chart_df <- dplyr::mutate(skickad_df, fokus = ifelse(aldergrp == "totalt", 1, 0))
  } else chart_df <- skickad_df

  # korrigera fokus-variabeln om den finns, annars NA
  diagram_fokus_var <- if ("fokus" %in% names(chart_df)) {
    if (length(unique(chart_df$prognos_ar)) > 1) NA else "fokus"
  } else NA

  # välj om man ska ha flera grupper eller inte - antingen kön, flera prognosår eller inga alls
  vald_xgrupp <- dplyr::case_when(konsuppdelat == TRUE ~ "kön",
                           length(unique(skickad_df$prognos_ar)) > 1 ~ "ar_beskr",
                           TRUE ~ NA)

  gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = dplyr::filter(chart_df, regionkod %in% regionkoder),
                               skickad_x_var = x_var,
                               skickad_y_var = ifelse(skickad_andel, "andel", "antal"),
                               skickad_x_grupp = vald_xgrupp,
                               diagram_titel = if (utan_diagramtitel) NULL else diagramtitel,
                               output_mapp = output_fold,
                               diagram_capt = diagram_capt,
                               facet_grp = if (skickad_facetinst) facet_var else NULL,
                               facet_scale = facet_scale,
                               facet_legend_bottom = if (!is.na(vald_xgrupp)) TRUE else skickad_facetinst,
                               stodlinjer_avrunda_fem = stodlinjer_avrunda_fem,
                               x_var_fokus = if (konsuppdelat) NA else diagram_fokus_var,
                               manual_color = farger_diagram,
                               logga_scaling = logga_storlek,
                               manual_y_axis_title = ifelse(skickad_andel, "procent", y_lbl_axel),
                               x_axis_lutning = ifelse(skickad_facetinst, 45, 0),
                               manual_x_axis_text_vjust = ifelse(skickad_facetinst, 1, 0),
                               manual_x_axis_text_hjust = ifelse(skickad_facetinst, 1, 0.5),
                               facet_x_axis_storlek = facet_x_axis_storlek,
                               lagg_pa_logga = ta_med_logga,
                               logga_path = logga_path,
                               dataetiketter = dataetiketter,
                               filnamn_diagram = filnamn,
                               diagram_bildformat = stringr::str_sub(filformat, -3),
                               skriv_till_diagramfil = skapa_fil)

  retur_list <- list(gg_obj)
  names(retur_list)[length(retur_list)] <- stringr::str_remove(filnamn, filformat)

  if (spara_excelfil) writexl::write_xlsx(skickad_df, paste0(output_fold, stringr::str_replace(filnamn, filformat, ".xlsx")))

  return(retur_list)
}
