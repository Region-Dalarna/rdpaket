# SkapaLinjeDiagram() - utbrutet ur func_SkapaDiagram.R och städat enligt
# REVIEW-SkapaDiagram.md.

#' Skapa ett linjediagram enligt Region Dalarnas profil
#'
#' @param skickad_df Data.frame med rådata.
#' @param skickad_x_var,skickad_y_var Kolumnnamn (strängar) för x respektive y.
#' @param skickad_x_grupp Kolumnnamn för grupperingsvariabel, eller `NULL`.
#' @param output_mapp,filnamn_diagram Målmapp och filnamn.
#' @param diagram_titel,diagram_undertitel,diagram_capt Titeltexter, eller `NULL`.
#' @param manual_x_axis_title,manual_y_axis_title Egna axeltitlar, eller `NULL`.
#' @param berakna_index Räkna om serierna till index (100 = första året).
#' @param lagga_till_punkter Lägg till punkter vid varje observation.
#' @param facet_grp Kolumnnamn att facetta på, eller `NULL`.
#' @param facet_scale,facet_sort,facet_kolumner,facet_rader,facet_legend_bottom
#'   Facet-inställningar.
#' @param facet_x_axis_storlek,facet_y_axis_storlek,facet_rubrik_storlek,facet_space_diag_horisont,facet_oka_avstand_vid_visa_sista_vardet
#'   Facet-styling.
#' @param farger `NULL` (default), en vektor med hex-färger eller ett
#'   RColorBrewer-palettnamn.
#' @param na_varden_behall_i_dataset Behåll `NA`-värden (bryt linjerna).
#' @param linjetyp_kolumn Kolumn som styr linjetyp, eller `NULL`.
#' @param linjetyp_typvektor Vektor med linjetyper, eller `NULL`.
#' @param marginal_y_axis Marginal (expand) på y-axeln.
#' @param stodlinjer_avrunda_fem,stodlinjer_minor_tabort Stödlinjeinställningar.
#' @param y_axis_borjar_pa_noll,y_axis_100proc,y_axis_minus_plus_samma_axel,procent_0_100_10intervaller
#'   Y-axelbeteende.
#' @param x_axis_lutning,x_axis_storlek,y_axis_storlek Axeltext-styling.
#' @param x_axis_visa_var_xe_etikett Visa bara var n:te x-etikett, eller `NULL`.
#' @param inkludera_sista_vardet_var_xe_etikett,x_axis_var_xe_etikett_ta_bort_nast_sista_vardet
#'   Detaljer för `x_axis_visa_var_xe_etikett`.
#' @param legend_titel Titel på teckenförklaringen, eller `NULL`.
#' @param legend_storlek,legend_tabort,legend_vand_ordning,legend_rader,legend_kolumner,legend_byrow
#'   Legend-inställningar.
#' @param diagram_titel_storlek,undertitel_storlek,undertitel_hjust,diagram_caption_storlek
#'   Titelstyling.
#' @param logga `TRUE` = standardlogga, `FALSE` = ingen, en sökväg = egen logga.
#' @param logga_storlek Loggans relativa storlek.
#' @param skriv_till_diagramfil Om `TRUE` skrivs diagrammet till fil.
#' @param diagramfil_bredd,diagramfil_hojd,diagram_bildformat Filinställningar.
#'
#' @return Ett `ggplot`-objekt (osynligt om det skrivs till fil).
#' @export
SkapaLinjeDiagram <- function(
    skickad_df, skickad_x_var, skickad_y_var, skickad_x_grupp = NULL,
    output_mapp, filnamn_diagram,
    diagram_titel = NULL, diagram_undertitel = NULL, diagram_capt = NULL,
    manual_x_axis_title = NULL, manual_y_axis_title = NULL,
    berakna_index = FALSE, lagga_till_punkter = FALSE,
    facet_grp = NULL, facet_scale = "free", facet_sort = FALSE,
    facet_kolumner = NULL, facet_rader = NULL, facet_legend_bottom = FALSE,
    facet_x_axis_storlek = 8, facet_y_axis_storlek = 8, facet_rubrik_storlek = 12,
    facet_space_diag_horisont = 5.5, facet_oka_avstand_vid_visa_sista_vardet = 1.5,
    farger = NULL, na_varden_behall_i_dataset = FALSE,
    linjetyp_kolumn = NULL, linjetyp_typvektor = NULL,
    marginal_y_axis = c(0, 0),
    stodlinjer_avrunda_fem = FALSE, stodlinjer_minor_tabort = FALSE,
    y_axis_borjar_pa_noll = TRUE, y_axis_100proc = FALSE,
    y_axis_minus_plus_samma_axel = FALSE, procent_0_100_10intervaller = FALSE,
    x_axis_lutning = 45, x_axis_storlek = 10.5, y_axis_storlek = 12,
    x_axis_visa_var_xe_etikett = NULL, inkludera_sista_vardet_var_xe_etikett = TRUE,
    x_axis_var_xe_etikett_ta_bort_nast_sista_vardet = FALSE,
    legend_titel = NULL, legend_storlek = 12, legend_tabort = FALSE,
    legend_vand_ordning = FALSE, legend_rader = NULL, legend_kolumner = NULL,
    legend_byrow = FALSE,
    diagram_titel_storlek = 20, undertitel_storlek = 11, undertitel_hjust = 0.5,
    diagram_caption_storlek = 11,
    logga = TRUE, logga_storlek = 15,
    skriv_till_diagramfil = TRUE, diagramfil_bredd = 12, diagramfil_hojd = 7,
    diagram_bildformat = "png") {

  har_grupp <- !is.null(skickad_x_grupp)
  har_facet <- !is.null(facet_grp)
  na_rm <- !na_varden_behall_i_dataset

  if (!grepl("[/\\\\]$", output_mapp)) output_mapp <- paste0(output_mapp, "/")
  if (inkludera_sista_vardet_var_xe_etikett) {
    facet_space_diag_horisont <- facet_space_diag_horisont + facet_oka_avstand_vid_visa_sista_vardet
  }

  grupp_var <- unique(stats::na.omit(c(
    skickad_x_var,
    if (har_grupp) skickad_x_grupp,
    if (har_facet) facet_grp,
    linjetyp_kolumn
  )))

  plot_df <- intern_forbered_plotdata(skickad_df, grupp_var, skickad_y_var, na_rm = na_rm)

  if (berakna_index) {
    idx_grp <- if (har_grupp) skickad_x_grupp else character(0)
    plot_df <- plot_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(idx_grp))) |>
      dplyr::mutate(
        total = round(.data$total / .data$total[.data[[skickad_x_var]] == min(.data[[skickad_x_var]])] * 100, 0)
      ) |>
      dplyr::ungroup()
    y_titel <- manual_y_axis_title %||%
      paste0(strsplit(skickad_y_var, ",")[[1]][1], ", index 100 = ", min(plot_df[[skickad_x_var]]))
  } else {
    y_titel <- manual_y_axis_title %||% skickad_y_var
  }
  if (identical(manual_y_axis_title, "procent")) y_titel <- NULL

  # y-axel
  grans <- intern_yaxel_grans(plot_df, skickad_x_var, skickad_x_grupp, facet_grp,
                              stacked = FALSE, y_axis_100proc = y_axis_100proc)
  st <- Berakna_varden_stodlinjer(
    min_varde = grans$min, max_varde = grans$max,
    y_borjar_pa_noll = y_axis_borjar_pa_noll,
    procent_0_100_10intervaller = procent_0_100_10intervaller,
    avrunda_fem = stodlinjer_avrunda_fem,
    minus_plus_samma = y_axis_minus_plus_samma_axel
  )

  antal_grupper <- if (har_grupp) dplyr::n_distinct(plot_df[[skickad_x_grupp]]) else 1L
  chart_col <- intern_valj_farger(farger, antal_grupper)
  if (!har_grupp) chart_col <- chart_col[1]

  if (is.null(linjetyp_typvektor)) linjetyp_typvektor <- rep("solid", antal_grupper)

  legend_pos <- "none"

  if (!har_grupp) {
    p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data[[skickad_x_var]], y = .data$total,
                                               group = chart_col))
    if (berakna_index) p <- p + ggplot2::geom_hline(yintercept = 100, color = "grey32", linewidth = 1.2)
    p <- p + ggplot2::geom_line(ggplot2::aes(color = chart_col), linewidth = 1.5)
  } else {
    p <- ggplot2::ggplot(plot_df, ggplot2::aes(
      x = .data[[skickad_x_var]], y = .data$total,
      group = .data[[skickad_x_grupp]], linetype = .data[[skickad_x_grupp]]
    ))
    if (berakna_index) p <- p + ggplot2::geom_hline(yintercept = 100, color = "grey32", linewidth = 1.2)
    p <- p + ggplot2::geom_line(ggplot2::aes(color = .data[[skickad_x_grupp]]), linewidth = 1.5)
    if (!har_facet || facet_legend_bottom) legend_pos <- "bottom"
  }
  if (legend_tabort) legend_pos <- "none"

  if (lagga_till_punkter && har_grupp) {
    p <- p + ggplot2::geom_point(ggplot2::aes(color = .data[[skickad_x_grupp]]), size = 2.5)
  } else if (lagga_till_punkter) {
    p <- p + ggplot2::geom_point(size = 2.5)
  }

  p <- p + intern_rd_diagramtema(
    legend_pos = legend_pos, x_axis_lutning = x_axis_lutning,
    x_axis_storlek = if (har_facet) facet_x_axis_storlek else x_axis_storlek,
    y_axis_storlek = if (har_facet) facet_y_axis_storlek else y_axis_storlek,
    legend_storlek = legend_storlek, diagram_titel_storlek = diagram_titel_storlek,
    undertitel_hjust = undertitel_hjust, undertitel_storlek = undertitel_storlek,
    diagram_caption_storlek = diagram_caption_storlek,
    stodlinjer_minor_tabort = stodlinjer_minor_tabort,
    facet_space_mm = facet_space_diag_horisont, linjediagram = TRUE
  )

  p <- p +
    ggplot2::labs(title = diagram_titel, subtitle = diagram_undertitel,
                  x = manual_x_axis_title, caption = diagram_capt, y = y_titel,
                  color = legend_titel, linetype = NULL) +
    ggplot2::guides(color = ggplot2::guide_legend(
      title.position = "top", title.hjust = 0.5, reverse = legend_vand_ordning,
      ncol = legend_kolumner, nrow = legend_rader, byrow = legend_byrow
    )) +
    ggplot2::scale_color_manual(values = chart_col)

  if (!is.null(linjetyp_typvektor) && !all(linjetyp_typvektor == "solid")) {
    p <- p + ggplot2::scale_linetype_manual(values = linjetyp_typvektor)
  }

  if (!is.null(x_axis_visa_var_xe_etikett)) {
    p <- p + ggplot2::scale_x_discrete(
      expand = c(0, 0),
      breaks = every_nth(x_axis_visa_var_xe_etikett, inkludera_sista_vardet_var_xe_etikett,
                         x_axis_var_xe_etikett_ta_bort_nast_sista_vardet)
    )
  }

  fast_yskala <- !har_facet || (facet_scale == "fixed" && har_facet)
  p <- p + if (fast_yskala) {
    ggplot2::scale_y_continuous(
      breaks = seq(st$min_yvar, st$max_yvar, by = st$maj_by_yvar),
      minor_breaks = if (stodlinjer_minor_tabort) NULL else seq(st$min_yvar, st$max_yvar, by = st$min_by_yvar),
      labels = intern_etikett_format(manual_y_axis_title),
      limits = c(st$min_yvar, st$max_yvar), expand = marginal_y_axis
    )
  } else {
    ggplot2::scale_y_continuous(labels = intern_etikett_format(manual_y_axis_title),
                                expand = marginal_y_axis)
  }

  if (har_facet) {
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", facet_grp)),
                                 scales = "free", ncol = facet_kolumner, nrow = facet_rader)
    if (facet_sort) {
      if (!requireNamespace("tidytext", quietly = TRUE)) {
        stop("Paketet 'tidytext' krävs för facet_sort.", call. = FALSE)
      }
      p <- p + tidytext::scale_x_reordered()
    }
    p <- p + ggplot2::theme(
      strip.text = ggplot2::element_text(color = "black", size = facet_rubrik_storlek),
      strip.background = ggplot2::element_blank()
    )
  } else {
    p <- p + ggplot2::theme(strip.text = ggplot2::element_blank())
  }

  if (skriv_till_diagramfil) {
    skriv_till_diagramfil(
      ggplot_objekt = p, output_mapp = output_mapp, filnamn_diagram = filnamn_diagram,
      diagramfil_bredd = diagramfil_bredd, diagramfil_hojd = diagramfil_hojd,
      logga = logga, logga_storlek = logga_storlek, diagram_bildformat = diagram_bildformat
    )
    return(invisible(p))
  }
  p
}
