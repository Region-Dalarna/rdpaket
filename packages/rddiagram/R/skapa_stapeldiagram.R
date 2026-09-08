# SkapaStapelDiagram() - utbrutet ur func_SkapaDiagram.R och städat enligt
# REVIEW-SkapaDiagram.md (filter-mekanism, berakna_index, AF_special,
# utan_diagramtitel, skriv_till_excelfil m.m. borttaget; NA-sentineller -> NULL;
# lagg_pa_logga+logga_path -> logga; manual_color+brew_palett -> farger;
# x_axis_sort_value+x_axis_sort_grp -> sortera_x).

intern_sortera_stapel_x <- function(plot_df, x_var, x_grupp, sortera_x,
                                    vand_sortering, diagram_liggande) {
  plot_df[[x_var]] <- factor(plot_df[[x_var]])

  # riktning: stående diagram sorteras fallande (störst först), liggande stigande
  fallande <- !diagram_liggande
  if (vand_sortering) fallande <- !fallande

  if (isTRUE(sortera_x)) {
    ordning <- if (fallande) dplyr::desc(plot_df$total) else plot_df$total
    plot_df[[x_var]] <- stats::reorder(plot_df[[x_var]], ordning)
    return(plot_df)
  }

  # sortera_x är ett heltal = index för den x-grupp att sortera på
  grupp_niva <- unique(plot_df[[x_grupp]])[sortera_x]
  sort_df <- plot_df |>
    dplyr::filter(.data[[x_grupp]] == grupp_niva) |>
    dplyr::arrange(dplyr::desc(.data$total)) |>
    dplyr::transmute(dplyr::across(dplyr::all_of(x_var)), .sort = dplyr::row_number())

  plot_df <- plot_df |>
    dplyr::left_join(sort_df, by = x_var) |>
    dplyr::arrange(.data$.sort)

  ordning <- if (fallande) dplyr::desc(plot_df$.sort) else plot_df$.sort
  plot_df[[x_var]] <- stats::reorder(plot_df[[x_var]], ordning)
  plot_df$.sort <- NULL
  plot_df
}

#' Skapa ett stapeldiagram enligt Region Dalarnas profil
#'
#' @param skickad_df Data.frame med rådata.
#' @param skickad_x_var,skickad_y_var Kolumnnamn (strängar) för x respektive y.
#' @param skickad_x_grupp Kolumnnamn för grupperingsvariabel, eller `NULL`.
#' @param output_mapp,filnamn_diagram Målmapp och filnamn.
#' @param diagram_titel,diagram_undertitel,diagram_capt Titeltexter, eller `NULL`.
#' @param manual_x_axis_title,manual_y_axis_title Egna axeltitlar, eller `NULL`.
#'   `manual_y_axis_title = "procent"` lägger `%` efter värdena.
#' @param facet_grp Kolumnnamn att facetta på, eller `NULL` för inget facet.
#' @param facet_scale,facet_sort,facet_kolumner,facet_rader,facet_legend_bottom
#'   Facet-inställningar.
#' @param farger `NULL` (default), en vektor med hex-färger (manuell skala) eller
#'   en sträng (namn på en RColorBrewer-palett).
#' @param skickad_namngiven_fargvektor,farg_variabler Namngiven färgvektor och
#'   de variabler som styr färgvalet, eller `NULL`.
#' @param sortera_x `NULL` (ingen sortering), `TRUE` (sortera på totalen) eller
#'   ett heltal (index för den x-grupp som ska styra sorteringen).
#' @param vand_sortering Vänd sorteringsordningen.
#' @param x_var_fokus Kolumnnamn att fokusera färg på, eller `NULL`.
#' @param y_axis_borjar_pa_noll,y_axis_100proc,y_axis_minus_plus_samma_axel
#'   Y-axelbeteende.
#' @param procent_0_100_10intervaller Fast y-skala 0-100 med 10-steg.
#' @param stodlinjer_avrunda_fem,stodlinjer_minor_tabort Stödlinjeinställningar.
#' @param noll_linje_betona Färg på en betonad nollinje, eller `NULL`.
#' @param dataetiketter Rita ut dataetiketter.
#' @param dataetikett_storlek,dataetiketter_antal_dec,dataetikett_noll_visa_ej,dataetiketter_justering_hojdled,dataetiketter_farg
#'   Styling för dataetiketter.
#' @param diagram_liggande Vrид diagrammet liggande.
#' @param geom_position_stack Staplade staplar i stället för grupperade.
#' @param fokusera_varden Lista med annoteringar (`geom = "rect"` eller
#'   `"text"`), eller `NULL`.
#' @param legend_titel Titel på teckenförklaringen, eller `NULL`.
#' @param legend_storlek,legend_tabort,legend_vand_ordning,legend_rader,legend_kolumner,legend_byrow
#'   Legend-inställningar.
#' @param legend_kategorier_tabort_xgrupp Ta bort x-gruppen ur legendkategorierna.
#' @param x_axis_lutning,x_axis_storlek,y_axis_storlek,y_axis_lutning,manual_x_axis_text_hjust,manual_x_axis_text_vjust
#'   Axeltext-styling.
#' @param x_axis_visa_var_xe_etikett Visa bara var n:te x-etikett, eller `NULL`.
#' @param inkludera_sista_vardet_var_xe_etikett,x_axis_var_xe_etikett_ta_bort_nast_sista_vardet
#'   Detaljer för `x_axis_visa_var_xe_etikett`.
#' @param diagram_titel_storlek,undertitel_storlek,undertitel_hjust,diagram_caption_storlek
#'   Titelstyling.
#' @param facet_x_axis_storlek,facet_y_axis_storlek,facet_rubrik_storlek,facet_space_diag_horisont,facet_oka_avstand_vid_visa_sista_vardet
#'   Facet-styling.
#' @param logga `TRUE` = standardlogga, `FALSE` = ingen, en sökväg = egen logga.
#' @param logga_storlek Loggans relativa storlek.
#' @param skriv_till_diagramfil Om `TRUE` skrivs diagrammet till fil.
#' @param diagramfil_bredd,diagramfil_hojd,diagram_bildformat Filinställningar.
#'
#' @return Ett `ggplot`-objekt (osynligt om det skrivs till fil).
#' @export
SkapaStapelDiagram <- function(
    skickad_df, skickad_x_var, skickad_y_var, skickad_x_grupp = NULL,
    output_mapp, filnamn_diagram,
    diagram_titel = NULL, diagram_undertitel = NULL, diagram_capt = NULL,
    manual_x_axis_title = NULL, manual_y_axis_title = NULL,
    facet_grp = NULL, facet_scale = "free", facet_sort = FALSE,
    facet_kolumner = NULL, facet_rader = NULL, facet_legend_bottom = FALSE,
    farger = NULL, skickad_namngiven_fargvektor = NULL, farg_variabler = NULL,
    sortera_x = NULL, vand_sortering = FALSE, x_var_fokus = NULL,
    y_axis_borjar_pa_noll = TRUE, y_axis_100proc = FALSE,
    y_axis_minus_plus_samma_axel = FALSE, procent_0_100_10intervaller = FALSE,
    stodlinjer_avrunda_fem = FALSE, stodlinjer_minor_tabort = FALSE,
    noll_linje_betona = "grey40",
    dataetiketter = FALSE, dataetikett_storlek = 2.3, dataetiketter_antal_dec = 1,
    dataetikett_noll_visa_ej = FALSE, dataetiketter_justering_hojdled = 0,
    dataetiketter_farg = "#464d48",
    diagram_liggande = FALSE, geom_position_stack = FALSE, fokusera_varden = NULL,
    legend_titel = NULL, legend_storlek = 12, legend_tabort = FALSE,
    legend_vand_ordning = FALSE, legend_rader = NULL, legend_kolumner = NULL,
    legend_byrow = FALSE, legend_kategorier_tabort_xgrupp = FALSE,
    x_axis_lutning = 45, x_axis_storlek = 10.5, y_axis_storlek = 12, y_axis_lutning = 0,
    manual_x_axis_text_hjust = 0.5, manual_x_axis_text_vjust = 0,
    x_axis_visa_var_xe_etikett = NULL, inkludera_sista_vardet_var_xe_etikett = TRUE,
    x_axis_var_xe_etikett_ta_bort_nast_sista_vardet = FALSE,
    diagram_titel_storlek = 20, undertitel_storlek = 11, undertitel_hjust = 0.5,
    diagram_caption_storlek = 11,
    facet_x_axis_storlek = 8, facet_y_axis_storlek = 8, facet_rubrik_storlek = 12,
    facet_space_diag_horisont = 5.5, facet_oka_avstand_vid_visa_sista_vardet = 1.5,
    logga = TRUE, logga_storlek = 20,
    skriv_till_diagramfil = TRUE, diagramfil_bredd = 12, diagramfil_hojd = 7,
    diagram_bildformat = "png") {

  har_grupp <- !is.null(skickad_x_grupp)
  har_facet <- !is.null(facet_grp)
  har_fokus <- !is.null(x_var_fokus)
  har_namngiven <- !is.null(skickad_namngiven_fargvektor) && !is.null(farg_variabler)
  stacked <- isTRUE(geom_position_stack)

  if (!grepl("[/\\\\]$", output_mapp)) output_mapp <- paste0(output_mapp, "/")
  if (inkludera_sista_vardet_var_xe_etikett) {
    facet_space_diag_horisont <- facet_space_diag_horisont + facet_oka_avstand_vid_visa_sista_vardet
  }

  # gruppvariabler för summeringen
  grupp_var <- unique(stats::na.omit(c(
    skickad_x_var,
    if (har_grupp) skickad_x_grupp,
    if (har_fokus) x_var_fokus,
    if (har_facet) facet_grp
  )))

  plot_df <- intern_forbered_plotdata(skickad_df, grupp_var, skickad_y_var)

  # y-axel
  grans <- intern_yaxel_grans(plot_df, skickad_x_var, skickad_x_grupp, facet_grp,
                              stacked, y_axis_100proc)
  st <- Berakna_varden_stodlinjer(
    min_varde = grans$min, max_varde = grans$max,
    y_borjar_pa_noll = y_axis_borjar_pa_noll,
    procent_0_100_10intervaller = procent_0_100_10intervaller,
    avrunda_fem = stodlinjer_avrunda_fem,
    minus_plus_samma = y_axis_minus_plus_samma_axel
  )

  antal_grupper <- if (har_grupp) dplyr::n_distinct(plot_df[[skickad_x_grupp]]) else 0L

  # färger
  if (har_namngiven) {
    chart_col <- skickad_namngiven_fargvektor
  } else {
    chart_col <- intern_valj_farger(farger, if (har_grupp) antal_grupper else 1L)
  }

  y_titel <- manual_y_axis_title %||% skickad_y_var
  if (identical(manual_y_axis_title, "procent")) y_titel <- NULL

  geom_bar_position <- if (stacked) "stack" else "dodge"
  if (har_facet) y_axis_storlek <- facet_y_axis_storlek

  # sortering av x
  if (!is.null(sortera_x)) {
    plot_df <- intern_sortera_stapel_x(plot_df, skickad_x_var, skickad_x_grupp,
                                       sortera_x, vand_sortering, diagram_liggande)
  } else if (diagram_liggande) {
    plot_df[[skickad_x_var]] <- stats::reorder(
      factor(plot_df[[skickad_x_var]]),
      if (vand_sortering) dplyr::desc(plot_df[[skickad_x_var]]) else plot_df[[skickad_x_var]]
    )
  }

  # facet-sortering (kräver tidytext)
  if (har_facet && (facet_sort || har_namngiven)) {
    if (!requireNamespace("tidytext", quietly = TRUE)) {
      stop("Paketet 'tidytext' krävs för facet_sort / namngiven färgvektor i facet.", call. = FALSE)
    }
    plot_df[[facet_grp]] <- factor(plot_df[[facet_grp]])
    plot_df[[skickad_x_var]] <- tidytext::reorder_within(
      plot_df[[skickad_x_var]], plot_df$total, plot_df[[facet_grp]]
    )
  }

  legend_pos <- "none"

  # bygg ggplot
  if (har_fokus) {
    plot_df[[x_var_fokus]] <- factor(plot_df[[x_var_fokus]])
    p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data[[skickad_x_var]], y = .data$total)) +
      ggplot2::geom_bar(stat = "identity", position = geom_bar_position,
                        ggplot2::aes(fill = .data[[x_var_fokus]]))
  } else if (har_namngiven) {
    p <- ggplot2::ggplot(plot_df, ggplot2::aes(
      x = .data[[skickad_x_var]], y = .data$total,
      fill = interaction(.data[[names(farg_variabler)]], .data[[skickad_x_grupp]])
    ))
    if (!har_facet || facet_legend_bottom) legend_pos <- "bottom"
  } else if (!har_grupp) {
    p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data[[skickad_x_var]], y = .data$total,
                                               fill = chart_col))
  } else {
    p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data[[skickad_x_var]], y = .data$total,
                                               fill = as.factor(.data[[skickad_x_grupp]])))
    if (!har_facet || facet_legend_bottom) legend_pos <- "bottom"
  }
  if (legend_tabort) legend_pos <- "none"

  stapel_bredd <- if (antal_grupper > 3) 0.6 else 0.9
  if (!har_fokus) {
    p <- p + ggplot2::geom_bar(stat = "identity", width = stapel_bredd,
                               position = geom_bar_position)
  }

  # dataetiketter
  if (dataetiketter) {
    pos <- if (stacked) ggplot2::position_stack(vjust = 0.5) else ggplot2::position_dodge(width = stapel_bredd)
    label_expr <- if (dataetikett_noll_visa_ej) {
      ggplot2::aes(y = .data$total + sign(.data$total), x = .data[[skickad_x_var]],
                   label = ifelse(.data$total == 0, "", round(.data$total, dataetiketter_antal_dec)))
    } else {
      ggplot2::aes(y = .data$total + sign(.data$total), x = .data[[skickad_x_var]],
                   label = round(.data$total, dataetiketter_antal_dec))
    }
    p <- p + ggplot2::geom_text(
      label_expr,
      vjust = ifelse(plot_df$total >= 0, -0.5 - dataetiketter_justering_hojdled,
                     1 + dataetiketter_justering_hojdled),
      color = dataetiketter_farg, size = dataetikett_storlek, position = pos
    )
  }

  if (diagram_liggande) p <- p + ggplot2::coord_flip()

  # tema
  p <- p + intern_rd_diagramtema(
    legend_pos = legend_pos, x_axis_lutning = x_axis_lutning,
    x_axis_storlek = if (har_facet) facet_x_axis_storlek else x_axis_storlek,
    y_axis_storlek = y_axis_storlek, y_axis_lutning = y_axis_lutning,
    legend_storlek = legend_storlek, diagram_titel_storlek = diagram_titel_storlek,
    undertitel_hjust = undertitel_hjust, undertitel_storlek = undertitel_storlek,
    diagram_caption_storlek = diagram_caption_storlek,
    manual_x_axis_text_hjust = manual_x_axis_text_hjust,
    manual_x_axis_text_vjust = manual_x_axis_text_vjust,
    stodlinjer_minor_tabort = stodlinjer_minor_tabort,
    facet_space_mm = facet_space_diag_horisont
  )

  if (diagram_liggande) {
    minor_x <- if (stodlinjer_minor_tabort) ggplot2::element_blank() else
      ggplot2::element_line(linewidth = 0.4, colour = "lightgrey")
    p <- p + ggplot2::theme(
      panel.grid.major.x = ggplot2::element_line(linewidth = 0.8, colour = "lightgrey"),
      panel.grid.minor.x = minor_x,
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      plot.margin = ggplot2::unit(c(5.5, 25, 5.5, 5.5), "pt")
    )
  }

  p <- p +
    ggplot2::labs(title = diagram_titel, subtitle = diagram_undertitel,
                  caption = diagram_capt, x = manual_x_axis_title, y = y_titel,
                  fill = legend_titel) +
    ggplot2::guides(fill = ggplot2::guide_legend(
      title.position = "top", title.hjust = 0.5, reverse = legend_vand_ordning,
      ncol = legend_kolumner, nrow = legend_rader, byrow = legend_byrow
    ))

  if (har_namngiven) {
    legend_kategorier <- levels(interaction(
      plot_df[[names(farg_variabler)]], plot_df[[skickad_x_grupp]]
    ))
    legend_kategorier <- gsub("\\.", " ", legend_kategorier)
    p <- p + ggplot2::scale_fill_manual(values = skickad_namngiven_fargvektor,
                                        labels = legend_kategorier)
  } else {
    p <- p + ggplot2::scale_fill_manual(values = chart_col)
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
      limits = c(st$min_yvar, st$max_yvar), expand = c(0, 0)
    )
  } else {
    ggplot2::scale_y_continuous(labels = intern_etikett_format(manual_y_axis_title),
                                expand = c(0, 0))
  }

  if (har_facet) {
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", facet_grp)),
                                 scales = facet_scale, ncol = facet_kolumner, nrow = facet_rader)
    if (facet_sort || har_namngiven) p <- p + tidytext::scale_x_reordered()
    p <- p + ggplot2::theme(
      strip.text = ggplot2::element_text(color = "black", size = facet_rubrik_storlek),
      strip.background = ggplot2::element_blank()
    )
  } else {
    p <- p + ggplot2::theme(strip.text = ggplot2::element_blank())
  }

  if (!is.null(noll_linje_betona) && st$min_yvar <= 0 && st$max_yvar >= 0) {
    p <- p + ggplot2::geom_hline(yintercept = 0, colour = noll_linje_betona, linewidth = 0.8)
  }

  # annoteringar
  if (!is.null(fokusera_varden)) {
    annoteringar <- if (is.null(names(fokusera_varden))) fokusera_varden else list(fokusera_varden)
    for (a in annoteringar) {
      if (identical(a$geom, "rect")) {
        p <- p + ggplot2::annotate("rect", xmin = a$xmin, xmax = a$xmax,
                                   ymin = a$ymin, ymax = a$ymax, alpha = a$alpha, fill = a$fill)
      } else if (identical(a$geom, "text")) {
        p <- p + ggplot2::annotate("text", x = a$x, y = a$y, label = a$label,
                                   color = a$color, size = a$size, fontface = a$fontface,
                                   angle = a$angle)
      }
    }
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
