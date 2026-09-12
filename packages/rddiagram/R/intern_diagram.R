# Interna hjälpfunktioner som delas av SkapaStapelDiagram() och SkapaLinjeDiagram().

# Gruppera och summera indata till plot-data med kolumnen `total`.
intern_forbered_plotdata <- function(df, grupp_var, y_var, na_rm = TRUE) {
  df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grupp_var))) |>
    dplyr::summarise(total = sum(.data[[y_var]], na.rm = na_rm), .groups = "drop")
}

# Största (eller minsta) summerade stapelhöjd per x-grupp, för stacked-diagram.
intern_stack_extremvarde <- function(plot_df, grupper, fn = max, filter_tecken = NULL) {
  d <- plot_df
  if (!is.null(filter_tecken)) {
    d <- if (filter_tecken > 0) dplyr::filter(d, .data$total > 0) else dplyr::filter(d, .data$total < 0)
  }
  d |>
    dplyr::group_by(dplyr::across(dplyr::any_of(grupper))) |>
    dplyr::summarise(summ = sum(.data$total), .groups = "drop") |>
    dplyr::summarise(varde = fn(.data$summ)) |>
    dplyr::pull(.data$varde)
}

# Bestäm min/max för y-axeln, med hänsyn till stacked-läge och facet.
intern_yaxel_grans <- function(plot_df, x_var, x_grupp, facet_grp, stacked, y_axis_100proc) {
  max_v <- if (y_axis_100proc) 100 else max(plot_df$total, na.rm = TRUE)

  if (stacked) {
    if (max(plot_df$total, na.rm = TRUE) > 0 && min(plot_df$total, na.rm = TRUE) < 0) {
      max_v <- intern_stack_extremvarde(plot_df, x_var, max, filter_tecken = 1)
      min_v <- intern_stack_extremvarde(plot_df, x_var, min, filter_tecken = -1)
    } else {
      grupper <- if (!is.null(facet_grp)) {
        stats::na.omit(c(x_var, facet_grp, x_grupp))
      } else {
        x_var
      }
      max_v <- intern_stack_extremvarde(plot_df, grupper, max)
      min_v <- intern_stack_extremvarde(plot_df, grupper, min)
    }
  } else {
    min_v <- min(plot_df$total, na.rm = TRUE)
  }
  list(min = min_v, max = max_v)
}

# Etikettformat för y-axeln (svenskt talformat, ev. procenttecken).
intern_etikett_format <- function(manual_y_axis_title = NULL) {
  procent <- identical(manual_y_axis_title, "procent")
  function(x) {
    ut <- format(x, big.mark = " ", decimal.mark = ",", scientific = FALSE)
    if (procent) ut <- paste0(ut, " %")
    ut
  }
}

# Välj färgvektor: manuell färg/vektor (går alltid före, oavsett antal
# grupper - som i originalet, där manual_color kollas separat och FÖRE
# brew_palett/standardfärgen), annars brewer-palett eller default.
intern_valj_farger <- function(manual_color, brew_palett, antal_grupper) {
  har_manual <- is.character(manual_color) && !anyNA(manual_color) && length(manual_color) > 0
  if (har_manual) {
    if (length(manual_color) > 1) return(manual_color)
    return(manual_color[1])
  }

  palett <- if (is.character(brew_palett) && length(brew_palett) == 1 && !is.na(brew_palett)) {
    brew_palett
  } else {
    "Greens"
  }

  if (antal_grupper <= 1) return("#4f6228")
  if (antal_grupper == 2) return(c("#9bbb59", "#4f6228"))
  if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
    stop("Paketet 'RColorBrewer' krävs för fler än 2 grupper utan egen färgvektor.",
         call. = FALSE)
  }
  RColorBrewer::brewer.pal(antal_grupper, palett)
}

# Gemensamt tema för RD-diagram.
intern_rd_diagramtema <- function(legend_pos, x_axis_lutning, x_axis_storlek, y_axis_storlek,
                                  y_axis_lutning = 0, legend_storlek = 12,
                                  diagram_titel_storlek = 20, undertitel_hjust = 0.5,
                                  undertitel_storlek = 11, diagram_caption_storlek = 11,
                                  manual_x_axis_text_hjust = 0.5, manual_x_axis_text_vjust = 0,
                                  stodlinjer_minor_tabort = FALSE, facet_space_mm = 5.5,
                                  linjediagram = FALSE) {
  minor_y <- if (stodlinjer_minor_tabort) {
    ggplot2::element_blank()
  } else {
    ggplot2::element_line(linewidth = 0.4, colour = "lightgrey")
  }

  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      size = x_axis_storlek, angle = x_axis_lutning,
      hjust = if (linjediagram) 1 else manual_x_axis_text_hjust,
      vjust = if (linjediagram) NULL else manual_x_axis_text_vjust
    ),
    axis.text.y = ggplot2::element_text(size = y_axis_storlek, angle = y_axis_lutning),
    axis.ticks = ggplot2::element_blank(),
    legend.position = legend_pos,
    legend.margin = ggplot2::margin(0, 0, 0, 0),
    legend.title = if (linjediagram) ggplot2::element_blank() else ggplot2::element_text(),
    legend.text = ggplot2::element_text(size = legend_storlek),
    legend.key = if (linjediagram) ggplot2::element_rect(fill = "white") else ggplot2::element_blank(),
    plot.title = ggtext::element_textbox_simple(
      size = diagram_titel_storlek, width = ggplot2::unit(0.9, "npc"),
      halign = 0.5, margin = ggplot2::margin(7, 0, 7, 0)
    ),
    plot.title.position = "plot",
    plot.subtitle = ggplot2::element_text(hjust = undertitel_hjust, size = undertitel_storlek),
    plot.caption = ggplot2::element_text(face = "italic", hjust = 0, vjust = 0,
                                         size = diagram_caption_storlek),
    plot.caption.position = "plot",
    panel.background = ggplot2::element_rect(fill = "white"),
    panel.grid.major.y = ggplot2::element_line(linewidth = 0.8, colour = "lightgrey"),
    panel.grid.minor.y = minor_y,
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank(),
    panel.spacing.x = ggplot2::unit(facet_space_mm, "mm")
  )
}
