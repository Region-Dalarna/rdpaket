# skapa_koropletkarta_ggplot() - utbrutet ur func_SkapaDiagram.R och städat
# enligt funktioner/REVIEW-SkapaDiagram.md (döda exists()-wrappers borttagna,
# antal_unika beräknas en gång, karta_farg_hogst/karta_bredd -> NULL,
# returnera_ggobj borttagen, glue borttaget, namespace kvalificerat).

intern_karta_accuracy <- function(values) {
  values <- stats::na.omit(values)
  if (length(values) == 0 || all(values == round(values))) return(1)
  diffs <- diff(sort(unique(values)))
  if (length(diffs) == 0 || min(diffs) >= 0.1) return(0.1)
  if (min(diffs) >= 0.01) return(0.01)
  0.001
}

intern_snygga_klassetiketter <- function(klass, acc) {
  if (!is.factor(klass)) return(klass)
  forcats::fct_relabel(klass, function(niva) {
    vapply(niva, function(intervall) {
      intervall <- gsub("\\(|\\)|\\[|\\]", "", intervall)
      delar <- trimws(strsplit(intervall, ",")[[1]])
      nedre <- as.numeric(delar[1])
      ovre <- as.numeric(delar[2])
      if (is.infinite(nedre) && is.infinite(ovre)) {
        "Alla värden"
      } else if (is.infinite(nedre)) {
        paste0("< ", scales::number(ovre, accuracy = acc))
      } else if (is.infinite(ovre)) {
        paste0("> ", scales::number(nedre, accuracy = acc))
      } else {
        paste0(scales::number(nedre, accuracy = acc), " – ",
               scales::number(ovre, accuracy = acc))
      }
    }, character(1))
  })
}

#' Koropletkarta med ggplot2 enligt Region Dalarnas profil
#'
#' @param sf_objekt Ett `sf`-objekt med polygondata.
#' @param vardekolumn Kolumnnamn med värdet som färgsätts.
#' @param klassindelning `NULL` (auto), `"kvantil"`, `"pretty"`, `"natural"`,
#'   eller en numerisk vektor med klassgränser.
#' @param klasser_antal Antal klasser vid automatisk klassindelning.
#' @param klasser_kontinuerlig_skala_oavsett Tvinga kontinuerlig färgskala.
#' @param legend_titel Legendtitel, eller `NULL`.
#' @param legend_titel_storlek,legend_text_storlek,legend_objekt_storlek
#'   Legend-styling.
#' @param legend_position `"right"`, `"left"`, `"top"`, `"bottom"`, `"none"`,
#'   `"bottom-right"` m.fl., `"dala"`, eller `c(x, y)`.
#' @param legend_justification Legendens ankarpunkt.
#' @param karta_titel,karta_caption Titel och källtext, eller `NULL`.
#' @param karta_titel_storlek,karta_caption_storlek Textstorlekar.
#' @param karta_fargvektor Egen färgvektor, eller `NULL` för RD-grön.
#' @param karta_farg_hogst `"mork"` (mörk = höga värden), `"ljus"`, eller `NULL`
#'   (rör inte skalan).
#' @param na_farg Färg för `NA`-värden.
#' @param karta_bakgrund Bakgrundsfärg, eller `"transparent"`.
#' @param karta_hojd Kartans höjd i tum.
#' @param karta_bredd Kartans bredd i tum, eller `NULL`/`"auto"` för att räkna
#'   ut den ur lagrets form.
#' @param karta_upplosning Upplösning (dpi) vid sparande.
#' @param filnamn,output_mapp Filnamn och mapp om kartan ska sparas.
#' @param logga_url `NULL` (ingen), `"dala"` (standardlogga), eller en sökväg.
#' @param logga_storlek Loggans bredd som andel av kartans bredd.
#' @param logga_position `"bottom-right"`, `"bottom-left"`, `"top-right"`,
#'   `"top-left"`.
#' @param etiketter_kolumn Kolumn med polygonetiketter, eller `NULL`.
#' @param etiketter_storlek,etiketter_farg,etiketter_buffer_farg Etikett-styling.
#' @param granser_farg,granser_tjocklek Polygongränser.
#'
#' @return Ett `ggplot`-objekt (osynligt om kartan sparas till fil).
#' @export
skapa_koropletkarta_ggplot <- function(
    sf_objekt, vardekolumn,
    klassindelning = NULL, klasser_antal = 5,
    klasser_kontinuerlig_skala_oavsett = FALSE,
    legend_titel = NULL, legend_titel_storlek = 12, legend_text_storlek = 9,
    legend_objekt_storlek = 1.2, legend_position = "right",
    legend_justification = "center",
    karta_titel = NULL, karta_titel_storlek = 20,
    karta_caption = NULL, karta_caption_storlek = 9,
    karta_fargvektor = NULL, karta_farg_hogst = "mork",
    na_farg = "grey90", karta_bakgrund = "white",
    karta_hojd = 7, karta_bredd = NULL, karta_upplosning = 300,
    filnamn = NULL, output_mapp = NULL,
    logga_url = NULL, logga_storlek = 0.06, logga_position = "bottom-right",
    etiketter_kolumn = NULL, etiketter_storlek = 2, etiketter_farg = "black",
    etiketter_buffer_farg = NA,
    granser_farg = "darkgrey", granser_tjocklek = 0.2) {

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Paketet 'sf' krävs för koropletkartor.", call. = FALSE)
  }

  # säkerställ att geometrikolumnen heter "geometry"
  geom_kol <- attr(sf_objekt, "sf_column")
  if ("geometry" %in% names(sf_objekt) && geom_kol != "geometry") {
    names(sf_objekt)[names(sf_objekt) == "geometry"] <- "geometry_attr"
  }
  if (geom_kol != "geometry") {
    sf_objekt <- sf::st_set_geometry(
      dplyr::rename(sf_objekt, geometry = dplyr::all_of(geom_kol)), "geometry"
    )
  }

  # legendposition
  hornkarta <- list("bottom-right" = c(1, 0), "bottom-left" = c(0, 0),
                    "top-right" = c(1, 1), "top-left" = c(0, 1), "dala" = c(0, 0))
  legend_position_inside_vals <- NULL
  if (is.character(legend_position) && tolower(legend_position) %in% names(hornkarta)) {
    legend_position_inside_vals <- hornkarta[[tolower(legend_position)]]
    legend_justification <- legend_position_inside_vals
    legend_position <- "inside"
  } else if (is.numeric(legend_position)) {
    legend_position_inside_vals <- legend_position
    legend_justification <- "center"
    legend_position <- "inside"
  }

  if (identical(logga_url, "dala")) logga_url <- hamta_logga_path()

  antal_unika <- length(unique(stats::na.omit(sf_objekt[[vardekolumn]])))

  # --- klassindelning ---
  if (is.null(klassindelning)) {
    if (klasser_kontinuerlig_skala_oavsett || antal_unika >= 7) {
      sf_plot <- dplyr::mutate(sf_objekt, klass = .data[[vardekolumn]])
      diskret_skala <- FALSE
    } else {
      sf_plot <- dplyr::mutate(sf_objekt, klass = factor(.data[[vardekolumn]]))
      diskret_skala <- TRUE
    }
  } else {
    diskret_skala <- TRUE
    if (antal_unika < klasser_antal) {
      message("Justerar antal klasser från ", klasser_antal, " till ", antal_unika,
              " (så många unika värden finns).")
      klasser_antal <- antal_unika
    }

    if (is.numeric(klassindelning)) {
      breaks <- klassindelning
      acc <- intern_karta_accuracy(klassindelning)
    } else {
      klassindelning <- match.arg(klassindelning, c("kvantil", "pretty", "natural"))
      acc <- intern_karta_accuracy(sf_objekt[[vardekolumn]])
      v <- sf_objekt[[vardekolumn]]
      breaks <- switch(
        klassindelning,
        kvantil = stats::quantile(v, probs = seq(0, 1, length.out = klasser_antal + 1), na.rm = TRUE),
        pretty  = pretty(range(v, na.rm = TRUE), n = klasser_antal),
        natural = {
          if (!requireNamespace("classInt", quietly = TRUE)) {
            stop("Paketet 'classInt' krävs för klassindelning = \"natural\".", call. = FALSE)
          }
          suppressWarnings(classInt::classIntervals(v, n = klasser_antal, style = "jenks")$brks)
        }
      )
    }
    sf_plot <- dplyr::mutate(
      sf_objekt, klass = cut(.data[[vardekolumn]], breaks = breaks, include.lowest = TRUE)
    )
    if (!requireNamespace("forcats", quietly = TRUE)) {
      stop("Paketet 'forcats' krävs för klassetiketter.", call. = FALSE)
    }
    sf_plot$klass <- intern_snygga_klassetiketter(sf_plot$klass, acc)
  }

  # --- färger ---
  if (is.null(karta_fargvektor)) {
    n_klass <- length(unique(sf_plot$klass))
    karta_fargvektor <- diagramfarger(
      if (n_klass < 6) "rd_karta_gron_sex" else if (n_klass == 7) "rd_karta_gron_sju" else "rd_karta_gron"
    )
  }
  if (!is.null(karta_farg_hogst)) {
    lum <- function(f) {
      rgb <- grDevices::col2rgb(f)
      0.2126 * rgb[1] + 0.7152 * rgb[2] + 0.0722 * rgb[3]
    }
    if (lum(karta_fargvektor[1]) < lum(karta_fargvektor[length(karta_fargvektor)]) &&
        karta_farg_hogst == "mork") {
      karta_fargvektor <- rev(karta_fargvektor)
    }
  }

  # --- grundkarta ---
  p <- ggplot2::ggplot(sf_plot) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$klass), color = granser_farg,
                     size = granser_tjocklek)

  if (!is.null(etiketter_kolumn)) {
    if (!requireNamespace("ggrepel", quietly = TRUE)) {
      stop("Paketet 'ggrepel' krävs för etiketter_kolumn.", call. = FALSE)
    }
    p <- p + ggrepel::geom_text_repel(
      ggplot2::aes(label = .data[[etiketter_kolumn]], geometry = .data$geometry),
      stat = "sf_coordinates", size = etiketter_storlek, color = etiketter_farg,
      family = "sans", bg.r = 0.15, bg.color = etiketter_buffer_farg,
      box.padding = 0.3, point.padding = 0.2, max.overlaps = Inf,
      min.segment.length = 0
    )
  }

  p <- p + if (!diskret_skala) {
    ggplot2::scale_fill_gradientn(colours = karta_fargvektor, name = legend_titel, na.value = na_farg)
  } else if (length(karta_fargvektor) > 1) {
    ggplot2::scale_fill_manual(values = karta_fargvektor, name = legend_titel,
                               na.value = na_farg, labels = levels(sf_plot$klass))
  } else {
    ggplot2::scale_fill_brewer(palette = karta_fargvektor, name = legend_titel,
                               na.value = na_farg, labels = levels(sf_plot$klass))
  }

  bg <- if (karta_bakgrund == "transparent") NA else karta_bakgrund

  p <- p +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(caption = karta_caption, title = karta_titel) +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.title = ggtext::element_textbox(
        face = "bold", size = karta_titel_storlek, width = ggplot2::unit(0.95, "npc"),
        halign = 0.5, margin = ggplot2::margin(2, 0, 2, 0)
      ),
      legend.position = legend_position,
      legend.position.inside = legend_position_inside_vals,
      legend.justification = legend_justification,
      legend.box.margin = ggplot2::margin(0, 0, 0, 0),
      legend.title = ggplot2::element_text(size = legend_titel_storlek),
      legend.text = ggplot2::element_text(size = legend_text_storlek),
      legend.key.size = ggplot2::unit(legend_objekt_storlek, "lines"),
      plot.caption = ggplot2::element_text(face = "italic", hjust = 0, vjust = 0,
                                           size = karta_caption_storlek),
      plot.caption.position = "plot",
      axis.text = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = bg, colour = NA),
      plot.background = ggplot2::element_rect(fill = bg, colour = NA),
      plot.margin = ggplot2::margin(5, 5, 5, 5)
    ) +
    ggplot2::coord_sf(expand = FALSE)

  # --- logga ---
  if (!is.null(logga_url)) {
    if (!requireNamespace("cowplot", quietly = TRUE) || !requireNamespace("magick", quietly = TRUE)) {
      stop("Paketen 'cowplot' och 'magick' krävs för logga på kartan.", call. = FALSE)
    }
    m <- 0.01
    pos <- switch(
      logga_position,
      "bottom-right" = list(x = 1 - m, y = -0.5 + m, hjust = 1, vjust = 0),
      "bottom-left"  = list(x = m,     y = -0.5 + m, hjust = 0, vjust = 0),
      "top-right"    = list(x = 1 - m, y = 0.5 - m,  hjust = 1, vjust = 1),
      "top-left"     = list(x = m,     y = 0.5 - m,  hjust = 0, vjust = 1),
      list(x = 1 - m, y = m, hjust = 1, vjust = 0)
    )
    p <- cowplot::ggdraw(p) +
      cowplot::draw_image(logga_url, x = pos$x, y = pos$y, width = logga_storlek,
                          hjust = pos$hjust, vjust = pos$vjust)
  }

  # --- spara ---
  if (!is.null(output_mapp) && !is.null(filnamn)) {
    if (!endsWith(output_mapp, "/")) output_mapp <- paste0(output_mapp, "/")
    dir.create(output_mapp, recursive = TRUE, showWarnings = FALSE)

    bredd <- karta_bredd
    if (is.null(bredd) || identical(bredd, "auto")) {
      bbox <- sf::st_bbox(sf_objekt)
      bredd <- karta_hojd * (bbox$xmax - bbox$xmin) / (bbox$ymax - bbox$ymin)
    }
    fil_sokvag <- paste0(output_mapp, filnamn)
    ggplot2::ggsave(fil_sokvag, p, width = bredd, height = karta_hojd,
                    dpi = karta_upplosning, bg = bg)
    message("Karta skapad: ", fil_sokvag)
    return(invisible(p))
  }
  p
}
