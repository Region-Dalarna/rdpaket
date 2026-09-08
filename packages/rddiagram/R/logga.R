# Logga i diagram - utbrutet ur func_logga_i_diagram.R.

#' Lägg in en logga i ett sparat diagram
#'
#' Läser en diagrambild och komponerar in en logga i angivet hörn. Kräver
#' paketet `magick`.
#'
#' @param plot_path Sökväg till diagrambilden.
#' @param logo_path Sökväg till loggan.
#' @param logo_position `"top left"`, `"top right"`, `"bottom left"` eller
#'   `"bottom right"`.
#' @param logo_scale Loggans bredd blir plottens bredd delat med detta.
#' @param replace Om `TRUE` skrivs bilden i `plot_path` över med resultatet.
#'
#' @return Ett `magick`-bildobjekt med loggan inkomponerad.
#' @export
add_logo <- function(plot_path, logo_path, logo_position, logo_scale = 15, replace = FALSE) {
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Paketet 'magick' krävs för add_logo().", call. = FALSE)
  }
  if (!logo_position %in% c("top right", "top left", "bottom right", "bottom left")) {
    stop("Okänd logo_position. Använd 'top left', 'top right', 'bottom left' ",
         "eller 'bottom right'.", call. = FALSE)
  }

  plot <- magick::image_read(plot_path)
  logo_raw <- magick::image_read(logo_path)

  plot_height <- magick::image_info(plot)$height
  plot_width <- magick::image_info(plot)$width

  logo <- magick::image_scale(logo_raw, as.character(plot_width / logo_scale))
  logo_width <- magick::image_info(logo)$width
  logo_height <- magick::image_info(logo)$height

  pos <- switch(
    logo_position,
    "top right"    = c(plot_width - logo_width - 0.01 * plot_width, 0.01 * plot_height),
    "top left"     = c(0.01 * plot_width, 0.01 * plot_height),
    "bottom right" = c(plot_width - logo_width - 0.01 * plot_width,
                       plot_height - logo_height - 0.01 * plot_height),
    "bottom left"  = c(0.01 * plot_width, plot_height - logo_height - 0.01 * plot_height)
  )

  resultat <- magick::image_composite(
    plot, logo, offset = paste0("+", pos[1], "+", pos[2])
  )
  if (isTRUE(replace)) magick::image_write(resultat, plot_path)
  resultat
}
