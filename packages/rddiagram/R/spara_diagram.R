# Skriva diagram till fil - utbrutet ur func_SkapaDiagram.R.

#' Visa bara var n:te x-axeletikett
#'
#' Returnerar en funktion att skicka till `breaks` i `scale_x_discrete()`.
#'
#' @param n Visa var n:te etikett.
#' @param sista_vardet Om `TRUE` visas alltid det sista värdet.
#' @param ta_bort_nast_sista Om `TRUE` döljs näst sista värdet.
#'
#' @return En funktion `function(x)`.
#' @export
every_nth <- function(n, sista_vardet, ta_bort_nast_sista = FALSE) {
  function(x) {
    vec <- rep(c(TRUE, rep(FALSE, n - 1)), length.out = length(x))
    if (sista_vardet) vec[length(x)] <- TRUE
    if (ta_bort_nast_sista && length(x) > 1) vec[length(x) - 1] <- FALSE
    x[vec]
  }
}

#' Spara ett ggplot-objekt med försök och lokal fallback
#'
#' @param plot Ett ggplot-objekt.
#' @param target_path Full målsökväg.
#' @param width,height,dpi,units Skickas till `ggplot2::ggsave()`.
#' @param attempts Antal försök.
#' @param sleep_sec Grundvila mellan försök (multipliceras med försöksnummer).
#' @param use_local_fallback Spara lokalt och kopiera om direktsparning misslyckas.
#' @param device Grafikdevice, eller `NULL` för automatiskt val.
#'
#' @return `TRUE` vid framgång, annars `FALSE`.
#' @export
ggsave_retry <- function(plot, target_path, width = 12, height = 7, dpi = 300,
                         attempts = 5, sleep_sec = 0.8, use_local_fallback = TRUE,
                         device = NULL, units = "in") {
  dir.create(dirname(target_path), recursive = TRUE, showWarnings = FALSE)

  if (is.null(device)) {
    ext <- tolower(tools::file_ext(target_path))
    if (ext %in% c("png", "jpeg", "jpg") && requireNamespace("ragg", quietly = TRUE)) {
      device <- ragg::agg_png
    }
  }

  last_err <- NULL
  for (i in seq_len(attempts)) {
    suppressWarnings(try(unlink(target_path), silent = TRUE))
    ok <- tryCatch({
      ggplot2::ggsave(filename = target_path, plot = plot, width = width,
                      height = height, dpi = dpi, units = units, device = device)
      TRUE
    }, error = function(e) { last_err <<- conditionMessage(e); FALSE })
    if (isTRUE(ok) && file.exists(target_path)) return(TRUE)
    Sys.sleep(sleep_sec * i)
  }

  if (use_local_fallback) {
    local_tmp <- file.path(tempdir(), paste0("tmp_", basename(target_path)))
    ok2 <- tryCatch({
      ggplot2::ggsave(filename = local_tmp, plot = plot, width = width,
                      height = height, dpi = dpi, units = units, device = device)
      TRUE
    }, error = function(e) { last_err <<- paste0("Fallback ggsave: ", conditionMessage(e)); FALSE })

    if (isTRUE(ok2) && file.exists(local_tmp)) {
      for (i in seq_len(attempts)) {
        if (file.copy(local_tmp, target_path, overwrite = TRUE)) return(TRUE)
        Sys.sleep(sleep_sec * i)
      }
    }
  }

  message("ggsave_retry() fel för ", target_path, "\nDetalj: ", last_err %||% "<inget fel fångat>")
  FALSE
}

#' Spara ett ggplot-objekt som EPS med försök
#'
#' @param plot Ett ggplot-objekt.
#' @param target_path Full målsökväg.
#' @param width,height,units Bild-mått (skickas till `grDevices::cairo_ps()`).
#' @param attempts Antal försök.
#' @param sleep_sec Grundvila mellan försök.
#' @param pointsize,fallback_res Skickas till `grDevices::cairo_ps()`.
#'
#' @return `TRUE` vid framgång, annars `FALSE`.
#' @export
save_eps_retry <- function(plot, target_path, width, height, attempts = 8,
                           sleep_sec = 0.8, pointsize = 12, fallback_res = 300,
                           units = "in") {
  dir.create(dirname(target_path), recursive = TRUE, showWarnings = FALSE)
  for (i in seq_len(attempts)) {
    ok <- try({
      grDevices::cairo_ps(filename = target_path, width = width, height = height,
                          pointsize = pointsize, fallback_resolution = fallback_res,
                          onefile = FALSE, family = "sans")
      print(plot)
      grDevices::dev.off()
      TRUE
    }, silent = TRUE)
    if (!inherits(ok, "try-error") && file.exists(target_path)) return(TRUE)
    Sys.sleep(sleep_sec * i)
  }
  FALSE
}

#' Skriv ett diagram till fil (med logga)
#'
#' @param ggplot_objekt Ett ggplot-objekt.
#' @param output_mapp Målmapp.
#' @param filnamn_diagram Filnamn (ev. filändelse ersätts av `diagram_bildformat`).
#' @param diagramfil_bredd,diagramfil_hojd Bild-mått i tum.
#' @param logga `TRUE` = Region Dalarnas standardlogga, `FALSE` = ingen logga,
#'   en sökväg = egen logga.
#' @param logga_storlek Loggans bredd blir bildens bredd delat med detta.
#' @param diagram_bildformat Filändelse: `"png"`, `"svg"`, `"pdf"`, `"eps"` m.fl.
#'
#' @return Inget - skriver filen.
#' @export
skriv_till_diagramfil <- function(ggplot_objekt, output_mapp, filnamn_diagram,
                                  diagramfil_bredd = 12, diagramfil_hojd = 7,
                                  logga = TRUE, logga_storlek = 15,
                                  diagram_bildformat = "png") {

  if (!grepl("[/\\\\]$", output_mapp)) output_mapp <- paste0(output_mapp, "/")

  format <- sub("^\\.+", "", tolower(trimws(diagram_bildformat)))
  filnamn_bas <- sub("\\.+$", "", tools::file_path_sans_ext(basename(filnamn_diagram)))
  fullpath <- paste0(output_mapp, filnamn_bas, ".", format)

  dev <- NULL
  if (format %in% c("jpg", "jpeg")) {
    dev <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_jpeg else "jpeg"
  } else if (format == "svg") {
    dev <- if (requireNamespace("svglite", quietly = TRUE)) svglite::svglite else "svg"
  } else if (format == "pdf") {
    dev <- if (exists("cairo_pdf", where = asNamespace("grDevices"))) grDevices::cairo_pdf else NULL
  } else if (format == "ps") {
    dev <- if (exists("cairo_ps", where = asNamespace("grDevices"))) grDevices::cairo_ps else "postscript"
  }

  ok <- if (format == "eps") {
    save_eps_retry(ggplot_objekt, fullpath, width = diagramfil_bredd,
                   height = diagramfil_hojd, units = "in")
  } else if (format %in% c("png", "jpg", "jpeg", "pdf", "svg", "ps")) {
    ggsave_retry(ggplot_objekt, fullpath, width = diagramfil_bredd,
                 height = diagramfil_hojd, dpi = 300, device = dev, units = "in")
  } else {
    stop("Okänt bildformat: ", format, call. = FALSE)
  }
  if (!ok) stop("Kunde inte spara diagrammet till: ", fullpath, call. = FALSE)

  if (!isFALSE(logga)) {
    logga_path <- if (isTRUE(logga)) hamta_logga_path() else logga
    add_ok <- FALSE
    for (i in 1:5) {
      add_ok <- tryCatch({
        add_logo(fullpath, logga_path, "bottom right", logo_scale = logga_storlek, replace = TRUE)
        TRUE
      }, error = function(e) FALSE)
      if (isTRUE(add_ok)) break
      Sys.sleep(0.5 * i)
    }
    if (!isTRUE(add_ok)) warning("Kunde inte lägga på logga på: ", fullpath, call. = FALSE)
  }
  invisible(fullpath)
}
