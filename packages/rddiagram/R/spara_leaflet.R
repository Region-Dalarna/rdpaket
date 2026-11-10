#' Skriv en leaflet-karta till fil
#'
#' Motsvarigheten till [skriv_till_diagramfil()] för leaflet-/htmlwidget-objekt
#' i stället för ggplot-objekt.
#'
#' @param leaflet_objekt Ett leaflet-/htmlwidget-objekt.
#' @param output_mapp Målmapp.
#' @param filnamn_karta Filnamn (ev. filändelse ersätts av `karta_filformat`).
#' @param karta_filformat `"html"` (standard) - en interaktiv fil, en
#'   självständig enfilsversion om pandoc är installerat, annars en html-fil
#'   med en medföljande `_files`-mapp. `"png"` - en statisk ögonblicksbild via
#'   paketet webshot2 (kräver det paketet samt en lokalt installerad
#'   Chrome/Chromium).
#' @param karta_bredd,karta_hojd Bildmått i pixlar. Gäller bara `"png"`.
#' @param karta_fordrojning Antal sekunder att vänta innan bilden tas, så att
#'   bakgrundskartan hinner ritas. Gäller bara `"png"`.
#'
#' @return Inget - skriver filen.
#' @export
skriv_leaflet_till_fil <- function(leaflet_objekt, output_mapp, filnamn_karta,
                                    karta_filformat = "html",
                                    karta_bredd = 1000, karta_hojd = 800,
                                    karta_fordrojning = 4) {

  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    stop("Paketet 'htmlwidgets' krävs. Installera med install.packages(\"htmlwidgets\").", call. = FALSE)
  }

  if (!grepl("[/\\\\]$", output_mapp)) output_mapp <- paste0(output_mapp, "/")

  format <- sub("^\\.+", "", tolower(trimws(karta_filformat)))
  filnamn_bas <- sub("\\.+$", "", tools::file_path_sans_ext(basename(filnamn_karta)))

  if (format == "html") {

    fullpath <- paste0(output_mapp, filnamn_bas, ".html")

    # selfcontained = TRUE (en portabel enfilsversion, lätt att dela/maila) kräver pandoc. Om det
    # saknas sparas i stället en vanlig html-fil med en medföljande "_files"-mapp - fungerar precis
    # lika bra lokalt/på en webbserver, bara inte lika portabel - i stället för att krascha.
    sparad_portabel <- tryCatch({
      htmlwidgets::saveWidget(leaflet_objekt, fullpath, selfcontained = TRUE)
      TRUE
    }, error = function(e) FALSE)

    if (!sparad_portabel) {
      htmlwidgets::saveWidget(leaflet_objekt, fullpath, selfcontained = FALSE)
      message(
        "Pandoc hittades inte - sparade en icke-självständig html-fil (med en medföljande ",
        "\"_files\"-mapp) i stället för en portabel enfilsversion."
      )
    }

  } else if (format == "png") {

    if (!requireNamespace("webshot2", quietly = TRUE)) {
      stop(
        "Paketet 'webshot2' krävs för att spara en leaflet-karta som png (kräver även en ",
        "lokalt installerad Chrome/Chromium). Installera med install.packages(\"webshot2\").",
        call. = FALSE
      )
    }

    fullpath <- paste0(output_mapp, filnamn_bas, ".png")

    # Widgeten sparas först som en vanlig (icke-portabel) temporär html-fil - bara webshot2 ska läsa
    # den lokalt, så pandoc/selfcontained behövs inte här.
    temp_html <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(leaflet_objekt, temp_html, selfcontained = FALSE)
    on.exit({
      unlink(temp_html)
      unlink(sub("\\.html$", "_files", temp_html), recursive = TRUE)
    }, add = TRUE)

    # bakgrundskartan (se lagg_till_bakgrundskarta()) hämtas och ritas först när sidan visas
    webshot2::webshot(temp_html, file = fullpath, vwidth = karta_bredd, vheight = karta_hojd,
                      delay = karta_fordrojning)

  } else {
    stop("Okänt kartfilformat: ", format, " (stöds: \"html\", \"png\")", call. = FALSE)
  }

  invisible(NULL)
}
