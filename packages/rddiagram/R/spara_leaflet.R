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
#'   Chrome eller Microsoft Edge; Edge används automatiskt om ingen Chrome
#'   hittas, annars kan du ange sökvägen med
#'   `Sys.setenv(CHROMOTE_CHROME = "sökväg")`).
#' @param delat_bibliotek `FALSE` (standard). `TRUE` skriver html-filen utan
#'   inbäddade bibliotek: kartans JavaScript läggs i en mapp `lib` bredvid
#'   filen och delas av alla kartor i samma mapp, så att varje fil blir
#'   liten. Bra för många kartor i samma mapp, till exempel exempelkartor i
#'   ett repo. Filen måste då ligga kvar bredvid `lib`. Gäller bara
#'   `"html"`.
#' @param karta_bredd,karta_hojd Bildmått i pixlar. Gäller bara `"png"`.
#' @param karta_fordrojning Antal sekunder att vänta innan bilden tas, så att
#'   bakgrundskartan hinner ritas. Gäller bara `"png"`.
#'
#' @return Osynligt: filens sökväg. Sökvägen skrivs också ut som ett meddelande.
#' @export
skriv_leaflet_till_fil <- function(leaflet_objekt, output_mapp, filnamn_karta,
                                    karta_filformat = "html",
                                    delat_bibliotek = FALSE,
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
    sparad_portabel <- !delat_bibliotek && tryCatch({
      htmlwidgets::saveWidget(leaflet_objekt, fullpath, selfcontained = TRUE)
      TRUE
    }, error = function(e) FALSE)

    if (delat_bibliotek) {
      htmlwidgets::saveWidget(leaflet_objekt, fullpath, selfcontained = FALSE, libdir = "lib")
    } else if (!sparad_portabel) {
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

    intern_sakerstall_webblasare()

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

  message("Karta skapad: ", fullpath)
  invisible(fullpath)
}

# webshot2 behöver en Chromium-baserad webbläsare. Hittar chromote ingen Chrome provas Microsoft Edge
# (som är Chromium-baserad och finns på de flesta Windows-datorer) och sökvägen sätts då för sessionen via
# CHROMOTE_CHROME. Finns ingen alls stoppas funktionen med ett meddelande om vad som behövs.
#' @noRd
intern_sakerstall_webblasare <- function() {
  hittad <- intern_hitta_webblasare()
  if (is.null(hittad)) {
    stop("Hittar ingen webbl\u00e4sare att ta bilden med. En png av en leaflet-karta g\u00f6rs med Chrome eller ",
         "Microsoft Edge (via paketet webshot2). Installera Chrome, eller ange s\u00f6kv\u00e4gen till en Chrome- eller ",
         "Edge-fil, t.ex.:\n  Sys.setenv(CHROMOTE_CHROME = \"C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe\")",
         call. = FALSE)
  }
  invisible(hittad)
}

#' @noRd
intern_hitta_webblasare <- function() {
  # sökväg angiven av användaren (CHROMOTE_CHROME) gäller först
  angiven <- Sys.getenv("CHROMOTE_CHROME", "")
  if (nzchar(angiven) && file.exists(angiven)) return(angiven)

  chrome <- tryCatch(unname(chromote::find_chrome()), error = function(e) NULL)
  if (length(chrome) == 1 && !is.na(chrome) && nzchar(chrome) && file.exists(chrome)) return(chrome)

  edge <- c(
    "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe",
    "C:/Program Files/Microsoft/Edge/Application/msedge.exe",
    "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge",
    Sys.which("microsoft-edge")
  )
  edge <- edge[nzchar(edge) & file.exists(edge)]
  if (length(edge) > 0) {
    Sys.setenv(CHROMOTE_CHROME = edge[[1]])
    message("Hittade ingen Chrome - anv\u00e4nder Microsoft Edge: ", edge[[1]])
    return(edge[[1]])
  }
  NULL
}
