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
#'   Fungerar inte webshot2 (t.ex. när en grupprincip blockerar att R startar
#'   programmet som webshot2 behöver, `processx/supervisor.exe`) tas bilden i
#'   stället direkt av Chrome eller Edge i bakgrunden, utan webshot2.
#' @param karta_bredd,karta_hojd Bildmått i pixlar. Gäller bara `"png"`.
#' @param zoom Pixeltäthet för `"png"`: `1` (standard) ger bilden i
#'   `karta_bredd` x `karta_hojd` pixlar, `2` eller `3` ger samma utsnitt och
#'   samma storlek på text och symboler men två respektive tre gånger så många
#'   pixlar - skarpare, för utskrift och presentationer.
#' @param karta_fordrojning Antal sekunder att vänta innan bilden tas, så att
#'   bakgrundskartan hinner ritas. Gäller bara `"png"`.
#' @param favicon Ikonen som visas på webbläsarens flik. `TRUE` (standard) = Region
#'   Dalarnas ikon (följer med paketet), `FALSE` = ingen, eller sökväg till en png-, svg-
#'   eller ico-fil som bäddas in i html-filen. Gäller bara `"html"`.
#'
#' @return Osynligt: filens sökväg. Sökvägen skrivs också ut som ett meddelande.
#' @export
skriv_leaflet_till_fil <- function(leaflet_objekt, output_mapp, filnamn_karta,
                                    karta_filformat = "html",
                                    delat_bibliotek = FALSE,
                                    karta_bredd = 1000, karta_hojd = 800,
                                    karta_fordrojning = 4,
                                    zoom = 1,
                                    favicon = TRUE) {

  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    stop("Paketet 'htmlwidgets' krävs. Installera med install.packages(\"htmlwidgets\").", call. = FALSE)
  }

  if (!grepl("[/\\\\]$", output_mapp)) output_mapp <- paste0(output_mapp, "/")

  format <- sub("^\\.+", "", tolower(trimws(karta_filformat)))
  filnamn_bas <- sub("\\.+$", "", tools::file_path_sans_ext(basename(filnamn_karta)))

  if (format == "html") {

    fullpath <- paste0(output_mapp, filnamn_bas, ".html")
    leaflet_objekt <- intern_lagg_till_favicon(leaflet_objekt, favicon)

    # selfcontained = TRUE (en portabel enfilsversion, lätt att dela/maila) kräver pandoc. Om det
    # saknas sparas i stället en vanlig html-fil med en medföljande "_files"-mapp - fungerar precis
    # lika bra lokalt/på en webbserver, bara inte lika portabel - i stället för att krascha.
    sparad_portabel <- !delat_bibliotek && tryCatch({
      htmlwidgets::saveWidget(leaflet_objekt, fullpath, selfcontained = TRUE, title = filnamn_bas)
      TRUE
    }, error = function(e) FALSE)

    if (delat_bibliotek) {
      htmlwidgets::saveWidget(leaflet_objekt, fullpath, selfcontained = FALSE, libdir = "lib", title = filnamn_bas)
    } else if (!sparad_portabel) {
      htmlwidgets::saveWidget(leaflet_objekt, fullpath, selfcontained = FALSE, title = filnamn_bas)
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

    webblasare <- intern_sakerstall_webblasare()

    # Widgeten sparas först som en vanlig (icke-portabel) temporär html-fil - bara webbläsaren ska läsa
    # den lokalt, så pandoc/selfcontained behövs inte här.
    temp_html <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(leaflet_objekt, temp_html, selfcontained = FALSE)
    on.exit({
      unlink(temp_html)
      unlink(sub("\\.html$", "_files", temp_html), recursive = TRUE)
    }, add = TRUE)

    # bakgrundskartan (se lagg_till_bakgrundskarta()) hämtas och ritas först när sidan visas.
    # webshot2 startar webbläsaren via processx. Går det inte (t.ex. blockerat av en grupprincip) tas
    # bilden i stället direkt av webbläsaren med ett vanligt systemanrop.
    klar <- tryCatch({
      webshot2::webshot(temp_html, file = fullpath, vwidth = karta_bredd, vheight = karta_hojd,
                        delay = karta_fordrojning, zoom = zoom)
      file.exists(fullpath)
    }, error = function(e) FALSE)
    if (!klar) {
      # Edge provas först, därefter Chrome (se intern_hitta_alla_webblasare()); den första som ger en bild räcker
      for (kandidat in intern_hitta_alla_webblasare()) {
        message("webshot2 kunde inte starta webbl\u00e4saren - tar bilden direkt med ", basename(kandidat), ".")
        klar <- isTRUE(intern_png_med_webblasare(kandidat, temp_html, fullpath, karta_bredd, karta_hojd, zoom, karta_fordrojning))
        if (klar) break
      }
      if (!klar) {
        stop("Kunde inte ta bilden med n\u00e5gon webbl\u00e4sare. Spara kartan som html i st\u00e4llet ",
             "(leaflet_filformat = \"html\") och ta en sk\u00e4rmbild eller skriv ut den som pdf.", call. = FALSE)
      }
    }

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
intern_hitta_alla_webblasare <- function() {
  # sökväg angiven av användaren (CHROMOTE_CHROME) gäller först, därefter Microsoft Edge, sist Chrome
  angiven <- Sys.getenv("CHROMOTE_CHROME", "")
  edge <- c(
    "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe",
    "C:/Program Files/Microsoft/Edge/Application/msedge.exe",
    "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge",
    Sys.which("microsoft-edge")
  )
  chrome <- tryCatch(suppressMessages(unname(chromote::find_chrome())), error = function(e) NULL)
  kandidater <- c(angiven, edge, if (length(chrome) == 1) chrome)
  kandidater <- kandidater[!is.na(kandidater) & nzchar(kandidater) & file.exists(kandidater)]
  unique(kandidater)
}

#' @noRd
intern_hitta_webblasare <- function() {
  alla <- intern_hitta_alla_webblasare()
  if (length(alla) == 0) return(NULL)
  forsta <- alla[[1]]
  if (!nzchar(Sys.getenv("CHROMOTE_CHROME", ""))) {
    Sys.setenv(CHROMOTE_CHROME = forsta)
    message("Anv\u00e4nder ", basename(forsta), " f\u00f6r att ta bilden: ", forsta)
  }
  forsta
}

# Tar en png av en html-fil direkt med Chrome/Edge (headless), utan webshot2/chromote/processx.
# Returnerar (osynligt) TRUE om en bild skapades.
#
# Bakgrundskartan ritas med WebGL (MapLibre), och hur man får WebGL i headless-läge utan grafikkort skiljer
# sig mellan webbläsarversioner. Därför provas några flaggsatser i tur och ordning och den bild som ser ut
# att ha bakgrund behålls: en karta med bakgrund är flera gånger större i bytes per pixel än en utan.
# Tar första flaggsatsen fram en bild som ser ut att ha bakgrund (se grans) provas inga fler.
#' @noRd
intern_png_med_webblasare <- function(webblasare, html_fil, png_fil, bredd, hojd, zoom, fordrojning) {
  png_fil <- normalizePath(png_fil, winslash = "/", mustWork = FALSE)
  url <- paste0("file:///", sub("^/", "", normalizePath(html_fil, winslash = "/", mustWork = TRUE)))
  grans <- 0.2 * bredd * hojd * zoom^2  # bytes: över detta ser bilden ut att ha bakgrundskartan med

  flaggsatser <- list(
    c("--enable-webgl", "--ignore-gpu-blocklist", "--enable-unsafe-swiftshader"),
    c("--use-gl=angle", "--use-angle=swiftshader", "--enable-unsafe-swiftshader"),
    c("--disable-gpu")
  )

  bast <- NULL
  logg <- character(0)
  for (flaggor in flaggsatser) {
    profil <- tempfile("webblasare_profil_")
    dir.create(profil)
    loggfil <- tempfile("webblasare_logg_", fileext = ".txt")
    forsok <- tempfile("webblasare_bild_", fileext = ".png")
    args <- c("--headless", "--hide-scrollbars", "--no-first-run", flaggor,
              paste0("--user-data-dir=", shQuote(normalizePath(profil, winslash = "/", mustWork = FALSE))),
              paste0("--screenshot=", shQuote(normalizePath(forsok, winslash = "/", mustWork = FALSE))),
              paste0("--window-size=", bredd, ",", hojd),
              paste0("--force-device-scale-factor=", zoom),
              paste0("--virtual-time-budget=", round(max(fordrojning, 8) * 1000)),
              shQuote(url))
    suppressWarnings(system2(webblasare, args, stdout = loggfil, stderr = loggfil, timeout = 90))

    if (file.exists(forsok)) {
      storlek <- file.size(forsok)
      if (is.null(bast) || storlek > file.size(bast)) bast <- forsok
      if (storlek > grans) break
    } else if (file.exists(loggfil)) {
      logg <- c(logg, utils::tail(readLines(loggfil, warn = FALSE), 3))
    }
    unlink(profil, recursive = TRUE)
  }

  if (is.null(bast)) {
    if (length(logg) > 0) message("Webbl\u00e4saren skrev:\n", paste(unique(logg), collapse = "\n"))
    return(invisible(FALSE))
  }
  if (file.size(bast) <= grans) {
    message("Bilden ser ut att sakna bakgrundskarta (webbl\u00e4saren fick inte fram WebGL). Prova en h\u00f6gre ",
            "karta_fordrojning, eller spara kartan som html och ta en sk\u00e4rmbild.")
  }
  kopierad <- isTRUE(file.copy(bast, png_fil, overwrite = TRUE))
  # kontrollera att målfilen faktiskt blev uppdaterad (t.ex. inte låst av ett program som har bilden öppen)
  if (kopierad) {
    ny <- file.exists(png_fil) && file.size(png_fil) == file.size(bast) &&
      as.numeric(difftime(Sys.time(), file.mtime(png_fil), units = "secs")) < 120
    if (!ny) {
      message("Varning: ", png_fil, " verkar inte ha uppdaterats (storlek ", file.size(png_fil), " byte, \u00e4ndrad ",
              format(file.mtime(png_fil), "%Y-%m-%d %H:%M:%S"), "; nu \u00e4r klockan ", format(Sys.time(), "%H:%M:%S"),
              "). St\u00e4ng bilden om den \u00e4r \u00f6ppen i ett program, och uppdatera mappen.")
    }
  } else {
    message("Kunde inte skriva ", png_fil, ". St\u00e4ng bilden om den \u00e4r \u00f6ppen i ett program, eller kontrollera skrivr\u00e4ttigheterna.")
  }
  invisible(kopierad)
}

# Lägger en favicon i en leaflet-karta (html-filens <head>). TRUE = Region Dalarnas ikon, FALSE = ingen,
# annars sökväg till en png-, svg- eller ico-fil som bäddas in.
#' @noRd
intern_lagg_till_favicon <- function(leaflet_objekt, favicon) {
  if (isFALSE(favicon) || is.null(favicon)) return(leaflet_objekt)

  href <- if (isTRUE(favicon)) {
    # Region Dalarnas ikon (256x256 png ur favicon.ico i Region-Dalarna/depot), följer med paketet
    ikon <- system.file("extdata", "favicon_regiondalarna.png", package = "rddiagram")
    if (nzchar(ikon)) {
      paste0("data:image/png;base64,", jsonlite::base64_enc(readBin(ikon, "raw", file.size(ikon))))
    } else {
      svg <- paste0(
        "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 32 32'>",
        "<rect width='32' height='32' rx='6' fill='#158daf'/>",
        "<path d='M16 5a7 7 0 0 0-7 7c0 5 7 14 7 14s7-9 7-14a7 7 0 0 0-7-7z' fill='#fff'/>",
        "<circle cx='16' cy='12' r='2.6' fill='#158daf'/></svg>")
      paste0("data:image/svg+xml,", utils::URLencode(svg, reserved = TRUE))
    }
  } else {
    if (!is.character(favicon) || length(favicon) != 1 || !file.exists(favicon)) {
      stop("favicon ska vara TRUE, FALSE eller s\u00f6kv\u00e4gen till en befintlig bildfil.", call. = FALSE)
    }
    typ <- switch(tolower(tools::file_ext(favicon)), png = "image/png", svg = "image/svg+xml",
                  ico = "image/x-icon", stop("favicon: filen ska vara png, svg eller ico.", call. = FALSE))
    paste0("data:", typ, ";base64,", jsonlite::base64_enc(readBin(favicon, "raw", file.size(favicon))))
  }
  htmlwidgets::prependContent(leaflet_objekt, htmltools::tags$link(rel = "icon", href = href))
}
