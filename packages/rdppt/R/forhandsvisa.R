#' Titta på bilderna i en pptx-fil
#'
#' Gör en bild av varje slide i en pptx-fil och visar dem som en översikt
#' med slidens nummer och rubrik, så att man kan se vad man har utan att
#' öppna filen. Numret är det som [ppt_befintlig()] använder i `slide`.
#' Översikten öppnas i RStudios Viewer-flik, eller i webbläsaren.
#'
#' Bilderna görs med PowerPoint om du kör Windows och har PowerPoint
#' installerat (PowerPoint öppnas då osynligt i bakgrunden), annars med
#' LibreOffice. Bilderna från LibreOffice kan skilja sig något från hur
#' PowerPoint ritar sliden, framför allt typsnitt och radbrytningar.
#' LibreOffice behöver också `pdftools` eller `pdftoppm` för att göra bilder
#' av sliderna.
#'
#' @param fil Sökväg till pptx-filen. `NULL` (standard) = en exempelfil med
#'   en slide per layout i mallen, se [ppt_mall_exempel()] - det visar hur
#'   mallens layouter ser ut.
#' @param slide Vilka slides som ska visas, som nummer. `NULL` (standard)
#'   = alla.
#' @param mall Bara när `fil` är `NULL`: vilken mall layouterna ska visas
#'   för. `NULL` = den mall som gäller, se [ppt_mall_ange()].
#' @param bredd Bredden i pixlar på varje bild i filerna (och i översikten
#'   om fönstret är stort nog).
#' @param oppna TRUE (standard) för att öppna översikten.
#'
#' @return Osynligt: sökvägarna till bildfilerna (png), med översiktens
#'   sökväg som attributet `oversikt`.
#' @seealso [ppt_befintlig_visa()] för en lista med text,
#'   [ppt_mall_visa()] för mallens layouter.
#' @examples
#' \dontrun{
#' # hur ser mallens layouter ut?
#' ppt_forhandsvisa()
#'
#' # vilka färdiga slides finns i den här filen?
#' ppt_forhandsvisa("G:/skript/ppt/mall_rus_utan_bakgrund_demografi.pptx")
#' ppt_forhandsvisa("G:/skript/ppt/mall_rus_utan_bakgrund_demografi.pptx", slide = 2:3)
#' }
#' @export
ppt_forhandsvisa <- function(fil = NULL, slide = NULL, mall = NULL, bredd = 640, oppna = TRUE) {
  if (is.null(fil)) {
    fil <- suppressMessages(ppt_mall_exempel(mall, oppna = FALSE))
    rubrik <- "Layouterna i mallen"
  } else {
    if (!is.character(fil) || length(fil) != 1 || !file.exists(fil)) {
      stop("Hittar inte pptx-filen:\n  ", fil, call. = FALSE)
    }
    rubrik <- basename(fil)
  }

  mapp <- tempfile("forhandsvisning_")
  dir.create(mapp)
  bilder <- intern_rendera_slides(fil, mapp, bredd)
  if (length(bilder) == 0) stop("Kunde inte göra bilder av sliderna i ", fil, call. = FALSE)

  if (!is.null(slide)) {
    if (!is.numeric(slide) || any(is.na(slide)) || any(slide < 1) || any(slide > length(bilder)) || any(slide != round(slide))) {
      stop("slide ska vara nummer mellan 1 och ", length(bilder), " - så många slides finns i filen.", call. = FALSE)
    }
  } else {
    slide <- seq_along(bilder)
  }

  # rubrik och layout är bara en bonus i bildtexterna - går de inte att läsa visas enbart numret
  innehall <- tryCatch(intern_slides_i_fil(intern_las_pptx(fil)), error = function(e) NULL)
  rubriker <- c(innehall$rubrik, rep("", length(bilder)))[seq_along(bilder)]
  layouter <- c(innehall$layout, rep("", length(bilder)))[seq_along(bilder)]

  oversikt <- intern_skriv_oversikt(bilder, slide, rubriker, layouter, rubrik, bredd, mapp)
  if (isTRUE(oppna)) {
    visare <- getOption("viewer")
    if (is.function(visare)) visare(oversikt) else utils::browseURL(oversikt)
  }
  message(length(slide), " slides, översikt: ", oversikt)

  invisible(structure(bilder[slide], oversikt = oversikt))
}

# Gör en png av varje slide i en pptx-fil och returnerar sökvägarna i slideordning. PowerPoint om det
# går (Windows), annars LibreOffice.
intern_rendera_slides <- function(fil, mapp, bredd) {
  fil <- normalizePath(fil, mustWork = TRUE)
  storlek <- officer::slide_size(intern_las_pptx(fil))
  hojd <- round(bredd * storlek$height / storlek$width)

  if (.Platform$OS.type == "windows") {
    bilder <- tryCatch(intern_rendera_powerpoint(fil, mapp, bredd, hojd), error = function(e) character(0))
    if (length(bilder) > 0) return(bilder)
  }

  bilder <- intern_rendera_libreoffice(fil, mapp, bredd)
  if (length(bilder) == 0) {
    stop("Kan inte göra bilder av sliderna: varken PowerPoint (Windows) eller LibreOffice hittades.\n",
         "Öppna filen direkt med ppt_befintlig_visa(fil, oppna = TRUE) i stället, eller installera LibreOffice ",
         "(https://www.libreoffice.org).", call. = FALSE)
  }
  bilder
}

# PowerPoint styrs via PowerShell. Filen öppnas skrivskyddad och utan fönster. PowerPoint stängs bara
# om den inte redan var igång med andra presentationer öppna.
intern_rendera_powerpoint <- function(fil, mapp, bredd, hojd) {
  skript <- tempfile(fileext = ".ps1")
  on.exit(unlink(skript))
  writeLines(c(
    "param([string]$fil, [string]$mapp, [int]$bredd, [int]$hojd)",
    "$ErrorActionPreference = 'Stop'",
    "$app = New-Object -ComObject PowerPoint.Application",
    "try {",
    "  $pres = $app.Presentations.Open($fil, -1, 0, 0)",
    "  $pres.Export($mapp, 'PNG', $bredd, $hojd)",
    "  $pres.Close()",
    "} finally {",
    "  if ($app.Presentations.Count -eq 0) { $app.Quit() }",
    "}"
  ), skript)

  utan_resultat <- suppressWarnings(system2(
    "powershell", c("-NoProfile", "-ExecutionPolicy", "Bypass", "-File", shQuote(skript),
                    shQuote(fil), shQuote(normalizePath(mapp)), bredd, hojd),
    stdout = TRUE, stderr = TRUE))
  if (!is.null(attr(utan_resultat, "status"))) return(character(0))

  # PowerPoint döper filerna efter språk (Slide1.PNG, Bild1.PNG ...) - ordna efter numret
  filer <- list.files(mapp, pattern = "\\.png$", full.names = TRUE, ignore.case = TRUE)
  nummer <- as.integer(sub(".*?([0-9]+)\\.png$", "\\1", filer, ignore.case = TRUE))
  filer[order(nummer)]
}

intern_rendera_libreoffice <- function(fil, mapp, bredd) {
  soffice <- Sys.which(c("soffice", "libreoffice"))
  soffice <- soffice[nzchar(soffice)]
  if (length(soffice) == 0) {
    vanliga <- c("C:/Program Files/LibreOffice/program/soffice.exe",
                 "C:/Program Files (x86)/LibreOffice/program/soffice.exe",
                 "/Applications/LibreOffice.app/Contents/MacOS/soffice")
    soffice <- vanliga[file.exists(vanliga)]
  }
  if (length(soffice) == 0) return(character(0))

  # en egen profilmapp gör att körningen inte krockar med ett LibreOffice som redan är öppet
  profil <- tempfile("lo_profil_")
  suppressWarnings(system2(
    soffice[[1]],
    c(paste0("-env:UserInstallation=file://", if (.Platform$OS.type == "windows") "/", gsub("\\\\", "/", normalizePath(profil, mustWork = FALSE))),
      "--headless", "--convert-to", "pdf", "--outdir", shQuote(mapp), shQuote(fil)),
    stdout = FALSE, stderr = FALSE, env = intern_miljo_extern()))
  pdf <- file.path(mapp, sub("\\.pptx?$", ".pdf", basename(fil), ignore.case = TRUE))
  if (!file.exists(pdf)) return(character(0))

  # upplösning (punkter per tum) som ger önskad bredd i pixlar
  dpi <- bredd / as.numeric(officer::slide_size(intern_las_pptx(fil))$width)
  if (requireNamespace("pdftools", quietly = TRUE)) {
    antal <- pdftools::pdf_info(pdf)$pages
    return(pdftools::pdf_convert(pdf, format = "png", dpi = dpi,
                                 filenames = file.path(mapp, sprintf("slide-%03d.png", seq_len(antal))), verbose = FALSE))
  }
  pdftoppm <- Sys.which("pdftoppm")
  if (nzchar(pdftoppm)) {
    system2(pdftoppm, c("-r", round(dpi), "-png", shQuote(pdf), shQuote(file.path(mapp, "slide"))),
            stdout = FALSE, stderr = FALSE, env = intern_miljo_extern())
    return(sort(list.files(mapp, pattern = "^slide-[0-9]+\\.png$", full.names = TRUE)))
  }

  stop("LibreOffice hittades, men för att göra bilder av sliderna behövs också paketet pdftools: install.packages(\"pdftools\").",
       call. = FALSE)
}

# Miljövariabler för program utanför R. På Linux sätter R en LD_LIBRARY_PATH som gör att LibreOffice
# inte hittar sina egna bibliotek ("libreglo.so: cannot open shared object file").
intern_miljo_extern <- function() {
  if (identical(Sys.info()[["sysname"]], "Linux")) "LD_LIBRARY_PATH=" else character(0)
}

# En html-sida med bilderna i ett rutnät, i samma mapp som bilderna så att de hittas med relativa sökvägar.
intern_skriv_oversikt <- function(bilder, slide, rubriker, layouter, rubrik, bredd, mapp) {
  html_text <- function(x) gsub(">", "&gt;", gsub("<", "&lt;", gsub("&", "&amp;", x, fixed = TRUE), fixed = TRUE), fixed = TRUE)
  figurer <- vapply(slide, function(nr) {
    paste0('<figure><img src="', basename(bilder[nr]), '" alt="Slide ', nr, '">',
           "<figcaption><b>", nr, "</b> ", html_text(rubriker[nr]),
           if (nzchar(layouter[nr])) paste0('<br><span class="layout">Layout: ', html_text(layouter[nr]), "</span>"),
           "</figcaption></figure>")
  }, character(1))

  sida <- file.path(mapp, "oversikt.html")
  utfil <- file(sida, "w", encoding = "UTF-8")
  on.exit(close(utfil))
  writeLines(c(
    "<!DOCTYPE html>", '<html lang="sv"><head><meta charset="utf-8">',
    paste0("<title>", html_text(rubrik), "</title>"),
    "<style>",
    "body { font-family: sans-serif; margin: 16px; color: #222; background: #fff; }",
    "h1 { font-size: 1.1rem; margin: 0 0 12px; }",
    paste0(".rutnat { display: grid; grid-template-columns: repeat(auto-fill, minmax(", min(bredd, 320), "px, 1fr)); gap: 16px; }"),
    "figure { margin: 0; }",
    "img { width: 100%; height: auto; border: 1px solid #bbb; display: block; background: #fff; }",
    "figcaption { margin-top: 4px; font-size: 0.85rem; }",
    ".layout { color: #777; font-size: 0.75rem; }",
    "@media (prefers-color-scheme: dark) { body { background: #1e1e1e; color: #ddd; } img { border-color: #555; } }",
    "</style></head><body>",
    paste0("<h1>", html_text(rubrik), "</h1>"),
    '<div class="rutnat">', figurer, "</div>", "</body></html>"
  ), utfil)
  sida
}
