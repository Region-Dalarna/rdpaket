#' Titta på bilderna i en pptx-fil
#'
#' Gör en bild av varje slide i en pptx-fil och visar dem som en översikt
#' med slidens nummer och rubrik, så att man kan se vad man har utan att
#' öppna filen. Numret är det som `ppt_lagg_till_slide("befintlig", slide = )` använder.
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

  ppt_orsak <- NULL
  if (.Platform$OS.type == "windows") {
    # Windows Script Host (VBScript) först: på datorer där PowerShell är låst (signerade skript, "constrained
    # language mode") går det inte att styra PowerPoint därifrån, men VBScript brukar fungera.
    for (metod in list(intern_rendera_powerpoint_vbs, intern_rendera_powerpoint)) {
      bilder <- tryCatch(metod(fil, mapp, bredd, hojd),
                         error = function(e) structure(character(0), orsak = conditionMessage(e)))
      if (length(bilder) > 0) return(bilder)
      ppt_orsak <- c(ppt_orsak, attr(bilder, "orsak"))
    }
    ppt_orsak <- paste(unique(ppt_orsak), collapse = "\n  ")
  }

  bilder <- intern_rendera_libreoffice(fil, mapp, bredd)
  if (length(bilder) == 0) {
    stop("Kan inte göra bilder av sliderna: ",
         if (is.null(ppt_orsak)) "varken PowerPoint (Windows) eller LibreOffice hittades.\n"
         else paste0("PowerPoint gick inte att styra och LibreOffice hittades inte.\nFel från PowerPoint: ", ppt_orsak, "\n"),
         "Öppna filen direkt med ppt_befintlig_visa(fil, oppna = TRUE) i stället, eller installera LibreOffice ",
         "(https://www.libreoffice.org).", call. = FALSE)
  }
  bilder
}

# PowerPoint styrs via ett VBScript som körs med cscript. Skriptet sparas som UTF-16 så att å/ä/ö i sökvägar går igenom.
intern_rendera_powerpoint_vbs <- function(fil, mapp, bredd, hojd) {
  skriptfil <- tempfile(fileext = ".vbs")
  on.exit(unlink(skriptfil))
  vbs_text <- function(x) paste0('"', gsub('"', '""', x, fixed = TRUE), '"')
  skript <- c(
    'Set app = CreateObject("PowerPoint.Application")',
    paste0("Set pres = app.Presentations.Open(", vbs_text(fil), ", -1, 0, 0)"),
    paste0("pres.Export ", vbs_text(normalizePath(mapp)), ', "PNG", ', as.integer(bredd), ", ", as.integer(hojd)),
    "pres.Close",
    "If app.Presentations.Count = 0 Then app.Quit"
  )
  con <- file(skriptfil, "wb")
  writeBin(c(as.raw(c(0xFF, 0xFE)), iconv(paste(skript, collapse = "\r\n"), "UTF-8", "UTF-16LE", toRaw = TRUE)[[1]]), con)
  close(con)

  utan_resultat <- suppressWarnings(system2("cscript", c("//nologo", shQuote(normalizePath(skriptfil))),
                                            stdout = TRUE, stderr = TRUE))
  if (!is.null(attr(utan_resultat, "status"))) {
    return(structure(character(0), orsak = paste("VBScript:", paste(trimws(utan_resultat), collapse = " "))))
  }
  intern_png_i_ordning(mapp, paste("PowerPoint (VBScript) körde klart men gjorde inga bilder.", paste(trimws(utan_resultat), collapse = " ")))
}

# PowerPoint döper filerna efter språk (Slide1.PNG, Bild1.PNG ...) - ordna efter numret
intern_png_i_ordning <- function(mapp, orsak_om_inga) {
  filer <- list.files(mapp, pattern = "\\.png$", full.names = TRUE, ignore.case = TRUE)
  if (length(filer) == 0) return(structure(character(0), orsak = orsak_om_inga))
  nummer <- as.integer(sub(".*?([0-9]+)\\.png$", "\\1", filer, ignore.case = TRUE))
  filer[order(nummer)]
}

# PowerPoint styrs via PowerShell. Filen öppnas skrivskyddad och utan fönster. PowerPoint stängs bara
# om den inte redan var igång med andra presentationer öppna.
# Skriptet körs inte som en .ps1-fil med -File: många datorer har en körpolicy som kräver signerade skript
# ("is not digitally signed") och den går inte att kringgå med -ExecutionPolicy Bypass. Körpolicyn gäller bara
# skriptfiler, så skriptets text läses in och körs som ett skriptblock i stället.
intern_rendera_powerpoint <- function(fil, mapp, bredd, hojd) {
  skriptfil <- tempfile(fileext = ".txt")
  on.exit(unlink(skriptfil))
  ps_text <- function(x) paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
  skript <- c(
    "$ErrorActionPreference = 'Stop'",
    paste0("$fil = ", ps_text(fil)),
    paste0("$mapp = ", ps_text(normalizePath(mapp))),
    "$app = New-Object -ComObject PowerPoint.Application",
    "try {",
    "  $pres = $app.Presentations.Open($fil, -1, 0, 0)",
    paste0("  $pres.Export($mapp, 'PNG', ", as.integer(bredd), ", ", as.integer(hojd), ")"),
    "  $pres.Close()",
    "} finally {",
    "  if ($app.Presentations.Count -eq 0) { $app.Quit() }",
    "}"
  )
  # UTF-8 med BOM, så att å/ä/ö i sökvägar läses rätt av PowerShell
  con <- file(skriptfil, "wb")
  writeBin(c(as.raw(c(0xEF, 0xBB, 0xBF)), charToRaw(enc2utf8(paste(skript, collapse = "\r\n")))), con)
  close(con)

  kommando <- paste0("& ([scriptblock]::Create((Get-Content -Raw -Encoding UTF8 -LiteralPath ",
                     ps_text(normalizePath(skriptfil)), ")))")
  utan_resultat <- suppressWarnings(system2(
    "powershell", c("-NoProfile", "-Command", shQuote(kommando)),
    stdout = TRUE, stderr = TRUE))
  if (!is.null(attr(utan_resultat, "status"))) {
    return(structure(character(0), orsak = paste(trimws(utan_resultat), collapse = " ")))
  }

  # PowerPoint döper filerna efter språk (Slide1.PNG, Bild1.PNG ...) - ordna efter numret
  filer <- list.files(mapp, pattern = "\\.png$", full.names = TRUE, ignore.case = TRUE)
  if (length(filer) == 0) {
    return(structure(character(0), orsak = paste("PowerPoint körde klart men gjorde inga bilder.", paste(trimws(utan_resultat), collapse = " "))))
  }
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
