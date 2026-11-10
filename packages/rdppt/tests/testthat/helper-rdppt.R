# Testerna körs mot den mall som är inbyggd i officer och mot den kopia av Region Dalarnas mall som
# följer med paketet, så att de inte kräver någon fil på G:.

# En tom mapp att spara presentationer i, som städas bort när testet är klart.
testmapp <- function(env = parent.frame()) {
  mapp <- tempfile("rdppt_test_")
  dir.create(mapp)
  withr::defer(unlink(mapp, recursive = TRUE), envir = env)
  mapp
}

# En diagramfunktion med samma gränssnitt som de i rddiagramkatalog: returnerar en namngiven lista
# med ggplot-objekt och sparar en png per diagram i utmapp om skriv_diagramfil = TRUE.
testdiagram <- function(region_vekt = "20", utmapp = NA, skriv_diagramfil = FALSE, farg = "blue") {
  lapply(stats::setNames(region_vekt, paste0("testdiagram_", region_vekt)), function(region) {
    if (region == "fel") stop("ingen data för regionen")
    diagram <- ggplot2::ggplot(data.frame(x = 1:3, y = c(2, 5, 3)), ggplot2::aes(x, y)) +
      ggplot2::geom_col(fill = farg)
    if (skriv_diagramfil) {
      ggplot2::ggsave(paste0(utmapp, "testdiagram_", region, ".png"), diagram, width = 4, height = 2, dpi = 30)
    }
    diagram
  })
}

# En png-bild att lägga in i en presentation.
testbild <- function(mapp, namn = "bild.png") {
  fil <- file.path(mapp, namn)
  grDevices::png(fil, width = 200, height = 100)
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  grDevices::dev.off()
  fil
}

# En pptx-fil skapad utifrån den inbyggda mallen, med färdiga slides som har de angivna rubrikerna.
testfil_med_slides <- function(mapp, rubriker) {
  pres <- officer::read_pptx()
  for (rubrik in rubriker) {
    pres <- officer::add_slide(pres, layout = "Title Only", master = "Office Theme")
    pres <- officer::ph_with(pres, value = rubrik, location = officer::ph_location_type(type = "title"))
  }
  fil <- file.path(mapp, "fardiga_slides.pptx")
  print(pres, target = fil)
  fil
}

# Den första textsträngen på varje slide i en pptx-fil, i den ordning sliderna visas. Läses direkt
# ur filen eftersom officer::pptx_summary() redovisar slides i filordning, inte i visningsordning.
pptx_ordning <- function(fil) {
  mapp <- tempfile()
  utils::unzip(fil, exdir = mapp)
  on.exit(unlink(mapp, recursive = TRUE))
  las <- function(...) paste(readLines(file.path(mapp, ...), warn = FALSE, encoding = "UTF-8"), collapse = "")

  kopplingar <- regmatches(las("ppt/_rels/presentation.xml.rels"),
                           gregexpr("<Relationship [^>]*>", las("ppt/_rels/presentation.xml.rels")))[[1]]
  id <- sub('.* Id="([^"]+)".*', "\\1", kopplingar)
  target <- sub('.* Target="([^"]+)".*', "\\1", kopplingar)
  slides <- regmatches(las("ppt/presentation.xml"), gregexpr("<p:sldId [^>]*>", las("ppt/presentation.xml")))[[1]]

  vapply(sub('.*r:id="([^"]+)".*', "\\1", slides), function(rid) {
    xml <- las("ppt", target[id == rid])
    texter <- regmatches(xml, gregexpr("<a:t>[^<]*</a:t>", xml))[[1]]
    if (length(texter) == 0) "" else gsub("<[^>]+>", "", texter[1])
  }, character(1), USE.NAMES = FALSE)
}

# Innehållet i en sparad presentation, en rad per stycke/bild.
pptx_innehall <- function(fil) {
  officer::pptx_summary(officer::read_pptx(fil))
}
