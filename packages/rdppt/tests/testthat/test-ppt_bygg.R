ny_testppt <- function(...) {
  ppt_ny("Testpresentation", mall = "inbyggd", anv_uppgifter = list(namn = "Test Testsson", epost = "test@example.org"), ...)
}

test_that("en presentation byggs steg för steg och sparas", {
  mapp <- testmapp()

  ppt <- ny_testppt() |>
    ppt_lagg_till_slide("bild", bild = testbild(mapp)) |>
    ppt_lagg_till_slide("avsnitt", rubrik = "Ett avsnitt") |>
    ppt_lagg_till_slide("text", rubrik = "En rubrik", text = c("Stycke 1", "Stycke 2")) |>
    ppt_lagg_till_slide("slutsatser", text = c("Slutsats 1", "Slutsats 2")) |>
    ppt_lagg_till_slide("avslut")

  expect_s3_class(ppt, "rdppt")
  expect_equal(nrow(ppt$slides), 5)
  expect_output(print(ppt), "5 slides")

  fil <- suppressMessages(ppt_spara(ppt, file.path(mapp, "kedja.pptx")))
  expect_equal(fil, file.path(mapp, "kedja.pptx"))
  expect_equal(pptx_ordning(fil), c("Testpresentation", "", "Ett avsnitt", "En rubrik", "Slutsatser", "Tack för mig!"))
})

test_that("filnamnet bildas av huvudrubriken om inget anges", {
  mapp <- testmapp()
  ppt <- ppt_ny("Befolkning & arbetsmarknad i Älvdalen", mall = "inbyggd") |> ppt_lagg_till_slide("text", rubrik = "Rubrik", text = "Text")

  fil <- suppressMessages(ppt_spara(ppt, mapp = mapp))
  expect_equal(basename(fil), "befolkning_arbetsmarknad_i_alvdalen.pptx")
})

test_that("ppt_lagg_till_slide(\"diagram\") kör funktionen per region och hittar bilderna själv", {
  ppt <- ny_testppt()
  expect_message(ppt <- ppt_lagg_till_slide(ppt, "diagram", funktion = testdiagram, region_vekt = c("20", "2080"), farg = "red"), "2 diagram tillagda")

  expect_equal(basename(ppt$slides$dia_path), c("testdiagram_20.png", "testdiagram_2080.png"))
  expect_true(all(file.exists(ppt$slides$dia_path)))
  expect_true(all(startsWith(ppt$slides$dia_path, ppt$bildmapp)))

  # en körning med alla regioner på en gång
  expect_message(ppt <- ppt_lagg_till_slide(ppt, "diagram", funktion = testdiagram, region_vekt = c("25", "26"), per_region = FALSE), "2 diagram tillagda")
  expect_equal(nrow(ppt$slides), 4)
})

test_that("ppt_lagg_till_slide(\"diagram\") använder ggplot-objekten när inga filer skrivs", {
  ppt <- suppressMessages(ppt_lagg_till_slide(ny_testppt(), "diagram", funktion = testdiagram, skriv_diagramfil = FALSE, rubrik = "Vektor"))

  expect_equal(nrow(ppt$slides), 1)
  expect_true(is.na(ppt$slides$dia_path))
  expect_s3_class(ppt$slides$gg_obj[[1]], "ggplot")

  # även när en bild med samma namn redan ligger i bildmappen från ett tidigare steg
  ppt <- suppressMessages(ny_testppt() |> ppt_lagg_till_slide("diagram", funktion = testdiagram) |> ppt_lagg_till_slide("diagram", funktion = testdiagram, skriv_diagramfil = FALSE))
  expect_equal(is.na(ppt$slides$dia_path), c(FALSE, TRUE))
  expect_s3_class(ppt$slides$gg_obj[[2]], "ggplot")
})

test_that("ppt_lagg_till_slide(\"diagram\") hoppar över regioner som ger fel, eller stoppar om man ber om det", {
  expect_warning(
    ppt <- suppressMessages(ppt_lagg_till_slide(ny_testppt(), "diagram", funktion = testdiagram, region_vekt = c("20", "fel", "2080"))),
    "gav fel för fel och hoppas över: ingen data för regionen")
  expect_equal(basename(ppt$slides$dia_path), c("testdiagram_20.png", "testdiagram_2080.png"))

  expect_error(suppressMessages(ppt_lagg_till_slide(ny_testppt(), "diagram", funktion = testdiagram, region_vekt = "fel", stoppa_vid_fel = TRUE)),
               "ingen data för regionen")
})

test_that("ppt_lagg_till_slide(\"diagram\") stoppar direkt på felstavade parametrar", {
  expect_error(ppt_lagg_till_slide(ny_testppt(), "diagram", funktion = testdiagram, felstavad = 1), "ingen parameter som heter 'felstavad'")
  expect_error(ppt_lagg_till_slide(ny_testppt(), "diagram", funktion = function(kommun) NULL, region_vekt = "20"), "ingen parameter som heter 'region_vekt'")
})

test_that("ett felstavat layoutnamn stoppas när sliden läggs till", {
  expect_error(ppt_lagg_till_slide(ny_testppt(), "Finns inte", rubrik = "Rubrik"), "varken en sorts slide eller en layout")
  expect_error(ppt_lagg_till_slide(ny_testppt(), "bild", bild = "finns_inte.png"), "Hittar inte bildfilen")
  expect_error(ppt_lagg_till_slide("inte en presentation", "text", rubrik = "Rubrik"), "ppt_ny")
})

test_that("ppt_lagg_till_slide(\"befintlig\") lägger in färdiga slides där den anropas", {
  mapp <- testmapp()
  fardiga <- testfil_med_slides(mapp, c("Färdig A", "Färdig B", "Färdig C"))

  ppt <- ny_testppt() |>
    ppt_lagg_till_slide("text", rubrik = "Ny 1") |>
    ppt_lagg_till_slide("befintlig", fil = fardiga, slide = 2:3) |>
    ppt_lagg_till_slide("text", rubrik = "Ny 2") |>
    ppt_lagg_till_slide("befintlig", slide = 1) |>
    ppt_lagg_till_slide("text", rubrik = "Ny 3")

  fil <- suppressMessages(ppt_spara(ppt, file.path(mapp, "med_fardiga.pptx")))
  expect_equal(pptx_ordning(fil), c("Testpresentation", "Ny 1", "Färdig B", "Färdig C", "Ny 2", "Färdig A", "Ny 3"))

  # utan slide läggs alla in, och de som inte läggs in alls kommer inte med
  alla <- ny_testppt() |> ppt_lagg_till_slide("befintlig", fil = fardiga) |> ppt_lagg_till_slide("text", rubrik = "Sist")
  expect_equal(pptx_ordning(suppressMessages(ppt_spara(alla, file.path(mapp, "alla.pptx")))),
               c("Testpresentation", "Färdig A", "Färdig B", "Färdig C", "Sist"))
  en <- ny_testppt() |> ppt_lagg_till_slide("befintlig", fil = fardiga, slide = 2)
  expect_equal(pptx_ordning(suppressMessages(ppt_spara(en, file.path(mapp, "en.pptx")))), c("Testpresentation", "Färdig B"))
})

test_that("ppt_lagg_till_slide(\"befintlig\") säger ifrån om slides som inte finns eller används två gånger", {
  mapp <- testmapp()
  fardiga <- testfil_med_slides(mapp, c("Färdig A", "Färdig B"))

  expect_error(ppt_lagg_till_slide(ny_testppt(), "befintlig", fil = fardiga, slide = 3), "mellan 1 och 2")
  expect_error(ppt_lagg_till_slide(ny_testppt(), "befintlig"), "inga färdiga slides")
  expect_error(ny_testppt() |> ppt_lagg_till_slide("befintlig", fil = fardiga, slide = 1) |> ppt_lagg_till_slide("befintlig", slide = 1), "bara läggas in på ett ställe")
  expect_error(ny_testppt() |> ppt_lagg_till_slide("befintlig", fil = fardiga, slide = 1) |> ppt_lagg_till_slide("befintlig", fil = file.path(mapp, "annan.pptx")), "en fil per presentation")
})

test_that("ppt_befintlig_visa() listar sliderna i visningsordning med rubrik, innehåll och layout", {
  mapp <- testmapp()
  pres <- officer::read_pptx()
  pres <- officer::add_slide(pres, layout = "Title and Content", master = "Office Theme") |>
    officer::ph_with("Punkter", officer::ph_location_type(type = "title")) |>
    officer::ph_with(c("Ett", "Två"), officer::ph_location_type(type = "body"))
  pres <- officer::add_slide(pres, layout = "Title Only", master = "Office Theme") |>
    officer::ph_with("Bild och textruta", officer::ph_location_type(type = "title")) |>
    officer::ph_with(officer::external_img(testbild(mapp)), officer::ph_location(left = 1, top = 2, width = 4, height = 2)) |>
    officer::ph_with("En fri textruta", officer::ph_location(left = 6, top = 2, width = 3, height = 1))
  pres <- officer::add_slide(pres, layout = "Blank", master = "Office Theme") |>
    officer::ph_with(head(iris, 2), officer::ph_location(left = 1, top = 1, width = 6, height = 2))
  # sista sliden flyttas först, så att visningsordningen skiljer sig från filordningen
  pres <- officer::move_slide(pres, index = 3, to = 1)
  fil <- file.path(mapp, "fardiga.pptx")
  print(pres, target = fil)

  expect_output(slides <- ppt_befintlig_visa(fil), "ppt_lagg_till_slide\\(\"befintlig\"")
  expect_equal(slides$slide, 1:3)
  expect_equal(slides$rubrik, c("", "Punkter", "Bild och textruta"))
  expect_equal(slides$text, c("", "Ett", "En fri textruta"))
  expect_equal(slides$innehall, c("1 tabell", "", "1 bild"))
  expect_equal(slides$layout, c("Blank", "Title and Content", "Title Only"))

  # numren är desamma som ppt_lagg_till_slide("befintlig") använder
  ny <- ny_testppt() |> ppt_lagg_till_slide("befintlig", fil = fil, slide = 3)
  expect_equal(pptx_ordning(suppressMessages(ppt_spara(ny, file.path(mapp, "ny.pptx")))),
               c("Testpresentation", "Bild och textruta"))

  expect_output(tom <- ppt_befintlig_visa(rdppt:::intern_rd_medfoljande_mall()), "inga slides")
  expect_equal(nrow(tom), 0)
  expect_error(ppt_befintlig_visa(file.path(mapp, "finns_inte.pptx")), "Hittar inte pptx-filen")
})

test_that("ppt_forhandsvisa() gör en bild per slide och en översikt", {
  skip_if(!nzchar(Sys.which("soffice")) || (!nzchar(Sys.which("pdftoppm")) && !requireNamespace("pdftools", quietly = TRUE)),
          "LibreOffice och pdftoppm/pdftools behövs för att göra bilder")
  mapp <- testmapp()
  fil <- testfil_med_slides(mapp, c("Färdig A", "Färdig B & C", "Färdig D"))

  expect_message(bilder <- ppt_forhandsvisa(fil, oppna = FALSE), "3 slides")
  expect_length(bilder, 3)
  expect_true(all(file.exists(bilder)))
  oversikt <- paste(readLines(attr(bilder, "oversikt"), encoding = "UTF-8"), collapse = "\n")
  expect_match(oversikt, "Färdig B &amp; C", fixed = TRUE)

  expect_length(suppressMessages(ppt_forhandsvisa(fil, slide = 2:3, oppna = FALSE)), 2)
  expect_error(ppt_forhandsvisa(fil, slide = 4, oppna = FALSE), "mellan 1 och 3")
  expect_error(ppt_forhandsvisa(file.path(mapp, "finns_inte.pptx"), oppna = FALSE), "Hittar inte pptx-filen")

  # utan fil visas mallens layouter
  expect_length(suppressMessages(ppt_forhandsvisa(mall = "inbyggd", oppna = FALSE)), 7)
})

test_that("ppt_lagg_till_slide(\"befintlig\") hämtar från options(rdppt.slidebank) när ingen fil anges", {
  fardiga <- testfil_med_slides(testmapp(), c("Färdig A", "Färdig B"))
  withr::local_options(rdppt.mall = "inbyggd", rdppt.slidebank = fardiga)
  ppt <- ppt_ny() |> ppt_lagg_till_slide("befintlig", slide = 2)
  expect_equal(ppt$grundfil, fardiga)
  expect_equal(ppt$slides$mall, "befintlig:2")
})

test_that("ppt_lagg_till_slide() lägger till olika sorters slides med anteckningar", {
  mapp <- testmapp()
  ppt <- ny_testppt() |>
    ppt_lagg_till_slide("text", rubrik = "R", text = "T", anteckningar = c("Första", "Andra")) |>
    ppt_lagg_till_slide("slutsatser", text = c("a", "b")) |>
    ppt_lagg_till_slide("bild", bild = testbild(mapp), anteckningar = "Om bilden")
  expect_equal(nrow(ppt$slides), 3)
  expect_equal(ppt$slides$titel, c("R", "Slutsatser", NA))
  expect_equal(ppt$slides$anteckningar[[1]], c("Första", "Andra"))
  expect_equal(ppt$slides$mall[2], "Slutsatser_Samhallsanalys")

  ppt <- suppressMessages(ppt_lagg_till_slide(ny_testppt(), "diagram", funktion = testdiagram,
                                              region_vekt = c("20", "2080"), farg = "red", anteckningar = "Diagram"))
  expect_equal(nrow(ppt$slides), 2)
  expect_equal(ppt$slides$anteckningar[[2]], "Diagram")
})

test_that("ppt_lagg_till_slide() accepterar mallens egna layouter och med innehåll efter vad som anges", {
  mapp <- testmapp()
  ppt <- ny_testppt() |>
    ppt_lagg_till_slide("Title and Content", rubrik = "R", text = c("a", "b")) |>
    ppt_lagg_till_slide("Title Only", bild = testbild(mapp), rubrik = "Bild")
  expect_equal(ppt$slides$mall, c("Title and Content", "Title Only"))
  expect_false(is.na(ppt$slides$dia_path[2]))
  fil <- ppt_spara(ppt, "layouter.pptx", mapp = mapp)
  expect_true(file.exists(fil))
})

test_that("ppt_lagg_till_slide() säger ifrån om parametrar som inte gäller sliden", {
  p <- ny_testppt()
  expect_error(ppt_lagg_till_slide(p), "Ange vilken sorts slide")
  expect_error(ppt_lagg_till_slide(p, "slutsatser", text = "a", bild = "x.png"), "ingen parameter som heter 'bild'")
  expect_error(ppt_lagg_till_slide(p, "slutsatser"), "kräver text")
  expect_error(ppt_lagg_till_slide(p, "diagram"), "kräver funktion")
  expect_error(ppt_lagg_till_slide(p, "text", rubrik = "R", farg = "red"), "bara anges för diagram")
  expect_error(ppt_lagg_till_slide(p, "rapporter", rubrik = "R"), "ingen parameter som heter 'rubrik'")
  expect_error(ppt_lagg_till_slide(p, "Title Only", funktion = testdiagram, bild = "x.png"), "ingen parameter som heter 'bild'")
  expect_error(ppt_lagg_till_slide(p, "Finns inte"), "varken en sorts slide eller en layout")
})

test_that("ppt_lagg_till_slide() kan hämta färdiga slides och ppt_slidetyper() listar sorterna", {
  fardiga <- testfil_med_slides(testmapp(), c("A", "B"))
  ppt <- ny_testppt() |> ppt_lagg_till_slide("befintlig", fil = fardiga, slide = 2)
  expect_equal(ppt$slides$mall, "befintlig:2")

  capture.output(typer <- ppt_slidetyper())
  expect_true(all(c("diagram", "slutsatser", "befintlig") %in% typer$layout))
})

test_that("anteckningar kan vara en funktion av regionen", {
  ppt <- suppressMessages(ppt_lagg_till_slide(
    ny_testppt(), "diagram", funktion = testdiagram, region_vekt = c("20", "2080"),
    anteckningar = \(region) paste("Anteckning för", region)))
  expect_equal(ppt$slides$anteckningar[[1]], "Anteckning för 20")
  expect_equal(ppt$slides$anteckningar[[2]], "Anteckning för 2080")

  # en funktion som ger fel utelämnar bara anteckningarna
  expect_warning(
    ppt <- suppressMessages(ppt_lagg_till_slide(ny_testppt(), "diagram", funktion = testdiagram,
                                                anteckningar = \(region) stop("ingen data"))),
    "anteckningar")
  expect_equal(nrow(ppt$slides), 1)
  expect_null(ppt$slides$anteckningar[[1]])

  expect_error(ppt_lagg_till_slide(ny_testppt(), "text", rubrik = "R", anteckningar = \(region) "x"), "bara för diagram")
})

test_that("en tom regionvektor hoppar över diagrammet", {
  ppt <- suppressMessages(ppt_lagg_till_slide(ny_testppt(), "diagram", funktion = testdiagram, region_vekt = character(0)))
  expect_equal(nrow(ppt$slides), 0)
  expect_equal(ppt$logg$status, "hoppad")

  # NULL är något annat: körs en gång utan region
  ppt <- suppressMessages(ppt_lagg_till_slide(ny_testppt(), "diagram", funktion = testdiagram))
  expect_equal(nrow(ppt$slides), 1)
})

test_that("diagram som hoppas över loggas, i loggfilen och som sammanfattning vid sparandet", {
  mapp <- testmapp()
  logg <- file.path(mapp, "logg.txt")
  ppt <- suppressMessages(suppressWarnings(
    ny_testppt(loggfil = logg) |>
      ppt_lagg_till_slide("diagram", funktion = testdiagram, region_vekt = c("20", "fel", "2080")) |>
      ppt_lagg_till_slide("text", rubrik = "R", text = "T")))
  expect_equal(ppt$logg$status, "fel")
  expect_equal(ppt$logg$region, "fel")
  expect_match(readLines(logg), "testdiagram \\(fel\\) fel: ingen data för regionen")

  expect_message(ppt_spara(ppt, "logg.pptx", mapp = mapp), "1 diagram saknas")

  # ingen sammanfattning när allt gick bra
  bra <- suppressMessages(ny_testppt() |> ppt_lagg_till_slide("text", rubrik = "R"))
  expect_false(any(grepl("OBS", testthat::capture_messages(ppt_spara(bra, "bra.pptx", mapp = mapp)))))
})

test_that("ppt_spara() sparar en fil per person när flera anges", {
  mapp <- testmapp()
  ppt <- ny_testppt() |> ppt_lagg_till_slide("text", rubrik = "R", text = "T") |> ppt_lagg_till_slide("avslut")
  testthat::local_mocked_bindings(
    intern_anv_uppgifter = function(anv_uppgifter) list(namn = paste("Namn", anv_uppgifter), epost = "x@example.org"))
  filer <- suppressMessages(ppt_spara(ppt, "utmaningar.pptx", mapp = mapp, anv_uppgifter = c("peter", "jon")))
  expect_equal(basename(filer), c("utmaningar_peter.pptx", "utmaningar_jon.pptx"))
  expect_true(all(file.exists(filer)))

  # en person: inget tillägg i filnamnet
  fil <- suppressMessages(ppt_spara(ppt, "en.pptx", mapp = mapp, anv_uppgifter = list(namn = "A", epost = "a@b.se")))
  expect_equal(basename(fil), "en.pptx")
})

test_that("options(rdppt.stil) och ppt_ny(stil = ) styr rubrikformateringen", {
  withr::local_options(rdppt.mall = NULL, rdppt.stil = list(titel_stlk = 54))
  expect_equal(intern_mall_aktuell("regiondalarna")$stil$titel_stlk, 54)
  expect_equal(intern_mall_aktuell("regiondalarna")$stil$undertitel_stlk, 20)

  ppt <- ppt_ny("T", mall = "regiondalarna", stil = list(titel_stlk = 60))
  expect_equal(ppt$profil$stil$titel_stlk, 60)
})

test_that("funktionsparametrar ger diagramfunktionen parametrar som krockar med ppt_lagg_till_slide()", {
  # funktionens egen region_vekt får ett fast värde, medan regionvariabel styr vad som körs per region
  diagram <- function(region_vekt = "20", fokus = "a", utmapp = NA, skriv_diagramfil = FALSE) {
    list(ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) + ggplot2::geom_point() +
           ggplot2::ggtitle(paste(paste(region_vekt, collapse = ","), fokus)))
  }
  ppt <- suppressMessages(ppt_lagg_till_slide(
    ny_testppt(), "diagram", funktion = diagram, regionvariabel = "fokus", region_vekt = c("a", "b"),
    funktionsparametrar = list(region_vekt = c("20", "2021"))))
  expect_equal(nrow(ppt$slides), 2)
  expect_equal(ppt$slides$gg_obj[[2]]$labels$title, "20,2021 b")

  expect_error(ppt_lagg_till_slide(ny_testppt(), "diagram", funktion = diagram, funktionsparametrar = list("a")), "namngiven lista")
  expect_error(ppt_lagg_till_slide(ny_testppt(), "diagram", funktion = diagram, fokus = "x", funktionsparametrar = list(fokus = "y")),
               "både direkt och i funktionsparametrar")
  expect_error(ppt_lagg_till_slide(ny_testppt(), "text", rubrik = "R", funktionsparametrar = list(a = 1)), "ingen parameter som heter 'funktionsparametrar'")
})
