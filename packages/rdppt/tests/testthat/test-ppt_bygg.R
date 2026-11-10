ny_testppt <- function(...) {
  ppt_ny("Testpresentation", mall = "inbyggd", anv_uppgifter = list(namn = "Test Testsson", epost = "test@example.org"), ...)
}

test_that("en presentation byggs steg för steg och sparas", {
  mapp <- testmapp()

  ppt <- ny_testppt() |>
    ppt_bild(testbild(mapp)) |>
    ppt_avsnitt("Ett avsnitt") |>
    ppt_text("En rubrik", c("Stycke 1", "Stycke 2")) |>
    ppt_slutsatser(c("Slutsats 1", "Slutsats 2")) |>
    ppt_avslut()

  expect_s3_class(ppt, "rdppt")
  expect_equal(nrow(ppt$slides), 5)
  expect_output(print(ppt), "5 slides")

  fil <- suppressMessages(ppt_spara(ppt, file.path(mapp, "kedja.pptx")))
  expect_equal(fil, file.path(mapp, "kedja.pptx"))
  expect_equal(pptx_ordning(fil), c("Testpresentation", "", "Ett avsnitt", "En rubrik", "Slutsatser", "Tack för mig!"))
})

test_that("filnamnet bildas av huvudrubriken om inget anges", {
  mapp <- testmapp()
  ppt <- ppt_ny("Befolkning & arbetsmarknad i Älvdalen", mall = "inbyggd") |> ppt_text("Rubrik", "Text")

  fil <- suppressMessages(ppt_spara(ppt, mapp = mapp))
  expect_equal(basename(fil), "befolkning_arbetsmarknad_i_alvdalen.pptx")
})

test_that("ppt_diagram() kör funktionen per region och hittar bilderna själv", {
  ppt <- ny_testppt()
  expect_message(ppt <- ppt_diagram(ppt, testdiagram, region_vekt = c("20", "2080"), farg = "red"), "2 diagram tillagda")

  expect_equal(basename(ppt$slides$dia_path), c("testdiagram_20.png", "testdiagram_2080.png"))
  expect_true(all(file.exists(ppt$slides$dia_path)))
  expect_true(all(startsWith(ppt$slides$dia_path, ppt$bildmapp)))

  # en körning med alla regioner på en gång
  expect_message(ppt <- ppt_diagram(ppt, testdiagram, region_vekt = c("25", "26"), per_region = FALSE), "2 diagram tillagda")
  expect_equal(nrow(ppt$slides), 4)
})

test_that("ppt_diagram() använder ggplot-objekten när inga filer skrivs", {
  ppt <- suppressMessages(ppt_diagram(ny_testppt(), testdiagram, skriv_diagramfil = FALSE, rubrik = "Vektor"))

  expect_equal(nrow(ppt$slides), 1)
  expect_true(is.na(ppt$slides$dia_path))
  expect_s3_class(ppt$slides$gg_obj[[1]], "ggplot")

  # även när en bild med samma namn redan ligger i bildmappen från ett tidigare steg
  ppt <- suppressMessages(ny_testppt() |> ppt_diagram(testdiagram) |> ppt_diagram(testdiagram, skriv_diagramfil = FALSE))
  expect_equal(is.na(ppt$slides$dia_path), c(FALSE, TRUE))
  expect_s3_class(ppt$slides$gg_obj[[2]], "ggplot")
})

test_that("ppt_diagram() hoppar över regioner som ger fel, eller stoppar om man ber om det", {
  expect_warning(
    ppt <- suppressMessages(ppt_diagram(ny_testppt(), testdiagram, region_vekt = c("20", "fel", "2080"))),
    "gav fel för fel och hoppas över: ingen data för regionen")
  expect_equal(basename(ppt$slides$dia_path), c("testdiagram_20.png", "testdiagram_2080.png"))

  expect_error(suppressMessages(ppt_diagram(ny_testppt(), testdiagram, region_vekt = "fel", stoppa_vid_fel = TRUE)),
               "ingen data för regionen")
})

test_that("ppt_diagram() stoppar direkt på felstavade parametrar", {
  expect_error(ppt_diagram(ny_testppt(), testdiagram, felstavad = 1), "ingen parameter som heter 'felstavad'")
  expect_error(ppt_diagram(ny_testppt(), function(kommun) NULL, region_vekt = "20"), "ingen parameter som heter 'region_vekt'")
})

test_that("ett felstavat layoutnamn stoppas när sliden läggs till", {
  expect_error(ppt_text(ny_testppt(), "Rubrik", layout = "Finns inte"), "finns inte i mallen")
  expect_error(ppt_bild(ny_testppt(), "finns_inte.png"), "Hittar inte bildfilen")
  expect_error(ppt_text("inte en presentation", "Rubrik"), "ppt_ny")
})

test_that("ppt_befintlig() lägger in färdiga slides där den anropas", {
  mapp <- testmapp()
  fardiga <- testfil_med_slides(mapp, c("Färdig A", "Färdig B", "Färdig C"))

  ppt <- ny_testppt() |>
    ppt_text("Ny 1") |>
    ppt_befintlig(fardiga, slide = 2:3) |>
    ppt_text("Ny 2") |>
    ppt_befintlig(slide = 1) |>
    ppt_text("Ny 3")

  fil <- suppressMessages(ppt_spara(ppt, file.path(mapp, "med_fardiga.pptx")))
  expect_equal(pptx_ordning(fil), c("Testpresentation", "Ny 1", "Färdig B", "Färdig C", "Ny 2", "Färdig A", "Ny 3"))

  # utan slide läggs alla in, och de som inte läggs in alls kommer inte med
  alla <- ny_testppt() |> ppt_befintlig(fardiga) |> ppt_text("Sist")
  expect_equal(pptx_ordning(suppressMessages(ppt_spara(alla, file.path(mapp, "alla.pptx")))),
               c("Testpresentation", "Färdig A", "Färdig B", "Färdig C", "Sist"))
  en <- ny_testppt() |> ppt_befintlig(fardiga, slide = 2)
  expect_equal(pptx_ordning(suppressMessages(ppt_spara(en, file.path(mapp, "en.pptx")))), c("Testpresentation", "Färdig B"))
})

test_that("ppt_befintlig() säger ifrån om slides som inte finns eller används två gånger", {
  mapp <- testmapp()
  fardiga <- testfil_med_slides(mapp, c("Färdig A", "Färdig B"))

  expect_error(ppt_befintlig(ny_testppt(), fardiga, slide = 3), "mellan 1 och 2")
  expect_error(ppt_befintlig(ny_testppt()), "inga färdiga slides")
  expect_error(ny_testppt() |> ppt_befintlig(fardiga, slide = 1) |> ppt_befintlig(slide = 1), "bara läggas in på ett ställe")
  expect_error(ny_testppt() |> ppt_befintlig(fardiga, slide = 1) |> ppt_befintlig(file.path(mapp, "annan.pptx")), "en fil per presentation")
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

  expect_output(slides <- ppt_befintlig_visa(fil), "ppt_befintlig\\(fil, slide = <nummer>\\)")
  expect_equal(slides$slide, 1:3)
  expect_equal(slides$rubrik, c("", "Punkter", "Bild och textruta"))
  expect_equal(slides$text, c("", "Ett", "En fri textruta"))
  expect_equal(slides$innehall, c("1 tabell", "", "1 bild"))
  expect_equal(slides$layout, c("Blank", "Title and Content", "Title Only"))

  # numren är desamma som ppt_befintlig() använder
  ny <- ny_testppt() |> ppt_befintlig(fil, slide = 3)
  expect_equal(pptx_ordning(suppressMessages(ppt_spara(ny, file.path(mapp, "ny.pptx")))),
               c("Testpresentation", "Bild och textruta"))

  expect_output(tom <- ppt_befintlig_visa(rdppt:::intern_rd_medfoljande_mall()), "inga slides")
  expect_equal(nrow(tom), 0)
  expect_error(ppt_befintlig_visa(file.path(mapp, "finns_inte.pptx")), "Hittar inte pptx-filen")
})
