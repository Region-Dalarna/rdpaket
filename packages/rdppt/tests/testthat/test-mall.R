test_that("den inbyggda mallens layouter får roller utifrån sina namn", {
  profil <- ppt_mallprofil()

  expect_s3_class(profil, "rdppt_mallprofil")
  expect_equal(profil$master, "Office Theme")
  expect_equal(profil$layouter[["titel"]], "Title Slide")
  expect_equal(profil$layouter[["bild"]], "Blank")
  expect_equal(profil$layouter[["bild_rubrik"]], "Title Only")
  expect_equal(profil$layouter[["punktlista"]], "Title and Content")
  expect_false(anyNA(profil$layouter))
})

test_that("layouter kan pekas ut för en roll, och felstavade namn stoppas", {
  profil <- ppt_mallprofil(layouter = c(bild = "Title Only"))
  expect_equal(profil$layouter[["bild"]], "Title Only")

  expect_error(ppt_mallprofil(layouter = c(bild = "Finns inte")), "finns inte i mallen")
  expect_error(ppt_mallprofil(layouter = c(okand_roll = "Blank")), "Giltiga roller")
  expect_error(ppt_mallprofil(master = "Finns inte"), "Mastern")
  expect_error(ppt_mallprofil(mall_fil = "finns/inte.pptx"), "Hittar inte pptx-filen")
})

test_that("ppt_mall_ange() väljer mall för sessionen och NULL återställer", {
  withr::local_options(rdppt.mall = NULL)

  expect_output(profil <- ppt_mall_ange("inbyggd"), "Title Slide")
  expect_identical(getOption("rdppt.mall"), profil)
  expect_null(profil$stil)

  expect_message(ppt_mall_ange(NULL), "återställt")
  expect_null(getOption("rdppt.mall"))
})

test_that("ppt_mall_visa() listar layouterna och platshållarna i en layout", {
  expect_output(layouter <- ppt_mall_visa("inbyggd"), "Two Content")
  expect_setequal(layouter$layout, c("Title Slide", "Title and Content", "Section Header", "Two Content",
                                     "Comparison", "Title Only", "Blank"))
  expect_equal(layouter$text[layouter$layout == "Two Content"], 2)
  expect_equal(layouter$rubrik[layouter$layout == "Blank"], "-")
  expect_equal(layouter$anvands_for[layouter$layout == "Title Only"], "bild_rubrik")

  grDevices::pdf(NULL)
  withr::defer(grDevices::dev.off())
  expect_output(platshallare <- ppt_mall_visa("inbyggd", layout = "Two Content"), "Layout: 'Two Content'")
  expect_equal(platshallare$typ, c("rubrik", "text", "text"))
  expect_error(ppt_mall_visa("inbyggd", layout = "Finns inte"), "finns inte i mallen")
})

test_that("ppt_mall_exempel() ger en slide per layout", {
  mapp <- testmapp()
  expect_message(fil <- ppt_mall_exempel("inbyggd", fil = file.path(mapp, "exempel.pptx"), oppna = FALSE), "7 layouter")

  expect_equal(length(officer::read_pptx(fil)), 7)
  expect_equal(pptx_ordning(fil)[1:2], c("Title Slide", "Title and Content"))
})

test_that("Region Dalarnas mall följer med paketet och är standardvalet", {
  withr::local_options(rdppt.mall = NULL)

  profil <- ppt_mallprofil(rdppt:::intern_rd_medfoljande_mall())
  expect_equal(profil$master, "VCdag")
  expect_equal(profil$layouter[["titel"]], "Rubrikbild")
  expect_equal(profil$layouter[["bild"]], "Bild utan text och rubrik")
  expect_false(anyNA(profil$layouter))
  expect_equal(profil$stil$farg_rubrik, "#2286AC")

  # utan vald mall: filen på G: om den går att nå, annars kopian - i båda fallen Region Dalarnas
  expect_equal(ppt_ny("Rubrik")$profil$layouter[["titel"]], "Rubrikbild")
  expect_equal(suppressMessages(ppt_ny("Rubrik", mall = "regiondalarna"))$profil$mall_fil,
               rdppt:::intern_rd_medfoljande_mall())
})

test_that("en presentation går att bygga i Region Dalarnas mall, även med äldre layoutnamn", {
  mapp <- testmapp()
  bild <- testbild(mapp)

  ppt <- ppt_ny("Rubrik", mall = "regiondalarna", anv_uppgifter = list(namn = "Test Testsson", epost = "test@example.org")) |>
    ppt_bild(bild) |>
    ppt_bild(bild, rubrik = "Bild med rubrik") |>
    ppt_bild(bild, rubrik = "Bild med text", text = "Text") |>
    ppt_text("Klimat", c("Punkt 1", "Punkt 2"), layout = "Klimatsmart Dalarna") |>
    ppt_avsnitt("Avsnitt") |>
    ppt_slutsatser(c("Ett", "Två")) |>
    ppt_avslut()
  fil <- suppressMessages(ppt_spara(ppt, file.path(mapp, "rd.pptx")))
  expect_equal(pptx_ordning(fil), c("Rubrik", "", "Bild med rubrik", "Bild med text", "Klimat", "Avsnitt", "Slutsatser",
                                    "Tack för mig!"))

  # "Bild och text till höger" finns inte i mallen, men fungerade med äldre mallar
  ppt_lista <- initiera_ppt_df() |>
    dplyr::add_row(dia_path = bild, titel = "Äldre layoutnamn", beskr = list("Text"), mall = "Bild och text till höger")
  fil <- suppressMessages(skapa_ppt_fran_lista(ppt_lista, huvudrubrik = NULL, output_mapp = mapp, mall_fil = "regiondalarna"))
  expect_equal(pptx_ordning(fil), "Äldre layoutnamn")
})
