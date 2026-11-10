anv <- list(namn = "Test Testsson", epost = "test@example.org")

test_that("en ppt-lista blir en presentation med titelsida och en slide per rad", {
  mapp <- testmapp()
  bild <- testbild(mapp)

  ppt_lista <- initiera_ppt_df() |>
    dplyr::add_row(dia_path = bild, mall = "Bild utan text och rubrik") |>
    dplyr::add_row(mall = "Slutsatser_Samhallsanalys", titel = "Slutsatser",
                   beskr = list(c("Första", "Andra")), anteckningar = list(c("rad 1", "rad 2"))) |>
    dplyr::add_row(mall = "Avslutsbild_Samhallsanalys")

  # mappen anges utan avslutande snedstreck, vilket tidigare gav fel sökväg
  expect_message(
    fil <- skapa_ppt_fran_lista(ppt_lista, huvudrubrik = "Huvudrubriken", output_mapp = mapp,
                                filnamn = "test.pptx", mall_fil = "inbyggd", anv_uppgifter = anv),
    "4 slides sparad")

  expect_equal(fil, file.path(mapp, "test.pptx"))
  expect_true(file.exists(fil))
  expect_equal(pptx_ordning(fil), c("Huvudrubriken", "", "Slutsatser", "Tack för mig!"))

  innehall <- pptx_innehall(fil)
  expect_equal(sum(innehall$content_type == "image"), 1)
  expect_true(all(c("Första", "Andra") %in% innehall$text))
  expect_true(any(grepl("test@example.org", innehall$text, fixed = TRUE)))
})

test_that("anteckningar kommer med, även på bildslides", {
  mapp <- testmapp()
  ppt_lista <- initiera_ppt_df() |>
    dplyr::add_row(dia_path = testbild(mapp), anteckningar = list("Anteckning till bilden"))

  fil <- suppressMessages(skapa_ppt_fran_lista(ppt_lista, huvudrubrik = "Rubrik", output_mapp = mapp, mall_fil = "inbyggd"))

  utils::unzip(fil, exdir = file.path(mapp, "uppackad"))
  anteckningsfiler <- list.files(file.path(mapp, "uppackad", "ppt", "notesSlides"), pattern = "\\.xml$", full.names = TRUE)
  expect_length(anteckningsfiler, 1)
  expect_match(paste(readLines(anteckningsfiler, warn = FALSE, encoding = "UTF-8"), collapse = ""), "Anteckning till bilden")
})

test_that("ett ggplot-objekt i gg_obj läggs in som vektorgrafik", {
  mapp <- testmapp()
  ppt_lista <- initiera_ppt_df() |>
    dplyr::add_row(titel = "Diagram", gg_obj = list(testdiagram()[[1]]))

  fil <- suppressMessages(skapa_ppt_fran_lista(ppt_lista, huvudrubrik = "Rubrik", output_mapp = mapp, mall_fil = "inbyggd"))

  expect_equal(pptx_ordning(fil), c("Rubrik", "Diagram"))
  utils::unzip(fil, exdir = file.path(mapp, "uppackad"))
  slide <- paste(readLines(file.path(mapp, "uppackad", "ppt", "slides", "slide2.xml"), warn = FALSE), collapse = "")
  # rvg ritar diagrammet som en grupp av figurer, inte som en inbäddad bild
  expect_match(slide, "<p:grpSp ", fixed = TRUE)
  expect_false(any(pptx_innehall(fil)$content_type == "image"))
})

test_that("layouten väljs efter innehållet när mall är NA, och alla layouter i mallen går att ange", {
  mapp <- testmapp()
  bild <- testbild(mapp)
  layouter_i_fil <- function(ppt_lista, ...) {
    fil <- suppressMessages(skapa_ppt_fran_lista(ppt_lista, huvudrubrik = NULL, output_mapp = mapp, mall_fil = "inbyggd", ...))
    officer::read_pptx(fil)$slide$get_metadata()$layout_file |> basename()
  }
  layoutfil <- function(namn) {
    layouter <- officer::layout_summary(officer::read_pptx())
    basename(officer::read_pptx()$slideLayouts$get_metadata()$filename[match(namn, layouter$layout)])
  }

  ppt_lista <- initiera_ppt_df() |>
    dplyr::add_row(dia_path = bild) |>
    dplyr::add_row(dia_path = bild, titel = "Rubrik") |>
    dplyr::add_row(dia_path = bild, titel = "Rubrik", beskr = list("Text")) |>
    dplyr::add_row(titel = "Rubrik", beskr = list(c("a", "b"))) |>
    dplyr::add_row(titel = "Rubrik", mall = "Comparison") |>
    dplyr::add_row(titel = "Rubrik", mall = "roll:avsnitt")

  expect_equal(layouter_i_fil(ppt_lista),
               layoutfil(c("Blank", "Title Only", "Two Content", "Title and Content", "Comparison", "Section Header")))
  # utan rubriker och text blir även slides med rubrik och text rena bildslides
  expect_equal(layouter_i_fil(ppt_lista[1:3, ], utan_rubriker = TRUE, utan_text = TRUE), layoutfil(rep("Blank", 3)))
})

test_that("text kan läggas i namngivna platshållare", {
  mapp <- testmapp()
  ppt_lista <- initiera_ppt_df() |>
    dplyr::add_row(titel = "Två spalter", mall = "Two Content",
                   beskr = list(list("Content Placeholder 2" = "Vänster", "Content Placeholder 3" = "Höger")))

  fil <- suppressMessages(skapa_ppt_fran_lista(ppt_lista, huvudrubrik = NULL, output_mapp = mapp, mall_fil = "inbyggd"))
  expect_true(all(c("Två spalter", "Vänster", "Höger") %in% pptx_innehall(fil)$text))

  ppt_lista$beskr <- list(list("Finns inte" = "Text"))
  expect_error(skapa_ppt_fran_lista(ppt_lista, huvudrubrik = NULL, output_mapp = mapp, mall_fil = "inbyggd"),
               "ingen platshållare som heter 'Finns inte'")
})

test_that("fel i listan upptäcks innan något byggs, och alla redovisas på en gång", {
  mapp <- testmapp()
  ppt_lista <- initiera_ppt_df() |>
    dplyr::add_row(titel = "Rubrik", mall = "Felstavad layout") |>
    dplyr::add_row(dia_path = file.path(mapp, "finns_inte.png")) |>
    dplyr::add_row(mall = "befintlig:1")

  fel <- expect_error(skapa_ppt_fran_lista(ppt_lista, huvudrubrik = "Rubrik", output_mapp = mapp, mall_fil = "inbyggd"),
                      "Presentationen kan inte byggas")
  expect_match(conditionMessage(fel), "Rad 1: layouten 'Felstavad layout' finns inte i mallen")
  expect_match(conditionMessage(fel), "Rad 2: hittar inte bildfilen")
  expect_match(conditionMessage(fel), "Rad 3: 'befintlig:1' pekar på en slide som inte finns")
  expect_match(conditionMessage(fel), "Layouter i mallen: 'Title Slide'")
  expect_length(list.files(mapp, pattern = "\\.pptx$"), 0)

  expect_error(skapa_ppt_fran_lista(initiera_ppt_df(), huvudrubrik = "Rubrik", output_mapp = file.path(mapp, "finns_inte"),
                                    mall_fil = "inbyggd"),
               "Mappen presentationen ska sparas i finns inte")
})

test_that("färdiga slides i mallfilen placeras med befintlig:<nr>, och oanvända tas bort", {
  mapp <- testmapp()
  grundfil <- testfil_med_slides(mapp, c("Färdig A", "Färdig B", "Färdig C"))

  ppt_lista <- initiera_ppt_df() |>
    dplyr::add_row(titel = "Ny 1") |>
    dplyr::add_row(mall = "befintlig:3") |>
    dplyr::add_row(titel = "Ny 2") |>
    dplyr::add_row(mall = "befintlig:1")

  fil <- suppressMessages(skapa_ppt_fran_lista(ppt_lista, huvudrubrik = "Huvudrubriken", output_mapp = mapp, mall_fil = grundfil))
  expect_equal(pptx_ordning(fil), c("Huvudrubriken", "Ny 1", "Färdig C", "Ny 2", "Färdig A"))

  ppt_lista <- dplyr::add_row(ppt_lista, mall = "befintlig:3")
  expect_error(skapa_ppt_fran_lista(ppt_lista, huvudrubrik = "Huvudrubriken", output_mapp = mapp, mall_fil = grundfil),
               "används mer än en gång")
})

test_that("slides_flytta fungerar som tidigare, och färdiga slides ligger då kvar först", {
  mapp <- testmapp()
  grundfil <- testfil_med_slides(mapp, c("Färdig A", "Färdig B"))
  ppt_lista <- initiera_ppt_df() |>
    dplyr::add_row(titel = "Ny 1") |>
    dplyr::add_row(titel = "Ny 2")

  fil <- suppressMessages(skapa_ppt_fran_lista(ppt_lista, huvudrubrik = "Huvudrubriken", output_mapp = mapp, mall_fil = grundfil,
                                               slides_flytta = list(list(fran = 1, till = 5), list(fran = 1, till = 5))))
  expect_equal(pptx_ordning(fil), c("Huvudrubriken", "Ny 1", "Ny 2", "Färdig A", "Färdig B"))
})

test_that("presentatörens uppgifter som inte går att slå upp utelämnas i stället för att stoppa bygget", {
  mapp <- testmapp()
  ppt_lista <- initiera_ppt_df() |>
    dplyr::add_row(mall = "Avslutsbild_Samhallsanalys")

  expect_message(
    expect_message(
      fil <- skapa_ppt_fran_lista(ppt_lista, huvudrubrik = "Rubrik", output_mapp = mapp, mall_fil = "inbyggd",
                                  anv_uppgifter = "namn_som_inte_finns_i_listan"),
      "Kunde inte slå upp namn och e-post"),
    "2 slides sparad")
  expect_true(any(grepl("samhallsanalys@regiondalarna.se", pptx_innehall(fil)$text, fixed = TRUE)))
})
