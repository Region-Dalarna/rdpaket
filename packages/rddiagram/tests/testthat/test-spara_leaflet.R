test_that("skriv_leaflet_till_fil() meddelar sökvägen och returnerar den", {
  skip_if_not_installed("leaflet")
  skip_if_not_installed("htmlwidgets")
  mapp <- withr::local_tempdir()
  karta <- leaflet::leaflet() |> leaflet::addTiles()

  expect_message(
    sokvag <- skriv_leaflet_till_fil(karta, mapp, "karta_a", delat_bibliotek = TRUE),
    "Karta skapad: .*karta_a\\.html"
  )
  expect_true(file.exists(sokvag))
})

test_that("delat_bibliotek = TRUE ger ett gemensamt lib-bibliotek för flera kartor", {
  skip_if_not_installed("leaflet")
  skip_if_not_installed("htmlwidgets")
  mapp <- withr::local_tempdir()
  karta <- leaflet::leaflet() |> lagg_till_bakgrundskarta()

  suppressMessages({
    skriv_leaflet_till_fil(karta, mapp, "a", delat_bibliotek = TRUE)
    skriv_leaflet_till_fil(karta, mapp, "b", delat_bibliotek = TRUE)
  })

  expect_setequal(list.files(mapp), c("a.html", "b.html", "lib"))
  expect_true(any(grepl("maplibre-gl", list.files(file.path(mapp, "lib")))))
  expect_false(any(grepl("_files", list.files(mapp))))
})

test_that("intern_hitta_webblasare() använder CHROMOTE_CHROME när den pekar på en fil", {
  fil <- withr::local_tempfile(fileext = ".exe")
  file.create(fil)
  withr::local_envvar(CHROMOTE_CHROME = fil)
  expect_equal(intern_hitta_webblasare(), fil)
})

test_that("intern_sakerstall_webblasare() ger ett tydligt fel när ingen webbläsare finns", {
  testthat::local_mocked_bindings(intern_hitta_webblasare = function() NULL)
  expect_error(intern_sakerstall_webblasare(), "Chrome eller Microsoft Edge")
})

test_that("html-kartan får favicon och titel, och favicon = FALSE tar bort ikonen", {
  skip_if_not_installed("leaflet")
  skip_if_not_installed("htmlwidgets")
  mapp <- withr::local_tempdir()
  karta <- leaflet::leaflet() |> leaflet::addTiles()

  suppressMessages({
    skriv_leaflet_till_fil(karta, mapp, "med_ikon", delat_bibliotek = TRUE)
    skriv_leaflet_till_fil(karta, mapp, "utan_ikon", delat_bibliotek = TRUE, favicon = FALSE)
  })
  med <- paste(readLines(file.path(mapp, "med_ikon.html"), warn = FALSE), collapse = "\n")
  utan <- paste(readLines(file.path(mapp, "utan_ikon.html"), warn = FALSE), collapse = "\n")
  expect_match(med, 'rel="icon"', fixed = TRUE)
  expect_match(med, "<title>med_ikon</title>", fixed = TRUE)
  expect_false(grepl('rel="icon"', utan, fixed = TRUE))
})

test_that("favicon accepterar en bildfil och avvisar annat", {
  skip_if_not_installed("leaflet")
  skip_if_not_installed("jsonlite")
  karta <- leaflet::leaflet()
  png_fil <- withr::local_tempfile(fileext = ".png")
  writeBin(as.raw(c(0x89, 0x50, 0x4e, 0x47)), png_fil)
  m <- intern_lagg_till_favicon(karta, png_fil)
  expect_true(any(grepl("data:image/png;base64,", unlist(lapply(m$prepend, as.character)), fixed = TRUE)))
  expect_error(intern_lagg_till_favicon(karta, "finns_inte.png"), "favicon ska vara")
})

test_that("png tas direkt med webbläsaren när webshot2 misslyckas", {
  skip_if_not_installed("leaflet")
  skip_if_not_installed("webshot2")
  mapp <- withr::local_tempdir()
  falsk_webblasare <- "falsk_chrome"
  anropad <- NULL
  testthat::local_mocked_bindings(
    intern_sakerstall_webblasare = function() falsk_webblasare,
    intern_png_med_webblasare = function(webblasare, html_fil, png_fil, bredd, hojd, zoom, fordrojning) {
      anropad <<- list(webblasare = webblasare, bredd = bredd, zoom = zoom)
      writeBin(as.raw(1), png_fil)
    }
  )
  testthat::local_mocked_bindings(webshot = function(...) stop("blockerad av grupprincip"), .package = "webshot2")
  expect_message(
    skriv_leaflet_till_fil(leaflet::leaflet(), mapp, "k", karta_filformat = "png", zoom = 3),
    "webshot2 kunde inte starta"
  )
  expect_equal(anropad$zoom, 3)
  expect_true(file.exists(file.path(mapp, "k.png")))
})
