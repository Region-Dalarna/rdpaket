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
