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
