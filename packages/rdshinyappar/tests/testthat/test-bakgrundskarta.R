test_that("lagg_till_bakgrundskarta() bäddar in MapLibre och lägger till bakgrunden", {
  skip_if_not_installed("leaflet")
  karta <- lagg_till_bakgrundskarta(leaflet::leaflet())

  expect_s3_class(karta, "leaflet")
  namn <- vapply(karta$dependencies, function(d) d$name, character(1))
  expect_true(all(c("maplibre-gl", "maplibre-gl-leaflet") %in% namn))
  # filerna som beskrivs finns på riktigt i paketet
  for (d in karta$dependencies[namn %in% c("maplibre-gl", "maplibre-gl-leaflet")]) {
    expect_true(all(file.exists(file.path(d$src$file, c(d$script, d$stylesheet)))))
  }

  js <- karta$jsHooks$render[[1]]
  expect_equal(js$data$stil_url, "https://tiles.openfreemap.org/styles/positron")
  expect_true(js$data$etiketter)
  expect_match(js$data$attribution, "OpenStreetMap")
})

test_that("stil och etiketter styr vad som läggs till, och felaktiga värden stoppas", {
  skip_if_not_installed("leaflet")
  karta <- lagg_till_bakgrundskarta(leaflet::leaflet(), stil = "bright", etiketter = FALSE)
  js <- karta$jsHooks$render[[1]]
  expect_equal(js$data$stil_url, "https://tiles.openfreemap.org/styles/bright")
  expect_false(js$data$etiketter)

  expect_equal(lagg_till_bakgrundskarta(leaflet::leaflet(), stil = "https://exempel.se/stil.json")$jsHooks$render[[1]]$data$stil_url,
               "https://exempel.se/stil.json")
  expect_error(lagg_till_bakgrundskarta(leaflet::leaflet(), stil = "mörk"), "Okänd stil")
  expect_error(lagg_till_bakgrundskarta(leaflet::leaflet(), stil = "http://osaker.se/stil.json"), "Okänd stil")
  expect_error(lagg_till_bakgrundskarta("ingen karta"), "leaflet-objekt")
})

test_that("kartan med bakgrund sparas som html med filerna inbäddade", {
  skip_if_not_installed("leaflet")
  mapp <- tempfile("karta_")
  dir.create(mapp)
  withr::defer(unlink(mapp, recursive = TRUE))

  karta <- leaflet::leaflet() |> lagg_till_bakgrundskarta() |> leaflet::setView(15.63, 60.61, 8)
  htmlwidgets::saveWidget(karta, file.path(mapp, "karta.html"), selfcontained = FALSE)

  filer <- list.files(file.path(mapp, "karta_files"), recursive = TRUE)
  expect_true(all(c("maplibre-gl.js", "maplibre-gl.css", "leaflet-maplibre-gl.js") %in% basename(filer)))
  expect_false(any(grepl("unpkg|cdn", readLines(file.path(mapp, "karta.html"), warn = FALSE))))
})
