dalatrafik_testdata <- function() {
  list(
    agency = data.frame(
      agency_id = c("a_dt", "a_sj"),
      agency_name = c("Dalatrafik AB", "SJ")
    ),
    routes = data.frame(route_id = c("r_dt", "r_sj"), agency_id = c("a_dt", "a_sj")),
    trips = data.frame(trip_id = c("t_dt", "t_sj"), route_id = c("r_dt", "r_sj")),
    stop_times = data.frame(
      trip_id = c("t_dt", "t_dt", "t_dt", "t_sj"),
      stop_id = c("s_dubbel", "s_dt", "s_dt_utan_traff", "s_bara_sj")
    ),
    stops = data.frame(
      stop_id = c("s_dubbel", "s_bara_sj", "s_dt", "s_dt_utan_traff"),
      hpl_id = c("h_dubbel", "h_bara_sj", "h_dt", "h_dt_utan_traff"),
      stop_name = c("Dubbel", "Bara SJ", "Dalatrafik", "Utan träff"),
      stop_lon = c(18, 16, 15.000001, 17),
      stop_lat = c(63, 61, 60.000001, 62)
    ),
    calendar_dates = data.frame(service_id = "s1", date = "20260101")
  )
}

dalatrafik_regional <- function() {
  list(stops = data.frame(
    stop_id = c("R1", "R2", "R3_forsta", "R3_andra"),
    hpl_id = c(101, 102, 103, 104),
    stop_lon = c(15.000004, 16, 18, 18),
    stop_lat = c(60.000004, 61, 63, 63)
  ))
}

mocka_regional_hamtning <- function(regional = dalatrafik_regional(), env = parent.frame()) {
  anrop <- new.env()
  testthat::local_mocked_bindings(
    hamta_gtfs_data = function(gtfs_dataset, ...) {
      anrop$gtfs_dataset <- gtfs_dataset
      regional
    },
    .package = "rdgtfs",
    .env = env
  )
  anrop
}

test_that("Dalatrafik-hållplatser får regionala ID via koordinatmatchning", {
  anrop <- mocka_regional_hamtning()

  resultat <- rdgtfs:::lagg_till_alternativa_id_dalatrafik(dalatrafik_testdata())
  stops <- resultat$stops

  expect_equal(anrop$gtfs_dataset, "dt")
  expect_equal(stops$dt_hpl_id[stops$stop_id == "s_dt"], "101")
  expect_equal(stops$dt_stop_id[stops$stop_id == "s_dt"], "R1")
  expect_type(stops$dt_hpl_id, "character")
  expect_type(stops$dt_stop_id, "character")
  expect_false("koord_sträng" %in% names(stops))
})

test_that("agency_namn matchas skiftlägesokänsligt", {
  mocka_regional_hamtning()

  resultat <- rdgtfs:::lagg_till_alternativa_id_dalatrafik(
    dalatrafik_testdata(), agency_namn = "DALATRAFIK"
  )

  expect_equal(resultat$stops$dt_stop_id[resultat$stops$stop_id == "s_dt"], "R1")
})

test_that("decimaler styr koordinatmatchningen", {
  mocka_regional_hamtning()

  resultat <- rdgtfs:::lagg_till_alternativa_id_dalatrafik(dalatrafik_testdata(), decimaler = 6)
  s_dt <- resultat$stops[resultat$stops$stop_id == "s_dt", ]

  expect_true(is.na(s_dt$dt_hpl_id))
  expect_true(is.na(s_dt$dt_stop_id))
})

test_that("hållplatser utan Dalatrafik eller utan regional träff får NA", {
  mocka_regional_hamtning()

  stops <- rdgtfs:::lagg_till_alternativa_id_dalatrafik(dalatrafik_testdata())$stops

  for (id in c("s_bara_sj", "s_dt_utan_traff")) {
    expect_identical(stops$dt_hpl_id[stops$stop_id == id], NA_character_)
    expect_identical(stops$dt_stop_id[stops$stop_id == id], NA_character_)
  }
})

test_that("dubbla regionala koordinater ger inga fler rader och första raden används", {
  mocka_regional_hamtning()

  stops <- rdgtfs:::lagg_till_alternativa_id_dalatrafik(dalatrafik_testdata())$stops

  expect_equal(nrow(stops), 4L)
  expect_equal(stops$dt_hpl_id[stops$stop_id == "s_dubbel"], "103")
  expect_equal(stops$dt_stop_id[stops$stop_id == "s_dubbel"], "R3_forsta")
})

test_that("radordning, befintliga kolumner och övriga listkomponenter bevaras", {
  mocka_regional_hamtning()
  indata <- dalatrafik_testdata()

  resultat <- rdgtfs:::lagg_till_alternativa_id_dalatrafik(indata)

  expect_identical(names(resultat), names(indata))
  ovriga <- setdiff(names(indata), "stops")
  expect_identical(resultat[ovriga], indata[ovriga])
  expect_identical(names(resultat$stops), c(names(indata$stops), "dt_hpl_id", "dt_stop_id"))
  expect_equal(as.data.frame(resultat$stops)[names(indata$stops)], indata$stops)
})
