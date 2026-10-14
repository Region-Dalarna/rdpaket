test_that("geografisk filtrering behåller sammanhängande GTFS-tabeller", {
  skip_if_not_installed("sf")
  skip_if_not_installed("gtfstools")

  gtfs <- list(
    routes = data.frame(route_id = c("r1", "r2"), route_type = c(3L, 3L),
                        agency_id = c("a1", "a2")),
    trips = data.frame(trip_id = c("t1", "t2"), route_id = c("r1", "r2"),
                       service_id = c("s1", "s2"), shape_id = c("sh1", "sh2")),
    stops = data.frame(stop_id = c("inside", "outside"), stop_lon = c(15, 18),
                       stop_lat = c(60, 63)),
    stop_times = data.frame(trip_id = c("t1", "t2"), stop_id = c("inside", "outside")),
    agency = data.frame(agency_id = c("a1", "a2"), agency_name = c("Local", "Full operator")),
    calendar = data.frame(service_id = c("s1", "s2")),
    calendar_dates = data.frame(service_id = c("s1", "s2")),
    shapes = data.frame(shape_id = c("sh1", "sh2")),
    transfers = data.frame(from_stop_id = c("inside", "inside"), to_stop_id = c("inside", "outside")),
    attributions = data.frame(trip_id = c("t1", "t2", NA_character_))
  )
  class(gtfs) <- "dt_gtfs"
  polygon <- sf::st_sfc(sf::st_polygon(list(rbind(
    c(14, 59), c(16, 59), c(16, 61), c(14, 61), c(14, 59)
  ))), crs = 4326)

  resultat <- rdgtfs:::gtfs_filtrera_geografiskt(gtfs, polygon)
  expect_equal(resultat$routes$route_id, "r1")
  expect_equal(resultat$trips$trip_id, "t1")
  expect_equal(resultat$stops$stop_id, "inside")
  expect_equal(nrow(resultat$calendar), 1L)
  expect_equal(nrow(resultat$calendar_dates), 1L)
  expect_equal(resultat$shapes$shape_id, "sh1")
  expect_equal(nrow(resultat$transfers), 1L)
  expect_equal(resultat$attributions$trip_id, c("t1", NA_character_))
})

test_that("geografisk filtrering kan behålla fullständiga operatörer", {
  skip_if_not_installed("sf")
  skip_if_not_installed("gtfstools")

  gtfs <- list(
    routes = data.frame(route_id = c("r1", "r2"), route_type = c(3L, 3L),
                        agency_id = c("a1", "a2")),
    trips = data.frame(trip_id = c("t1", "t2"), route_id = c("r1", "r2"),
                       service_id = c("s1", "s2"), shape_id = c("sh1", "sh2")),
    stops = data.frame(stop_id = c("inside", "outside"), stop_lon = c(15, 18),
                       stop_lat = c(60, 63)),
    stop_times = data.frame(trip_id = c("t1", "t2"), stop_id = c("inside", "outside")),
    agency = data.frame(agency_id = c("a1", "a2"), agency_name = c("Local", "Chosen Transit")),
    calendar_dates = data.frame(service_id = c("s1", "s2"))
  )
  class(gtfs) <- "dt_gtfs"
  polygon <- sf::st_sfc(sf::st_polygon(list(rbind(
    c(14, 59), c(16, 59), c(16, 61), c(14, 61), c(14, 59)
  ))), crs = 4326)

  resultat <- rdgtfs:::gtfs_filtrera_geografiskt(
    gtfs, polygon, fullstandiga_operatorer = "chosen transit"
  )
  expect_equal(resultat$routes$route_id, c("r1", "r2"))
  expect_equal(resultat$trips$trip_id, c("t1", "t2"))
  expect_equal(resultat$stops$stop_id, c("inside", "outside"))
})

test_that("mapview wrapper reports missing shapes", {
  skip_if_not_installed("mapview")
  expect_error(rdgtfs:::mapview_gtfs(list(shapes = NULL)), "ingen shapes-information")
})
