test_that("database cleanup normalizes stop sequences per trip", {
  gtfs <- list(
    stops = data.frame(stop_lat = c("60.1", "60.2"), stop_lon = c("15.1", "15.2")),
    stop_times = data.frame(
      trip_id = c("trip-b", "trip-a", "trip-b", "trip-a"),
      stop_sequence = c(20L, 40L, 10L, 30L)
    )
  )

  resultat <- rdgtfs:::gtfs_rensa_for_db(gtfs)
  expect_type(resultat$stops$stop_lat, "double")
  expect_equal(resultat$stop_times$trip_id, c("trip-a", "trip-a", "trip-b", "trip-b"))
  expect_equal(resultat$stop_times$stop_sequence, c(1L, 2L, 1L, 2L))
})

test_that("r5r cleanup cascades filters and normalizes GTFS tables", {
  gtfs <- list(
    routes = data.frame(route_id = c("r1", "r2"), route_type = c(100L, 1501L),
                        agency_id = c("a1", "a2")),
    trips = data.frame(trip_id = c("t1", "t2"), route_id = c("r1", "r2"),
                       service_id = c("s1", "s2"), shape_id = c("sh1", "sh2")),
    stops = data.frame(
      stop_id = c("valid", "outside"),
      stop_lat = c(60, 20), stop_lon = c(15, 15),
      location_type = c(0L, 0L), parent_station = c(NA, NA),
      platform_code = c(NA, NA), hpl_id = c("1", "2")
    ),
    stop_times = data.frame(
      trip_id = c("t1", "t2"), stop_id = c("valid", "outside"),
      shape_dist_traveled = c(0, 1)
    ),
    calendar = data.frame(service_id = c("s1", "s2")),
    calendar_dates = data.frame(
      service_id = c("s1", "s2"), date = as.Date(c("2024-01-01", "2024-01-02"))
    ),
    shapes = data.frame(shape_id = c("sh1", "sh2"), shape_dist_traveled = c(0, 1)),
    agency = data.frame(agency_id = c("a1", "a2")),
    feed_info = data.frame(feed_start_date = NA_character_, feed_end_date = NA_character_),
    transfers = data.frame(from_stop_id = character()),
    attributions = data.frame(trip_id = character())
  )

  resultat <- rdgtfs:::gtfs_rensa_for_r5r(gtfs)
  expect_equal(resultat$routes$route_id, "r1")
  expect_equal(resultat$routes$route_type, 2L)
  expect_equal(resultat$trips$trip_id, "t1")
  expect_equal(resultat$stops$stop_id, "valid")
  expect_equal(resultat$calendar_dates$service_id, "s1")
  expect_equal(resultat$shapes$shape_id, "sh1")
  expect_equal(resultat$agency$agency_id, "a1")
  expect_equal(resultat$feed_info$feed_start_date, "20240101")
  expect_false("transfers" %in% names(resultat))
  expect_false("attributions" %in% names(resultat))
  expect_false("shape_dist_traveled" %in% names(resultat$stop_times))
})
