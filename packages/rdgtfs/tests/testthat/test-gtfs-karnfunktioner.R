test_that("GTFS datum och numeriska fält konverteras", {
  datum <- rdgtfs:::gtfs_text_till_datum("20240115")
  expect_equal(datum, as.Date("2024-01-15"))
  expect_equal(rdgtfs:::gtfs_datum_till_text(datum), "20240115")

  df <- data.frame(antal = c("2", "x"), lat = c("60.5", "61.5"))
  resultat <- rdgtfs:::gtfs_konvertera(df, heltal = "antal", decimal = "lat")
  expect_identical(resultat$antal, c(2L, NA_integer_))
  expect_equal(resultat$lat, c(60.5, 61.5))
})

test_that("GTFS kalenderdagar baseras på tillagd trafik", {
  calendar_dates <- data.frame(
    service_id = c("vardag", "vardag", "vardag"),
    date = c("20240101", "20240102", "20240103"),
    exception_type = c(1L, 2L, 1L)
  )

  resultat <- rdgtfs:::gtfs_fyll_calendar_dagar(calendar_dates)
  expect_equal(resultat$monday, 1L)
  expect_equal(resultat$tuesday, 0L)
  expect_equal(resultat$wednesday, 1L)
  expect_equal(resultat$start_date, as.Date("2024-01-01"))
  expect_equal(resultat$end_date, as.Date("2024-01-03"))
})

test_that("calendar expanderas till datum med aktiva veckodagar", {
  calendar <- data.frame(
    service_id = "vardag",
    monday = 1L, tuesday = 0L, wednesday = 1L, thursday = 0L,
    friday = 0L, saturday = 0L, sunday = 0L,
    start_date = "20240101", end_date = "20240107"
  )

  resultat <- rdgtfs:::gtfs_fyll_calendar_dates_fran_calendar(calendar)
  expect_equal(as.character(resultat$date), c("2024-01-01", "2024-01-03"))
  expect_equal(resultat$exception_type, c(1L, 1L))
})
