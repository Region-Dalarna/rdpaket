test_that("database schema helpers derive historical names and route labels", {
  expect_equal(rdgtfs:::gtfs_historiskt_schema("gtfs"), "gtfs_historisk")
  expect_match(rdgtfs:::gtfs_fordonstyp_sql(), "Förbeställningstrafik")
  expect_match(rdgtfs:::gtfs_fordonstyp_sql(), "BETWEEN 700 AND 799")
})

test_that("view parameters accept an explicit suffix without an agency filter", {
  parametrar <- rdgtfs:::gtfs_vy_parametrar(
    con = NULL,
    schema = "gtfs",
    agency_id = NULL,
    agency_namn = NULL,
    vy_suffix = "_regional"
  )
  expect_equal(parametrar$vy_suffix, "_regional")
  expect_equal(parametrar$agency_filter, "")
})
