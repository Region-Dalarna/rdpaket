test_that("pgrouting_hastighet_* ger km/h", {
  expect_equal(pgrouting_hastighet_gang(), 5)
  expect_equal(pgrouting_hastighet_cykel(), 16)
  expect_equal(pgrouting_hastighet_elcykel(), 22)
})

test_that("intern_ver_stampel bygger en läsbar versionsstämpel", {
  s <- intern_ver_stampel(as.POSIXct("2026-09-09 14:30:00", tz = "UTC"))
  expect_match(s, "^9sep2026_1430$")
})

test_that("intern_meta_state är mutabelt och startar tomt", {
  st <- intern_meta_state()
  expect_false(st$lyckad)
  expect_true(is.na(st$kommentar))
  st$lyckad <- TRUE
  expect_true(st$lyckad)
})

test_that("intern_rutt_con släpper igenom en befintlig anslutning", {
  fejk <- structure(list(), class = "DBIConnection")
  res <- intern_rutt_con(fejk)
  expect_identical(res$con, fejk)
  expect_false(res$egen)
})

test_that("postgis_installera_i_postgres_db är en alias för aktivera", {
  expect_identical(postgis_installera_i_postgres_db, postgis_aktivera_i_postgres_db)
})
