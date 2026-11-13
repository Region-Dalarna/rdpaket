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

test_that("PostGIS-kompatibilitetswrappers behåller de flyttade funktionernas argument", {
  expect_identical(formals(postgis_tabell), formals(rdgis::postgis_tabell))
  expect_identical(formals(postgis_aktivera_i_postgres_db),
                   formals(rdpostgres::postgis_aktivera_i_postgres_db))
  expect_identical(formals(postgis_sf_till_postgistabell),
                   formals(rdgis::postgis_sf_till_postgistabell))
  expect_identical(formals(postgis_databas_skriv_med_metadata),
                   formals(rdgis::postgis_databas_skriv_med_metadata))
  expect_identical(formals(postgis_kopiera_tabell_mellan_databaser),
                   formals(rdgis::postgis_kopiera_tabell_mellan_databaser))
  expect_identical(formals(postgis_skapa_omradesnyckel_tabell_vy),
                   formals(rdgis::postgis_skapa_omradesnyckel_tabell_vy))
  expect_identical(formals(postgis_omradesnyckel_exportera_till_mikrodb),
                   formals(rdgis::postgis_omradesnyckel_exportera_till_mikrodb))
  expect_identical(formals(postgis_kopiera_tabell),
                   formals(rdpostgres::postgis_kopiera_tabell))
  expect_identical(formals(postgis_flytta_tabell),
                   formals(rdpostgres::postgis_flytta_tabell))
})
