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

test_that("view parameters prevent SQL identifier injection", {
  expect_error(
    rdgtfs:::gtfs_vy_parametrar(NULL, "gtfs; DROP SCHEMA public", NULL, NULL, NULL),
    "giltigt SQL-schemaidentifierare"
  )
  parametrar <- rdgtfs:::gtfs_vy_parametrar(
    NULL, "gtfs", NULL, NULL, "_Regional; DROP SCHEMA public"
  )
  expect_equal(parametrar$vy_suffix, "_Regional__DROP_SCHEMA_public")
})

test_that("stops-tabellerna skapas med Dalatrafiks alternativa ID", {
  sql <- character(0)
  local_mocked_bindings(
    dbExecute = function(conn, statement, ...) {
      sql <<- c(sql, gsub("\\s+", " ", statement))
      0L
    },
    dbQuoteIdentifier = function(conn, x, ...) x,
    .package = "DBI"
  )

  rdgtfs:::skapa_tabeller(con = NULL, schema = "gtfs")

  stops <- sql[grepl("CREATE TABLE IF NOT EXISTS gtfs(_historisk)?\\.stops \\(", sql)]
  expect_length(stops, 2L)
  expect_true(all(grepl(
    "hpl_id VARCHAR, dt_hpl_id VARCHAR, dt_stop_id VARCHAR, stop_name",
    stops, fixed = TRUE
  )))
})

fanga_vy_sql <- function(skapa_vyer, env = parent.frame()) {
  sql <- character(0)
  testthat::local_mocked_bindings(
    gtfs_skapa_geomindex = function(...) invisible(0L),
    .package = "rdgtfs",
    .env = env
  )
  testthat::local_mocked_bindings(
    dbExecute = function(conn, statement, ...) {
      sql <<- c(sql, gsub("\\s+", " ", statement))
      0L
    },
    .package = "DBI",
    .env = env
  )
  skapa_vyer(con = NULL, schema = "gtfs")
  sql
}

vy_sql <- function(sql, vy) {
  sql[grepl(paste0("CREATE MATERIALIZED VIEW ", vy, " AS"), sql, fixed = TRUE)]
}

forvantade_dt_kolumner <- list(
  hallplatslage = "st.stop_id, st.hpl_id, st.dt_hpl_id, st.dt_stop_id, st.stop_name,",
  select = paste(
    "COALESCE(h.hpl_id, nva.hpl_id) AS hpl_id,",
    "COALESCE(h.dt_hpl_id, nva.dt_hpl_id) AS dt_hpl_id,",
    "COALESCE(h.dt_stop_id, nva.dt_stop_id) AS dt_stop_id,",
    "COALESCE(h.stop_name, nva.stop_name) AS stop_name,"
  ),
  group_by = paste(
    "COALESCE(h.hpl_id, nva.hpl_id),",
    "COALESCE(h.dt_hpl_id, nva.dt_hpl_id), COALESCE(h.dt_stop_id, nva.dt_stop_id),",
    "COALESCE(h.stop_name, nva.stop_name),"
  )
)

test_that("aktuella hållplatsvyer innehåller Dalatrafiks alternativa ID", {
  sql <- fanga_vy_sql(rdgtfs:::skapa_vyer_hallplats)

  lage <- vy_sql(sql, "gtfs.vy_hallplatslage_avgangar")
  hallplats <- vy_sql(sql, "gtfs.vy_hallplats_avgangar")
  expect_length(lage, 1L)
  expect_length(hallplats, 1L)
  expect_true(grepl(forvantade_dt_kolumner$hallplatslage, lage, fixed = TRUE))
  expect_true(grepl(forvantade_dt_kolumner$select, hallplats, fixed = TRUE))
  expect_true(grepl(paste("GROUP BY COALESCE(h.stop_id, nva.stop_id),", forvantade_dt_kolumner$group_by),
                    hallplats, fixed = TRUE))
})

test_that("historiska hållplatsvyer innehåller alternativa ID och behåller versionskoppling", {
  sql <- fanga_vy_sql(rdgtfs:::skapa_vyer_historisk_hallplats)

  lage <- vy_sql(sql, "gtfs_historisk.vy_historisk_hallplatslage_avgangar")
  hallplats <- vy_sql(sql, "gtfs_historisk.vy_historisk_hallplats_avgangar")
  expect_length(lage, 1L)
  expect_length(hallplats, 1L)
  expect_true(grepl(forvantade_dt_kolumner$hallplatslage, lage, fixed = TRUE))
  expect_true(grepl(forvantade_dt_kolumner$select, hallplats, fixed = TRUE))
  expect_true(grepl(paste("GROUP BY COALESCE(h.stop_id, nva.stop_id),", forvantade_dt_kolumner$group_by),
                    hallplats, fixed = TRUE))
  expect_true(grepl(
    "LEFT JOIN gtfs_historisk.stops h ON nva.parent_station = h.stop_id AND nva.version = h.version GROUP BY",
    hallplats, fixed = TRUE
  ))
})
