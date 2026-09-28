skip_if_not_installed("sf")

# Geometri som hex-kodad EWKB, så som RPostgres levererar den från PostGIS
ewkb_hex <- function(geom, klass) {
  hex <- vapply(sf::st_as_binary(geom, EWKB = TRUE, hex = TRUE), identity, character(1))
  structure(hex, class = klass)
}

ruta <- function(x, y) {
  sf::st_polygon(list(rbind(c(x, y), c(x + 1000, y), c(x + 1000, y + 1000), c(x, y + 1000), c(x, y))))
}

geom <- sf::st_sfc(ruta(540000, 6700000), sf::st_multipolygon(list(ruta(560000, 6700000))), crs = 3006)

test_that("df_till_sf() tolkar pq_geometry från RPostgres", {
  df <- data.frame(id = 1:2)
  df$geom <- ewkb_hex(geom, "pq_geometry")

  res <- df_till_sf(df, geom_col = "geom")

  expect_s3_class(res, "sf")
  expect_equal(attr(res, "sf_column"), "geom")
  expect_equal(sf::st_crs(res)$epsg, 3006L)
  expect_equal(as.character(sf::st_geometry_type(res)), c("POLYGON", "MULTIPOLYGON"))
  expect_equal(sf::st_bbox(res), sf::st_bbox(geom))
})

test_that("df_till_sf() tolkar WKB och sätter valt koordinatsystem", {
  df <- data.frame(id = 1:2)
  df$geometry <- ewkb_hex(sf::st_transform(geom, 4326), "WKB")

  res <- df_till_sf(df, crs = 4326)

  expect_s3_class(res, "sf")
  expect_equal(sf::st_crs(res)$epsg, 4326L)
  expect_equal(res$id, 1:2)
})
