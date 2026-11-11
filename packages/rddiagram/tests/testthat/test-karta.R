
test_that("diskreta klasser: färgen följer klassen även när lägsta klasserna saknas", {
  skip_if_not_installed("sf")
  skip_if_not_installed("forcats")
  ruta <- function(i) sf::st_polygon(list(rbind(c(i, 0), c(i + 1, 0), c(i + 1, 1), c(i, 1), c(i, 0))))
  sf_obj <- sf::st_sf(varde = c(5, 7, 9), geometry = sf::st_sfc(lapply(0:2, ruta), crs = 3006))

  # sex klasser (0-2, 2-4, ... 10-12), bara tre används; tre ankarfärger ska interpoleras
  p <- skapa_koropletkarta_ggplot(sf_obj, "varde", klassindelning = c(0, 2, 4, 6, 8, 10, 12),
                                  karta_fargvektor = c("red", "white", "blue"))
  fyllning <- ggplot2::ggplot_build(p)$data[[1]]$fill
  forvantat <- grDevices::colorRampPalette(c("red", "white", "blue"))(6)[c(3, 4, 5)]
  expect_equal(toupper(fyllning), toupper(forvantat))
})

test_that("legend_position = 'auto' väljer ett hörn utan polygoner, annars utanför kartan", {
  skip_if_not_installed("sf")
  ruta <- function(x0, y0, x1, y1) sf::st_polygon(list(rbind(c(x0, y0), c(x1, y0), c(x1, y1), c(x0, y1), c(x0, y0))))
  gor_sf <- function(...) sf::st_sf(varde = seq_along(list(...)), geometry = sf::st_sfc(..., crs = 3006))
  klasser <- c("0 – 10", "10 – 20", "20 – 30")
  valj <- function(x) intern_valj_legendhorn(x, klasser, "Titel", 12, 9, 1.2, 7, NULL)

  # polygoner längs diagonalen från uppe till vänster till nere till höger: nere till vänster är fritt
  diagonal <- gor_sf(ruta(0, 8, 2, 10), ruta(4, 4, 6, 6), ruta(8, 0, 10, 2))
  expect_equal(valj(diagonal), "bottom-left")

  # nere till vänster är upptaget: nästa hörn i ordningen (uppe till höger) väljs
  upptaget <- gor_sf(ruta(0, 0, 3, 3), ruta(0, 8, 2, 10), ruta(8, 0, 10, 2))
  expect_equal(valj(upptaget), "top-right")

  # polygoner i alla hörn: legenden hamnar utanför kartan
  alla_horn <- gor_sf(ruta(0, 0, 3, 3), ruta(7, 7, 10, 10), ruta(7, 0, 10, 3), ruta(0, 7, 3, 10))
  expect_equal(valj(alla_horn), "right")
})
