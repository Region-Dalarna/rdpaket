
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
