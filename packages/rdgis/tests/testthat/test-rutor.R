test_that("rutstorlek_estimera känner igen rutstorlekar", {
  expect_equal(rutstorlek_estimera(c(674100, 674200), c(6720300, 6720400)), 100)
  expect_equal(rutstorlek_estimera(c(674500, 675000), c(6720000, 6720500)), 500)
  expect_equal(rutstorlek_estimera(c(674000, 675000), c(6720000, 6721000)), 1000)
})

test_that("berakna_mittpunkter lägger till mittpunktskolumner efter rutkolumnerna", {
  df <- data.frame(rutid = "x", xruta = 674000, yruta = 6720000)
  res <- berakna_mittpunkter(df, "xruta", "yruta", 1000)
  expect_equal(res$mitt_x, 674500)
  expect_equal(res$mitt_y, 6720500)
  expect_equal(names(res), c("rutid", "xruta", "yruta", "mitt_x", "mitt_y"))
})

test_that("hamta_karttabell ger en tabell med sökord", {
  tab <- hamta_karttabell()
  expect_s3_class(tab, "data.frame")
  expect_true("kommun_scb" %in% tab$namn)
  expect_true(is.list(tab$sokord))
  expect_true("kommun" %in% tab$sokord[[which(tab$namn == "kommun_scb")]])
})

test_that("adresser_inv_reg_folke_bearbeta städar adresser", {
  df <- data.frame(adress = c("Storgatan 5A lgh 1201", "Kyrkvägen 12"))
  res <- adresser_inv_reg_folke_bearbeta(df)
  expect_equal(res$adress_join[1], "storgatan 5 a")
  expect_equal(res$adress_join[2], "kyrkvägen 12")
})
