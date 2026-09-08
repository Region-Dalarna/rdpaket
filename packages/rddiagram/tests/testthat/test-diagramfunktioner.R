test_that("nDigits räknar siffror", {
  expect_equal(nDigits(5), 1)
  expect_equal(nDigits(-1234), 4)
})

test_that("nice_breaks ger runda steg", {
  expect_equal(nice_breaks(100), 25)
  expect_equal(nice_breaks(47), 5)
  expect_true(nice_breaks(1000) %in% c(100, 250))
})

test_that("avrunda_till_multipel avrundar uppåt", {
  ut <- avrunda_till_multipel(73)
  expect_true(ut$max_varde >= 73)
  expect_named(ut, c("max_varde", "maj_by_var", "min_by_var"))
})

test_that("Berakna_varden_stodlinjer ger en fullständig lista", {
  st <- Berakna_varden_stodlinjer(0, 87)
  expect_named(st, c("min_yvar", "max_yvar", "min_by_yvar", "maj_by_yvar"))
  expect_gte(st$max_yvar, 87)
  expect_equal(st$min_yvar, 0)

  p <- Berakna_varden_stodlinjer(0, 55, procent_0_100_10intervaller = TRUE)
  expect_equal(p$max_yvar, 100)
  expect_equal(p$maj_by_yvar, 10)
})

test_that("diagramfarger returnerar hex-vektorer", {
  expect_length(diagramfarger("kon"), 2)
  expect_true(all(grepl("^#", diagramfarger("rus_sex"))))
  expect_error(diagramfarger("finns_inte"), "Okänd färgskala")
})

test_that("SkapaProcForandrTvaAr räknar förändring", {
  df <- data.frame(
    ar = c(2020, 2020, 2024, 2024),
    grp = c("a", "b", "a", "b"),
    v = c(100, 200, 150, 200)
  )
  ut <- SkapaProcForandrTvaAr(df, "ar", "grp", "v")
  expect_equal(nrow(ut), 2)
  proc_kol <- ut[[ncol(ut)]]
  expect_equal(proc_kol[ut$grp == "a"], 50)
  expect_equal(proc_kol[ut$grp == "b"], 0)
})

test_that("skalcirklar_skapa ger unika runda värden", {
  ut <- skalcirklar_skapa(10, 1000, 4)
  expect_true(length(ut) <= 4)
  expect_true(all(ut == round(ut)))
})

test_that("kontrastfarg_hitta ljusar upp mörkt och mörkar ljust", {
  ut <- kontrastfarg_hitta(c("#000000", "#ffffff"))
  expect_length(ut, 2)
  expect_true(all(grepl("^#", ut)))
})
