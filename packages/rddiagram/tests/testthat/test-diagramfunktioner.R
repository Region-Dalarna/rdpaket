test_that("nDigits räknar siffror", {
  expect_equal(nDigits(5), 1)
  expect_equal(nDigits(-1234), 4)
})

test_that("lagg_till_ckm_notering lägger bara till noteringen när har_ckm_data är TRUE", {
  expect_equal(lagg_till_ckm_notering("Källa: SCB.", FALSE), "Källa: SCB.")
  expect_equal(lagg_till_ckm_notering(NULL, FALSE), NULL)

  ut <- lagg_till_ckm_notering("Källa: SCB.", TRUE)
  expect_true(startsWith(ut, "Källa: SCB. "))
  expect_true(grepl("CKM", ut))

  # ingen tidigare bildtext - noteringen blir hela texten, ingen krasch på NULL/NA
  expect_equal(lagg_till_ckm_notering(NULL, TRUE), lagg_till_ckm_notering(NA, TRUE))
  expect_true(grepl("CKM", lagg_till_ckm_notering(NULL, TRUE)))

  # egen text går att skicka med
  expect_equal(lagg_till_ckm_notering("X.", TRUE, ckm_text = "Y."), "X. Y.")
})

test_that("nice_breaks ger steg på 1/2/5 x 10^k", {
  expect_equal(nice_breaks(100), 20)
  expect_equal(nice_breaks(47), 10)
  expect_equal(nice_breaks(1000), 200)
  expect_equal(nice_breaks(8), 2)
  expect_equal(nice_breaks(0.7), 0.1)
  expect_equal(nice_breaks(8500), 2000)
  # inga "konstiga" steg
  for (s in c(3, 8, 23, 47, 137, 999, 0.03, 0.7, 12.5)) {
    steg <- nice_breaks(s)
    ledande <- steg / 10^floor(log10(steg))
    expect_true(round(ledande, 6) %in% c(1, 2, 5), info = paste("spann", s))
  }
})

test_that("nice_breaks kraschar inte på spann 0, NA eller negativt", {
  expect_equal(nice_breaks(0), 1)
  expect_equal(nice_breaks(NA), 1)
  expect_equal(nice_breaks(-5), 1)
  expect_equal(nice_breaks(Inf), 1)
})

test_that("Berakna_varden_stodlinjer klarar en enda datapunkt (spann 0)", {
  # tidigare: 'missing value where TRUE/FALSE needed' i nice_breaks()
  st <- Berakna_varden_stodlinjer(42, 42, avrunda_fem = TRUE)
  expect_equal(st$min_yvar, 0)
  expect_gte(st$max_yvar, 42)
  expect_true(st$maj_by_yvar > 0)
  # värden tätt samlade men långt från noll
  st2 <- Berakna_varden_stodlinjer(95, 103, avrunda_fem = TRUE)
  expect_true(st2$maj_by_yvar > 0 && is.finite(st2$maj_by_yvar))
})

test_that("Berakna_varden_stodlinjer: minor-steget är begripligt (1/2/5)", {
  for (mx in c(38, 73, 137, 8500, 0.7, 12)) {
    st <- Berakna_varden_stodlinjer(0, mx, avrunda_fem = TRUE)
    ledande <- st$min_by_yvar / 10^floor(log10(st$min_by_yvar))
    expect_true(round(ledande, 6) %in% c(1, 2, 5), info = paste("max", mx))
  }
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
