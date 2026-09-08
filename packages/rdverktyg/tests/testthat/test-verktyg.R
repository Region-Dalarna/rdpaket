test_that("nummer_till_text hanterar 1-20", {
  expect_equal(nummer_till_text(3), "tre")
  expect_equal(nummer_till_text(c(1, 20)), c("ett", "tjugo"))
  expect_warning(x <- nummer_till_text(c(2, 99)), "1-20")
  expect_equal(x, "två")
  expect_error(nummer_till_text(99), "1-20")
})

test_that("slash_lagg_till lägger till snedstreck", {
  expect_equal(slash_lagg_till("a/b"), "a/b/")
  expect_equal(slash_lagg_till("a/b/"), "a/b/")
  expect_equal(slash_lagg_till(c("x", "y/")), c("x/", "y/"))
})

test_that("avrundning_dynamisk rundar efter storlek", {
  expect_equal(avrundning_dynamisk(0.234), 0.23)
  expect_equal(avrundning_dynamisk(4.56), 4.6)
  expect_equal(avrundning_dynamisk(1234), 1000)
  expect_true(is.na(avrundning_dynamisk(NA_real_)))
})

test_that("varden_jamnt_spridda_valj_ut behåller min, max och jämnt fördelat", {
  ut <- varden_jamnt_spridda_valj_ut(1:10, 3)
  expect_length(ut, 10)
  expect_equal(ut[c(1, 10)], c("1", "10"))
  expect_equal(sum(nzchar(ut)), 3)
})

test_that("skapa_intervaller ger rätt antal gränser", {
  expect_length(skapa_intervaller(c(5, 20, 55, 130, 400), 5), 5)
})

test_that("skapa_aldersgrupper delar in korrekt", {
  alder <- c(5, 20, 40, 70, 90)
  grp <- skapa_aldersgrupper(alder, c(19, 35, 50, 65, 80))
  expect_s3_class(grp, "factor")
  expect_equal(as.character(grp[1]), "5-18 år")
  expect_equal(as.character(grp[5]), "80+ år")
})

test_that("suppress_specific_warning dämpar rätt varning", {
  expect_warning(
    suppress_specific_warning(warning("annat problem"), "NAs introduced"),
    "annat problem"
  )
  expect_no_warning(
    suppress_specific_warning(warning("NAs introduced by coercion"))
  )
})

test_that("period_jmfr_filter plockar rätt perioder (position bakåt/framåt)", {
  koll <- c("2022M03", "2023M03", "2024M03", "2025M03")
  ut <- period_jmfr_filter(koll, "2025M03", c(-1, -2), inkludera_vald_period = FALSE)
  expect_equal(sort(ut), c("2023M03", "2024M03"))

  ut2 <- period_jmfr_filter(koll, "2025M03", c(-1), inkludera_vald_period = TRUE)
  expect_equal(sort(ut2), c("2024M03", "2025M03"))
})

test_that("funktion_upprepa_forsok_om_fel returnerar vid framgång", {
  raknare <- 0
  f <- function() {
    raknare <<- raknare + 1
    if (raknare < 3) stop("inte än")
    "klart"
  }
  expect_equal(
    funktion_upprepa_forsok_om_fel(f, max_forsok = 5, vanta_sekunder = 0),
    "klart"
  )
})

test_that("funktion_upprepa_forsok_om_fel returnerar sentinel vid fel", {
  expect_null(
    funktion_upprepa_forsok_om_fel(function() stop("nej"), max_forsok = 2, vanta_sekunder = 0)
  )
})

test_that("skriptrader_upprepa_om_fel kör om vid matchande fel", {
  e <- new.env()
  e$n <- 0
  ut <- skriptrader_upprepa_om_fel(
    {
      e$n <- e$n + 1
      if (e$n < 2) stop("timeout occurred")
      resultat <- e$n * 10
    },
    vila_sek = 0,
    exportera_till_globalenv = FALSE
  )
  expect_equal(ut$resultat, 20)
  expect_equal(e$n, 2)
})

test_that("skriptrader_upprepa_om_fel kastar fel som inte matchar", {
  expect_error(
    skriptrader_upprepa_om_fel(stop("helt annat fel"), max_forsok = 2, vila_sek = 0),
    "helt annat fel"
  )
})
