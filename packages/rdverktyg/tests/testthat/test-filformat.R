test_that("intern_separator_gissa gissar rätt", {
  f <- withr::local_tempfile()
  writeLines("a;b;c", f)
  expect_equal(rdverktyg:::intern_separator_gissa(f), ";")
  writeLines("a,b,c", f)
  expect_equal(rdverktyg:::intern_separator_gissa(f), ",")
})

test_that("las_b64 avkodar", {
  f <- withr::local_tempfile()
  writeLines(jsonlite::base64_enc(charToRaw("hemligt värde")), f)
  expect_equal(las_b64(f), "hemligt värde")
  expect_error(las_b64(tempfile()), "finns inte")
})

test_that("csv_fran_zipfiler_inlasning läser csv ur zip", {
  skip_if_not_installed("zip")
  d <- withr::local_tempdir()
  utils::write.csv(data.frame(x = 1:2, y = c("a", "b")), file.path(d, "en.csv"), row.names = FALSE)
  utils::write.csv(data.frame(x = 3, y = "c"), file.path(d, "tva.csv"), row.names = FALSE)
  z <- file.path(d, "data.zip")
  zip::zip(z, files = c("en.csv", "tva.csv"), root = d, mode = "cherry-pick")

  ut <- csv_fran_zipfiler_inlasning(z)
  expect_type(ut, "list")
  expect_length(ut, 2)
  expect_equal(names(ut), c("data.zip/en.csv", "data.zip/tva.csv"))
  expect_equal(nrow(ut[["data.zip/en.csv"]]), 2)
  expect_true(all(c("x", "y") %in% names(ut[["data.zip/en.csv"]])))

  ut_bunden <- csv_fran_zipfiler_inlasning(z, bind_ihop_dataseten = TRUE)
  expect_equal(nrow(ut_bunden), 3)
  expect_true(all(c("x", "y") %in% names(ut_bunden)))

  ut_bunden2 <- csv_fran_zipfiler_inlasning(z, kalla_som_kolumn = TRUE, bind_ihop_dataseten = TRUE)
  expect_true(all(c("zip_fil", "csv_fil") %in% names(ut_bunden2)))
})

test_that("skolverket_hitta_startrad hittar första blocket", {
  df <- data.frame(v = c("", "x", "", "a", "b", "c", "d", "e"))
  expect_equal(skolverket_hitta_startrad(df, "v", min_langd = 4), 4)
})

test_that("skolverket_generera_kolumnnamn slår ihop rubriknivåer", {
  df <- data.frame(
    A = c("Grupp1", "Grupp1", "kol1"),
    B = c("Grupp1", "Grupp1", "kol2"),
    C = c("unik", "unik", "unik")
  )
  namn <- skolverket_generera_kolumnnamn(df, namnrad = 3)
  expect_equal(namn[3], "unik")
  expect_true(grepl("kol1", namn[1]))
})
