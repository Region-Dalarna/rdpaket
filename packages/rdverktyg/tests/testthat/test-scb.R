test_that("manader_bearbeta_scbtabeller bygger tidskolumner", {
  df <- data.frame(
    månad = c("2023M11", "2024M01", "2023M12"),
    varde = c(3, 1, 2)
  )
  ut <- manader_bearbeta_scbtabeller(df)

  expect_true(all(c("tid", "år", "månad", "år_månad", "månad_år") %in% names(ut)))
  expect_false("mån_år" %in% names(ut))
  expect_s3_class(ut$månad_år, "factor")

  # sorterade i kronologisk ordning
  expect_equal(levels(ut$månad_år), c("november 2023", "december 2023", "januari 2024"))
  expect_equal(as.character(ut$månad[ut$tid == "2023M11"]), "november")
})

test_that("kortmanad = TRUE ger mån_år", {
  df <- data.frame(månad = "2026M01", varde = 1)
  ut <- manader_bearbeta_scbtabeller(df, kortmanad = TRUE)
  expect_true("mån_år" %in% names(ut))
  expect_equal(as.character(ut$mån_år), "jan 2026")
})
