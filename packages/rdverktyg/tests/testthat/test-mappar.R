test_that("sökvägsfunktionerna går att skriva över med options()", {
  withr::local_options(rdverktyg.utskriftsmapp = "/tmp/eget/")
  expect_equal(utskriftsmapp(), "/tmp/eget/")
})

test_that("standardvärden returneras utan options", {
  withr::local_options(rdverktyg.mapp_temp = NULL)
  expect_match(mapp_temp(), "temp", ignore.case = TRUE)
})

test_that("rdverktyg_mappar_status ger en tibble utan att fela", {
  st <- rdverktyg_mappar_status()
  expect_s3_class(st, "tbl_df")
  expect_named(st, c("funktion", "sokvag", "finns"))
  expect_equal(nrow(st), 5)
  expect_type(st$finns, "logical")
})
