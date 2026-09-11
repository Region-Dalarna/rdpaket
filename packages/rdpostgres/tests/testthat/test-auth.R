test_that("rdpostgres_auth_check körs utan fel och ger en data.frame", {
  # keyring kan skriva en engångsvarning om backend-val i miljöer utan OS-
  # nyckelring (t.ex. huvudlösa CI-sandlådor) - ointressant för det vi testar.
  res <- suppressWarnings(suppressMessages(rdpostgres_auth_check()))
  expect_s3_class(res, "data.frame")
  expect_equal(names(res), c("service", "finns"))
  expect_setequal(res$service, c("databas_adm", "rd_geodata"))
})

test_that("rdpostgres_auth_check tar egna services", {
  res <- suppressWarnings(suppressMessages(rdpostgres_auth_check(services = "en_okand_service_xyz")))
  expect_equal(res$service, "en_okand_service_xyz")
})
