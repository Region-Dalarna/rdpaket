test_that("intern_foraldrade_keyring_services returnerar en teckenvektor", {
  # keyring kan skriva en engångsvarning om backend-val i miljöer utan OS-
  # nyckelring (t.ex. huvudlösa CI-sandlådor) - ointressant för det vi testar.
  res <- suppressWarnings(intern_foraldrade_keyring_services())
  expect_type(res, "character")
  expect_true(all(res %in% c("github", "git2r")))
})
