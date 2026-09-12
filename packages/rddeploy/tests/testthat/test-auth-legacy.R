test_that("intern_foraldrade_keyring_services returnerar en teckenvektor", {
  res <- intern_foraldrade_keyring_services()
  expect_type(res, "character")
  expect_true(all(res %in% c("github", "git2r")))
})

test_that("intern_foraldrade_keyring_services döljer keyrings backend-varning", {
  # keyring skriver en engångsvarning om backend-val i miljöer utan OS-
  # nyckelring (t.ex. huvudlösa CI-sandlådor) - ointressant brus, ska inte
  # synas för användaren när rddeploy anropar keyring.
  expect_no_warning(intern_foraldrade_keyring_services())
})

test_that("intern_suppress_keyring_backend_varning döljer bara den varningen", {
  expect_no_warning(
    intern_suppress_keyring_backend_varning(warning("Selecting 'env' backend. Secrets are stored in environment variables"))
  )
  expect_warning(
    intern_suppress_keyring_backend_varning(warning("en helt annan varning")),
    "en helt annan varning"
  )
})
