test_that("service-namn valideras", {
  expect_error(shiny_get_password("bad name"), "A-Z")
  expect_error(shiny_set_password("bad/name", "x"), "A-Z")
  expect_error(shiny_delete_password("bad.name"), "A-Z")
})

test_that("lösenord kan sparas, hämtas, listas och tas bort", {
  skip_if_not_installed("withr")
  tmp_home <- withr::local_tempdir()

  withr::with_envvar(c(HOME = tmp_home), {
    expect_true(shiny_set_password("mintjanst", "hemligt123"))
    expect_equal(shiny_get_password("mintjanst"), "hemligt123")

    expect_true(shiny_set_password("annan_tjanst", "abc"))
    expect_setequal(shiny_list_passwords(), c("mintjanst", "annan_tjanst"))

    # skriv över
    expect_true(shiny_set_password("mintjanst", "nytt_varde"))
    expect_equal(shiny_get_password("mintjanst"), "nytt_varde")

    shiny_delete_password("mintjanst")
    expect_error(shiny_get_password("mintjanst"), "saknas")
    expect_equal(shiny_list_passwords(), "annan_tjanst")
  })
})

test_that("shiny_list_passwords felar om .Renviron saknas", {
  skip_if_not_installed("withr")
  tmp_home <- withr::local_tempdir()
  withr::with_envvar(c(HOME = tmp_home), {
    expect_error(shiny_list_passwords(), "finns inte")
  })
})
