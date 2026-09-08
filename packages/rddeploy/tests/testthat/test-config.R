test_that("intern_opt läser getOption med prefix", {
  withr::with_options(list(rddeploy.testnyckel = "hej"), {
    expect_equal(intern_opt("testnyckel", "default"), "hej")
  })
  expect_equal(intern_opt("finns_inte", "d"), "d")
})

test_that("intern_slash lägger på avslutande snedstreck", {
  expect_equal(intern_slash("c:/gh"), "c:/gh/")
  expect_equal(intern_slash("c:/gh/"), "c:/gh/")
})

test_that("rddeploy_config_status returnerar en data.frame med förväntade rader", {
  df <- rddeploy_config_status()
  expect_s3_class(df, "data.frame")
  expect_true(all(c("namn", "varde", "finns") %in% names(df)))
  expect_true("rddeploy.gh_mapp" %in% df$namn)
})

test_that("anv_hamta_namn_epost_fran_lista slår upp känd person och kan konfigureras", {
  expect_equal(anv_hamta_namn_epost_fran_lista("peter")$namn, "Peter Möller")
  expect_null(anv_hamta_namn_epost_fran_lista("okänd_person"))

  withr::with_options(
    list(rddeploy.namn_epost_lista = list(anna = list(namn = "Anna A", epost = "anna@x.se"))),
    expect_equal(anv_hamta_namn_epost_fran_lista("anna")$epost, "anna@x.se")
  )
})
