test_that("shinyapp_config validerar och sätter defaults", {
  cfg <- shinyapp_config("minapp", github_org = "Region-Dalarna",
                         grundsokvag = "/tmp/gh")
  expect_s3_class(cfg, "rddeploy_shinyapp_config")
  expect_equal(cfg$github_repo, "minapp")
  expect_equal(cfg$target, "publik")
  expect_true(cfg$telemetri)
  expect_equal(cfg$sokvag_proj, "/tmp/gh/minapp")

  expect_error(shinyapp_config("bad namn"), "bokstäver")
  expect_error(shinyapp_config(""), "måste anges")
})

test_that("intern_scaffold_struktur skapar rätt mappar", {
  tmp <- withr::local_tempdir()
  sokvag <- file.path(tmp, "app1")

  intern_scaffold_struktur(sokvag, fork = FALSE)
  expect_true(dir.exists(file.path(sokvag, "www", "fonts")))
  expect_true(dir.exists(file.path(sokvag, "R")))
  expect_true(file.exists(file.path(sokvag, "R", ".gitkeep")))
  expect_true(dir.exists(file.path(sokvag, ".github", "workflows")))

  sokvag2 <- file.path(tmp, "app2")
  intern_scaffold_struktur(sokvag2, fork = TRUE)
  expect_false(dir.exists(file.path(sokvag2, "www")))
  expect_true(dir.exists(file.path(sokvag2, ".github", "workflows")))
})

test_that("intern_scaffold_appfiler skriver appfiler med/utan telemetri", {
  tmp <- withr::local_tempdir()
  sokvag <- file.path(tmp, "app")
  intern_scaffold_struktur(sokvag)

  cfg_ja <- shinyapp_config("app", grundsokvag = tmp, telemetri = TRUE)
  intern_scaffold_appfiler(sokvag, cfg_ja)
  ui <- paste(readLines(file.path(sokvag, "ui.R")), collapse = "\n")
  expect_match(ui, "telemetri_ui", fixed = TRUE)
  expect_match(ui, "app", fixed = TRUE)

  cfg_nej <- shinyapp_config("app", grundsokvag = tmp, telemetri = FALSE)
  intern_scaffold_appfiler(sokvag, cfg_nej)
  server <- paste(readLines(file.path(sokvag, "server.R")), collapse = "\n")
  expect_no_match(server, "telemetri_server", fixed = TRUE)
})

test_that("intern_scaffold_meta skriver target i _publicering_till_server.yml", {
  tmp <- withr::local_tempdir()
  sokvag <- file.path(tmp, "app")
  intern_scaffold_struktur(sokvag)
  cfg <- shinyapp_config("app", grundsokvag = tmp, target = "intern")
  intern_scaffold_meta(sokvag, cfg)
  expect_equal(intern_shinyapp_las_target(sokvag), "intern")
  expect_true(file.exists(file.path(sokvag, "README.md")))
})
