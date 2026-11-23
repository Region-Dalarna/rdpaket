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

test_that("_dependencies.R listar rdshinyappar och shiny.telemetry bara med telemetri", {
  tmp <- withr::local_tempdir()
  for (t in c(TRUE, FALSE)) {
    d <- file.path(tmp, paste0("app_", t)); dir.create(file.path(d, "www"), recursive = TRUE)
    intern_scaffold_appfiler(d, shinyapp_config("app", grundsokvag = tmp, telemetri = t))
    dep <- readLines(file.path(d, "_dependencies.R"))
    expect_equal(any(grepl("library(rdshinyappar)", dep, fixed = TRUE)), t)
    expect_equal(any(grepl("library(shiny.telemetry)", dep, fixed = TRUE)), t)
  }
})

test_that("intern_pat_for_telemetri avbryter med tydligt fel om token saknas", {
  testthat::local_mocked_bindings(rddeploy_pat = function(...) stop("ingen token"))
  expect_error(intern_pat_for_telemetri(), "GitHub-token")
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

test_that("intern_byggverktyg_problem: mellanslag är fel, fel Rtools-version är varning", {
  # inte Windows: ingenting att kontrollera
  expect_length(intern_byggverktyg_problem("C:/Program Files/rtools45/usr/bin/make.exe", "4.6", windows = FALSE)$fel, 0)

  ok <- intern_byggverktyg_problem("C:/rtools46/usr/bin/make.exe", "4.6", windows = TRUE)
  expect_length(ok$fel, 0)
  expect_length(ok$varning, 0)

  mellanslag <- intern_byggverktyg_problem("C:/Program Files/rtools45/usr/bin/make.exe", "4.5", windows = TRUE)
  expect_match(mellanslag$fel, "mellanslag")
  expect_length(mellanslag$varning, 0)

  fel_version <- intern_byggverktyg_problem("C:/rtools45/usr/bin/make.exe", "4.6", windows = TRUE)
  expect_length(fel_version$fel, 0)
  expect_match(fel_version$varning, "rtools45.*rtools46")

  saknas <- intern_byggverktyg_problem("", "4.6", windows = TRUE)
  expect_length(saknas$fel, 0)
  expect_match(saknas$varning, "make.exe")
})
