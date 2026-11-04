test_that("intern_beskrivning_citera valjer ratt citattecken", {
  expect_equal(intern_beskrivning_citera("Visar befolkningen i Dalarna."),
               "'Visar befolkningen i Dalarna.'")

  # apostrof -> dubbla citattecken
  expect_equal(intern_beskrivning_citera("Visar kommunens befolkning, appen's data."),
               "\"Visar kommunens befolkning, appen's data.\"")

  # apostrof + dubbla citattecken -> dubbla citattecken, inre citat typografiska
  expect_equal(intern_beskrivning_citera("Appen's \"siffror\" om befolkningen."),
               "\"Appen's ”siffror” om befolkningen.\"")

  # a/a/o funkar rakt av, oavsett citatform
  expect_equal(intern_beskrivning_citera("Visar å-ä-ö i Dalarna."),
               "'Visar å-ä-ö i Dalarna.'")
})

test_that("intern_beskrivning_citera avvisar backslash och radbrytning", {
  expect_error(intern_beskrivning_citera("Text med \\ i sig"), "backslash")
  expect_error(intern_beskrivning_citera("Text med\nradbrytning"), "radbrytning")
})

test_that("intern_beskrivning_citera varnar men stoppar inte vid lang text", {
  lang <- strrep("x", 130)
  expect_warning(resultat <- intern_beskrivning_citera(lang), "120 tecken")
  expect_equal(resultat, paste0("'", lang, "'"))
})

test_that("intern_beskrivning_rad ger placeholder for NA/tom beskrivning", {
  expect_match(intern_beskrivning_rad(NA_character_), "^# APP_BESKRIVNING")
  expect_match(intern_beskrivning_rad(""), "^# APP_BESKRIVNING")
  expect_match(intern_beskrivning_rad("   "), "^# APP_BESKRIVNING")
})

test_that("intern_beskrivning_rad ger aktiv rad for angiven beskrivning", {
  expect_equal(intern_beskrivning_rad("Visar befolkningen i Dalarna."),
               "APP_BESKRIVNING <- 'Visar befolkningen i Dalarna.'")
})

test_that("shinyapp_config beraknar app_beskrivning_rad (med och utan beskrivning, apostrof, a/a/o)", {
  cfg_ingen <- shinyapp_config("app1", grundsokvag = "/tmp/gh")
  expect_match(cfg_ingen$app_beskrivning_rad, "^# APP_BESKRIVNING")

  cfg_vanlig <- shinyapp_config("app2", grundsokvag = "/tmp/gh",
                                beskrivning = "Visar befolkningen år for år.")
  expect_equal(cfg_vanlig$app_beskrivning_rad,
               "APP_BESKRIVNING <- 'Visar befolkningen år for år.'")

  cfg_apostrof <- shinyapp_config("app3", grundsokvag = "/tmp/gh",
                                  beskrivning = "Kommunens befolkning, appen's egna data.")
  expect_match(cfg_apostrof$app_beskrivning_rad, '^APP_BESKRIVNING <- "')
})

test_that("intern_scaffold_appfiler skriver APP_BESKRIVNING-raden i genererad ui.R (test_skapa_ej_repo)", {
  tmp <- withr::local_tempdir()

  sokvag_med <- file.path(tmp, "med_beskrivning")
  intern_scaffold_struktur(sokvag_med)
  cfg_med <- shinyapp_config("med_beskrivning", grundsokvag = tmp,
                             beskrivning = "En mening om vad appen visar.")
  intern_scaffold_appfiler(sokvag_med, cfg_med)
  ui_med <- paste(readLines(file.path(sokvag_med, "ui.R")), collapse = "\n")
  expect_match(ui_med, "APP_BESKRIVNING <- 'En mening om vad appen visar.'", fixed = TRUE)
  expect_no_match(ui_med, "<<app_beskrivning_rad>>", fixed = TRUE)

  sokvag_utan <- file.path(tmp, "utan_beskrivning")
  intern_scaffold_struktur(sokvag_utan)
  cfg_utan <- shinyapp_config("utan_beskrivning", grundsokvag = tmp)
  intern_scaffold_appfiler(sokvag_utan, cfg_utan)
  ui_utan <- paste(readLines(file.path(sokvag_utan, "ui.R")), collapse = "\n")
  expect_match(ui_utan, "# APP_BESKRIVNING <-", fixed = TRUE)

  sokvag_apo <- file.path(tmp, "med_apostrof")
  intern_scaffold_struktur(sokvag_apo)
  cfg_apo <- shinyapp_config("med_apostrof", grundsokvag = tmp,
                             beskrivning = "Visar kommunens befolkning, appen's data år for år.")
  intern_scaffold_appfiler(sokvag_apo, cfg_apo)
  ui_apo <- paste(readLines(file.path(sokvag_apo, "ui.R")), collapse = "\n")
  expect_match(ui_apo, 'APP_BESKRIVNING <- "Visar kommunens befolkning, appen\'s data år for år."',
               fixed = TRUE)
})

test_that("shinyapp_beskrivning_satt infogar efter APP_TITEL och ar idempotent", {
  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "appX"))
  writeLines(c(
    "APP_TITEL <- 'Min testapp'",
    "shinyUI(fluidPage(tags$div(class = 'rd-header__title', APP_TITEL)))"
  ), file.path(tmp, "appX", "ui.R"))

  shinyapp_beskrivning_satt("appX", "En mening om testappen.", lokal_root = tmp)
  rader1 <- readLines(file.path(tmp, "appX", "ui.R"))
  expect_equal(rader1[2], "APP_BESKRIVNING <- 'En mening om testappen.'")
  expect_length(rader1, 3)

  # idempotent: samma anrop igen ska ge identiskt resultat, inte en till rad
  shinyapp_beskrivning_satt("appX", "En mening om testappen.", lokal_root = tmp)
  rader2 <- readLines(file.path(tmp, "appX", "ui.R"))
  expect_identical(rader1, rader2)

  # uppdatering till ny text ersatter pa plats, lagger inte till en rad
  shinyapp_beskrivning_satt("appX", "En annan mening nu.", lokal_root = tmp)
  rader3 <- readLines(file.path(tmp, "appX", "ui.R"))
  expect_equal(rader3[2], "APP_BESKRIVNING <- 'En annan mening nu.'")
  expect_length(rader3, 3)
})

test_that("shinyapp_beskrivning_satt stoppar med instruktion om APP_TITEL saknas", {
  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "appY"))
  writeLines("shinyUI(fluidPage(h1('Hej')))", file.path(tmp, "appY", "ui.R"))

  expect_error(
    shinyapp_beskrivning_satt("appY", "Nagot.", lokal_root = tmp),
    "APP_TITEL"
  )
})

test_that("shinyapp_beskrivning_satt kraver appnamn och beskrivning", {
  tmp <- withr::local_tempdir()
  expect_error(shinyapp_beskrivning_satt("", "Text.", lokal_root = tmp), "appnamn")
  expect_error(shinyapp_beskrivning_satt("app", NA_character_, lokal_root = tmp), "beskrivning")
})
