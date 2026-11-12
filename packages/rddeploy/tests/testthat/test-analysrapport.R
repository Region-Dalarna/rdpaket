nytt_rapportrepo <- function(namn = "rapport", med_hero = TRUE) {
  rot <- withr::local_tempdir(.local_envir = parent.frame())
  repo <- file.path(rot, namn)
  dir.create(file.path(repo, ".git"), recursive = TRUE)
  if (med_hero) writeLines(c("<div class=\"hero-image\">", "  <div>Titel</div>", "</div>"), file.path(repo, "hero_image.html"))
  list(rot = rot, repo = repo)
}

test_that("analysrapport_gor_publicerbar skriver workflows och deploynamn, och är idempotent", {
  r <- nytt_rapportrepo("laget_i_Dalarna")
  writeLines("<html></html>", file.path(r$repo, "laget_i_dalarna.html"))

  skrivna <- suppressMessages(analysrapport_gor_publicerbar("laget_i_Dalarna", deploynamn = "laget_i_dalarna", grundsokvag = r$rot))
  expect_true(file.exists(file.path(r$repo, ".github", "workflows", "deploy.yml")))
  expect_true(file.exists(file.path(r$repo, ".github", "workflows", "avpublicera.yml")))
  expect_equal(readLines(file.path(r$repo, ".rapport_deploy_namn")), "laget_i_dalarna")
  expect_equal(intern_analysrapport_deploynamn(r$repo, "laget_i_Dalarna"), "laget_i_dalarna")
  expect_length(skrivna, 3)

  # andra körningen skriver inget nytt
  expect_length(suppressMessages(analysrapport_gor_publicerbar("laget_i_Dalarna", grundsokvag = r$rot)), 0)

  # ändrad workflow skrivs inte över utan skriv_over
  fil <- file.path(r$repo, ".github", "workflows", "deploy.yml")
  writeLines("name: egen", fil)
  suppressMessages(analysrapport_gor_publicerbar("laget_i_Dalarna", grundsokvag = r$rot))
  expect_equal(readLines(fil), "name: egen")
  suppressMessages(analysrapport_gor_publicerbar("laget_i_Dalarna", grundsokvag = r$rot, skriv_over = TRUE))
  expect_match(paste(readLines(fil), collapse = "\n"), "Deploy rapport")
})

test_that("deploy.yml-mallen tar med underrapporter", {
  expect_match(intern_las_mall("analysrapport", "deploy.yml"), "underrapporter", fixed = TRUE)
})

test_that("analysrapport_gor_publicerbar validerar deploynamn och repo", {
  r <- nytt_rapportrepo()
  expect_error(analysrapport_gor_publicerbar("rapport", deploynamn = "med blanksteg", grundsokvag = r$rot), "bara inneh")
  expect_error(analysrapport_gor_publicerbar("finns_inte", grundsokvag = r$rot), "git-repo")
})

test_that("intern_analysrapport_deploynamn rensar blanksteg och faller tillbaka på reponamnet", {
  r <- nytt_rapportrepo()
  expect_equal(intern_analysrapport_deploynamn(r$repo, "rapport"), "rapport")
  writeLines("  befolkningsutveckling \r", file.path(r$repo, ".rapport_deploy_namn"))
  expect_equal(intern_analysrapport_deploynamn(r$repo, "rapport"), "befolkningsutveckling")
  writeLines("", file.path(r$repo, ".rapport_deploy_namn"))
  expect_equal(intern_analysrapport_deploynamn(r$repo, "rapport"), "rapport")
})

test_that("analysrapport_beskrivning_satt skriver, ersätter och tar bort meta-taggen", {
  r <- nytt_rapportrepo()
  fil <- file.path(r$repo, "hero_image.html")
  suppressMessages(analysrapport_beskrivning_satt("rapport", "Läget i Dalarna: \"ekonomi\" & arbete <nu>.", grundsokvag = r$rot))
  txt <- readLines(fil, encoding = "UTF-8")
  expect_equal(txt[1], intern_beskrivning_start)
  expect_equal(txt[2], "<meta name=\"description\" content=\"Läget i Dalarna: &quot;ekonomi&quot; &amp; arbete &lt;nu&gt;.\">")
  expect_equal(txt[3], intern_beskrivning_slut)
  expect_equal(txt[4], "<div class=\"hero-image\">")

  # ersätter i stället för att duplicera
  suppressMessages(analysrapport_beskrivning_satt("rapport", "Ny text.", grundsokvag = r$rot))
  txt <- readLines(fil, encoding = "UTF-8")
  expect_equal(sum(txt == intern_beskrivning_start), 1)
  expect_match(txt[2], "Ny text.", fixed = TRUE)

  suppressMessages(analysrapport_beskrivning_satt("rapport", NA, grundsokvag = r$rot))
  expect_equal(readLines(fil), c("<div class=\"hero-image\">", "  <div>Titel</div>", "</div>"))
})

test_that("analysrapport_beskrivning_satt kräver hero_image.html och varnar vid lång text", {
  r <- nytt_rapportrepo(med_hero = FALSE)
  expect_error(analysrapport_beskrivning_satt("rapport", "Text.", grundsokvag = r$rot), "hero_image.html")
  r2 <- nytt_rapportrepo()
  expect_warning(suppressMessages(analysrapport_beskrivning_satt("rapport", strrep("x", 250), grundsokvag = r2$rot)), "250 tecken")
})

test_that("generatorns regex hittar beskrivningen som analysrapport_beskrivning_satt skriver", {
  r <- nytt_rapportrepo()
  suppressMessages(analysrapport_beskrivning_satt("rapport", "En mening om rapporten.", grundsokvag = r$rot))
  txt <- paste(readLines(file.path(r$repo, "hero_image.html")), collapse = "\n")
  expect_match(txt, "<meta name=\"description\" content=\"En mening om rapporten.\">", fixed = TRUE)
})

test_that("intern_analysrapport_html_foraldrad jämför html med Rmd/qmd (versaler spelar ingen roll)", {
  r <- nytt_rapportrepo("laget_i_Dalarna")
  html <- file.path(r$repo, "laget_i_dalarna.html"); rmd <- file.path(r$repo, "laget_i_Dalarna.Rmd")
  writeLines("<html/>", html); writeLines("x", rmd)
  Sys.setFileTime(html, Sys.time() - 3600); Sys.setFileTime(rmd, Sys.time())
  expect_true(intern_analysrapport_html_foraldrad(r$repo, "laget_i_Dalarna"))
  # inom toleransen (som efter en git pull) räknas inte som äldre
  Sys.setFileTime(html, Sys.time() - 5)
  expect_false(intern_analysrapport_html_foraldrad(r$repo, "laget_i_Dalarna"))
  # nyare html
  Sys.setFileTime(html, Sys.time() + 10)
  expect_false(intern_analysrapport_html_foraldrad(r$repo, "laget_i_Dalarna"))
  # saknad källfil -> kan inte jämföras
  unlink(rmd)
  expect_false(intern_analysrapport_html_foraldrad(r$repo, "laget_i_Dalarna"))
})

test_that("analysrapport_publicera kräver att html_fil finns", {
  expect_error(analysrapport_publicera("rapport", html_fil = "/finns/inte.html"), "html_fil")
})

test_that("analysrapport_html_beskrivning_satt lägger in, ersätter och tar bort beskrivningen i en html", {
  f <- tempfile(fileext = ".html")
  writeLines(c("<!DOCTYPE html>", "<html>", "<head>", "<meta charset=\"utf-8\">",
               "<meta name=\"description\" content=\"Gammal\" />", "<title>Rapport åäö</title>", "</head>",
               "<body>Text med \\1 och $ tecken</body>", "</html>"), f, useBytes = TRUE)
  original <- readLines(f, encoding = "UTF-8")
  suppressMessages(analysrapport_html_beskrivning_satt(f, "Ny \\1 beskrivning med åäö & mer."))
  txt <- readLines(f, encoding = "UTF-8")
  expect_equal(sum(grepl("name=\"description\"", txt)), 1)
  expect_equal(txt[4], intern_beskrivning_start)
  expect_match(txt[5], "Ny \\1 beskrivning med åäö &amp; mer.", fixed = TRUE)
  expect_equal(txt[3], "<head>")
  expect_true("<body>Text med \\1 och $ tecken</body>" %in% txt)

  suppressMessages(analysrapport_html_beskrivning_satt(f, "Andra."))
  txt <- readLines(f, encoding = "UTF-8")
  expect_equal(sum(txt == intern_beskrivning_start), 1)
  expect_equal(sum(grepl("name=\"description\"", txt)), 1)

  suppressMessages(analysrapport_html_beskrivning_satt(f, NA))
  expect_equal(readLines(f, encoding = "UTF-8"), original[-5])

  g <- tempfile(fileext = ".html"); writeLines("ingen head", g)
  expect_error(analysrapport_html_beskrivning_satt(g, "x"), "head")
})

test_that("analysrapport_publicera kräver html_fil för beskrivning", {
  expect_error(analysrapport_publicera("rapport", beskrivning = "x"), "html_fil")
})
