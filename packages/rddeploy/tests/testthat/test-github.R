test_that("intern_filtrera_filnamn: OR, AND, NOT", {
  filer <- c("hamta_scb_befolkning.R", "hamta_scb_arbete.R",
             "diagram_befolkning.R", "test_gammalt.R", "README.md")

  # NULL -> allt
  expect_equal(intern_filtrera_filnamn(filer, NULL), filer)

  # enkel delsträng
  expect_setequal(intern_filtrera_filnamn(filer, "scb"),
                  c("hamta_scb_befolkning.R", "hamta_scb_arbete.R"))

  # vektor = OR
  expect_setequal(intern_filtrera_filnamn(filer, c("arbete", "diagram")),
                  c("hamta_scb_arbete.R", "diagram_befolkning.R"))

  # AND
  expect_equal(intern_filtrera_filnamn(filer, "scb&befolkning"),
               "hamta_scb_befolkning.R")

  # NOT
  expect_false("test_gammalt.R" %in% intern_filtrera_filnamn(filer, "!test"))

  # AND + NOT
  expect_setequal(intern_filtrera_filnamn(filer, "hamta&!arbete"),
                  "hamta_scb_befolkning.R")
})

test_that("intern_kopiera_urklipp krånglar aldrig, oavsett om urklipp finns", {
  # Ska varken ge fel eller varning oavsett om clipr/systemets urklipp finns
  # i den här miljön - bara antingen kopiera och bekräfta, eller informera
  # om att det inte gick. Testar bara att den är tyst på fel/varningar.
  expect_no_error(intern_kopiera_urklipp("test"))
  expect_no_warning(intern_kopiera_urklipp("test"))

  # till_urklipp = FALSE ska alltid hoppa över utan sidoeffekt eller utskrift
  expect_no_error(intern_kopiera_urklipp("test", till_urklipp = FALSE))
})

test_that("intern_commit_meddelande sammanfattar git-status", {
  st <- data.frame(
    file   = c("a.R", "b.R", "c.R"),
    status = c("new", "modified", "deleted"),
    stringsAsFactors = FALSE
  )
  msg <- intern_commit_meddelande(st)
  expect_match(msg, "Nya filer: a.R")
  expect_match(msg, "Ändrade filer: b.R")
  expect_match(msg, "Borttagna filer: c.R")
})

test_that("intern_ppt_rad bygger en ppt_lista_fyll_pa-rad", {
  rad <- intern_ppt_rad("https://example.com/x.R")
  expect_match(rad, "ppt_lista_fyll_pa")
  expect_match(rad, 'source_url = "https://example.com/x.R"', fixed = TRUE)
})
