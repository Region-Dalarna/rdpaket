test_that("pgrouting_hastighet_* ger km/h", {
  expect_equal(pgrouting_hastighet_gang(), 5)
  expect_equal(pgrouting_hastighet_cykel(), 16)
  expect_equal(pgrouting_hastighet_elcykel(), 22)
})

test_that("intern_ver_stampel bygger en läsbar versionsstämpel", {
  s <- intern_ver_stampel(as.POSIXct("2026-09-09 14:30:00", tz = "UTC"))
  expect_match(s, "^9sep2026_1430$")
})

test_that("intern_meta_state är mutabelt och startar tomt", {
  st <- intern_meta_state()
  expect_false(st$lyckad)
  expect_true(is.na(st$kommentar))
  st$lyckad <- TRUE
  expect_true(st$lyckad)
})

test_that("intern_rutt_con släpper igenom en befintlig anslutning", {
  fejk <- structure(list(), class = "DBIConnection")
  res <- intern_rutt_con(fejk)
  expect_identical(res$con, fejk)
  expect_false(res$egen)
})

test_that("intern_tyst_notiser() döljer bara databasens notiser", {
  expect_silent(intern_tyst_notiser(message("NOTIS:  tabell \"la\" finns inte, hoppar över\n")))
  expect_silent(intern_tyst_notiser(message("NOTICE:  Ger rättigheter på table: grafer.data")))
  # andra meddelanden (t.ex. "Karta skapad: ...") passerar
  expect_message(intern_tyst_notiser(message("Karta skapad: x.html")), "Karta skapad")
  expect_equal(intern_tyst_notiser({ message("NOTIS:  x"); 42 }), 42)
})
