test_that("bbox-urvalet blir SQL i SWEREF 99 TM", {
  sql <- intern_bakgrund_urval_sql("bakgrundskarta_dalarna", "vatten", c(1000.5, 2000, 3000, 4000))
  expect_equal(sql, paste0("SELECT * FROM bakgrundskarta_dalarna.vatten WHERE geom && ",
                           "ST_MakeEnvelope(1000.5, 2000, 3000, 4000, 3006)"))
})

test_that("alla lager har en SQL-sats och en CREATE TABLE-kompatibel form", {
  sql <- intern_bakgrund_sql()
  expect_setequal(names(sql), intern_bakgrund_lager())
  expect_true(all(grepl("SELECT", unlist(sql))))
  expect_true(all(grepl("topografi_10\\.", unlist(sql))))
})

test_that("okända lager stoppas innan databasen används", {
  expect_error(hamta_bakgrundskarta(lager = "finns_inte"), "Okända lager")
  expect_error(skapa_bakgrundskarta_topografi(con = "x", lager = "finns_inte"), "Okända lager")
})
