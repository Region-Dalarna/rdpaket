test_that("postgres_lista_giltiga_rattigheter ger en tabell", {
  df <- postgres_lista_giltiga_rattigheter()
  expect_s3_class(df, "data.frame")
  expect_true(all(c("CONNECT", "SELECT", "CREATE") %in% df$Rattighet))
  expect_equal(ncol(df), 2)
})

test_that("postgres_felmeddelande känner igen behörighetsfel", {
  e_perm <- simpleError("permission denied for schema foo")
  msg <- postgres_felmeddelande(e_perm, "skapa tabellen")
  expect_match(msg, "saknar sannolikt behörighet")
  expect_match(msg, "uppkoppling_adm")

  e_other <- simpleError("relation \"x\" does not exist")
  expect_match(postgres_felmeddelande(e_other, "läsa tabellen"), "Ett fel uppstod")
})

test_that("uppdaterad_till_text_datum_tid delar ISO 8601", {
  res <- uppdaterad_till_text_datum_tid("2024-04-12T06:00:00Z")
  expect_equal(res$datum, "2024-04-12")
  expect_equal(res$tid, "06:00:00")
})

test_that("fil_dataset_uppdaterades ger ISO 8601-sträng", {
  tmp <- withr::local_tempfile()
  writeLines("x", tmp)
  res <- fil_dataset_uppdaterades(tmp)
  expect_match(res, "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$")
})

test_that("intern_opt läser getOption med prefix", {
  withr::with_options(list(rdpostgres.db_host = "min.host"), {
    expect_equal(intern_db_host(), "min.host")
  })
  expect_equal(intern_db_port(), 5432)
})

test_that("intern_con släpper igenom befintlig anslutning oförändrad", {
  fejk <- structure(list(), class = "FejkCon")
  res <- intern_con(fejk)
  expect_identical(res$con, fejk)
  expect_false(res$egen)
})
