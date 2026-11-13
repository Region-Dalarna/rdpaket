test_that("postgis_installera_i_postgres_db är alias för aktivera", {
  expect_identical(postgis_installera_i_postgres_db, postgis_aktivera_i_postgres_db)
})

test_that("PostGIS-tabelladministration återanvänder anslutningar som anroparen äger", {
  con <- structure(list(), class = "DBIConnection")

  resolved <- intern_postgis_con(con)

  expect_identical(resolved$con, con)
  expect_false(resolved$egen)
})
