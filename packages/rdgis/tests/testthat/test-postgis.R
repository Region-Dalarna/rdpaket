test_that("postgis_tabell skapar en lättviktig referens och printar den", {
  x <- postgis_tabell(schema = "rutor", tabell = "rutlager")

  expect_identical(x$con, NA)
  expect_identical(x$schema, "rutor")
  expect_identical(x$tabell, "rutlager")
  expect_s3_class(x, "postgis_tabell")
  expect_output(print(x), "<postgis_tabell>rutor.rutlager")
})

test_that("PostGIS-skrivning återanvänder anslutningar som anroparen äger", {
  con <- structure(list(), class = "DBIConnection")

  resolved <- intern_postgis_con(con)

  expect_identical(resolved$con, con)
  expect_false(resolved$egen)
})
