test_that("GTFS-hämtning avvisar flera dataset före nätverksanrop", {
  expect_error(
    rdgtfs:::hamta_gtfs_data(c("20", "21")),
    "bara ha ett värde"
  )
})

test_that("GTFS-filäsning skiljer på valfria och obligatoriska filer", {
  mapp <- tempfile("gtfs-test-")
  dir.create(mapp)
  on.exit(unlink(mapp, recursive = TRUE))

  expect_null(rdgtfs:::gtfs_las_fil(mapp, "optional.txt"))
  expect_error(
    rdgtfs:::gtfs_las_fil(mapp, "required.txt", obligatorisk = TRUE),
    "required.txt saknas"
  )
})
