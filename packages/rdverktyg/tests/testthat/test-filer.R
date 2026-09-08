test_that("sparafil_unik numrerar upp när filer finns", {
  d <- withr::local_tempdir()
  f <- file.path(d, "rapport.csv")

  expect_equal(sparafil_unik(f), f)               # finns inte -> oförändrad

  file.create(f)
  expect_equal(sparafil_unik(f), file.path(d, "rapport_2.csv"))

  file.create(file.path(d, "rapport_2.csv"))
  expect_equal(sparafil_unik(f), file.path(d, "rapport_3.csv"))

  expect_equal(sparafil_unik(f, skrivover = TRUE), f)
})

test_that("skapa_mapp_om_den_inte_finns skapar rekursivt", {
  d <- withr::local_tempdir()
  mal <- file.path(d, "a", "b", "c")
  expect_false(dir.exists(mal))
  expect_equal(skapa_mapp_om_den_inte_finns(mal), mal)
  expect_true(dir.exists(mal))
  expect_equal(skapa_mapp_om_den_inte_finns(mal), mal)   # idempotent
})

test_that("sparafil_en_backup_nvdb döper om befintlig fil", {
  d <- withr::local_tempdir()
  f <- file.path(d, "vagar.gpkg")
  writeLines("data", f)

  expect_equal(sparafil_en_backup_nvdb(f), f)
  expect_false(file.exists(f))
  expect_true(file.exists(file.path(d, "vagar_backup.gpkg")))
})

test_that("sparafil_backup_omfinns tar numrerad backup", {
  d <- withr::local_tempdir()
  f <- file.path(d, "data.csv")
  writeLines("x", f)

  sparafil_backup_omfinns(f)
  expect_false(file.exists(f))
  expect_true(file.exists(file.path(d, "data_old_2.csv")))
})

test_that("nextweekday hittar nästa veckodag", {
  # 2026-09-08 är en tisdag (wday 3)
  expect_equal(nextweekday("2026-09-08", 2), as.Date("2026-09-14")) # nästa måndag
  expect_equal(nextweekday("2026-09-08", 4), as.Date("2026-09-09")) # onsdag
  expect_equal(nextweekday("2026-09-08", 3), as.Date("2026-09-15")) # samma dag -> +7
})
