test_that("intern_validera_target godtar bara publik/intern", {
  expect_equal(intern_validera_target("publik"), "publik")
  expect_equal(intern_validera_target("intern"), "intern")
  expect_error(intern_validera_target("annat"), "Ogiltigt")
})

test_that("intern_validera_namn kräver giltiga app-namn", {
  expect_true(intern_validera_namn(c("brott", "export_2024")))
  expect_error(intern_validera_namn(character(0)), "icke-tom")
  expect_error(intern_validera_namn("app namn"), "Ogiltiga")
  expect_error(intern_validera_namn("app/../etc"), "Ogiltiga")
})

test_that("intern_validera_filnamn stoppar traversal", {
  expect_true(intern_validera_filnamn("data.csv"))
  expect_error(intern_validera_filnamn("../hemligt"), "Ogiltigt")
  expect_error(intern_validera_filnamn(".dold"), "Ogiltigt")
})

test_that("landningssida_ikoner_lista_tillgangliga returnerar kurerad lista", {
  df <- suppressMessages(landningssida_ikoner_lista_tillgangliga())
  expect_s3_class(df, "data.frame")
  expect_true(all(startsWith(df$ikon, "ti-")))
  expect_true("ti-map" %in% df$ikon)
})

test_that("intern_validera_landningssidenamn godkänner app-namn och pdf-rapporter, men inget annat", {
  expect_true(intern_validera_landningssidenamn(c("brott", "export_2024")))
  expect_true(intern_validera_landningssidenamn(c("pdf-rapporter/Befolkning_2025.pdf",
                                                  "pdf-rapporter/Årsrapport arbetsmarknad (2025).PDF")))
  expect_error(intern_validera_landningssidenamn(character(0)), "icke-tom")
  expect_error(intern_validera_landningssidenamn("app namn"), "Ogiltiga")
  expect_error(intern_validera_landningssidenamn("app/../etc"), "Ogiltiga")
  expect_error(intern_validera_landningssidenamn("pdf-rapporter/../hemlig.pdf"), "Ogiltiga")
  expect_error(intern_validera_landningssidenamn("pdf-rapporter/mapp/fil.pdf"), "Ogiltiga")
  expect_error(intern_validera_landningssidenamn("annan-mapp/fil.pdf"), "Ogiltiga")
  expect_error(intern_validera_landningssidenamn("pdf-rapporter/fil.txt"), "Ogiltiga")
  # nedladdningsfunktionerna ska fortsatt vara strikta
  expect_error(intern_validera_namn("pdf-rapporter/fil.pdf"), "Ogiltiga")
})
