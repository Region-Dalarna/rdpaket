# Lokala prognosfiler i samma format som de i Profet-mappen på G:, skapade i en temporär mapp.
skapa_prognosmapp <- function(env = parent.frame()) {
  skip_if_not_installed("writexl")
  mapp <- tempfile("prognos_")
  dir.create(mapp)
  withr::defer(unlink(mapp, recursive = TRUE), envir = env)

  # Hallands xlsx-format, så som filerna i Profet-mappen ser ut: regionkod och region redan med, kön som
  # text. Dalarna-filen har året som text och Gotland-filen som tal, och kolumnerna i olika ordning.
  innehall <- c("total_folkmangd", "fodda", "doda", "inrikes_inflyttning", "inrikes_utflyttning", "invandring", "utvandring")
  xlsx <- function(regionkod, region, ar_som_text) {
    d <- expand.grid(alder = 0:100, kon = c("kvinnor", "män"), ar = 2026:2040, regionkod = regionkod, stringsAsFactors = FALSE)
    d$region <- unname(region[d$regionkod])
    for (kol in innehall) d[[kol]] <- 100.5
    d$fodda[d$alder > 0] <- NA
    if (ar_som_text) d$ar <- as.character(d$ar)
    d[, c("regionkod", "region", if (ar_som_text) c("ar", "kon", "alder") else c("kon", "alder", "ar"), innehall)]
  }
  writexl::write_xlsx(xlsx(c("20", "2080"), c("20" = "Dalarnas län", "2080" = "Falun"), TRUE),
                      file.path(mapp, "befprognos_dalarna_lan_kommun_ettarsklasser_kon_2026-2050.xlsx"))
  writexl::write_xlsx(xlsx("0980", c("0980" = "Gotland"), FALSE),
                      file.path(mapp, "befprognos_gotland_kommun_ettarsklasser_kon_2026-2050.xlsx"))

  # Profet-csv för ett äldre prognosår
  csv <- expand.grid(lan_kod = "20", age = 0:100, kon = 1:2, year = 2024:2040)
  csv$pop <- 90; csv$fodda <- 1; csv$doda <- 1; csv$inrikes_inflyttade <- 1; csv$inrikes_utflyttade <- 1
  csv$immigranter <- 1; csv$emigranter <- 1
  utils::write.csv(csv, file.path(mapp, "Dalarna_lan_2024.csv"), row.names = FALSE)
  mapp
}

test_that("hamta_befprognos_data() läser Hallands xlsx-filer och summerar utan data.table", {
  mapp <- skapa_prognosmapp()
  r <- suppressMessages(hamta_befprognos_data(region_vekt = c("20", "2080"), tid_vekt = 2026:2030, url_prognos_vektor = mapp))

  expect_setequal(r$regionkod, c("20", "2080"))
  expect_setequal(r$år, as.character(2026:2030))
  expect_equal(unique(r$prognos_ar), "2026")               # senaste prognosåret i mappen
  expect_equal(nrow(r), 2 * 101 * 2 * 5)                   # region x ålder x kön x år
  expect_equal(unique(r$Folkmängd), 100.5)
  expect_equal(sort(unique(r$kön)), c("kvinnor", "män"))
  expect_equal(r$ålder[r$ålder == "100+ år"][1], "100+ år")
})

test_that("hamta_befprognos_data() hittar regioner i den andra xlsx-filen också", {
  mapp <- skapa_prognosmapp()
  r <- suppressMessages(hamta_befprognos_data(region_vekt = c("20", "0980"), tid_vekt = 2026:2027, url_prognos_vektor = mapp))
  expect_setequal(r$regionkod, c("20", "0980"))
  expect_equal(unique(r$region[r$regionkod == "0980"]), "Gotland")
})

test_that("hamta_befprognos_data() hämtar ett äldre prognosår ur Profet-csv när det efterfrågas", {
  mapp <- skapa_prognosmapp()
  r <- suppressMessages(hamta_befprognos_data(region_vekt = "20", tid_vekt = 2025:2026, url_prognos_vektor = mapp, prognos_ar = "2024"))

  expect_equal(unique(r$prognos_ar), "2024")
  expect_equal(unique(r$Folkmängd), 90)
})
