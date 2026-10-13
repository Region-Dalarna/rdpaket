test_that("intern_namn_fran_json avkodar, stadar och vander ordningen", {
  # Samma format som PowerShells `[string]$n | ConvertTo-Json -Compress`
  j <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE)

  # WMI/PowerShell FullName kommer som "Efternamn Fornamn"
  expect_equal(intern_namn_fran_json(j("Petersson Åsa")), "Åsa Petersson")
  expect_equal(intern_namn_fran_json(j("Andersson Örjan")), "Örjan Andersson")

  # Skrap efter namnet (t.ex. fran WMI) stads bort
  expect_equal(intern_namn_fran_json(j("Andersson Örjan#123")), "Örjan Andersson")
})

test_that("intern_namn_fran_json ger NA_character_ istallet for att krascha pa ogiltig indata", {
  # Tomt namn (motsvarar [string]$null | ConvertTo-Json -Compress -> "\"\"")
  expect_identical(intern_namn_fran_json('""'), NA_character_)
  # Inte ens giltig JSON
  expect_identical(intern_namn_fran_json("inte giltig json!!"), NA_character_)
})

test_that("intern_namn_fran_json fungerar aven under en icke-UTF-8-lokal", {
  # R pa Windows kor som standard i en native (icke-UTF-8) lokal, t.ex.
  # svensk cp1252. Base R:s sub()/trimws() tolkar en UTF-8-markerad strang
  # olika beroende pa AKTIV lokal; stringr (stringi/ICU) gor det inte -
  # detta testet verifierar det uttryckligen under LC_ALL=C.
  skip_on_os("windows")  # Sys.setlocale("LC_CTYPE", "C") beter sig annorlunda pa Windows
  gammal_lokal <- Sys.getlocale("LC_CTYPE")
  install_ok <- tryCatch({
    Sys.setlocale("LC_CTYPE", "C")
    TRUE
  }, warning = function(w) FALSE, error = function(e) FALSE)
  skip_if_not(install_ok, "C-lokalen gick inte att satta pa den har plattformen")
  withr::defer(Sys.setlocale("LC_CTYPE", gammal_lokal))

  j <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE)
  expect_equal(intern_namn_fran_json(j("Andersson Örjan")), "Örjan Andersson")
})

test_that("intern_namn_fran_json kraschar aldrig pa genuint trasig UTF-8", {
  # Den FAKTISKA roten till "input string N is invalid UTF-8", rapporterat
  # upprepade ganger fran en riktig Windows-dator trots flera tidigare
  # fixar (byte-tabell, [Console]::OutputEncoding, ConvertTo-Json,
  # perl = TRUE): nagonstans i PowerShell->R-kedjan pa den datorn kunde
  # sjalva byte-INNEHALLET bli genuint trasigt (inte bara "ratt UTF-8, fel
  # lokal", som de tidigare fixarna adresserade). Base R:s trimws()/
  # sub(perl = TRUE) kraschar hart pa sant - stringr/stringi (ICU) gor det
  # aldrig, vilket ocksa ar hur den ursprungliga, alltid fungerande
  # func_API.R-versionen (som anvander stringr) betett sig.
  trasig_utf8 <- rawToChar(as.raw(c(
    0x22, 0x41, 0x6e, 0x64, 0x65, 0x72, 0x73, 0x73, 0x6f, 0x6e,
    0x20, 0xe5, 0x72, 0x6a, 0x61, 0x6e, 0x22
  )))  # "Andersson <trasig byte>rjan" - en ofullstandig multi-byte-sekvens
  expect_no_error(resultat <- intern_namn_fran_json(trasig_utf8))
  expect_true(is.na(resultat) || is.character(resultat))
})

test_that("anv_fornamn_efternamn_hamta ger NA_character_ pa icke-Windows", {
  skip_if(.Platform$OS.type == "windows")
  expect_warning(
    expect_identical(anv_fornamn_efternamn_hamta(), NA_character_),
    "stöds bara på Windows"
  )
})
