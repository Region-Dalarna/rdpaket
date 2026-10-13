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
  # Inte ens giltig JSON - fick tidigare (fore JSON-omlaggningen, med Base64)
  # ge "input string 1 is invalid UTF-8" fran trimws()/sub() pa riktiga
  # Windows-datorer dar R:s pipe-lasning avkodade PowerShells UTF-8-bytes
  # med fel kodsida. Nu ska den bara ge NA, aldrig krascha.
  expect_identical(intern_namn_fran_json("inte giltig json!!"), NA_character_)
})

test_that("intern_namn_fran_json fungerar aven under en icke-UTF-8-lokal", {
  # R pa Windows kor som standard i en native (icke-UTF-8) lokal, t.ex.
  # svensk cp1252. Standardregexmotorn (TRE) tolkar en UTF-8-markerad
  # strang olika beroende pa AKTIV lokal och kan krascha/forvanska texten
  # dar - sag just sa pa en riktig Windows-dator. perl = TRUE (PCRE)
  # tolkar alltid UTF-8-markerad text som UTF-8, oavsett lokal - detta
  # testet lasar in och verifierar det uttryckligen under LC_ALL=C.
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

test_that("anv_fornamn_efternamn_hamta ger NA_character_ pa icke-Windows", {
  skip_if(.Platform$OS.type == "windows")
  expect_warning(
    expect_identical(anv_fornamn_efternamn_hamta(), NA_character_),
    "stöds bara på Windows"
  )
})
