test_that("intern_namn_fran_base64 avkodar, stadar och vander ordningen", {
  b64 <- function(x) jsonlite::base64_enc(charToRaw(enc2utf8(x)))

  # WMI/PowerShell FullName kommer som "Efternamn Fornamn"
  expect_equal(intern_namn_fran_base64(b64("Petersson Åsa")), "Åsa Petersson")
  expect_equal(intern_namn_fran_base64(b64("Andersson Örjan")), "Örjan Andersson")

  # Skrap efter namnet (t.ex. fran WMI) stads bort
  expect_equal(intern_namn_fran_base64(b64("Andersson Örjan#123")), "Örjan Andersson")
})

test_that("intern_namn_fran_base64 ger NA_character_ istallet for att krascha pa ogiltig indata", {
  # Tom strang/inget konto hittat
  expect_identical(intern_namn_fran_base64(""), NA_character_)
  # Inte ens giltig Base64 - fick tidigare (fore Base64-omlaggningen) ge
  # "input string 1 is invalid UTF-8" fran trimws()/sub() pa riktiga
  # Windows-datorer dar R:s pipe-lasning avkodade PowerShells UTF-8-bytes
  # med fel kodsida. Nu ska den bara ge NA, aldrig krascha.
  expect_identical(intern_namn_fran_base64("inte giltig base64!!"), NA_character_)
})

test_that("anv_fornamn_efternamn_hamta ger NA_character_ pa icke-Windows", {
  skip_if(.Platform$OS.type == "windows")
  expect_warning(
    expect_identical(anv_fornamn_efternamn_hamta(), NA_character_),
    "stöds bara på Windows"
  )
})
