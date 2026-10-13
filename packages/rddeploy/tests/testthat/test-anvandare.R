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

test_that("anv_fornamn_efternamn_hamta ger NA_character_ pa icke-Windows", {
  skip_if(.Platform$OS.type == "windows")
  expect_warning(
    expect_identical(anv_fornamn_efternamn_hamta(), NA_character_),
    "stöds bara på Windows"
  )
})
