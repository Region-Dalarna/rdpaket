kod <- function(x) paste(utf8ToInt(enc2utf8(x)), collapse = ",")

test_that("intern_namn_fran_teckenkoder avkodar, stadar och vander ordningen", {
  # Samma format som PowerShell-skriptets $kod -join ','
  expect_equal(intern_namn_fran_teckenkoder(kod("Petersson Åsa")), "Åsa Petersson")
  expect_equal(intern_namn_fran_teckenkoder(kod("Andersson Örjan")), "Örjan Andersson")

  # Riktig FullName fran Win32_UserAccount innehaller ofta mer an bara
  # namnet, t.ex. avdelning/ort efter ett snedstreck - verifierat mot
  # faktisk utdata fran en anvandares dator: "Moller Peter /Regional
  # utvecklingsforvaltning /Falun" ska stadas till "Peter Moller".
  expect_equal(
    intern_namn_fran_teckenkoder(kod("Möller Peter /Regional utvecklingsförvaltning /Falun")),
    "Peter Möller"
  )

  # Skrap efter namnet stads bort aven i enklare fall
  expect_equal(intern_namn_fran_teckenkoder(kod("Andersson Örjan#123")), "Örjan Andersson")
})

test_that("intern_namn_fran_teckenkoder ger NA_character_ istallet for att krascha pa ogiltig indata", {
  # Tomt namn (motsvarar att PowerShell-skriptet fick ett tomt FullName)
  expect_identical(intern_namn_fran_teckenkoder(""), NA_character_)
  # Inte giltiga teckenkoder alls
  expect_identical(intern_namn_fran_teckenkoder("inte giltiga koder!!"), NA_character_)
  # Delvis trasig lista (en kod som inte ar ett heltal)
  expect_identical(intern_namn_fran_teckenkoder("77,x,108"), NA_character_)
})

test_that("intern_namn_fran_teckenkoder fungerar aven under en icke-UTF-8-lokal", {
  # R pa Windows kor som standard i en native (icke-UTF-8) lokal, t.ex.
  # svensk cp1252. Teckenkoder (rena ASCII-siffror) och intToUtf8() ar
  # oberoende av detta - verifierar det uttryckligen under LC_ALL=C.
  skip_on_os("windows")  # Sys.setlocale("LC_CTYPE", "C") beter sig annorlunda pa Windows
  gammal_lokal <- Sys.getlocale("LC_CTYPE")
  install_ok <- tryCatch({
    Sys.setlocale("LC_CTYPE", "C")
    TRUE
  }, warning = function(w) FALSE, error = function(e) FALSE)
  skip_if_not(install_ok, "C-lokalen gick inte att satta pa den har plattformen")
  withr::defer(Sys.setlocale("LC_CTYPE", gammal_lokal))

  expect_equal(intern_namn_fran_teckenkoder(kod("Andersson Örjan")), "Örjan Andersson")
})

test_that("intern_namn_fran_teckenkoder kraschar aldrig pa trasig indata", {
  # Bakgrund (se NEWS 0.0.0.9009-9015): flera tidigare transportmetoder
  # (byte-tabell, [Console]::OutputEncoding, ConvertTo-Json) visade sig
  # kunna ge trasiga/felkodade bytes pa riktiga, hart styrda Windows-
  # datorer, vilket fick base R:s trimws()/sub() att krascha med "input
  # string N is invalid UTF-8". Teckenkoder (rena ASCII-siffror) kan inte
  # bli "trasig UTF-8" pa samma satt, men funktionen ska aldrig krascha
  # oavsett vad den far in.
  for (indata in c("", "   ", "77,,108", "-1,999999999", "NA", "77 246 108")) {
    expect_no_error(resultat <- intern_namn_fran_teckenkoder(indata))
    expect_true(is.na(resultat) || is.character(resultat))
  }
})

test_that("anv_fornamn_efternamn_hamta ger NA_character_ pa icke-Windows", {
  skip_if(.Platform$OS.type == "windows")
  expect_warning(
    expect_identical(anv_fornamn_efternamn_hamta(), NA_character_),
    "stöds bara på Windows"
  )
})
