# Uppgifter om den inloggade användaren, för git-identitet och commit-metadata.

#' Hämta OS-användarnamnet
#'
#' @return Kontonamnet som sträng.
#' @export
anv_anvandarkonto_hamta <- function() {
  as.character(Sys.info()[["user"]])
}

#' Hämta inloggad användares för- och efternamn (Windows)
#'
#' Läser fullständigt namn ur Windows användarkonto via PowerShell och städar
#' teckenkodning och ordföljd ("Efternamn Förnamn" -> "Förnamn Efternamn").
#'
#' @return Namnet som sträng, eller `NA_character_` på icke-Windows.
#' @export
anv_fornamn_efternamn_hamta <- function() {
  if (.Platform$OS.type != "windows") {
    cli::cli_warn("anv_fornamn_efternamn_hamta() st\u00f6ds bara p\u00e5 Windows.")
    return(NA_character_)
  }

  # Anvandarnamn och doman tas fran R (inte $env: i PowerShell-strangen), sa
  # att kommandot slipper nasta citattecken. Filtrering pa bade Name OCH
  # Domain gor CIM-sokningen snabb aven pa domananslutna datorer - annars
  # kan Win32_UserAccount utan Domain-filter racka over hela AD-domanen.
  #
  # FullName skickas tillbaka Base64-kodad (inte som vanlig text). Att satta
  # [Console]::OutputEncoding = UTF8 racker INTE i praktiken - R:s
  # system2(stdout = TRUE) laser pipen via Windows egen kodsida (t.ex.
  # CP1252), inte rabytena PowerShell faktiskt skrev, sa a/a/o kan fortfarande
  # bli ogiltig UTF-8 och krascha pa ett senare sub()/trimws()-anrop (sag
  # pa en riktig Windows-dator trots [Console]::OutputEncoding). Base64 ar
  # ren ASCII och ser identisk ut i VARJE kodsida - ingen omkodning nagonstans
  # i kedjan kan forvanska den. Den gamla, empiriskt felaktiga
  # byte-till-tecken-tabellen behovs inte langre, oavsett.
  filter <- sprintf(
    "Name=''%s'' AND Domain=''%s''",
    Sys.getenv("USERNAME"), Sys.getenv("USERDOMAIN")
  )
  ps <- sprintf(paste(
    "$n = (Get-CimInstance Win32_UserAccount -Filter '%s').FullName;",
    "[Convert]::ToBase64String([System.Text.Encoding]::UTF8.GetBytes([string]$n))"
  ), filter)

  namn_raw <- tryCatch(
    system2(
      "powershell",
      c("-NoProfile", "-NonInteractive", "-Command", shQuote(ps, type = "cmd")),
      stdout = TRUE, stderr = FALSE
    ),
    error = function(e) character(0)
  )
  namn_raw <- namn_raw[nzchar(trimws(namn_raw))]
  if (length(namn_raw) == 0) return(NA_character_)

  intern_namn_fran_base64(namn_raw[1])
}

# Avkodar och stadar FullName-strangen fran anv_fornamn_efternamn_hamta()s
# Base64-kodade PowerShell-utdata. Utbruten sa den gar att testa pa alla
# OS (sjalva PowerShell-anropet kraver Windows, den har logiken inte).
intern_namn_fran_base64 <- function(namn_b64) {
  namn <- tryCatch({
    txt <- rawToChar(jsonlite::base64_dec(trimws(namn_b64)))
    Encoding(txt) <- "UTF-8"
    txt
  }, error = function(e) NA_character_)
  # jsonlite::base64_dec() ar en TOLERANT avkodare - den kastar inte fel pa
  # trasig/ogiltig Base64, den avkodar bara vad den kan. validUTF8() fangar
  # da upp resultat som INTE ar giltig UTF-8 INNAN de nar sub()/trimws()
  # langre ner, som annars kraschar pa sadan text (se testet for detta).
  if (is.na(namn) || !nzchar(namn) || !validUTF8(namn)) return(NA_character_)

  namn |>
    sub("[^a-zA-Z\u00e5\u00e4\u00f6\u00c5\u00c4\u00d6 ].*", "", x = _) |>
    sub(" b ", " ", x = _) |>
    trimws() |>
    sub("^(\\S+)\\s+(.*)$", "\\2 \\1", x = _)
}

#' Hämta inloggad användares e-postadress via GitHub
#'
#' Slår upp användarens e-postadresser via GitHub API och returnerar den som
#' matchar den konfigurerade domänen ([rddeploy-config], `epost_doman`).
#'
#' @return E-postadressen som sträng, eller `NULL` vid fel.
#' @export
anv_epostadress_hamta <- function() {
  rddeploy_pat()
  tryCatch({
    epostlista <- gh::gh("GET /user/emails")
    adresser <- vapply(epostlista, function(x) x$email %||% NA_character_, character(1))
    traff <- adresser[grepl(intern_epost_doman(), adresser, fixed = TRUE)]
    if (length(traff) == 0) NULL else traff[1]
  }, error = function(e) {
    cli::cli_warn("Kunde inte hämta e-postadress: {conditionMessage(e)}")
    NULL
  })
}

#' Slå upp namn och e-post för en känd person, annars för inloggad användare
#'
#' @param skickat_namn Förnamn på en känd person (se
#'   `options(rddeploy.namn_epost_lista = ...)`). `NULL` (standard) ger uppgifter
#'   för den inloggade användaren via [anv_fornamn_efternamn_hamta()] och
#'   [anv_epostadress_hamta()].
#'
#' @return En lista med `namn` och `epost`, eller `NULL` om `skickat_namn` inte
#'   är känt.
#' @export
anv_hamta_namn_epost_fran_lista <- function(skickat_namn = NULL) {
  if (is.null(skickat_namn)) {
    return(list(namn  = anv_fornamn_efternamn_hamta(),
                epost = anv_epostadress_hamta()))
  }
  intern_namn_epost_lista()[[tolower(skickat_namn)]]
}
