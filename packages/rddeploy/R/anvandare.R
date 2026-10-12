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
  # [Console]::OutputEncoding satts till UTF-8 INNAN FullName skrivs ut, sa
  # PowerShell levererar ratt bytes direkt - den gamla, empiriskt felaktiga
  # byte-till-tecken-tabellen (som inte matchade nagon riktig kodsida)
  # behovs da inte langre.
  filter <- sprintf(
    "Name=''%s'' AND Domain=''%s''",
    Sys.getenv("USERNAME"), Sys.getenv("USERDOMAIN")
  )
  ps <- sprintf(
    "[Console]::OutputEncoding = [System.Text.Encoding]::UTF8; (Get-CimInstance Win32_UserAccount -Filter '%s').FullName",
    filter
  )

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

  namn_raw <- namn_raw[1]
  Encoding(namn_raw) <- "UTF-8"

  namn_raw |>
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
