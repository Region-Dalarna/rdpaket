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

  # Hela PowerShell-anropet + stadningen lindas aven i en tryCatch har - ett
  # sista, heltackande sakerhetsnat ovanpa stringr-omlaggningen (se NEWS och
  # kommentarerna langre ner). "input string N is invalid UTF-8" har dykt
  # upp pa flera olika stallen over flera omskrivningar (se NEWS for
  # 0.0.0.9009/9011/9012/9013) - funktionen ska ALDRIG krascha for
  # anroparen, bara ge NA_character_ i varsta fall, med en varning om vad
  # som hande, sa framtida overraskningar syns men stoppar aldrig kodflodet.
  namn <- tryCatch(
    intern_fornamn_efternamn_powershell(anv = Sys.getenv("USERNAME"),
                                        dom = Sys.getenv("USERDOMAIN")),
    error = function(e) {
      cli::cli_warn(c(
        "anv_fornamn_efternamn_hamta(): ov\u00e4ntat fel, ger NA_character_.",
        "i" = conditionMessage(e)
      ))
      NA_character_
    }
  )
  if (!is.character(namn) || length(namn) != 1) return(NA_character_)
  namn
}

intern_fornamn_efternamn_powershell <- function(anv, dom) {
  # Anvandarnamn och doman tas fran R (inte $env: i PowerShell-strangen), sa
  # att kommandot slipper nasta citattecken. Forsta forsoket filtrerar pa
  # bade Name OCH Domain (snabbare pa domananslutna datorer - annars kan
  # Win32_UserAccount utan Domain-filter racka over hela AD-domanen).
  # Get-CimInstance kan av skal som varierar per Windows-miljo (behorigheter,
  # CIM-tjanst, VPN till domankontrollant m.m.) kasta ett fel som annars
  # dodar hela -Command-anropet (sag pa en riktig dator: status 1, ingen
  # output alls) - se try/catch nedan. Misslyckas forsta forsoket (eller ger
  # tomt), provas ett andra utan Domain-filter - mer universellt
  # tillforlitligt, om an nagot langsammare.
  #
  # FullName skickas tillbaka som en JSON-strang (ConvertTo-Json), inte
  # Base64 via [Convert]::ToBase64String()/[System.Text.Encoding] - sadana
  # direkta anrop pa .NET-typer fungerar INTE pa datorer som kor PowerShell
  # i Constrained Language Mode (vanligt pa hart styrda foretags-/region-
  # datorer, t.ex. via WDAC/AppLocker). Sag pa en riktig sadan dator:
  # "Method invocation is supported only on core types in this language
  # mode". Get-CimInstance/Select-Object ar cmdlets och fungerar fint aven
  # i Constrained Language Mode - ConvertTo-Json ar ocksa en cmdlet (ingen
  # [Typ]::Metod()-syntax), sa den fungerar dar med. Den rakar ocksa losa
  # det ursprungliga kodningsproblemet pa kopet: ConvertTo-Json escapar
  # icke-ASCII-tecken som \uXXXX som standard, sa utdatan ar ren ASCII och
  # kan inte forvanskas av nagon kodsida nar R:s system2(stdout = TRUE)
  # laser PowerShells pipe (den gamla, empiriskt felaktiga byte-till-
  # tecken-tabellen fran den allra forsta varianten av funktionen behovs
  # inte langre, oavsett).
  # try/catch runt HELA CIM-delen (inte bara -ErrorAction SilentlyContinue
  # per cmdlet) sa ett fel nagonstans aldrig lamnar skriptet som helhet med
  # status != 0 - $n blir bara $null om nagot i try-blocket misslyckas,
  # oavsett orsak, och ConvertTo-Json av ett tomt namn gar alltid bra.
  ps <- sprintf(paste(
    "try {",
    "  $f1 = \"Name='%s' AND Domain='%s'\";",
    "  $f2 = \"Name='%s'\";",
    "  $u = Get-CimInstance Win32_UserAccount -Filter $f1 -ErrorAction Stop;",
    "  if (-not $u) { $u = Get-CimInstance Win32_UserAccount -Filter $f2 -ErrorAction Stop };",
    "  $n = $u | Select-Object -First 1 -ExpandProperty FullName;",
    "} catch { $n = $null };",
    "[string]$n | ConvertTo-Json -Compress"
  ), anv, dom, anv)

  res <- tryCatch(
    system2(
      "powershell",
      c("-NoProfile", "-NonInteractive", "-Command", shQuote(ps, type = "cmd")),
      stdout = TRUE, stderr = TRUE
    ),
    error = function(e) character(0)
  )
  status <- attr(res, "status")
  if (!is.null(status) && status != 0) {
    cli::cli_warn(c(
      "anv_fornamn_efternamn_hamta(): PowerShell-anropet misslyckades (status {status}).",
      "i" = paste(res, collapse = " ")
    ))
    return(NA_character_)
  }

  namn_raw <- res[nzchar(stringr::str_trim(res))]
  if (length(namn_raw) == 0) return(NA_character_)

  intern_namn_fran_json(namn_raw[1])
}

# Avkodar och stadar FullName-strangen fran anv_fornamn_efternamn_hamta()s
# JSON-kodade PowerShell-utdata (en enkel JSON-strang, t.ex. "\"Örjan\"").
# Utbruten sa den gar att testa pa alla OS (sjalva PowerShell-anropet
# kraver Windows, den har logiken inte).
intern_namn_fran_json <- function(namn_json) {
  namn <- tryCatch(jsonlite::fromJSON(stringr::str_trim(namn_json)),
                    error = function(e) NA_character_)
  if (!is.character(namn) || length(namn) != 1 || is.na(namn) || !nzchar(namn)) {
    return(NA_character_)
  }

  # AVGORANDE: stadningen anvander stringr (stringi/ICU), INTE base R:s
  # sub()/trimws(). Det ar den verifierade, faktiska skillnaden mot den
  # ursprungliga func_API.R-versionen (som alltid fungerat pa riktiga
  # Windows-datorer) - den anvander ocksa stringr (str_remove/str_squish/
  # str_replace), aldrig base R. Flera omskrivningar hit forsokte losa
  # "input string N is invalid UTF-8" genom att byta REGEXMOTOR (perl=TRUE)
  # eller genom att forst SAKERSTALLA giltig UTF-8 (ConvertTo-Json,
  # validUTF8()) - men roten visade sig vara nagot annat: nagonstans i
  # kedjan pa just den har hart styrda Windows-datorn (se kommentaren om
  # Constrained Language Mode ovan - kanske en inloggningsbanner eller
  # annan text som Group Policy injicerar i PowerShell-sessionen) kan
  # sjalva BYTE-INNEHALLET bli genuint trasigt - inte bara "ratt UTF-8,
  # fel lokal". Verifierat har: matade medvetet TRASIGA UTF-8-bytes (en
  # ofullstandig multi-byte-sekvens, inte bara ratt UTF-8 under fel lokal)
  # in i bada varianterna - base R:s trimws()/sub(perl=TRUE) KRASCHAR
  # DIREKT ("input string 1 is invalid UTF-8", exakt felet som
  # rapporterats upprepade ganger); stringr::str_trim()/str_remove()
  # kraschar ALDRIG pa samma indata - de hanterar bara de trasiga bytena.
  # Varsta fallet blir dar ett namn med nagot enstaka fel tecken i stallet
  # for en krasch - precis sa func-filen alltid betett sig. \p{L} (vilken
  # Unicode-bokstav som helst) ersatter ocksa den hardkodade a/a/o-listan -
  # fungerar for alla sprak, inte bara svenska.
  namn |>
    stringr::str_remove("[^\\p{L} ].*") |>
    stringr::str_remove(" b ") |>
    stringr::str_squish() |>
    stringr::str_replace("^(\\S+)\\s+(.*)$", "\\2 \\1")
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
