# source()-hjälpare - utbrutna ur func_API.R.
# Övergångsinfrastruktur: när alla funktionsfiler är paket behövs de inte längre.

#' Source:a en R-fil från GitHub utan risk för gammal CDN-cache
#'
#' `raw.githubusercontent.com` ligger bakom Fastly som kan servera en gammal
#' version en stund efter en push. Den här funktionen går via GitHubs
#' Contents-API med `Accept: raw`, som alltid speglar senaste commit.
#'
#' En GitHub-PAT höjer rate-limit från 60 till 5000 anrop/timme och krävs för
#' privata repon. Token läses i ordningen: argument, `GITHUB_PAT`, keyring
#' (`service = "github_token"`).
#'
#' @param url Vanlig raw-URL.
#' @param encoding Teckenkodning som skickas till `source()`.
#' @param echo Skickas till `source()`.
#' @param pat Valfri GitHub-PAT.
#'
#' @return Osynligt resultatet av `source()`.
#' @export
source_utan_cache <- function(url, encoding = "UTF-8", echo = FALSE, pat = NULL) {
  if (!requireNamespace("httr", quietly = TRUE)) stop("Paketet 'httr' krävs.", call. = FALSE)

  m <- regmatches(
    url, regexec("raw\\.githubusercontent\\.com/([^/]+)/([^/]+)/([^/]+)/(.+)$", url)
  )[[1]]
  if (length(m) != 5) return(invisible(source(url, encoding = encoding, echo = echo)))

  owner <- m[2]; repo <- m[3]; branch <- m[4]; path <- m[5]

  if (is.null(pat) || !nzchar(pat)) pat <- Sys.getenv("GITHUB_PAT", "")
  if (!nzchar(pat) && requireNamespace("keyring", quietly = TRUE)) {
    kp <- tryCatch(keyring::key_list(service = "github_token"), error = function(e) NULL)
    if (!is.null(kp) && nrow(kp) > 0) {
      pat <- tryCatch(keyring::key_get("github_token", kp$username[1]), error = function(e) "")
    }
  }

  api_url <- sprintf(
    "https://api.github.com/repos/%s/%s/contents/%s?ref=%s",
    owner, repo, utils::URLencode(path, reserved = FALSE), branch
  )
  hdrs <- c(Accept = "application/vnd.github.raw", "X-GitHub-Api-Version" = "2022-11-28")
  if (nzchar(pat)) hdrs <- c(hdrs, Authorization = paste("token", pat))

  res <- httr::GET(api_url, httr::add_headers(.headers = hdrs))

  if (httr::status_code(res) == 403) {
    body <- httr::content(res, "text", encoding = "UTF-8")
    if (grepl("rate limit", body, ignore.case = TRUE)) {
      stop("GitHub API rate-limit nådd. Sätt GITHUB_PAT för 5000 anrop/timme.", call. = FALSE)
    }
    if (grepl("too large|larger than", body, ignore.case = TRUE)) {
      message("source_utan_cache(): filen är för stor för Contents-API:t, ",
              "faller tillbaka på vanlig source().")
      return(invisible(source(url, encoding = encoding, echo = echo)))
    }
  }
  httr::stop_for_status(res)

  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)
  writeBin(httr::content(res, "raw"), tmp)
  invisible(source(tmp, encoding = encoding, echo = echo))
}

#' Source:a bara en eller flera namngivna funktioner från en fil
#'
#' @param skriptfil Sökväg eller URL till en R-fil.
#' @param funktioner Teckenvektor med funktionsnamn att plocka ut.
#'
#' @return Inget - de hittade funktionerna läggs i den globala miljön.
#' @export
source_funktioner <- function(skriptfil, funktioner) {
  tmp_env <- new.env(parent = globalenv())
  source(skriptfil, local = tmp_env)

  hittade <- funktioner[funktioner %in% ls(tmp_env)]
  ej_hittade <- setdiff(funktioner, hittade)

  for (namn in hittade) assign(namn, tmp_env[[namn]], envir = globalenv())

  if (length(ej_hittade) > 0) {
    verb <- if (length(ej_hittade) > 1) "Funktionerna " else "Funktionen "
    message(verb, list_komma_och(ej_hittade), " finns inte i skriptfilen '", skriptfil, "'.")
  }
  invisible(hittade)
}

#' Döp om R-skript i en mapp till .txt (för Copilot)
#'
#' @param appnamn Valfritt namn som läggs in i filnamnen.
#' @param mapp Mapp med `.R`-filer.
#'
#' @return Inget - filerna döps om på plats.
#' @export
copilot_konvertera <- function(appnamn = NULL, mapp = "c:/Lokalt/till_copilot") {
  if (!dir.exists(mapp)) stop("Mappen finns inte: ", mapp)

  for (fil in list.files(mapp, pattern = "\\.[Rr]$", full.names = TRUE)) {
    stam <- tools::file_path_sans_ext(basename(fil))
    nytt <- if (is.null(appnamn)) paste0(stam, ".txt") else paste0(stam, "_", appnamn, ".txt")
    file.rename(fil, file.path(dirname(fil), nytt))
  }
  invisible(NULL)
}
