# Uppdatera de installerade rd*-paketen från Region-Dalarna/rdpaket.

#' Uppdatera installerade rd*-paket från GitHub
#'
#' Jämför den installerade versionen av `rd*`-paketen i monorepot
#' `Region-Dalarna/rdpaket` mot `Version` i `DESCRIPTION` på GitHub (läst via
#' Contents-API:t, se [source_utan_cache()]) och installerar om, med
#' [remotes::install_github()], de paket där GitHub-versionen är nyare.
#'
#' @param paket Vilka paket som ska kontrolleras. Standard är alla `rd*`-paket
#'   (se `rdverktyg:::rdpaket_alla_paketnamn`) som redan är installerade
#'   lokalt.
#' @param repo GitHub-repo (`"ägare/repo"`) där paketen ligger.
#' @param gren Gren att jämföra och installera från.
#' @param installera Om `FALSE` görs bara en kontroll - inget installeras.
#' @param pat Valfri GitHub-PAT, se [source_utan_cache()].
#'
#' @return Osynligt en `data.frame` med en rad per kontrollerat paket:
#'   `paket`, `installerad_version`, `github_version` och `uppdaterad`.
#' @export
rdpaket_uppdatera_alla <- function(paket = NULL,
                                    repo = "Region-Dalarna/rdpaket",
                                    gren = "main",
                                    installera = TRUE,
                                    pat = NULL) {
  if (is.null(paket)) {
    paket <- Filter(
      function(p) requireNamespace(p, quietly = TRUE),
      rdpaket_alla_paketnamn
    )
  }
  if (length(paket) == 0) {
    message("Inga rd*-paket från '", repo, "' är installerade.")
    return(invisible(intern_rdpaket_status_df()))
  }

  m <- regmatches(repo, regexec("^([^/]+)/([^/]+)$", repo))[[1]]
  if (length(m) != 3) stop("`repo` måste anges som \"ägare/repo\".", call. = FALSE)
  owner <- m[2]; repo_namn <- m[3]

  status <- do.call(rbind, lapply(paket, function(p) {
    installerad_version <- as.character(utils::packageVersion(p))
    github_version <- intern_rdpaket_github_version(owner, repo_namn, p, gren, pat)
    intern_rdpaket_status_df(p, installerad_version, github_version, uppdaterad = FALSE)
  }))

  for (i in seq_len(nrow(status))) {
    if (is.na(status$github_version[i])) next

    nyare <- package_version(status$github_version[i]) >
      package_version(status$installerad_version[i])

    if (!nyare) {
      message(status$paket[i], ": redan senaste versionen (",
              status$installerad_version[i], ").")
    } else if (installera) {
      if (!requireNamespace("remotes", quietly = TRUE)) {
        stop("Paketet 'remotes' krävs för att installera uppdateringar.", call. = FALSE)
      }
      message("Uppdaterar ", status$paket[i], ": ", status$installerad_version[i],
              " -> ", status$github_version[i])
      remotes::install_github(
        repo, subdir = paste0("packages/", status$paket[i]),
        ref = gren, upgrade = "never"
      )
      status$uppdaterad[i] <- TRUE
    } else {
      message(status$paket[i], ": ny version tillgänglig (",
              status$installerad_version[i], " -> ", status$github_version[i], ").")
    }
  }

  invisible(status)
}

# Alla rd*-paket i monorepot, i den beroendeordning install_all.R använder.
rdpaket_alla_paketnamn <- c(
  "rdverktyg", "rddiagram", "rdpostgres", "rdgis", "rdgeorouting",
  "rdshinyappar", "rddeploy", "rdadminportal", "rd"
)

#' @noRd
intern_rdpaket_status_df <- function(paket = character(0),
                                     installerad_version = character(0),
                                     github_version = character(0),
                                     uppdaterad = logical(0)) {
  data.frame(
    paket = paket,
    installerad_version = installerad_version,
    github_version = github_version,
    uppdaterad = uppdaterad,
    stringsAsFactors = FALSE
  )
}

#' Läs `Version` ur `DESCRIPTION` för ett paket i monorepot på GitHub
#'
#' @noRd
intern_rdpaket_github_version <- function(owner, repo_namn, paket, gren, pat) {
  res <- intern_github_api_hamta(
    owner, repo_namn, paste0("packages/", paket, "/DESCRIPTION"), gren, pat
  )

  if (httr::status_code(res) != 200) {
    warning("Kunde inte hämta DESCRIPTION för '", paket, "' (HTTP ",
            httr::status_code(res), ").", call. = FALSE)
    return(NA_character_)
  }

  beskrivning <- tryCatch(
    read.dcf(textConnection(httr::content(res, as = "text", encoding = "UTF-8"))),
    error = function(e) NULL
  )
  if (is.null(beskrivning) || !"Version" %in% colnames(beskrivning)) {
    warning("Kunde inte tolka DESCRIPTION för '", paket, "'.", call. = FALSE)
    return(NA_character_)
  }

  unname(beskrivning[1, "Version"])
}
