# GitHub-autentisering.
#
# Standardstacken är gitcreds (token i OS:ets credential store) + gh (REST API)
# + gert/credentials (git-operationer). Token letas i denna ordning:
#   1. GITHUB_PAT / GITHUB_TOKEN i miljön
#   2. gitcreds (git credential helper)
#   3. keyring service "github_token"  (bakåtkompatibilitet med äldre uppsättning)
# När en token hittas sätts GITHUB_PAT i sessionen så att gh, gert och
# credentials hittar den automatiskt.

#' Hämta en GitHub-token
#'
#' @param service keyring-service att falla tillbaka på om varken miljövariabel
#'   eller gitcreds ger någon token.
#'
#' @return Osynligt: token som sträng. Fel om ingen hittas.
#' @export
rddeploy_pat <- function(service = "github_token") {
  pat <- Sys.getenv("GITHUB_PAT", unset = Sys.getenv("GITHUB_TOKEN", unset = ""))
  if (nzchar(pat)) return(invisible(pat))

  pat <- tryCatch(gitcreds::gitcreds_get()$password, error = function(e) "")
  if (!is.null(pat) && nzchar(pat)) {
    Sys.setenv(GITHUB_PAT = pat)
    return(invisible(pat))
  }

  if (requireNamespace("keyring", quietly = TRUE)) {
    poster <- tryCatch(intern_suppress_keyring_backend_varning(keyring::key_list(service = service)),
                       error = function(e) data.frame())
    if (nrow(poster) > 0) {
      pat <- intern_suppress_keyring_backend_varning(keyring::key_get(service, poster$username[1]))
      if (nzchar(pat)) {
        Sys.setenv(GITHUB_PAT = pat)
        return(invisible(pat))
      }
    }
  }

  cli::cli_abort(c(
    "Ingen GitHub-token hittades.",
    "i" = "Sätt {.envvar GITHUB_PAT}, kör {.run gitcreds::gitcreds_set()} eller
           spara en token i keyring service {.val {service}}.",
    "i" = "Skapa en token med {.run usethis::create_github_token()}."
  ))
}

# keyring väljer lagringsbackend första gången den anropas i en session och
# varnar då om den inte hittar ett riktigt OS-backend (t.ex. Linux utan
# secret_service, eller en huvudlös session) - ofarligt, säger bara att den
# föll tillbaka på "env"-backendet. Döljer bara den specifika varningen så
# att andra, riktiga varningar från keyring fortfarande syns.
intern_suppress_keyring_backend_varning <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    if (grepl("Selecting .* backend", conditionMessage(w))) {
      invokeRestart("muffleWarning")
    }
  })
}

# keyring-services som inte används av rddeploy längre. "github" var
# användarnamn+lösenord för git-auth - GitHub har inte stött det sedan
# augusti 2021. "git2r" gav bara git-identitet, ersatt av
# git_kontrollera_id_uppgifter() (läser den globala git-configen).
intern_foraldrade_keyring_services <- function() {
  if (!requireNamespace("keyring", quietly = TRUE)) return(character(0))
  Filter(function(s) {
    isTRUE(tryCatch(
      intern_suppress_keyring_backend_varning(nrow(keyring::key_list(service = s)) > 0),
      error = function(e) FALSE
    ))
  }, c("github", "git2r"))
}

#' Diagnostik för git- och GitHub-uppsättningen
#'
#' Motsvarar `usethis::git_sitrep()` men kortfattad: finns en token, vem är den
#' kopplad till, är git-identiteten satt - och flaggar föråldrade
#' keyring-poster (`"github"`, `"git2r"`) om de påträffas.
#'
#' @return Osynligt: en lista med `token`, `user`, `scopes`, `git_name`,
#'   `git_email`, `foraldrade_keyring_services`, `redo` (alla krav uppfyllda).
#' @export
rddeploy_auth_check <- function() {
  cli::cli_h2("git / GitHub")

  pat <- tryCatch(rddeploy_pat(), error = function(e) "")
  har_token <- nzchar(pat)
  cli::cli_li(if (har_token) "Token: {.green hittad}" else "Token: {.red saknas}")

  whoami <- NULL
  scopes <- NA_character_
  if (har_token) {
    whoami <- tryCatch(gh::gh_whoami(), error = function(e) NULL)
    if (!is.null(whoami)) {
      cli::cli_li("Konto: {.val {whoami$login}} ({whoami$name})")
      scopes <- whoami$scopes %||% NA_character_
      cli::cli_li("Scopes: {.val {scopes}}")
    } else {
      cli::cli_li("Konto: {.red kunde inte slå upp - token kanske ogiltig}")
    }
  }

  git_name  <- tryCatch(gert::git_config_global()$value[gert::git_config_global()$name == "user.name"][1],
                        error = function(e) NA_character_)
  git_email <- tryCatch(gert::git_config_global()$value[gert::git_config_global()$name == "user.email"][1],
                        error = function(e) NA_character_)
  cli::cli_li(if (isTRUE(nzchar(git_name)))  "git user.name: {.val {git_name}}"  else "git user.name: {.red ej satt}")
  cli::cli_li(if (isTRUE(nzchar(git_email))) "git user.email: {.val {git_email}}" else "git user.email: {.red ej satt}")

  foraldrade <- intern_foraldrade_keyring_services()
  if (length(foraldrade) > 0) {
    for (svc in foraldrade) {
      cli::cli_alert_warning(c(
        "Föråldrad keyring-post {.val {svc}} hittades - används inte av rddeploy. ",
        "{.val github} (användarnamn+lösenord) stöds inte av GitHub sedan 2021; ",
        "{.val git2r} ersattes av git_kontrollera_id_uppgifter(). ",
        "Kan raderas: {.code keyring::key_delete(\"{svc}\")}"
      ))
    }
  }

  redo <- har_token && isTRUE(nzchar(git_name)) && isTRUE(nzchar(git_email))
  if (redo) {
    cli::cli_alert_success("Allt som krävs för git/GitHub är konfigurerat.")
  } else {
    cli::cli_alert_danger("Något saknas ovan - se raderna markerade i rött.")
  }

  invisible(list(
    token     = har_token,
    user      = if (!is.null(whoami)) whoami$login else NA_character_,
    scopes    = scopes,
    git_name  = git_name,
    git_email = git_email,
    foraldrade_keyring_services = foraldrade,
    redo      = redo
  ))
}

#' Sätt lokal git-identitet från den globala konfigurationen
#'
#' Rensar `GIT_AUTHOR_*`/`GIT_COMMITTER_*` ur miljön (de vinner annars över
#' konfigurationen), läser `user.name`/`user.email` ur den globala git-configen
#' och sätter dem lokalt i repot.
#'
#' @param repo Sökväg till repot. Standard: aktuell katalog.
#'
#' @return Osynligt `TRUE`. Fel om global identitet saknas.
#' @export
git_kontrollera_id_uppgifter <- function(repo = ".") {
  Sys.unsetenv(c("GIT_AUTHOR_NAME", "GIT_AUTHOR_EMAIL",
                 "GIT_COMMITTER_NAME", "GIT_COMMITTER_EMAIL"))

  global <- gert::git_config_global()
  hamta <- function(key) {
    v <- global$value[global$name == key]
    if (length(v) == 0) "" else v[[1]]
  }
  name  <- hamta("user.name")
  email <- hamta("user.email")

  if (!nzchar(name) || !nzchar(email)) {
    cli::cli_abort(c(
      "Git-identitet saknas eller är tom.",
      "i" = 'Kör {.code git config --global user.name "Ditt Namn"}',
      "i" = 'Kör {.code git config --global user.email "din.epost@example.com"}'
    ))
  }

  gert::git_config_set("user.name",  name,  repo = repo)
  gert::git_config_set("user.email", email, repo = repo)
  invisible(TRUE)
}
