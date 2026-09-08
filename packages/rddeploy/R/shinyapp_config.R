#' Validerad konfiguration för shinyapp-scaffolding
#'
#' Samlar parametrarna till [shinyapp_skapa_med_github_repo()] och
#' [shinyapp_skapa_med_github_repo_forka_befintligt()] i ett validerat objekt.
#'
#' @param github_repo Namn på repo och Shiny-app (mapp på servern).
#' @param github_org Organisation, eller `NULL` för privat konto. Standard:
#'   [rddeploy-config] `gh_org`.
#' @param rapport_titel Titel som visas i appen. Standard: `github_repo`.
#' @param rapport_undertitel Valfri undertitel.
#' @param grundsokvag Lokal föräldermapp. Standard: [rddeploy-config] `gh_mapp`.
#' @param behorighet_team GitHub-team som får push-behörighet (`NULL` = inget).
#' @param target Default-server i `_publicering_till_server.yml`: `"publik"`
#'   eller `"intern"`.
#' @param telemetri Lägg in `shiny.telemetry`-boilerplate för användnings-
#'   statistik.
#'
#' @return Ett `rddeploy_shinyapp_config`-objekt (en validerad lista).
#' @export
shinyapp_config <- function(github_repo,
                            github_org        = intern_gh_org(),
                            rapport_titel     = github_repo,
                            rapport_undertitel = NULL,
                            grundsokvag       = intern_gh_mapp(),
                            behorighet_team   = intern_behorighet_team(),
                            target            = c("publik", "intern"),
                            telemetri         = TRUE) {
  target <- match.arg(target)
  if (missing(github_repo) || !nzchar(github_repo)) {
    cli::cli_abort("{.arg github_repo} måste anges.")
  }
  if (!grepl("^[A-Za-z0-9_.-]+$", github_repo)) {
    cli::cli_abort("{.arg github_repo} får bara innehålla bokstäver, siffror, {.val _.-}.")
  }

  structure(
    list(
      github_repo        = github_repo,
      github_org         = github_org,
      rapport_titel      = rapport_titel,
      rapport_undertitel = rapport_undertitel,
      grundsokvag        = intern_slash(grundsokvag),
      behorighet_team    = behorighet_team,
      target             = target,
      telemetri          = isTRUE(telemetri),
      sokvag_proj        = file.path(sub("/+$", "", grundsokvag), github_repo)
    ),
    class = "rddeploy_shinyapp_config"
  )
}

#' @export
print.rddeploy_shinyapp_config <- function(x, ...) {
  cli::cli_h2("shinyapp-config: {.val {x$github_repo}}")
  cli::cli_dl(c(
    "Org"          = x$github_org %||% "(privat konto)",
    "Titel"        = x$rapport_titel,
    "Lokal mapp"   = x$sokvag_proj,
    "Team"         = x$behorighet_team %||% "(inget)",
    "Target"       = x$target,
    "Telemetri"    = if (x$telemetri) "ja" else "nej"
  ))
  invisible(x)
}
