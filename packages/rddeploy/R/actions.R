# GitHub Actions och team-behörighet via gh.

# Trigga workflow_dispatch för en workflow-fil.
intern_trigga_workflow <- function(github_repo, workflow_filnamn,
                                   inputs = list(),
                                   github_org = intern_gh_org(),
                                   ref = "main") {
  rddeploy_pat()
  gh::gh("POST /repos/{owner}/{repo}/actions/workflows/{workflow_id}/dispatches",
         owner = github_org, repo = github_repo, workflow_id = workflow_filnamn,
         ref = ref, inputs = inputs)
  invisible(TRUE)
}

# Vänta tills senaste körningen av en workflow är klar. Fel om den misslyckas
# eller om tiden går ut.
intern_vanta_pa_workflow <- function(github_repo, workflow_filnamn,
                                     github_org = intern_gh_org(),
                                     max_vantetid_s = 600,
                                     poll_intervall_s = 5) {
  rddeploy_pat()
  startad <- Sys.time()
  Sys.sleep(3)

  repeat {
    runs <- gh::gh("GET /repos/{owner}/{repo}/actions/workflows/{workflow_id}/runs",
                   owner = github_org, repo = github_repo,
                   workflow_id = workflow_filnamn, per_page = 5)
    if (length(runs$workflow_runs) == 0) {
      cli::cli_abort("Hittade inga körningar av {.file {workflow_filnamn}}.")
    }
    senaste <- runs$workflow_runs[[1]]

    if (identical(senaste$status, "completed")) {
      if (identical(senaste$conclusion, "success")) {
        cli::cli_alert_success("Workflow {.file {workflow_filnamn}} slutfördes.")
        return(invisible(TRUE))
      }
      cli::cli_abort(c(
        "Workflow {.file {workflow_filnamn}} slutfördes med {.val {senaste$conclusion}}.",
        "i" = "Se {.url {senaste$html_url}}"
      ))
    }

    if (as.numeric(difftime(Sys.time(), startad, units = "secs")) > max_vantetid_s) {
      cli::cli_abort("Timeout: workflow inte klar efter {max_vantetid_s} s.")
    }
    cat(".")
    Sys.sleep(poll_intervall_s)
  }
}

# Ge ett GitHub-team push-behörighet på ett repo. team = NULL -> gör inget.
intern_ge_team_behorighet <- function(github_repo,
                                      team = intern_behorighet_team(),
                                      github_org = intern_gh_org(),
                                      permission = "push") {
  if (is.null(team) || is.null(github_org)) return(invisible(FALSE))
  rddeploy_pat()
  ok <- tryCatch({
    gh::gh("PUT /orgs/{org}/teams/{team_slug}/repos/{owner}/{repo}",
           org = github_org, team_slug = team, owner = github_org, repo = github_repo,
           permission = permission)
    TRUE
  }, error = function(e) {
    cli::cli_warn(c(
      "Kunde inte ge teamet {.val {team}} behörighet: {conditionMessage(e)}",
      "i" = "Kontrollera att teamet finns i organisationen {.val {github_org}}."
    ))
    FALSE
  })
  if (ok) cli::cli_alert_success("Teamet {.val {team}} har fått {permission}-behörighet.")
  invisible(ok)
}
