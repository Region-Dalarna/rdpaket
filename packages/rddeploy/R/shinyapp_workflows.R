# Uppdatering av deploy-/avpublicera-workflows i befintliga Shiny-app-repon. Efterarbetet på servern
# (restart.txt, landningssida, app-lista, borttagning) ligger i serverdrift (shiny_deploy.sh och
# shiny_avpublicera.sh); mallarna anropar bara de skripten.

# Är det här en deploy.yml som kommer från mallen? (kontroll innan den skrivs över)
intern_workflow_ar_mall <- function(deploy_txt) {
  grepl("name: Deploy Shiny app", deploy_txt, fixed = TRUE) &&
    grepl("/usr/local/bin/shiny_deploy.sh", deploy_txt, fixed = TRUE)
}

# Mappsuffixet i den gamla deploy.yml ("" för vanliga appar, t.ex. "/app" för forkade).
intern_workflow_suffix <- function(deploy_txt) {
  m <- regmatches(deploy_txt, regexec('TEMP_DIR="\\$\\{GITHUB_WORKSPACE\\}([^"]*)"', deploy_txt))[[1]]
  if (length(m) == 2) m[2] else NA_character_
}

# Jämför texter utan skillnader i radslut, tomrader sist och den rad scaffoldingen lade till i
# avpublicera.yml för att Actions skulle läsa in workflowen.
intern_workflow_normalisera <- function(txt) {
  txt <- gsub("\r\n", "\n", txt, fixed = TRUE)
  txt <- sub("\n*# \\(touch för workflow-registrering\\)\n*$", "", txt)
  sub("\\s+$", "", txt)
}

# Nya texter för en befintlig apps workflows (NULL om den inte kommer från mallen).
intern_workflow_nya_texter <- function(gammal_deploy) {
  if (!intern_workflow_ar_mall(gammal_deploy)) return(NULL)
  suffix <- intern_workflow_suffix(gammal_deploy)
  if (is.na(suffix)) return(NULL)
  list(
    deploy = as.character(intern_las_mall("shinyapp", "deploy.yml", list(temp_dir_suffix = suffix))),
    avpublicera = as.character(intern_las_mall("shinyapp", "avpublicera.yml"))
  )
}

#' Uppdatera deploy- och avpublicera-workflows i befintliga Shiny-app-repon
#'
#' Skriver om `.github/workflows/deploy.yml` och `avpublicera.yml` efter de nuvarande mallarna, där
#' efterarbetet på servern (återstart av appens processer med `restart.txt`, landningssida, app-lista
#' och borttagning vid avpublicering) görs av `shiny_deploy.sh` och `shiny_avpublicera.sh` i
#' serverdrift. Då kan det ändras på en plats, och en publicering av flera appar startar inte om hela
#' Shiny Server för varje app.
#'
#' Ändringen görs i default-branchen och börjar gälla vid nästa publicering av appen
#' ([shinyapp_publicera()]), som pushar default-branchen till `publicera-<target>`. Kör bara efter att
#' serverdrift med `shiny_avpublicera.sh` är utrullat på båda servrarna, annars anropar workflowen ett
#' skript som inte finns.
#'
#' Repon hoppas över om de har ohanterade ändringar, eller om deras `deploy.yml` inte ser ut att
#' komma från mallen (t.ex. handredigerad). Forkade appar behåller sitt mappsuffix.
#'
#' @param github_repo Reponamn, flera går bra. `NULL` = alla Shiny-app-repon som finns lokalt i
#'   `grundsokvag` (mappar vars `deploy.yml` kommer från mallen).
#' @param grundsokvag Lokal föräldermapp. Standard: [rddeploy-config] `gh_mapp`.
#' @param torrkorning `TRUE` (standard) visar bara vad som skulle ändras. `FALSE` committar och pushar.
#'
#' @return En tibble (osynligt) med en rad per repo och kolumnerna `repo` och `status`.
#' @export
shinyapp_uppdatera_workflows <- function(github_repo = NULL,
                                         grundsokvag = intern_gh_mapp(),
                                         torrkorning = TRUE) {
  if (is.null(github_repo)) {
    kandidater <- list.dirs(grundsokvag, recursive = FALSE, full.names = FALSE)
    github_repo <- kandidater[vapply(kandidater, function(r) {
      f <- file.path(grundsokvag, r, ".github", "workflows", "deploy.yml")
      file.exists(f) && intern_workflow_ar_mall(paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
    }, logical(1))]
    cli::cli_alert_info("Hittade {length(github_repo)} Shiny-app-repo i {.path {grundsokvag}}.")
  }
  if (!isTRUE(torrkorning)) rddeploy_pat()

  status <- vapply(github_repo, function(repo) {
    tryCatch(intern_uppdatera_workflows_ett_repo(repo, grundsokvag, torrkorning),
             error = function(e) paste0("FEL: ", conditionMessage(e)))
  }, character(1))

  res <- tibble::tibble(repo = github_repo, status = unname(status))
  for (i in seq_len(nrow(res))) {
    rad <- paste0(res$repo[i], ": ", res$status[i])
    if (grepl("^FEL|hoppar över", res$status[i])) cli::cli_alert_warning("{rad}") else cli::cli_alert_info("{rad}")
  }
  if (isTRUE(torrkorning)) cli::cli_alert_info("Torrkörning - inget ändrat. Kör med {.code torrkorning = FALSE} för att committa och pusha.")
  invisible(res)
}

# Ett repo; returnerar en statustext.
intern_uppdatera_workflows_ett_repo <- function(repo, grundsokvag, torrkorning) {
  repo_dir <- intern_shinyapp_repo_dir(repo, grundsokvag)
  wf <- file.path(repo_dir, ".github", "workflows")
  f_deploy <- file.path(wf, "deploy.yml")
  f_avpub <- file.path(wf, "avpublicera.yml")
  if (!file.exists(f_deploy)) return("hoppar över: ingen deploy.yml")

  # Uppdatera lokalt först, så att vi utgår från senaste main och inte skriver över nyare ändringar
  default_branch <- intern_shinyapp_default_branch(repo_dir)
  if (!torrkorning) {
    st <- gert::git_status(repo = repo_dir)
    if (nrow(st) > 0) return(paste0("hoppar över: ohanterade ändringar (", nrow(st), " fil(er))"))
    if (gert::git_branch(repo = repo_dir) != default_branch) gert::git_branch_checkout(default_branch, repo = repo_dir)
    gert::git_fetch(repo = repo_dir)
    suppressMessages(gert::git_pull(repo = repo_dir))
  }

  las <- function(f) if (file.exists(f)) paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n") else ""
  gammal_deploy <- las(f_deploy)
  gammal_avpub <- las(f_avpub)
  nya <- intern_workflow_nya_texter(gammal_deploy)
  if (is.null(nya)) return("hoppar över: deploy.yml ser inte ut att komma från mallen")

  ny_deploy <- !identical(intern_workflow_normalisera(gammal_deploy), intern_workflow_normalisera(nya$deploy))
  ny_avpub <- !identical(intern_workflow_normalisera(gammal_avpub), intern_workflow_normalisera(nya$avpublicera))
  if (!ny_deploy && !ny_avpub) return("redan uppdaterad")

  andras <- c(if (ny_deploy) "deploy.yml", if (ny_avpub) "avpublicera.yml")
  if (torrkorning) return(paste0("skulle uppdatera ", paste(andras, collapse = " och ")))

  if (ny_deploy) writeLines(nya$deploy, f_deploy, useBytes = TRUE)
  if (ny_avpub) writeLines(nya$avpublicera, f_avpub, useBytes = TRUE)
  gert::git_add(file.path(".github", "workflows", andras), repo = repo_dir)
  gert::git_commit("Uppdatera deploy-workflows: efterarbetet på servern ligger i serverdrift (shiny_deploy.sh/shiny_avpublicera.sh)",
                   repo = repo_dir)
  intern_gh_push(repo_dir, branch = default_branch)
  paste0("uppdaterad och pushad (", paste(andras, collapse = " och "), ")")
}
