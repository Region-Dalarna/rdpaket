# Scaffolding av Shiny-app-repos. Uppdelat i små, testbara stegfunktioner som
# skriver till disk, plus en som pratar med GitHub. Orkestreras av de två
# publika funktionerna längst ned.

# --- Stegfunktioner (disk) --------------------------------------------------

# Grundstruktur i repo-roten.
intern_scaffold_struktur <- function(sokvag, fork = FALSE) {
  mappar <- c(sokvag, file.path(sokvag, ".github", "workflows"))
  if (!fork) {
    mappar <- c(mappar,
                file.path(sokvag, "www", "fonts"),
                file.path(sokvag, "R"))
  }
  for (m in mappar) dir.create(m, recursive = TRUE, showWarnings = FALSE)
  if (!fork) file.create(file.path(sokvag, "R", ".gitkeep"))
  invisible(sokvag)
}

# global.R, ui.R, server.R, app.css, _dependencies.R (bara icke-fork).
intern_scaffold_appfiler <- function(sokvag, cfg) {
  telemetri_block <- if (cfg$telemetri) {
    paste0(
      'source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_shinyappar.R", encoding = "utf-8", echo = FALSE)\n\n',
      'telemetry <- skapa_telemetry("', cfg$github_repo, '")\n'
    )
  } else ""
  telemetri_ui_rad     <- if (cfg$telemetri) ",\n      rdshinyappar::telemetri_ui(telemetry)" else ""
  telemetri_server_rad <- if (cfg$telemetri) "  rdshinyappar::telemetri_server(telemetry, navigation_id = 'flikval', forsta_flik = 'Tab 1')\n" else ""
  telemetri_dep_rad    <- if (cfg$telemetri) "library(shiny.telemetry)\n" else ""

  intern_skriv_mall("shinyapp", "global.R", file.path(sokvag, "global.R"),
                    list(github_repo = cfg$github_repo, telemetri_block = telemetri_block))
  intern_skriv_mall("shinyapp", "ui.R", file.path(sokvag, "ui.R"),
                    list(github_repo = cfg$github_repo, rapport_titel = cfg$rapport_titel,
                         telemetri_ui_rad = telemetri_ui_rad))
  intern_skriv_mall("shinyapp", "server.R", file.path(sokvag, "server.R"),
                    list(telemetri_server_rad = telemetri_server_rad))
  intern_skriv_mall("shinyapp", "app.css", file.path(sokvag, "www", "app.css"),
                    list(github_repo = cfg$github_repo))
  intern_skriv_mall("shinyapp", "_dependencies.R", file.path(sokvag, "_dependencies.R"),
                    list(telemetri_dep_rad = telemetri_dep_rad))
  invisible(sokvag)
}

# Delade tillgångar (favicon, css, logo, fonts) från Region-Dalarna/depot.
intern_scaffold_www <- function(sokvag) {
  www <- file.path(sokvag, "www")
  depot_hamta_fran("favicon.ico",               file.path(www, "favicon.ico"),               as_text = FALSE)
  depot_hamta_fran("regiondalarna_ruf.css",     file.path(www, "regiondalarna_ruf.css"),     as_text = TRUE)
  depot_hamta_fran("logo_liggande_fri_vit.png", file.path(www, "logo_liggande_fri_vit.png"), as_text = FALSE)
  n <- tryCatch(
    length(depot_hamta_mapp_fran("fonts", file.path(www, "fonts"), as_text = FALSE)),
    error = function(e) 0
  )
  if (n == 0) cli::cli_warn("Kunde inte hämta typsnitt från depot/fonts/ - lägg till manuellt i www/fonts/.")
  invisible(sokvag)
}

# deploy.yml + avpublicera.yml.
intern_scaffold_workflows <- function(sokvag, temp_dir_suffix = "") {
  wf <- file.path(sokvag, ".github", "workflows")
  intern_skriv_mall("shinyapp", "deploy.yml", file.path(wf, "deploy.yml"),
                    list(temp_dir_suffix = temp_dir_suffix))
  intern_skriv_mall("shinyapp", "avpublicera.yml", file.path(wf, "avpublicera.yml"))
  invisible(sokvag)
}

# .gitignore, README.md, _publicering_till_server.yml.
intern_scaffold_meta <- function(sokvag, cfg, fork = FALSE) {
  intern_skriv_mall("shinyapp", "gitignore", file.path(sokvag, ".gitignore"))
  intern_skriv_mall("shinyapp", "_publicering_till_server.yml",
                    file.path(sokvag, "_publicering_till_server.yml"),
                    list(target = cfg$target))
  readme <- if (fork) "README_fork.md" else "README.md"
  vars <- list(rapport_titel = cfg$rapport_titel, github_repo = cfg$github_repo,
               kalla_repo_url = cfg$kalla_repo_url %||% "",
               kalla_branch = cfg$kalla_branch %||% "",
               gitprojekt_sokvag = sub("/$", "", sokvag))
  intern_skriv_mall("shinyapp", readme, file.path(sokvag, "README.md"), vars)
  invisible(sokvag)
}

# renv-bootstrap i en fräsch R-process (callr). Tvingar https-CRAN och tar bara
# runtime-beroenden (inte Suggests).
intern_scaffold_renv <- function(sokvag, telemetri = TRUE) {
  if (!requireNamespace("renv", quietly = TRUE) || !requireNamespace("callr", quietly = TRUE)) {
    cli::cli_abort("Paketen {.pkg renv} och {.pkg callr} krävs för renv-bootstrap.")
  }
  paket_app <- c("shiny", "shinyjs", "shinyWidgets", "DT", "ggiraph", "dplyr",
                 "tidyr", "readr", "ggplot2", "DBI", "RPostgres", "sf",
                 if (isTRUE(telemetri)) "shiny.telemetry")

  callr::r(
    func = function(project, packages) {
      Sys.setenv(RENV_CONFIG_REPOS_OVERRIDE = "https://cloud.r-project.org")
      options(repos = c(CRAN = "https://cloud.r-project.org"))
      renv::init(project = project, bare = TRUE, restart = FALSE, load = TRUE)
      ap <- utils::available.packages()
      deps <- tools::package_dependencies(packages, db = ap,
                                          which = c("Depends", "Imports", "LinkingTo"),
                                          recursive = TRUE)
      alla <- setdiff(unique(c(packages, unlist(deps, use.names = FALSE))), c("R", NA))
      renv::install(alla, project = project,
                    library = renv::paths$library(project = project), prompt = FALSE)
      renv::snapshot(project = project, prompt = FALSE)
      renv::restore(project = project, prompt = FALSE)
    },
    args = list(project = sub("/$", "", sokvag), packages = paket_app)
  )
  cli::cli_alert_success("renv initierat, grundpaket installerade, renv.lock skapad.")
  invisible(sokvag)
}

# Första git-commit i ett nyskapat repo.
intern_git_init_commit <- function(sokvag, meddelande = "Initiera Shinyapp-projekt") {
  gert::git_init(path = sokvag)
  git_kontrollera_id_uppgifter(repo = sokvag)
  gert::git_add(".", repo = sokvag)
  gert::git_commit(meddelande, repo = sokvag)
  invisible(sokvag)
}

# GitHub-repo + team-behörighet + registrera avpublicera-workflow. Repot måste
# redan vara initierat och ha minst en commit.
intern_init_git_och_github <- function(sokvag, cfg) {
  if (!requireNamespace("usethis", quietly = TRUE)) {
    cli::cli_abort("Paketet {.pkg usethis} krävs för att skapa GitHub-repot.")
  }
  rddeploy_pat()

  intern_med_wd(sokvag, {
    old_browser <- getOption("browser")
    on.exit(options(browser = old_browser), add = TRUE)
    options(browser = function(url) invisible(NULL))
    if (is.null(cfg$github_org)) {
      usethis::use_github(private = FALSE, protocol = "https")
    } else {
      usethis::use_github(organisation = cfg$github_org, private = FALSE,
                          visibility = "public", protocol = "https")
    }
  })

  intern_ge_team_behorighet(cfg$github_repo, team = cfg$behorighet_team,
                            github_org = cfg$github_org)

  # En workflow med bara workflow_dispatch indexeras inte alltid från
  # initial-pushen. Rör avpublicera.yml en gång till så Actions scannar in den.
  wf <- file.path(sokvag, ".github", "workflows", "avpublicera.yml")
  cat("\n# (touch för workflow-registrering)\n", file = wf, append = TRUE)
  gert::git_add(".github/workflows/avpublicera.yml", repo = sokvag)
  gert::git_commit("Registrera avpublicera-workflow", repo = sokvag)
  intern_gh_push(sokvag, branch = gert::git_branch(repo = sokvag))
  cli::cli_alert_success("avpublicera.yml registrerad hos GitHub Actions.")
  invisible(sokvag)
}

# Pre-flight: skriv ut vad som skapas, kräv att föräldermappen finns, be om
# bekräftelse (om inte force = TRUE).
intern_preflight <- function(sokvag, grundsokvag, force, extra = NULL) {
  if (!dir.exists(grundsokvag)) {
    cli::cli_abort(c(
      "Föräldermappen finns inte: {.path {grundsokvag}}",
      "i" = "Skapa den först, eller sätt {.code options(rddeploy.gh_mapp = ...)}."
    ))
  }
  if (dir.exists(file.path(sokvag, ".git"))) {
    cli::cli_abort("{.path {sokvag}} innehåller redan ett git-repo. Avbryter.")
  }
  cli::cli_h3("Detta skapas")
  cli::cli_li("Lokal mapp: {.path {sokvag}}")
  cli::cli_li("GitHub-repo (efter bekräftelse)")
  for (e in extra) cli::cli_li(e)

  if (isTRUE(force) || !interactive()) return(invisible(TRUE))
  svar <- readline(prompt = "Fortsätt? Skriv 'ja' för att bekräfta: ")
  if (!tolower(trimws(svar)) %in% c("ja", "j", "yes", "y")) {
    cli::cli_abort("Avbrutet.")
  }
  invisible(TRUE)
}

# --- Publika orkestratorer -------------------------------------------------

#' Skapa en ny Shiny-app med tillhörande GitHub-repo
#'
#' Skapar hela strukturen (appfiler i repo-roten, `www/`, workflows, renv) och
#' ett GitHub-repo. Appfilerna är ett körbart skelett.
#'
#' @param github_repo,github_org,rapport_titel,rapport_undertitel,grundsokvag,behorighet_team,target,telemetri
#'   Se [shinyapp_config()]. Alternativt kan ett färdigt `config`-objekt skickas.
#' @param config Ett [shinyapp_config()]-objekt (ersätter parametrarna ovan).
#' @param initiera_renv Kör renv-bootstrap (kräver `renv` + `callr`).
#' @param skapa_github_repo Skapa git-repo och GitHub-repo (annars bara lokala filer).
#' @param force Hoppa över den interaktiva bekräftelsen.
#'
#' @return Osynligt: sökvägen till det lokala projektet.
#' @export
shinyapp_skapa_med_github_repo <- function(github_repo,
                                           github_org        = intern_gh_org(),
                                           rapport_titel     = github_repo,
                                           rapport_undertitel = NULL,
                                           grundsokvag       = intern_gh_mapp(),
                                           behorighet_team   = intern_behorighet_team(),
                                           target            = c("publik", "intern"),
                                           telemetri         = TRUE,
                                           config            = NULL,
                                           initiera_renv     = TRUE,
                                           skapa_github_repo = TRUE,
                                           force             = FALSE) {
  cfg <- config %||% shinyapp_config(
    github_repo = github_repo, github_org = github_org, rapport_titel = rapport_titel,
    rapport_undertitel = rapport_undertitel, grundsokvag = grundsokvag,
    behorighet_team = behorighet_team, target = match.arg(target), telemetri = telemetri
  )
  sokvag <- cfg$sokvag_proj

  intern_preflight(sokvag, cfg$grundsokvag, force,
                   extra = if (initiera_renv) "renv-bibliotek (kan ta några minuter)")

  intern_scaffold_struktur(sokvag)
  intern_scaffold_appfiler(sokvag, cfg)
  intern_scaffold_www(sokvag)
  intern_scaffold_workflows(sokvag, temp_dir_suffix = "")
  intern_scaffold_meta(sokvag, cfg)
  if (isTRUE(initiera_renv)) intern_scaffold_renv(sokvag, telemetri = cfg$telemetri)

  if (isTRUE(skapa_github_repo)) {
    intern_git_init_commit(sokvag)
    intern_init_git_och_github(sokvag, cfg)
  } else {
    cli::cli_alert_info("skapa_github_repo = FALSE - projektet finns bara lokalt i {.path {sokvag}}")
  }

  cli::cli_alert_success("Klart: {.val {cfg$github_repo}}")
  invisible(sokvag)
}

#' Skapa en Shiny-app som forkar in ett befintligt repo via git subtree
#'
#' Appkoden hämtas till `app/` med `git subtree` (kräver git-CLI). Repo-roten
#' får deploy-workflows som pekar på `app/`.
#'
#' @param github_repo,github_org,rapport_titel,grundsokvag,behorighet_team,target
#'   Se [shinyapp_config()].
#' @param force Hoppa över den interaktiva bekräftelsen.
#' @param kalla_repo_url URL till källrepot, t.ex.
#'   `"https://github.com/nagon/coolapp.git"`.
#' @param kalla_branch Branch att hämta från i källrepot.
#'
#' @return Osynligt: sökvägen till det lokala projektet.
#' @export
shinyapp_skapa_med_github_repo_forka_befintligt <- function(github_repo,
                                                            kalla_repo_url,
                                                            kalla_branch      = "main",
                                                            github_org        = intern_gh_org(),
                                                            rapport_titel     = github_repo,
                                                            grundsokvag       = intern_gh_mapp(),
                                                            behorighet_team   = intern_behorighet_team(),
                                                            target            = c("publik", "intern"),
                                                            force             = FALSE) {
  if (missing(kalla_repo_url) || !nzchar(kalla_repo_url)) {
    cli::cli_abort("{.arg kalla_repo_url} måste anges.")
  }
  if (nchar(Sys.which("git")) == 0) {
    cli::cli_abort("Fork-varianten kräver git-CLI i PATH (för {.code git subtree}).")
  }

  cfg <- shinyapp_config(github_repo = github_repo, github_org = github_org,
                         rapport_titel = rapport_titel, grundsokvag = grundsokvag,
                         behorighet_team = behorighet_team, target = match.arg(target),
                         telemetri = FALSE)
  cfg$kalla_repo_url <- kalla_repo_url
  cfg$kalla_branch   <- kalla_branch
  sokvag <- cfg$sokvag_proj

  intern_preflight(sokvag, cfg$grundsokvag, force,
                   extra = c(paste0("app/ hämtas via git subtree från ", kalla_repo_url,
                                    " (branch ", kalla_branch, ")")))

  intern_scaffold_struktur(sokvag, fork = TRUE)
  intern_scaffold_workflows(sokvag, temp_dir_suffix = "/app")
  intern_scaffold_meta(sokvag, cfg, fork = TRUE)

  rddeploy_pat()
  intern_git_init_commit(sokvag, "Initiera Shinyapp-projekt med deploy-workflow")

  proj <- sub("/$", "", sokvag)
  cli::cli_alert_info("Hämtar {.val {kalla_repo_url}} (branch {.val {kalla_branch}}) till app/ ...")
  utgang <- system2("git", c("-C", proj, "subtree", "add", "--prefix=app",
                             kalla_repo_url, kalla_branch, "--squash"),
                    stdout = TRUE, stderr = TRUE)
  if (!is.null(attr(utgang, "status")) && attr(utgang, "status") != 0) {
    cli::cli_abort(c("git subtree add misslyckades:", paste(utgang, collapse = "\n")))
  }

  shiny_filer <- c("ui.R", "server.R", "app.R", "global.R")
  hittade <- shiny_filer[file.exists(file.path(sokvag, "app", shiny_filer))]
  if (length(hittade) == 0) {
    cli::cli_warn("Hittade inga typiska Shiny-filer direkt i app/. Deployen fungerar troligen inte.")
  } else {
    cli::cli_alert_success("Hittade Shiny-filer i app/: {.file {hittade}}")
  }

  intern_init_git_och_github(sokvag, cfg)
  cli::cli_alert_success("Klart: {.val {cfg$github_repo}}")
  invisible(sokvag)
}
