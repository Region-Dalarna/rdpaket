# Publicera / avpublicera / flytta Shiny-appar.

intern_shinyapp_app_url <- function(appnamn, target, anvand_http = FALSE) {
  protokoll <- if (anvand_http) "http" else "https"
  paste0(protokoll, "://", intern_shiny_host(target), "/", appnamn, "/")
}

# TRUE om appen svarar med HTTP 200. För 'intern': prova https, sen http.
intern_shinyapp_ping <- function(appnamn, target, timeout_s = 10) {
  prova_http <- if (target == "intern") c(FALSE, TRUE) else FALSE
  for (http in prova_http) {
    url <- intern_shinyapp_app_url(appnamn, target, anvand_http = http)
    kod <- tryCatch(
      curl::curl_fetch_memory(url, handle = curl::new_handle(timeout = timeout_s))$status_code,
      error = function(e) NA_integer_
    )
    if (isTRUE(kod == 200)) return(TRUE)
  }
  FALSE
}

intern_shinyapp_yml_sokvag <- function(repo_sokvag) {
  file.path(repo_sokvag, "_publicering_till_server.yml")
}

# "publik"/"intern", eller NA om filen saknas/ogiltig.
intern_shinyapp_las_target <- function(repo_sokvag) {
  yml <- intern_shinyapp_yml_sokvag(repo_sokvag)
  if (!file.exists(yml)) return(NA_character_)
  target <- if (requireNamespace("yaml", quietly = TRUE)) {
    tryCatch(yaml::read_yaml(yml)$target, error = function(e) NULL)
  } else {
    rad <- grep("^\\s*target\\s*:", readLines(yml, warn = FALSE), value = TRUE)
    if (length(rad)) trimws(sub("^[^:]*:\\s*", "", rad[1])) else NULL
  }
  if (is.null(target) || !target %in% c("publik", "intern")) NA_character_ else target
}

intern_shinyapp_skriv_target <- function(repo_sokvag, target) {
  writeLines(paste0("target: ", target), intern_shinyapp_yml_sokvag(repo_sokvag))
}

# Default-branch: origin/HEAD om satt, annars main/master.
intern_shinyapp_default_branch <- function(repo_sokvag) {
  bl <- gert::git_branch_list(repo = repo_sokvag)
  fjarr <- bl$name[!bl$local]
  if ("origin/main"   %in% fjarr) return("main")
  if ("origin/master" %in% fjarr) return("master")
  lok <- gert::git_branch(repo = repo_sokvag)
  if (!is.null(lok) && nzchar(lok)) return(lok)
  cli::cli_abort("Kunde inte hitta default-branch för {.path {repo_sokvag}}.")
}

intern_shinyapp_repo_dir <- function(github_repo, grundsokvag) {
  d <- file.path(grundsokvag, github_repo)
  if (!dir.exists(file.path(d, ".git"))) {
    cli::cli_abort("Hittar inget git-repo i {.path {d}}")
  }
  d
}

#' Publicera en Shiny-app till publik eller intern server
#'
#' Pushar default-branchens topp till `publicera-<target>`, vilket triggar
#' repots `deploy.yml`.
#'
#' @param github_repo Reponamn.
#' @param target `"publik"`, `"intern"` eller `NULL` (läs från
#'   `_publicering_till_server.yml`).
#' @param grundsokvag Lokal föräldermapp. Standard: [rddeploy-config] `gh_mapp`.
#' @param tvinga_omdeploy Om appen redan är publicerad på exakt denna commit:
#'   skriv en trigger-fil på publicera-branchen så att deployen körs om ändå.
#'
#' @return Osynligt `TRUE` om något pushades, annars `FALSE`.
#' @export
shinyapp_publicera <- function(github_repo,
                               target        = NULL,
                               grundsokvag   = intern_gh_mapp(),
                               tvinga_omdeploy = FALSE) {
  repo_dir <- intern_shinyapp_repo_dir(github_repo, grundsokvag)
  rddeploy_pat()

  if (is.null(target)) {
    target <- intern_shinyapp_las_target(repo_dir)
    if (is.na(target)) {
      cli::cli_alert_warning("Ingen giltig target i _publicering_till_server.yml - antar 'publik'.")
      target <- "publik"
    }
    cli::cli_alert_info("Target från config: {.val {target}}")
  } else if (!target %in% c("publik", "intern")) {
    cli::cli_abort("{.arg target} måste vara 'publik', 'intern' eller NULL.")
  }

  publicera_branch <- paste0("publicera-", target)
  default_branch <- intern_shinyapp_default_branch(repo_dir)

  st <- gert::git_status(repo = repo_dir)
  if (nrow(st) > 0) {
    cli::cli_abort(c("Repot har ohanterade ändringar - committa eller stasha först.",
                     "i" = "{nrow(st)} fil(er): {.file {st$file}}"))
  }

  if (gert::git_branch(repo = repo_dir) != default_branch) {
    gert::git_branch_checkout(default_branch, repo = repo_dir)
  }
  gert::git_fetch(repo = repo_dir)
  suppressMessages(gert::git_pull(repo = repo_dir))

  bi <- gert::git_branch_list(repo = repo_dir)
  default_sha    <- bi$commit[bi$name == default_branch]
  remote_pub_sha <- bi$commit[bi$name == paste0("origin/", publicera_branch)]
  redan <- length(default_sha) == 1 && length(remote_pub_sha) == 1 &&
    default_sha == remote_pub_sha

  if (redan && !isTRUE(tvinga_omdeploy)) {
    cli::cli_alert_info(c(
      "'{github_repo}' är redan publicerad på {target}-servern på denna commit. ",
      "Kör med tvinga_omdeploy = TRUE för att deploya om ändå."
    ))
    return(invisible(FALSE))
  }

  if (redan && isTRUE(tvinga_omdeploy)) {
    # Checka ut publicera-branchen, rör en trigger-fil, pusha.
    tmp_branch_finns <- gert::git_branch_exists(publicera_branch, local = TRUE, repo = repo_dir)
    if (tmp_branch_finns) {
      gert::git_branch_checkout(publicera_branch, repo = repo_dir)
    } else {
      gert::git_branch_create(publicera_branch, ref = paste0("origin/", publicera_branch),
                              repo = repo_dir, checkout = TRUE)
    }
    writeLines(format(Sys.time()), file.path(repo_dir, ".rddeploy-omdeploy"))
    gert::git_add(".rddeploy-omdeploy", repo = repo_dir)
    gert::git_commit(paste0("Trigga omdeploy ", Sys.Date()), repo = repo_dir)
    intern_gh_push(repo_dir, branch = publicera_branch)
    gert::git_branch_checkout(default_branch, repo = repo_dir)
  } else {
    gert::git_push(repo = repo_dir, remote = "origin",
                   refspec = paste0("refs/heads/", default_branch, ":refs/heads/", publicera_branch),
                   force = TRUE)
  }

  cli::cli_alert_success(c(
    "'{github_repo}' publicerat till {target}-servern - GitHub Actions deployar nu ",
    "till {intern_shiny_host(target)}."
  ))
  invisible(TRUE)
}

#' Avpublicera en Shiny-app från en server
#'
#' Triggar repots `avpublicera.yml` och väntar in körningen.
#'
#' @param github_repo Reponamn.
#' @param target `"publik"`, `"intern"` eller `NULL` (läs från config).
#' @param grundsokvag Lokal föräldermapp. Standard: [rddeploy-config] `gh_mapp`.
#' @param github_org Organisation. Standard: [rddeploy-config] `gh_org`.
#' @param bekrafta_automatiskt Hoppa över den interaktiva bekräftelsen (används
#'   av [shinyapp_flytta()]).
#'
#' @return Osynligt `TRUE`.
#' @export
shinyapp_avpublicera <- function(github_repo,
                                 target      = NULL,
                                 grundsokvag = intern_gh_mapp(),
                                 github_org  = intern_gh_org(),
                                 bekrafta_automatiskt = FALSE) {
  repo_dir <- intern_shinyapp_repo_dir(github_repo, grundsokvag)
  if (is.null(target)) target <- intern_shinyapp_las_target(repo_dir)
  if (is.na(target) || !target %in% c("publik", "intern")) {
    cli::cli_abort("Kunde inte avgöra target - ange {.arg target} = \"publik\"/\"intern\".")
  }
  server_url <- intern_shiny_host(target)
  app_url <- intern_shinyapp_app_url(github_repo, target)

  if (!bekrafta_automatiskt && interactive()) {
    cli::cli_h3("Avpublicering av Shiny-app")
    cli::cli_li("App: {.val {github_repo}}")
    cli::cli_li("Server: {.val {server_url}} ({target})")
    cli::cli_li("URL: {.url {app_url}}")
    cli::cli_text("Repot på GitHub påverkas inte - du kan publicera igen med {.run rddeploy::shinyapp_publicera(\"{github_repo}\")}.")
    svar <- readline(paste0("Skriv appnamnet ('", github_repo, "') för att bekräfta: "))
    if (trimws(svar) != github_repo) {
      cli::cli_alert_info("Avpublicering avbruten.")
      return(invisible(FALSE))
    }
  }

  default_branch <- intern_shinyapp_default_branch(repo_dir)
  cli::cli_alert_info("Triggar avpublicera-workflow på GitHub ...")
  intern_trigga_workflow(github_repo, "avpublicera.yml",
                         inputs = list(target = target, bekraftelse = github_repo),
                         github_org = github_org, ref = default_branch)
  intern_vanta_pa_workflow(github_repo, "avpublicera.yml", github_org = github_org)
  cli::cli_alert_success("Appen '{github_repo}' är avpublicerad från {server_url}.")
  invisible(TRUE)
}

#' Flytta en Shiny-app mellan publik och intern server
#'
#' Publicerar på den server appen inte ligger på, verifierar att den svarar,
#' uppdaterar `_publicering_till_server.yml` och avpublicerar från den gamla
#' servern.
#'
#' @param github_repo Reponamn.
#' @param grundsokvag Lokal föräldermapp. Standard: [rddeploy-config] `gh_mapp`.
#' @param github_org Organisation. Standard: [rddeploy-config] `gh_org`.
#' @param max_vantetid_s Maxtid att vänta in deploy-workflowen.
#'
#' @return Osynligt `TRUE`.
#' @export
shinyapp_flytta <- function(github_repo,
                            grundsokvag   = intern_gh_mapp(),
                            github_org    = intern_gh_org(),
                            max_vantetid_s = 600) {
  repo_dir <- intern_shinyapp_repo_dir(github_repo, grundsokvag)
  rddeploy_pat()

  st <- gert::git_status(repo = repo_dir)
  if (nrow(st) > 0) cli::cli_abort("Repot har ohanterade ändringar - committa eller stasha först.")

  default_branch <- intern_shinyapp_default_branch(repo_dir)
  gert::git_branch_checkout(default_branch, repo = repo_dir)
  suppressMessages(gert::git_pull(repo = repo_dir))

  nuvarande <- intern_shinyapp_las_target(repo_dir)
  if (is.na(nuvarande)) {
    cli::cli_alert_info("Ingen giltig config - pingar servrarna för att avgöra var appen ligger ...")
    pa_publik <- intern_shinyapp_ping(github_repo, "publik")
    pa_intern <- intern_shinyapp_ping(github_repo, "intern")
    if (pa_publik && pa_intern) cli::cli_abort("Appen svarar på BÅDA servrarna - lös manuellt.")
    if (!pa_publik && !pa_intern) cli::cli_abort("Appen svarar inte på någon server - publicera den först.")
    nuvarande <- if (pa_publik) "publik" else "intern"
    cli::cli_alert_info("Detekterat: appen ligger på {.val {nuvarande}}.")
    intern_shinyapp_skriv_target(repo_dir, nuvarande)
    gert::git_add("_publicering_till_server.yml", repo = repo_dir)
    gert::git_commit(paste0("Lägg till _publicering_till_server.yml (detekterat: ", nuvarande, ")"),
                     repo = repo_dir)
    intern_gh_push(repo_dir, branch = default_branch)
  }

  nytt <- if (nuvarande == "publik") "intern" else "publik"

  if (interactive()) {
    cli::cli_h3("Flytt av Shiny-app")
    cli::cli_li("App: {.val {github_repo}}")
    cli::cli_li("FRÅN: {.val {nuvarande}} ({intern_shinyapp_app_url(github_repo, nuvarande)})")
    cli::cli_li("TILL: {.val {nytt}} ({intern_shinyapp_app_url(github_repo, nytt)})")
    svar <- readline(paste0("Skriv appnamnet ('", github_repo, "') för att bekräfta: "))
    if (trimws(svar) != github_repo) {
      cli::cli_alert_info("Avbryter.")
      return(invisible(FALSE))
    }
  }

  cli::cli_alert_info("[1/4] Publicerar till {nytt}-servern ...")
  shinyapp_publicera(github_repo, target = nytt, grundsokvag = grundsokvag)

  cli::cli_alert_info("[2/4] Väntar in deploy.yml ...")
  intern_vanta_pa_workflow(github_repo, "deploy.yml", github_org = github_org,
                           max_vantetid_s = max_vantetid_s)

  cli::cli_alert_info("[3/4] Verifierar att appen svarar på {nytt}-servern ...")
  Sys.sleep(3)
  if (!intern_shinyapp_ping(github_repo, nytt)) {
    cli::cli_abort(c(
      "Appen svarade INTE med HTTP 200 på {nytt}-servern.",
      "i" = "Avbryter. Appen ligger nu på båda servrarna, config pekar på {.val {nuvarande}}.",
      "i" = "Felsök, eller avpublicera nya: {.run rddeploy::shinyapp_avpublicera(\"{github_repo}\", target = \"{nytt}\")}"
    ))
  }
  cli::cli_alert_success("Appen svarar.")

  cli::cli_alert_info("[4/4] Uppdaterar config och tar bort från {nuvarande}-servern ...")
  intern_shinyapp_skriv_target(repo_dir, nytt)
  gert::git_add("_publicering_till_server.yml", repo = repo_dir)
  gert::git_commit(paste0("Flytta target: ", nuvarande, " -> ", nytt), repo = repo_dir)
  intern_gh_push(repo_dir, branch = default_branch)

  shinyapp_avpublicera(github_repo, target = nuvarande, grundsokvag = grundsokvag,
                       github_org = github_org, bekrafta_automatiskt = TRUE)

  cli::cli_alert_success("Klart! '{github_repo}' flyttad från {nuvarande} till {nytt}.")
  cli::cli_alert_info("Ny URL: {.url {intern_shinyapp_app_url(github_repo, nytt)}}")
  invisible(TRUE)
}
