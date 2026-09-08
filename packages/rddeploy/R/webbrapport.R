# Webbrapporter (RMarkdown -> GitHub Pages via publicera_rapporter).

# Statiska filer i depot-roten som varje webbrapport behöver.
intern_webbrapport_assets <- list(
  bin  = c("Dalastrategin.jpg", "dalastrategin_hjul.png", "logga_korrekt.png",
           "logo_liggande_fri_vit.png", "logo_liggande_platta_farg.png",
           "logo_liggande_platta_svart.png", "rd_logo_liggande_fri_svart.png",
           "favicon.ico"),
  text = c("styles_hero.css", "favicon.html")
)

intern_webbrapport_skript <- c(
  "1_hamta_data.R"                = "1_hamta_data.R",
  "2_knitta_rapport.R"            = "2_knitta_rapport.R",
  "3_kopiera_till_publicera_rapporter_docs_for_publicering_pa_webben.R" =
    "3_kopiera_till_publicera_rapporter_docs_for_publicering_pa_webben.R",
  "4_push_av_hela_repo_till_github.R" = "4_push_av_hela_repo_till_github.R"
)

#' Skapa ett nytt webbrapport-repo på GitHub
#'
#' Skapar projektstrukturen (`figurer/`, `skript/`), hämtar delade
#' bild-/stilfiler och hjälpskript från `Region-Dalarna/depot`, skriver en
#' `hero_image.html` och ett `.Rmd`-skelett (som använder `rddiagram`), och
#' initierar git + GitHub-repo.
#'
#' @param github_repo Reponamn (döper mapp, repo och `.Rmd`-fil).
#' @param rapport_titel Rapportens titel.
#' @param rapport_undertitel Valfri undertitel.
#' @param github_org Organisation, eller `NULL` för privat konto. Standard:
#'   [rddeploy-config] `gh_org`.
#' @param behorighet_team GitHub-team som får push-behörighet (`NULL` = inget).
#' @param grundsokvag Lokal föräldermapp. Standard: [rddeploy-config] `gh_mapp`.
#' @param skapa_github_repo `FALSE` = skapa bara de lokala filerna, hoppa över
#'   git-init och GitHub.
#'
#' @return Osynligt: sökvägen till det lokala projektet.
#' @export
skapa_webbrapport_github <- function(github_repo,
                                     rapport_titel,
                                     rapport_undertitel = NULL,
                                     github_org         = intern_gh_org(),
                                     behorighet_team    = intern_behorighet_team(),
                                     grundsokvag        = intern_gh_mapp(),
                                     skapa_github_repo  = TRUE) {
  if (missing(rapport_titel)) rapport_titel <- github_repo
  sokvag_proj <- file.path(grundsokvag, github_repo)
  if (dir.exists(file.path(sokvag_proj, ".git"))) {
    cli::cli_abort("{.path {sokvag_proj}} innehåller redan ett git-repo. Avbryter.")
  }

  dir.create(file.path(sokvag_proj, "figurer"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(sokvag_proj, "skript"),  showWarnings = FALSE)

  if (requireNamespace("usethis", quietly = TRUE)) {
    usethis::create_project(sokvag_proj, open = FALSE)
  }

  # --- Hämta delade filer från depot ---
  cli::cli_alert_info("Hämtar bild- och stilfiler från depot ...")
  for (f in intern_webbrapport_assets$bin) {
    depot_hamta_fran(f, file.path(sokvag_proj, f), as_text = FALSE)
  }
  for (f in intern_webbrapport_assets$text) {
    depot_hamta_fran(f, file.path(sokvag_proj, f), as_text = TRUE)
  }
  for (i in seq_along(intern_webbrapport_skript)) {
    depot_hamta_fran(names(intern_webbrapport_skript)[i],
                     file.path(sokvag_proj, "skript", intern_webbrapport_skript[i]),
                     as_text = TRUE)
  }

  # --- hero_image.html + .Rmd-skelett ur paketmallar ---
  undertitel_html <- if (!is.null(rapport_undertitel)) {
    paste0('<div class="bottom_text">', rapport_undertitel, "</div>")
  } else ""

  intern_skriv_mall("webbrapport", "hero_image.html",
                    file.path(sokvag_proj, "hero_image.html"),
                    list(rapport_titel = rapport_titel, undertitel_html = undertitel_html))
  intern_skriv_mall("webbrapport", "rapport.Rmd",
                    file.path(sokvag_proj, paste0(github_repo, ".Rmd")),
                    list(rapport_titel = rapport_titel))
  intern_skriv_mall("webbrapport", "gitignore", file.path(sokvag_proj, ".gitignore"))

  cli::cli_alert_success("Lokal projektstruktur skapad i {.path {sokvag_proj}}")

  if (!isTRUE(skapa_github_repo)) {
    cli::cli_alert_info("skapa_github_repo = FALSE - hoppar över git och GitHub.")
    return(invisible(sokvag_proj))
  }

  rddeploy_pat()
  gert::git_init(path = sokvag_proj)
  git_kontrollera_id_uppgifter(repo = sokvag_proj)
  gert::git_add(".", repo = sokvag_proj)
  gert::git_commit("Initiera webbrapport", repo = sokvag_proj)

  if (!requireNamespace("usethis", quietly = TRUE)) {
    cli::cli_abort("Paketet {.pkg usethis} krävs för att skapa GitHub-repot.")
  }
  intern_med_wd(sokvag_proj, {
    if (is.null(github_org)) {
      usethis::use_github(private = FALSE, protocol = "https")
    } else {
      usethis::use_github(organisation = github_org, private = FALSE,
                          visibility = "public", protocol = "https")
    }
  })

  intern_ge_team_behorighet(github_repo, team = behorighet_team, github_org = github_org)
  cli::cli_alert_success("Webbrapport-repo {.val {github_repo}} skapat.")
  invisible(sokvag_proj)
}

# Kör ett uttryck med tillfälligt bytt working directory.
intern_med_wd <- function(dir, expr) {
  gammal <- getwd()
  on.exit(setwd(gammal), add = TRUE)
  setwd(dir)
  force(expr)
}

#' Publicera en webbrapport till publik eller intern server
#'
#' Committar ev. lokala ändringar på källbranchen och pushar den till
#' `publicera-<target>`, vilket triggar repots deploy-workflow.
#'
#' @param rapport_repo Reponamn.
#' @param target `"publik"` eller `"intern"`.
#' @param from_branch Källbranch. Standard `"main"` (faller tillbaka till
#'   `"master"` om den inte finns).
#' @param grundsokvag Lokal föräldermapp. Standard: [rddeploy-config] `gh_mapp`.
#' @param github_org Organisation. Standard: [rddeploy-config] `gh_org`.
#' @param commit_meddelande Meddelande för ev. commit av lokala ändringar.
#'
#' @return Osynligt: en lista med `rapport_repo`, `target`, `to_branch`.
#' @export
webbrapport_publicera <- function(rapport_repo,
                                  target           = c("publik", "intern"),
                                  from_branch      = "main",
                                  grundsokvag      = intern_gh_mapp(),
                                  github_org       = intern_gh_org(),
                                  commit_meddelande = NULL) {
  target <- match.arg(target)
  to_branch <- paste0("publicera-", target)
  repo_dir <- file.path(grundsokvag, rapport_repo)
  if (!dir.exists(file.path(repo_dir, ".git"))) {
    cli::cli_abort("Hittar inget git-repo i {.path {repo_dir}}")
  }
  rddeploy_pat()

  gert::git_fetch(repo = repo_dir)
  alla <- gert::git_branch_list(repo = repo_dir)$name
  finns <- function(n) n %in% alla || paste0("origin/", n) %in% alla
  if (!finns(from_branch)) {
    alt <- switch(from_branch, main = "master", master = "main", NA_character_)
    if (!is.na(alt) && finns(alt)) {
      cli::cli_alert_info("Branchen {.val {from_branch}} saknas - använder {.val {alt}}.")
      from_branch <- alt
    } else {
      cli::cli_abort("Hittar inte branchen {.val {from_branch}} i {.val {rapport_repo}}.")
    }
  }

  if (gert::git_branch(repo = repo_dir) != from_branch) {
    gert::git_branch_checkout(from_branch, repo = repo_dir)
  }
  suppressMessages(gert::git_pull(repo = repo_dir))

  st <- gert::git_status(repo = repo_dir)
  if (nrow(st) > 0) {
    if (is.null(commit_meddelande)) {
      commit_meddelande <- paste0("Uppdatera rapport: ", rapport_repo, " (", Sys.Date(), ")")
    }
    gert::git_add(".", repo = repo_dir)
    gert::git_commit(commit_meddelande, repo = repo_dir)
    intern_gh_push(repo_dir, branch = from_branch)
    cli::cli_alert_success("Committade och pushade ändringar till {.val {from_branch}}.")
  }

  gert::git_fetch(repo = repo_dir)
  bi <- gert::git_branch_list(repo = repo_dir)
  lokal_sha  <- bi$commit[bi$name == from_branch]
  remote_sha <- bi$commit[bi$name == paste0("origin/", to_branch)]
  if (length(lokal_sha) == 1 && length(remote_sha) == 1 && lokal_sha == remote_sha) {
    cli::cli_alert_info("Den här versionen är redan publicerad på {target}-servern.")
  } else {
    gert::git_push(repo = repo_dir, remote = "origin",
                   refspec = paste0("refs/heads/", from_branch, ":refs/heads/", to_branch),
                   force = TRUE)
    cli::cli_alert_success("Pushade {.val {from_branch}} -> {.val {to_branch}} - deploy triggas ({target}).")
  }

  invisible(list(rapport_repo = rapport_repo, target = target, to_branch = to_branch))
}

#' Avpublicera en webbrapport
#'
#' Triggar repots `avpublicera.yml`-workflow.
#'
#' @param rapport_repo Reponamn.
#' @param target `"publik"` eller `"intern"`.
#' @param github_org Organisation. Standard: [rddeploy-config] `gh_org`.
#' @param ref Branch workflow-filen läses från. Standard `"main"` (faller
#'   tillbaka till `"master"`).
#' @param bekrafta Fråga interaktivt innan triggning.
#'
#' @return Osynligt `TRUE` om workflow triggades, annars `FALSE`.
#' @export
webbrapport_avpublicera <- function(rapport_repo,
                                    target     = c("publik", "intern"),
                                    github_org = intern_gh_org(),
                                    ref        = "main",
                                    bekrafta   = TRUE) {
  target <- match.arg(target)

  if (isTRUE(bekrafta) && interactive()) {
    svar <- readline(paste0(
      "Avpublicera '", rapport_repo, "' från ", target,
      "-servern? Skriv repo-namnet för att bekräfta: "))
    if (svar != rapport_repo) {
      cli::cli_alert_info("Avbrutet - bekräftelsen matchade inte repo-namnet.")
      return(invisible(FALSE))
    }
  }

  res <- tryCatch(
    intern_trigga_workflow(rapport_repo, "avpublicera.yml",
                           inputs = list(target = target, bekraftelse = rapport_repo),
                           github_org = github_org, ref = ref),
    error = function(e) e
  )
  if (inherits(res, "error") && ref %in% c("main", "master") &&
      grepl("No ref found|Not Found", conditionMessage(res))) {
    alt <- if (ref == "main") "master" else "main"
    cli::cli_alert_info("Branchen {.val {ref}} hittades inte - provar {.val {alt}}.")
    intern_trigga_workflow(rapport_repo, "avpublicera.yml",
                           inputs = list(target = target, bekraftelse = rapport_repo),
                           github_org = github_org, ref = alt)
  } else if (inherits(res, "error")) {
    stop(res)
  }

  cli::cli_alert_success(c(
    "Avpublicering triggad för {.val {rapport_repo}} ({target}-server). ",
    "Följ: {.url https://github.com/{github_org}/{rapport_repo}/actions}"
  ))
  invisible(TRUE)
}
