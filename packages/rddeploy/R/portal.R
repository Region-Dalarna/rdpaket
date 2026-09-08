# Webbsidor/portaler och dokumentationssidor byggda på Quarto + GitHub Pages.

#' Skapa ett nytt webbsida/portal-repo på GitHub
#'
#' Skapar repot, klonar det lokalt, fyller i mallfilerna från
#' `Region-Dalarna/depot` (`mallar/webbsida_portal/`) och pushar grundstrukturen.
#'
#' @param github_repo Reponamn (blir även portalens URL-segment).
#' @param server `"publik"` eller `"intern"` – styr runner-label i deploy.yml.
#' @param titel,beskrivning Visas på portalen.
#' @param privat_repo Skapa repot privat.
#' @param github_org Organisation. Standard: [rddeploy-config] `gh_org`.
#' @param behorighet_team GitHub-team som får push-behörighet (`NULL` = inget).
#' @param grundsokvag Lokal föräldermapp. Standard: [rddeploy-config] `gh_mapp`.
#'
#' @return Osynligt: sökvägen till det lokala repot.
#' @export
webbsida_med_portal_skapa_med_github_repo <- function(github_repo,
                                                      server         = c("publik", "intern"),
                                                      titel          = github_repo,
                                                      beskrivning    = titel,
                                                      privat_repo    = TRUE,
                                                      github_org     = intern_gh_org(),
                                                      behorighet_team = intern_behorighet_team(),
                                                      grundsokvag    = intern_gh_mapp()) {
  server <- match.arg(server)
  rddeploy_pat()

  runner_label <- switch(server, publik = "rapport", intern = "rapport-intern")
  lokal_path <- file.path(grundsokvag, github_repo)
  if (dir.exists(lokal_path)) {
    cli::cli_abort("Mappen finns redan: {.path {lokal_path}}. Avbryter.")
  }

  cli::cli_alert_info("Skapar repo {.val {github_org}/{github_repo}} på GitHub ...")
  gh::gh("POST /orgs/{org}/repos", org = github_org,
         name = github_repo, private = privat_repo, auto_init = FALSE)

  intern_ge_team_behorighet(github_repo, team = behorighet_team, github_org = github_org)

  repo_url <- sprintf("https://github.com/%s/%s.git", github_org, github_repo)
  gert::git_clone(repo_url, path = lokal_path)
  aktuell <- gert::git_branch(repo = lokal_path)
  if (!identical(aktuell, "main")) {
    gert::git_branch_move(aktuell, "main", repo = lokal_path)
  }

  dir.create(file.path(lokal_path, ".github", "workflows"), recursive = TRUE, showWarnings = FALSE)
  variabler <- list(titel = titel, beskrivning = beskrivning,
                    runner_label = runner_label, github_repo = github_repo)

  depot_skriv_mall_fran("_quarto.yml.tmpl", file.path(lokal_path, "_quarto.yml"), variabler)
  depot_skriv_mall_fran("index.qmd.tmpl",   file.path(lokal_path, "index.qmd"),   variabler)
  depot_skriv_mall_fran("deploy.yml.tmpl",  file.path(lokal_path, ".github/workflows/deploy.yml"), variabler)
  depot_skriv_mall_fran("README.md.tmpl",   file.path(lokal_path, "README.md"),   variabler)

  depot_hamta_fran("regiondalarna_ruf.css",
                   file.path(lokal_path, "regiondalarna_ruf.css"), as_text = TRUE)
  depot_hamta_fran("mallar/webbsida_portal/portal_overrides.css",
                   file.path(lokal_path, "portal_overrides.css"), as_text = TRUE)

  writeLines(c(".quarto/", "_site/", "*.html", "*_files/"),
             file.path(lokal_path, ".gitignore"))

  gert::git_add(".", repo = lokal_path)
  gert::git_commit("Initiera webbsida/portal-struktur", repo = lokal_path)
  intern_gh_push(lokal_path, branch = "main", set_upstream = TRUE)

  gh::gh("PATCH /repos/{owner}/{repo}", owner = github_org, repo = github_repo,
         default_branch = "main")

  portal_url <- paste0("https://", intern_portal_host(server), "/", github_repo, "/")
  cli::cli_alert_success("Repo skapat: {.url https://github.com/{github_org}/{github_repo}}")
  cli::cli_alert_info("Publiceras vid push till main: {.url {portal_url}}")
  cli::cli_alert_info("(kräver att runnern {.val {runner_label}} är aktiv på rätt server)")
  invisible(lokal_path)
}

# Enkel translitterering för filnamnsförslag (inte full Latin-ASCII).
intern_asciifiera <- function(x) {
  chartr("åäöÅÄÖ", "aaoAAO", x)
}

#' Skapa en ny dokumentationssida (.qmd) i ett lokalt repo
#'
#' Validerar filnamnet strikt (bara gemener, siffror, `_`, `-`), skapar en
#' `.qmd` med YAML-header och rubrikskelett.
#'
#' @param titel,beskrivning Visas i listningen på dokumentationsportalen.
#' @param filnamn Filnamn (utan eller med `.qmd`).
#' @param undertitel Valfri undertitel.
#' @param repo Reponamn. Standard: `"dokumentation"`.
#' @param grundsokvag Lokal föräldermapp. Standard: [rddeploy-config] `gh_mapp`.
#' @param oppna_i_rstudio Öppna filen i RStudio efteråt.
#' @param commit_och_push Committa och pusha direkt via [github_commit_push()].
#'
#' @return Osynligt: sökvägen till den skapade filen.
#' @export
dokumentation_sida_skapa <- function(titel, beskrivning, filnamn,
                                     undertitel      = NULL,
                                     repo            = "dokumentation",
                                     grundsokvag     = intern_gh_mapp(),
                                     oppna_i_rstudio = TRUE,
                                     commit_och_push = FALSE) {
  fel <- character(0)
  if (grepl(" ", filnamn))        fel <- c(fel, "innehåller mellanslag")
  if (grepl("[A-ZÅÄÖ]", filnamn)) fel <- c(fel, "innehåller versaler")
  if (grepl("[åäöÅÄÖ]", filnamn)) fel <- c(fel, "innehåller å/ä/ö")
  if (length(fel) > 0) {
    forslag <- gsub(" ", "_", intern_asciifiera(tolower(filnamn)))
    cli::cli_abort(c(
      "Ogiltigt filnamn {.val {filnamn}}: {fel}.",
      "i" = "Bara gemener, siffror, understreck och bindestreck.",
      "i" = "Förslag: {.val {forslag}}"
    ))
  }
  if (!grepl("\\.qmd$", filnamn)) filnamn <- paste0(filnamn, ".qmd")

  repo_path <- file.path(grundsokvag, repo)
  if (!dir.exists(repo_path)) {
    cli::cli_abort(c(
      "Repot {.val {repo}} finns inte lokalt under {.path {repo_path}}.",
      "i" = "Klona det först med {.run rddeploy::github_lagg_till_repo_fran_github(\"{repo}\")}."
    ))
  }

  fil_path <- file.path(repo_path, filnamn)
  if (file.exists(fil_path)) cli::cli_abort("Filen finns redan: {.path {fil_path}}. Avbryter.")

  subtitel <- if (is.null(undertitel)) "" else undertitel
  innehall <- c(
    "---",
    paste0('title: "', titel, '"'),
    paste0('subtitle: "', subtitel, '"'),
    paste0('listing-description: "', beskrivning, '"'),
    "---",
    "", "## Inledning", "", "## Struktur", "", "## Sammanfattning", ""
  )
  writeLines(innehall, fil_path, useBytes = TRUE)
  cli::cli_alert_success("Skapade {.path {fil_path}}.")

  if (oppna_i_rstudio && requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    rstudioapi::navigateToFile(fil_path)
  }

  if (commit_och_push) {
    github_commit_push(repo, commit_txt = paste0("Ny sida: ", titel),
                       grundsokvag = grundsokvag)
  } else {
    cli::cli_alert_info(c(
      "Filen ligger bara lokalt. Publicera med ",
      "{.run rddeploy::github_commit_push(\"{repo}\")} när du är klar."
    ))
  }
  invisible(fil_path)
}
