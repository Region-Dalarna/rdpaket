# GitHub-arbetsflöden byggda på gh (REST API) och gert (git-operationer).

# Filtrera filnamn med en liten sök-DSL:
#  - teckenvektor: OR ("brott|befolkning")
#  - "&": AND ("scb&kvinnor")
#  - "!" i början av ett led: NOT ("!test", "scb&!gammal")
intern_filtrera_filnamn <- function(namn, filter) {
  if (is.null(filter) || all(is.na(filter))) return(namn)
  if (length(filter) > 1) filter <- paste0(filter, collapse = "|")

  led_matchar <- function(fil, led) {
    if (startsWith(led, "!")) {
      !grepl(tolower(sub("^!", "", led)), tolower(fil), fixed = TRUE)
    } else {
      grepl(tolower(led), tolower(fil), fixed = TRUE)
    }
  }

  if (grepl("&", filter, fixed = TRUE)) {
    led <- strsplit(filter, "&", fixed = TRUE)[[1]]
    namn[vapply(namn, function(f) all(vapply(led, led_matchar, logical(1), fil = f)), logical(1))]
  } else if (startsWith(filter, "!")) {
    namn[!grepl(tolower(sub("^!", "", filter)), tolower(namn), fixed = TRUE)]
  } else {
    namn[grepl(tolower(filter), tolower(namn))]
  }
}

#' Lista repositories hos en GitHub-organisation eller -användare
#'
#' @param owner Organisation/användare. Standard: [rddeploy-config] `gh_org`.
#' @param skriv_ut Skriv reponamnen i konsolen.
#'
#' @return Osynligt (eller synligt om `skriv_ut = FALSE`): en tibble med `namn`,
#'   `url`, `url_clone`, `privat`, `arkiverad`.
#' @export
github_lista_repos <- function(owner = intern_gh_org(), skriv_ut = TRUE) {
  tryCatch(rddeploy_pat(), error = function(e) NULL)
  svar <- gh::gh("GET /users/{owner}/repos", owner = owner, .limit = Inf)

  res <- tibble::tibble(
    namn      = purrr::map_chr(svar, "name"),
    url       = purrr::map_chr(svar, "html_url"),
    url_clone = purrr::map_chr(svar, "clone_url"),
    privat    = purrr::map_lgl(svar, "private"),
    arkiverad = purrr::map_lgl(svar, "archived")
  )

  if (skriv_ut) {
    cli::cli_h3("Repositories hos {.val {owner}}")
    cli::cli_ul(res$namn)
    invisible(res)
  } else {
    res
  }
}

#' @rdname github_lista_repos
#' @export
github_lista_repos_analytikernatverket <- function(owner = intern_gh_org_an(),
                                                   skriv_ut = TRUE) {
  github_lista_repos(owner = owner, skriv_ut = skriv_ut)
}

#' Lista filer i ett GitHub-repo
#'
#' Hämtar hela filträdet i repots default-branch i ett anrop och returnerar det
#' i valt format.
#'
#' @param repo Reponamn.
#' @param owner Organisation/användare. Standard: [rddeploy-config] `gh_org`.
#' @param filter Sök-DSL på filnamn: teckenvektor (OR), `"&"` (AND), `"!"`
#'   (NOT). `NULL` = alla filer.
#' @param retur Format: `"source"` (färdiga `source()`-satser),
#'   `"url"` (rå-URL:er), `"df"` (tibble med `namn` + `url`), `"ppt"`
#'   (kodrader för `ppt_lista_fyll_pa()`).
#' @param till_urklipp Kopiera resultatet till urklipp (för `"source"`/`"ppt"`).
#' @param lista_ej_systemfiler Filtrera bort `LICENSE` och dotfiler.
#'
#' @return Beror på `retur`: en teckenvektor, en tibble, eller (för
#'   `"source"`/`"ppt"`) osynligt en teckenvektor efter utskrift.
#' @export
github_lista_repo_filer <- function(repo,
                                    owner = intern_gh_org(),
                                    filter = NULL,
                                    retur = c("source", "url", "df", "ppt"),
                                    till_urklipp = TRUE,
                                    lista_ej_systemfiler = TRUE) {
  retur <- match.arg(retur)
  tryCatch(rddeploy_pat(), error = function(e) NULL)

  info <- gh::gh("GET /repos/{owner}/{repo}", owner = owner, repo = repo)
  branch <- info$default_branch
  tree <- gh::gh("GET /repos/{owner}/{repo}/git/trees/{branch}",
                 owner = owner, repo = repo, branch = branch, recursive = 1)
  if (isTRUE(tree$truncated)) {
    cli::cli_warn("Filträdet för {.val {repo}} är avkortat av GitHub - alla filer listas inte.")
  }

  namn <- purrr::map_chr(purrr::keep(tree$tree, ~ identical(.x$type, "blob")), "path")

  if (lista_ej_systemfiler) {
    namn <- namn[basename(namn) != "LICENSE" & !grepl("(^|/)\\.", namn)]
  }
  namn <- intern_filtrera_filnamn(namn, filter)
  if (length(namn) == 0) cli::cli_abort("Inga filer matchade sökningen.")
  namn <- sort(namn)

  url <- sprintf("https://raw.githubusercontent.com/%s/%s/%s/%s", owner, repo, branch, namn)

  kopiera <- function(txt) {
    if (till_urklipp && requireNamespace("clipr", quietly = TRUE) && clipr::clipr_available()) {
      clipr::write_clip(txt)
    }
  }

  switch(retur,
    url = url,
    df  = tibble::tibble(namn = namn, url = url),
    source = {
      rader <- sprintf('source("%s")', url)
      cat(rader, sep = "\n")
      kopiera(rader)
      invisible(rader)
    },
    ppt = {
      rader <- vapply(url, intern_ppt_rad, character(1), USE.NAMES = FALSE)
      cat(rader, sep = "\n")
      kopiera(rader)
      invisible(rader)
    }
  )
}

#' @rdname github_lista_repo_filer
#' @export
github_lista_repo_filer_analytikernatverket <- function(repo,
                                                        owner = intern_gh_org_an(),
                                                        filter = NULL,
                                                        retur = c("source", "url", "df", "ppt"),
                                                        till_urklipp = TRUE,
                                                        lista_ej_systemfiler = TRUE) {
  github_lista_repo_filer(repo = repo, owner = owner, filter = filter,
                          retur = match.arg(retur), till_urklipp = till_urklipp,
                          lista_ej_systemfiler = lista_ej_systemfiler)
}

intern_ppt_rad <- function(source_url) {
  paste0(
    "ppt_lista <- ppt_lista_fyll_pa(\n",
    "\tppt_lista = ppt_lista,\n",
    '\tsource_url = "', source_url, '",\n',
    "\tparameter_argument = list(output_mapp = utmapp_bilder),\n",
    "\tregion_vekt = region_vekt,\n",
    "\tutmapp_bilder = utmapp_bilder)\n"
  )
}

#' Snabblistning av filer i vanliga repos
#'
#' `gh_dia()` listar `diagram`-repot, `gh_ppt()` samma som `ppt`-kodrader,
#' `gh_hamta_analytikernatverket()` listar Analytikernätverkets `hamta_data`.
#'
#' @param filter Sök-DSL, se [github_lista_repo_filer()].
#' @return Se [github_lista_repo_filer()].
#' @export
gh_dia <- function(filter = NULL) {
  github_lista_repo_filer(repo = "diagram", filter = filter, retur = "source")
}

#' @rdname gh_dia
#' @export
gh_ppt <- function(filter = NULL) {
  github_lista_repo_filer(repo = "diagram", filter = filter, retur = "ppt")
}

#' @rdname gh_dia
#' @export
gh_hamta_analytikernatverket <- function(filter = NULL) {
  github_lista_repo_filer_analytikernatverket(repo = "hamta_data", filter = filter, retur = "source")
}

#' Skriv en kodrad för `ppt_lista_fyll_pa()` till urklipp
#'
#' @param ppt_url Rå-URL till diagramskriptet.
#' @return Osynligt: kodraden som sträng.
#' @export
ppt_lista_rader <- function(ppt_url = "") {
  rad <- intern_ppt_rad(ppt_url)
  cat(rad)
  if (requireNamespace("clipr", quietly = TRUE) && clipr::clipr_available()) {
    clipr::write_clip(rad)
  }
  invisible(rad)
}

#' Visa status för ett lokalt klonat repo
#'
#' @param repo Reponamn.
#' @param grundsokvag Föräldermapp där repot ligger. Standard: [rddeploy-config]
#'   `gh_mapp`.
#'
#' @return Osynligt: `gert::git_status()`-tibblen. Skriver en sammanfattning.
#' @export
github_status_filer_lokalt_repo <- function(repo, grundsokvag = intern_gh_mapp()) {
  repo_sokvag <- file.path(grundsokvag, repo)
  if (!dir.exists(file.path(repo_sokvag, ".git"))) {
    cli::cli_abort("Hittar inget git-repo i {.path {repo_sokvag}}")
  }

  st <- gert::git_status(repo = repo_sokvag)

  if (nrow(st) == 0) {
    cli::cli_alert_success("Inga ändringar i {.path {repo_sokvag}}")
    return(invisible(st))
  }

  ny       <- st$file[st$status == "new"]
  andrad   <- st$file[st$status %in% c("modified", "renamed", "typechange")]
  borttagen <- st$file[st$status == "deleted"]

  cli::cli_h3("Ändringar i {.path {repo_sokvag}}")
  if (length(ny))        cli::cli_alert_info("{length(ny)} ny(a): {.file {ny}}")
  if (length(andrad))    cli::cli_alert_info("{length(andrad)} ändrad(e): {.file {andrad}}")
  if (length(borttagen)) cli::cli_alert_info("{length(borttagen)} borttagen/borttagna: {.file {borttagen}}")
  invisible(st)
}

#' @rdname github_status_filer_lokalt_repo
#' @export
github_status_filer_lokalt_repo_analytikernatverket <- function(repo,
                                                                grundsokvag = intern_gh_mapp_an()) {
  github_status_filer_lokalt_repo(repo = repo, grundsokvag = grundsokvag)
}

# Bygg ett commit-meddelande ur git-status.
intern_commit_meddelande <- function(st) {
  bitar <- c(
    if (any(st$status == "new"))       paste0("Nya filer: ", paste(st$file[st$status == "new"], collapse = ", ")),
    if (any(st$status %in% c("modified", "renamed", "typechange")))
      paste0("Ändrade filer: ", paste(st$file[st$status %in% c("modified", "renamed", "typechange")], collapse = ", ")),
    if (any(st$status == "deleted"))   paste0("Borttagna filer: ", paste(st$file[st$status == "deleted"], collapse = ", "))
  )
  paste(bitar, collapse = ". ")
}

#' Committa och pusha alla ändringar i ett lokalt repo
#'
#' @param repo Reponamn.
#' @param commit_txt Commit-meddelande. `NULL` = genereras ur git-status.
#' @param grundsokvag Föräldermapp. Standard: [rddeploy-config] `gh_mapp`.
#' @param pull_forst Kör `git pull` innan push (om det finns en upstream-branch).
#'
#' @return Osynligt `TRUE` om något pushades, annars `FALSE`.
#' @export
github_commit_push <- function(repo,
                               commit_txt  = NULL,
                               grundsokvag = intern_gh_mapp(),
                               pull_forst  = TRUE) {

  repo_sokvag <- file.path(grundsokvag, repo)
  if (!dir.exists(file.path(repo_sokvag, ".git"))) {
    cli::cli_abort("Hittar inget git-repo i {.path {repo_sokvag}}")
  }
  rddeploy_pat()

  # Skydd mot parallell körning
  lockfil <- file.path(tempdir(), paste0("rddeploy_push_", repo, ".lock"))
  if (file.exists(lockfil)) cli::cli_abort("En annan push-process verkar redan köra för {.val {repo}}.")
  writeLines(as.character(Sys.time()), lockfil)
  on.exit(unlink(lockfil), add = TRUE)

  st <- gert::git_status(repo = repo_sokvag)
  if (nrow(st) == 0) {
    cli::cli_alert_info("Inga nya eller ändrade filer att ladda upp.")
    return(invisible(FALSE))
  }

  if (is.null(commit_txt) || is.na(commit_txt)) {
    commit_txt <- intern_commit_meddelande(st)
  }

  if (pull_forst && intern_har_upstream(repo_sokvag)) {
    gert::git_pull(repo = repo_sokvag)
  }

  gert::git_add(".", repo = repo_sokvag)
  gert::git_commit(commit_txt, repo = repo_sokvag)
  intern_gh_push(repo_sokvag, branch = intern_gh_branch(repo_sokvag))

  cli::cli_alert_success("Commit och push till {.val {repo}} klar.")
  cli::cli_text(commit_txt)
  invisible(TRUE)
}

#' @rdname github_commit_push
#' @export
github_commit_push_analytikernatverket <- function(repo,
                                                   commit_txt  = NULL,
                                                   grundsokvag = intern_gh_mapp_an(),
                                                   pull_forst  = TRUE) {
  github_commit_push(repo = repo, commit_txt = commit_txt,
                     grundsokvag = grundsokvag, pull_forst = pull_forst)
}

#' Pulla ett eller flera lokala repon från GitHub
#'
#' Kör en förkontroll: har repot lokala ändringar hoppas pull över (en pull kan
#' annars misslyckas tyst).
#'
#' @param repo Reponamn, en vektor med flera, eller `"*"` för alla mappar i
#'   `grundsokvag`.
#' @param grundsokvag Föräldermapp. Standard: [rddeploy-config] `gh_mapp`.
#'
#' @return Osynligt `NULL`.
#' @export
github_pull_lokalt_repo_fran_github <- function(repo, grundsokvag = intern_gh_mapp()) {
  rddeploy_pat()
  if (identical(repo, "*")) repo <- list.files(grundsokvag)

  purrr::walk(repo, function(r) {
    repo_sokvag <- file.path(grundsokvag, r)
    if (!dir.exists(file.path(repo_sokvag, ".git"))) {
      cli::cli_alert_warning("{.val {r}}: inget git-repo, hoppar över.")
      return(invisible(NULL))
    }

    st <- gert::git_status(repo = repo_sokvag)
    if (nrow(st) > 0) {
      cli::cli_alert_warning(c(
        "{.val {r}}: lokala ändringar - pull SKIPPAD. ",
        "Committa, stasha (gert::git_stash_save()) eller städa först."
      ))
      return(invisible(NULL))
    }

    if (!intern_har_upstream(repo_sokvag)) {
      cli::cli_alert_info("{.val {r}}: ingen upstream-branch, hoppar över.")
      return(invisible(NULL))
    }

    resultat <- tryCatch(gert::git_pull(repo = repo_sokvag),
                         error = function(e) conditionMessage(e))
    if (is.character(resultat)) {
      cli::cli_alert_danger("{.val {r}}: {resultat}")
    } else {
      cli::cli_alert_success("{.val {r}}: uppdaterad.")
    }
  })
  invisible(NULL)
}

#' @rdname github_pull_lokalt_repo_fran_github
#' @export
github_pull_lokalt_repo_fran_github_analytikernatverket <- function(repo,
                                                                    grundsokvag = intern_gh_mapp_an()) {
  github_pull_lokalt_repo_fran_github(repo = repo, grundsokvag = grundsokvag)
}

#' Klona ett GitHub-repo lokalt
#'
#' @param repo_namn Reponamnet (utan URL), t.ex. `"hamta_data"`.
#' @param repo_org Organisation. Standard: [rddeploy-config] `gh_org`.
#' @param grundsokvag Lokal föräldermapp. Standard: [rddeploy-config] `gh_mapp`.
#' @param oppna_rproj Skapa `.Rproj`-fil och öppna projektet (kräver `usethis`
#'   respektive RStudio).
#'
#' @return Osynligt: sökvägen till det klonade repot.
#' @export
github_lagg_till_repo_fran_github <- function(repo_namn,
                                              repo_org    = intern_gh_org(),
                                              grundsokvag = intern_gh_mapp(),
                                              oppna_rproj = FALSE) {
  if (missing(repo_namn) || !nzchar(repo_namn)) cli::cli_abort("{.arg repo_namn} måste anges.")
  tryCatch(rddeploy_pat(), error = function(e) NULL)

  lokal_sokvag <- file.path(grundsokvag, repo_namn)
  if (dir.exists(lokal_sokvag)) cli::cli_abort("Katalogen finns redan: {.path {lokal_sokvag}}")

  finns <- tryCatch({
    gh::gh("GET /repos/{owner}/{repo}", owner = repo_org, repo = repo_namn)
    TRUE
  }, http_error_404 = function(e) FALSE, error = function(e) {
    cli::cli_abort("Kunde inte nå GitHub för att kontrollera repot: {conditionMessage(e)}")
  })
  if (!finns) {
    cli::cli_abort(c(
      "Repot hittades inte på GitHub: {.val {repo_org}/{repo_namn}}",
      "i" = "Kör {.run rddeploy::github_lista_repos()} för att se vilka som finns."
    ))
  }

  if (!dir.exists(grundsokvag)) dir.create(grundsokvag, recursive = TRUE)

  repo_url <- sprintf("https://github.com/%s/%s.git", repo_org, repo_namn)
  gert::git_clone(repo_url, path = lokal_sokvag)
  cli::cli_alert_success("Klonade {.val {repo_namn}} till {.path {lokal_sokvag}}")

  rproj <- file.path(lokal_sokvag, paste0(repo_namn, ".Rproj"))
  if (oppna_rproj && !file.exists(rproj) && requireNamespace("usethis", quietly = TRUE)) {
    usethis::create_project(lokal_sokvag, open = FALSE, rstudio = TRUE)
  }
  if (oppna_rproj && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    rstudioapi::openProject(lokal_sokvag, newSession = TRUE)
  }

  invisible(lokal_sokvag)
}

#' @rdname github_lagg_till_repo_fran_github
#' @export
github_lagg_till_repo_fran_github_analytikernatverket <- function(repo_namn,
                                                                  repo_org    = intern_gh_org_an(),
                                                                  grundsokvag = intern_gh_mapp_an(),
                                                                  oppna_rproj = FALSE) {
  github_lagg_till_repo_fran_github(repo_namn = repo_namn, repo_org = repo_org,
                                   grundsokvag = grundsokvag, oppna_rproj = oppna_rproj)
}
