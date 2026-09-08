# Hämta mallar och statiska filer ur Region-Dalarna/depot.
#
# Textfiler hämtas via GitHub Contents-API:t med raw-accept (går förbi Fastly-
# CDN:en, som annars kan servera en gammal version i några minuter efter en
# push). Binärfiler hämtas via download_url (de ändras sällan).

#' Hämta en fil ur depot-repot
#'
#' @param rel_sokvag Filens sökväg inom repot, t.ex. `"fonts/Poppins.ttf"`.
#' @param target_path Lokal sökväg att spara till. `NULL` = returnera innehållet.
#' @param as_text `TRUE` för textfil, `FALSE` för binärfil.
#' @param repo,org,branch Repo, organisation och branch. Standard:
#'   `depot` / [rddeploy-config] `gh_org` / `main`.
#'
#' @return Om `target_path` är `NULL`: filinnehållet (sträng respektive `raw`).
#'   Annars osynligt `target_path`.
#' @export
depot_hamta_fran <- function(rel_sokvag,
                             target_path = NULL,
                             as_text     = TRUE,
                             repo        = "depot",
                             org         = intern_gh_org(),
                             branch      = "main") {
  tryCatch(rddeploy_pat(), error = function(e) NULL)

  if (as_text) {
    innehall <- tryCatch(
      gh::gh("GET /repos/{org}/{repo}/contents/{path}",
             org = org, repo = repo, path = rel_sokvag, ref = branch,
             .accept = "application/vnd.github.raw+json"),
      http_error_404 = function(e) cli::cli_abort(
        "Filen hittades inte i depot: {.path {rel_sokvag}} ({org}/{repo}@{branch})"
      )
    )
    innehall <- paste(as.character(innehall), collapse = "")
    if (is.null(target_path)) return(innehall)
    dir.create(dirname(target_path), recursive = TRUE, showWarnings = FALSE)
    writeLines(innehall, target_path, useBytes = TRUE)
    return(invisible(target_path))
  }

  # Binärfil
  meta <- tryCatch(
    gh::gh("GET /repos/{org}/{repo}/contents/{path}",
           org = org, repo = repo, path = rel_sokvag, ref = branch),
    http_error_404 = function(e) cli::cli_abort(
      "Filen hittades inte i depot: {.path {rel_sokvag}} ({org}/{repo}@{branch})"
    )
  )
  bytes <- curl::curl_fetch_memory(meta$download_url)$content
  if (is.null(target_path)) return(bytes)
  dir.create(dirname(target_path), recursive = TRUE, showWarnings = FALSE)
  writeBin(bytes, target_path)
  invisible(target_path)
}

#' Ladda ner alla filer i en mapp i depot-repot
#'
#' Laddar inte ner undermappar rekursivt.
#'
#' @param rel_mapp Mappens sökväg inom repot, t.ex. `"mallar/webbsida_portal"`.
#' @param target_dir Lokal mapp att spara filerna i.
#' @param as_text `TRUE` om filerna är textfiler, `FALSE` om binära (kan inte
#'   blandas i samma anrop).
#' @param repo,org,branch Se [depot_hamta_fran()].
#'
#' @return Osynligt: en teckenvektor med namnen på de nedladdade filerna.
#' @export
depot_hamta_mapp_fran <- function(rel_mapp,
                                  target_dir,
                                  as_text = FALSE,
                                  repo    = "depot",
                                  org     = intern_gh_org(),
                                  branch  = "main") {
  tryCatch(rddeploy_pat(), error = function(e) NULL)

  innehall <- tryCatch(
    gh::gh("GET /repos/{org}/{repo}/contents/{path}",
           org = org, repo = repo, path = rel_mapp, ref = branch),
    error = function(e) {
      cli::cli_warn("Kunde inte lista depot/{rel_mapp}/: {conditionMessage(e)}")
      list()
    }
  )
  filer <- purrr::keep(innehall, ~ identical(.x$type, "file"))
  if (length(filer) == 0) {
    cli::cli_warn("Mappen depot/{rel_mapp}/ är tom eller saknar filer.")
    return(invisible(character(0)))
  }

  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  namn <- purrr::map_chr(filer, "name")
  purrr::walk(namn, function(filnamn) {
    depot_hamta_fran(
      rel_sokvag  = paste0(rel_mapp, "/", filnamn),
      target_path = file.path(target_dir, filnamn),
      as_text     = as_text,
      repo = repo, org = org, branch = branch
    )
  })
  cli::cli_alert_success("Hämtade {length(namn)} fil(er) från depot/{rel_mapp}/.")
  invisible(namn)
}

#' Hämta en mallfil ur depot och fyll i `{{variabler}}`
#'
#' @param mallfil Filnamn inom `mallmapp`, t.ex. `"_quarto.yml.tmpl"`.
#' @param malfil Lokal sökväg den ifyllda filen skrivs till.
#' @param variabler Namngiven lista med värden, t.ex.
#'   `list(titel = "Min portal")`.
#' @param mallmapp Sökväg till mallmappen inom depot-repot.
#' @param repo,org,branch Se [depot_hamta_fran()].
#'
#' @return Osynligt: `malfil`.
#' @export
depot_skriv_mall_fran <- function(mallfil, malfil, variabler,
                                  mallmapp = "mallar/webbsida_portal",
                                  repo     = "depot",
                                  org      = intern_gh_org(),
                                  branch   = "main") {
  raw_mall <- depot_hamta_fran(
    rel_sokvag = paste0(mallmapp, "/", mallfil),
    as_text = TRUE, repo = repo, org = org, branch = branch
  )
  ifylld <- glue::glue(raw_mall, .open = "{{", .close = "}}",
                       .envir = list2env(variabler, parent = emptyenv()))
  dir.create(dirname(malfil), recursive = TRUE, showWarnings = FALSE)
  writeLines(ifylld, malfil, useBytes = TRUE)
  invisible(malfil)
}
