# Interna git-hjälpare byggda på gert.

# Push mot GitHub via gert. credentials-paketet plockar upp GITHUB_PAT ur miljön
# automatiskt (rddeploy_pat() ser till att den finns).
intern_gh_push <- function(repo, branch,
                           set_upstream = FALSE,
                           force        = FALSE,
                           ta_bort      = FALSE) {

  if (!dir.exists(file.path(repo, ".git"))) {
    cli::cli_abort("{.arg repo} måste peka på ett lokalt git-repository: {.path {repo}}")
  }

  rddeploy_pat()

  refspec <- if (isTRUE(ta_bort)) {
    set_upstream <- FALSE
    paste0(":refs/heads/", branch)          # tom källa => radera ref på remote
  } else {
    paste0("refs/heads/", branch)
  }

  gert::git_push(
    remote       = "origin",
    refspec      = refspec,
    set_upstream = set_upstream,
    force        = force,
    repo         = repo
  )
  invisible(TRUE)
}

# Aktuell branch i ett repo.
intern_gh_branch <- function(repo) {
  gert::git_branch(repo = repo)
}

# Har repot en upstream/tracking-branch?
intern_har_upstream <- function(repo) {
  ib <- tryCatch(gert::git_branch_list(repo = repo), error = function(e) NULL)
  if (is.null(ib)) return(FALSE)
  rad <- ib[!ib$local & ib$name == paste0("origin/", intern_gh_branch(repo)), ]
  nrow(rad) > 0
}
