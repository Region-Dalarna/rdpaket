# Analysrapporter: gör ett befintligt rapportrepo publicerbart, sätt beskrivning för landningssidan och
# kontroller som analysrapport_publicera() gör före publicering.

# Deploynamnet: det namn som styr källfilen (<namn>.html), servermappen/adressen och landningssidans namn.
# Normalt reponamnet; en fil .rapport_deploy_namn i repo-roten ersätter det (se deploy.yml).
intern_analysrapport_deploynamn <- function(repo_dir, rapport_repo) {
  fil <- file.path(repo_dir, ".rapport_deploy_namn")
  if (!file.exists(fil)) return(rapport_repo)
  namn <- gsub("[[:space:]]", "", paste(readLines(fil, n = 1, warn = FALSE), collapse = ""))
  if (nzchar(namn)) namn else rapport_repo
}

intern_validera_deploynamn <- function(namn) {
  if (!is.character(namn) || length(namn) != 1 || is.na(namn) || !grepl("^[A-Za-z0-9_-]+$", namn)) {
    cli::cli_abort("Deploynamnet får bara innehålla bokstäver (a-z, A-Z), siffror, _ och -: {.val {namn}}")
  }
  invisible(namn)
}

#' Gör ett befintligt rapportrepo publicerbart på Shinyservern
#'
#' Lägger till `deploy.yml` och `avpublicera.yml` i `.github/workflows/` (samma
#' mallar som nya analysrapporter får) och, om rapportens html inte heter som
#' repot, en fil `.rapport_deploy_namn` med **deploynamnet**.
#'
#' Deploynamnet styr tre saker: vilken fil i repo-roten som publiceras
#' (`<deploynamn>.html`), mappen på servern (och därmed adressen
#' `shiny.regiondalarna.se/<deploynamn>/`) och rapportens namn på landningssidan
#' och i adminportalen. Normalt är det reponamnet. Sätt `deploynamn` när htmlen
#' heter något annat än repot, eller när du vill behålla en befintlig adress.
#' Namnet måste stämma exakt (versaler/gemener) med html-filens namn.
#'
#' Mappen `underrapporter/` i repot (om den finns) följer med vid deploy och
#' hamnar under rapportens adress, utan eget kort på landningssidan.
#'
#' Skriver bara lokala filer - committa och publicera själv efteråt, med
#' [github_commit_push()] och [analysrapport_publicera()].
#'
#' @param rapport_repo Reponamn.
#' @param deploynamn Deploynamn (se ovan) eller `NULL` = reponamnet. Skrivs till
#'   `.rapport_deploy_namn` om det skiljer sig från reponamnet.
#' @param grundsokvag Lokal föräldermapp. Standard: [rddeploy-config] `gh_mapp`.
#' @param skriv_over `TRUE` för att ersätta workflow-filer som redan finns och
#'   skiljer sig från mallen. Standard `FALSE`.
#'
#' @return Osynligt: sökvägarna till de filer som skrevs.
#' @export
analysrapport_gor_publicerbar <- function(rapport_repo,
                                        deploynamn  = NULL,
                                        grundsokvag = intern_gh_mapp(),
                                        skriv_over  = FALSE) {
  repo_dir <- file.path(grundsokvag, rapport_repo)
  if (!dir.exists(file.path(repo_dir, ".git"))) {
    cli::cli_abort("Hittar inget git-repo i {.path {repo_dir}}")
  }
  if (!is.null(deploynamn)) intern_validera_deploynamn(deploynamn)

  skrivna <- character(0)
  for (fil in c("deploy.yml", "avpublicera.yml")) {
    mal <- file.path(repo_dir, ".github", "workflows", fil)
    ny  <- intern_las_mall("analysrapport", fil)
    if (file.exists(mal)) {
      nuvarande <- paste(readLines(mal, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      if (identical(trimws(nuvarande), trimws(ny))) {
        cli::cli_alert_info("{.file {fil}} är redan aktuell.")
        next
      }
      if (!isTRUE(skriv_over)) {
        cli::cli_alert_warning("{.file {fil}} finns redan och skiljer sig från mallen - lämnas orörd (använd {.code skriv_over = TRUE} för att ersätta).")
        next
      }
    }
    intern_skriv_mall("analysrapport", fil, mal)
    skrivna <- c(skrivna, mal)
    cli::cli_alert_success("Skrev {.path {mal}}")
  }

  namn_fil <- file.path(repo_dir, ".rapport_deploy_namn")
  if (!is.null(deploynamn) && !identical(deploynamn, rapport_repo)) {
    writeLines(deploynamn, namn_fil)
    skrivna <- c(skrivna, namn_fil)
    cli::cli_alert_success("Deploynamn {.val {deploynamn}} skrivet till {.file .rapport_deploy_namn}")
  } else if (file.exists(namn_fil)) {
    cli::cli_alert_info("{.file .rapport_deploy_namn} finns redan ({.val {intern_analysrapport_deploynamn(repo_dir, rapport_repo)}}) och lämnas orörd.")
  }

  namn <- intern_analysrapport_deploynamn(repo_dir, rapport_repo)
  if (!file.exists(file.path(repo_dir, paste0(namn, ".html")))) {
    cli::cli_alert_warning(c(
      "Hittar inte {.file {namn}.html} i repo-roten - deployen stoppar utan den. ",
      "Knitta rapporten, eller ange rätt {.arg deploynamn}."))
  }

  cli::cli_alert_info("Nästa steg: committa och pusha, och kör sedan {.code analysrapport_publicera(\"{rapport_repo}\")}.")
  invisible(skrivna)
}

# Markörer för beskrivningsblocket i hero_image.html.
intern_beskrivning_start <- "<!-- beskrivning:start -->"
intern_beskrivning_slut  <- "<!-- beskrivning:slut -->"

#' Sätt eller ta bort en analysrapports beskrivning för landningssidan
#'
#' Skriver en `<meta name="description">`-rad överst i rapportens
#' `hero_image.html`, som redan ingår i rapportens `<head>`. Landningssidans
#' generator läser den ur den publicerade htmlen och visar den på rapportens
#' kort. Beskrivningen följer alltså med rapporten vid varje omknittning.
#'
#' En beskrivning som skrivits i adminportalen går före den här. Välj en
#' källa - helst den här - och använd adminportalen bara för undantag.
#'
#' Skriver bara den lokala filen. Knitta om rapporten, committa och publicera
#' efteråt.
#'
#' @param rapport_repo Reponamn.
#' @param beskrivning En eller två meningar (ca 120-200 tecken). `NULL`, `NA`
#'   eller `""` tar bort beskrivningen.
#' @param grundsokvag Lokal föräldermapp. Standard: [rddeploy-config] `gh_mapp`.
#'
#' @return Osynligt: sökvägen till `hero_image.html`.
#' @export
analysrapport_beskrivning_satt <- function(rapport_repo, beskrivning, grundsokvag = intern_gh_mapp()) {
  fil <- file.path(grundsokvag, rapport_repo, "hero_image.html")
  if (!file.exists(fil)) {
    cli::cli_abort(c("Hittar inte {.path {fil}}.",
                     "i" = "Beskrivningen skrivs i {.file hero_image.html}, som rapportens Rmd inkluderar i {.code in_header}."))
  }
  text <- readLines(fil, warn = FALSE, encoding = "UTF-8")

  # ta bort ett ev. befintligt block
  start <- which(text == intern_beskrivning_start)
  slut  <- which(text == intern_beskrivning_slut)
  if (length(start) == 1 && length(slut) == 1 && slut >= start) text <- text[-(start:slut)]

  ny <- intern_beskrivning_block(beskrivning)
  ingen <- is.null(ny)
  text <- c(ny, text)

  con <- file(fil, "wb")
  on.exit(close(con))
  writeLines(enc2utf8(text), con, useBytes = TRUE)
  if (ingen) cli::cli_alert_success("Beskrivningen borttagen ur {.path {fil}}.")
  else cli::cli_alert_success("Beskrivning skriven till {.path {fil}}. Knitta om rapporten och publicera.")
  invisible(fil)
}

# Beskrivningsblocket (markörer + meta-tagg) som textrader, eller NULL om beskrivningen är tom.
intern_beskrivning_block <- function(beskrivning) {
  ingen <- is.null(beskrivning) || length(beskrivning) == 0 || is.na(beskrivning[1]) || !nzchar(trimws(beskrivning[1]))
  if (ingen) return(NULL)
  beskrivning <- trimws(gsub("[[:space:]]+", " ", beskrivning[1]))
  if (nchar(beskrivning) > 200) {
    cli::cli_warn("Beskrivningen är {nchar(beskrivning)} tecken lång - kortet visar ca 3 rader, överväg att korta ner den.")
  }
  esk <- gsub("\"", "&quot;", gsub(">", "&gt;", gsub("<", "&lt;", gsub("&", "&amp;", beskrivning, fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE)
  c(intern_beskrivning_start, paste0("<meta name=\"description\" content=\"", esk, "\">"), intern_beskrivning_slut)
}

# Beskrivningen som står i repots hero_image.html (blocket som analysrapport_beskrivning_satt() skriver), som vanlig
# text, eller NULL om den inte finns.
intern_las_hero_beskrivning <- function(repo_dir) {
  fil <- file.path(repo_dir, "hero_image.html")
  if (!file.exists(fil)) return(NULL)
  text <- readLines(fil, warn = FALSE, encoding = "UTF-8")
  start <- which(text == intern_beskrivning_start)
  if (length(start) != 1 || length(text) < start + 1) return(NULL)
  m <- regmatches(text[start + 1], regexec("content=\"(.*)\">$", text[start + 1]))[[1]]
  if (length(m) < 2) return(NULL)
  x <- m[2]
  for (par in list(c("&quot;", "\""), c("&lt;", "<"), c("&gt;", ">"), c("&amp;", "&"))) x <- gsub(par[1], par[2], x, fixed = TRUE)
  x
}

#' Sätt eller ta bort beskrivningen i en färdig html-fil
#'
#' Som [analysrapport_beskrivning_satt()], men skriver direkt i en redan
#' knittad html-fil i stället för i `hero_image.html`, så att du slipper knitta
#' om. Passar t.ex. en html som kopierats från `publicera_rapporter/docs`
#' under migreringen. Beskrivningen läggs som en `<meta name="description">`
#' direkt efter `<head>`, och en `description` som redan finns i filen tas
#' bort så att det bara finns en. Att köra funktionen igen ersätter den.
#'
#' Knittas rapporten om försvinner beskrivningen ur htmlen, om den inte också
#' ligger i `hero_image.html` (se [analysrapport_beskrivning_satt()]).
#'
#' @param html_fil Sökväg till html-filen. Skrivs över på plats.
#' @param beskrivning En eller två meningar (ca 120-200 tecken). `NULL`, `NA`
#'   eller `""` tar bort beskrivningen.
#'
#' @return Osynligt: `html_fil`.
#' @seealso [analysrapport_publicera()] (`html_fil` och `beskrivning`).
#' @export
analysrapport_html_beskrivning_satt <- function(html_fil, beskrivning) {
  if (!is.character(html_fil) || length(html_fil) != 1 || !file.exists(html_fil)) {
    cli::cli_abort("Hittar inte html-filen: {.path {html_fil}}")
  }
  # filen läses som en enda sträng (bytes) så att utf-8 och radbrytningar bevaras exakt
  bytes <- readBin(html_fil, "raw", file.info(html_fil)$size)
  text <- rawToChar(bytes)
  Encoding(text) <- "UTF-8"

  blockmonster <- paste0("(?s)", intern_beskrivning_start, ".*?", intern_beskrivning_slut, "\\r?\\n?")
  text <- gsub(blockmonster, "", text, perl = TRUE)
  text <- gsub("(?is)<meta\\b[^>]*\\bname\\s*=\\s*([\"'])description\\1[^>]*>[ \\t]*\\r?\\n?", "", text, perl = TRUE)

  ny <- intern_beskrivning_block(beskrivning)
  if (!is.null(ny)) {
    if (!grepl("(?i)<head\\b[^>]*>", text, perl = TRUE)) {
      cli::cli_abort("Hittar ingen {.code <head>} i {.path {html_fil}} - är det en färdig html-fil?")
    }
    text <- sub("(?i)(<head\\b[^>]*>)", paste0("\\1\n", gsub("\\", "\\\\", paste(ny, collapse = "\n"), fixed = TRUE)), text, perl = TRUE)
  }

  writeBin(charToRaw(enc2utf8(text)), html_fil)
  if (is.null(ny)) cli::cli_alert_success("Beskrivningen borttagen ur {.path {html_fil}}.")
  else cli::cli_alert_success("Beskrivning skriven i {.path {html_fil}}.")
  invisible(html_fil)
}

# TRUE om rapportens html är äldre (mer än `tolerans` sekunder) än dess Rmd/qmd-källa. Källfilen letas upp
# med reponamnet eller deploynamnet som basnamn (versaler spelar ingen roll). Hittas ingen källa är svaret
# FALSE (kan inte jämföras). Filtider bevaras inte av git, därför toleransen.
intern_analysrapport_html_foraldrad <- function(repo_dir, rapport_repo, tolerans = 60) {
  namn <- intern_analysrapport_deploynamn(repo_dir, rapport_repo)
  filer <- list.files(repo_dir, full.names = TRUE)
  basnamn <- tolower(basename(filer))
  html <- filer[basnamn == tolower(paste0(namn, ".html"))]
  kalla <- filer[basnamn %in% tolower(c(paste0(c(namn, rapport_repo), ".Rmd"), paste0(c(namn, rapport_repo), ".qmd")))]
  if (length(html) == 0 || length(kalla) == 0) return(FALSE)
  as.numeric(difftime(max(file.mtime(kalla)), file.mtime(html[1]), units = "secs")) > tolerans
}
