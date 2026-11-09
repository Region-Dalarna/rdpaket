# Schemalagd körning: genererar exempelbilder för de diag_*()/karta_*()-funktioner i
# rddiagramkatalog som saknar en, och pushar dem automatiskt till GitHub om något nytt skapades.
#
# Tänkt att köras regelbundet (t.ex. en gång i veckan) via Schemaläggaren, på en maskin/VM med
# åtkomst till G:-enheten och databasen - annars missas exempelbilder för de diagram/kartor som
# behöver det (skapa_exempeldiagram.R hoppar bara tyst över dem med ett FEL-meddelande i
# sammanfattningen, se dess egen källkod).
#
# Körs INTE som en vanlig GitHub Actions-körning (molnbaserade runners når varken G:-enheten eller
# databasen) - se KONVENTIONER.md och skapa_exempeldiagram.R:s egen kommentar om varför.
#
# Förutsättningar på maskinen som kör skriptet:
# - En EGEN, dedikerad git-klon av Region-Dalarna/rdpaket på sökvägen nedan - inte samma
#   arbetskopia som någon använder interaktivt, för att undvika att skriptet råkar committa
#   någons pågående, ocommittade arbete.
# - git push konfigurerat att fungera icke-interaktivt för det kontot Schemaläggaren kör som
#   (en SSH-nyckel utan lösenfras, eller en sparad personal access token via credential helper).
# - R och Rtools (för R CMD INSTALL) installerat och i PATH.
#
# Schemaläggaren: skapa en uppgift vars åtgärd kör
#   Rscript.exe "C:\sokvag\till\rdpaket\packages\rddiagramkatalog\data-raw\schemalagd_bildgenerering.R"
# Körs oberoende av inloggad användare (samma mönster som era andra schemalagda R-skript).

repo_sokvag <- "C:/sokvag/till/rdpaket"   # ÄNDRA till den dedikerade klonens faktiska sökväg

logfil <- file.path(repo_sokvag, "schemalagd_bildgenerering.log")

logga <- function(...) {
  rad <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", paste(..., collapse = " "))
  cat(rad, "\n")
  cat(rad, "\n", file = logfil, append = TRUE)
}

kor_kommando <- function(kommando, argument) {
  utdata <- tryCatch(
    system2(kommando, argument, stdout = TRUE, stderr = TRUE),
    warning = function(w) paste("VARNING (icke-nollstatus):", conditionMessage(w))
  )
  logga(paste(c(paste(kommando, paste(argument, collapse = " ")), utdata), collapse = "\n  "))
  invisible(utdata)
}

tryCatch({

  logga("=== Startar schemalagd bildgenerering ===")

  if (!dir.exists(repo_sokvag)) {
    stop("Hittar inte repo-klonen på: ", repo_sokvag, ". Kontrollera repo_sokvag i skriptet.")
  }
  setwd(repo_sokvag)

  logga("Hämtar senaste från GitHub (git pull --ff-only)...")
  kor_kommando("git", c("pull", "--ff-only"))

  logga("Installerar rddiagramkatalog och dess paketberoenden (i beroendeordning)...")
  for (paket in c("rdverktyg", "rdhamtadata", "rddiagram", "rdgis", "rddiagramkatalog")) {
    kor_kommando("R", c("CMD", "INSTALL", "--no-byte-compile", "--no-test-load",
                        file.path("packages", paket)))
  }

  logga("Genererar exempelbilder (skriv_over_befintliga_bilder = FALSE - bara saknade skapas)...")
  source("packages/rddiagramkatalog/data-raw/skapa_exempeldiagram.R")

  logga("Kollar om några bilder faktiskt skapades/ändrades...")
  status <- kor_kommando("git", c("status", "--porcelain", "--",
                                  "packages/rddiagramkatalog/man/figures"))

  if (length(status) > 0 && any(nzchar(status))) {
    logga("Nya/ändrade bilder hittades - committar och pushar.")
    kor_kommando("git", c("add", "packages/rddiagramkatalog/man/figures"))
    kor_kommando("git", c("commit", "-m",
      "rddiagramkatalog: automatisk uppdatering av exempelbilder (schemalagd körning)"))
    kor_kommando("git", c("push"))
  } else {
    logga("Inga nya bilder - inget att committa.")
  }

  logga("=== Klart ===")

}, error = function(e) {
  logga("FEL - körningen avbröts:", conditionMessage(e))
})
