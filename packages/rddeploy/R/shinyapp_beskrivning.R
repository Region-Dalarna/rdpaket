# App-beskrivning for landningssidan. Kontrakt (delas med landningssida-repots
# generator): en app anger sin beskrivning som en rad i ui.R pa formen
#   APP_BESKRIVNING <- 'En mening om vad appen visar.'
# Landningssidan laser raden med en enkel regex: strangen far inte innehalla
# backslash eller radbrytning, och bortkommenterade rader ignoreras.

intern_beskrivning_placeholder <- "# APP_BESKRIVNING <- 'En mening om vad appen visar (visas på landningssidan).'"

# Validerar och citerar en text for infogning i ui.R. Enkla citattecken i
# normalfallet; dubbla om texten innehaller apostrof; om den da ocksa
# innehaller dubbla citattecken byts de inre forekomsterna mot ett
# typografiskt citattecken, sa att wrappningen - som landningssidans enkla
# regex kraver - forblir ett vanligt ASCII-citattecken.
intern_beskrivning_citera <- function(text) {
  if (grepl("\\", text, fixed = TRUE)) {
    cli::cli_abort("Beskrivningen får inte innehålla ett backslash-tecken (\\).")
  }
  if (grepl("[\r\n]", text)) {
    cli::cli_abort("Beskrivningen får inte innehålla radbrytning - skriv den som en sammanhängande mening.")
  }
  if (nchar(text) > 120) {
    cli::cli_warn("Beskrivningen är {nchar(text)} tecken lång - överväg att korta ner den (ca 120 tecken rekommenderas).")
  }

  har_apostrof    <- grepl("'", text, fixed = TRUE)
  har_dubbelcitat <- grepl('"', text, fixed = TRUE)

  if (!har_apostrof) {
    return(paste0("'", text, "'"))
  }
  if (!har_dubbelcitat) {
    return(paste0('"', text, '"'))
  }
  # bade apostrof och dubbla citattecken: behall dubbla citattecken som
  # wrapper, men byt de inre forekomsterna mot ett typografiskt citattecken
  text_typografisk <- gsub('"', "”", text, fixed = TRUE)
  paste0('"', text_typografisk, '"')
}

# Bygger hela ui.R-raden: en aktiv tilldelning om beskrivning ar angiven,
# annars en bortkommenterad paminnelse.
intern_beskrivning_rad <- function(beskrivning) {
  if (is.na(beskrivning) || !nzchar(trimws(beskrivning))) {
    return(intern_beskrivning_placeholder)
  }
  citerad <- intern_beskrivning_citera(trimws(beskrivning))
  paste0("APP_BESKRIVNING <- ", citerad)
}

#' Sätt eller uppdatera en Shiny-apps beskrivning för landningssidan
#'
#' Skriver (eller ersätter) raden `APP_BESKRIVNING <- '...'` i en befintlig
#' apps `ui.R`, lokalt. Finns raden redan aktiv (inte bortkommenterad)
#' ersätts den på plats. Annars söks en aktiv `APP_TITEL`-rad upp och den nya
#' raden infogas direkt efter den. Hittas ingen `APP_TITEL`-rad stoppar
#' funktionen och ber dig lägga till raden manuellt.
#'
#' Skriver bara till den lokala filen - committar, pushar eller publicerar
#' ingenting. Kör [github_commit_push()] och [shinyapp_publicera()] själv
#' efteråt.
#'
#' @param appnamn Appens mappnamn under `lokal_root` (= repo-namnet).
#' @param beskrivning En mening om vad appen visar.
#' @param lokal_root Lokal föräldermapp till appens repo. Standard:
#'   [rddeploy-config] `gh_mapp`.
#'
#' @return Osynligt: sökvägen till den uppdaterade `ui.R`.
#' @export
shinyapp_beskrivning_satt <- function(appnamn, beskrivning, lokal_root = intern_gh_mapp()) {
  if (missing(appnamn) || !nzchar(appnamn)) {
    cli::cli_abort("{.arg appnamn} måste anges.")
  }
  if (missing(beskrivning) || is.na(beskrivning) || !nzchar(trimws(beskrivning))) {
    cli::cli_abort("{.arg beskrivning} måste anges (en mening om vad appen visar).")
  }

  appmapp <- file.path(sub("/+$", "", lokal_root), appnamn)
  ui_fil  <- file.path(appmapp, "ui.R")
  if (!file.exists(ui_fil)) {
    cli::cli_abort("Hittar ingen {.file ui.R} i {.path {appmapp}}.")
  }

  ny_rad <- intern_beskrivning_rad(beskrivning)

  rader <- readLines(ui_fil, warn = FALSE, encoding = "UTF-8")

  regel_aktiv_beskrivning <- "^\\s*APP_BESKRIVNING\\s*(?:<-|=)"
  regel_aktiv_titel       <- "^\\s*APP_TITEL\\s*(?:<-|=)"

  idx_beskrivning <- which(grepl(regel_aktiv_beskrivning, rader, perl = TRUE))

  if (length(idx_beskrivning) > 0) {
    rader[idx_beskrivning[1]] <- ny_rad
    atgard <- "ersattes"
  } else {
    idx_titel <- which(grepl(regel_aktiv_titel, rader, perl = TRUE))
    if (length(idx_titel) == 0) {
      cli::cli_abort(c(
        "Hittar ingen aktiv {.code APP_TITEL}-rad i {.file {ui_fil}}.",
        "i" = "Lägg till raden manuellt, t.ex. där appens rubrik sätts:",
        " " = "{.code {ny_rad}}"
      ))
    }
    rader <- append(rader, ny_rad, after = idx_titel[1])
    atgard <- "lades till"
  }

  writeLines(rader, ui_fil, useBytes = TRUE)
  cli::cli_alert_success("Beskrivningen {atgard} i {.file {ui_fil}}.")
  cli::cli_alert_info("Lokal fil uppdaterad - committa/pusha själv med {.fn github_commit_push} och publicera med {.fn shinyapp_publicera}.")
  invisible(ui_fil)
}
