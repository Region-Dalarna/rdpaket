# Nedladdningsfiler i www/nedladdning/ för Shiny-appar.

intern_validera_filnamn <- function(filnamn) {
  if (!grepl("^[a-zA-Z0-9_.-]+$", filnamn) || grepl("\\.\\.", filnamn) ||
      startsWith(filnamn, ".")) {
    stop("Ogiltigt filnamn: '", filnamn, "'", call. = FALSE)
  }
  invisible(TRUE)
}

#' Synka nedladdningsfiler mot filsystemet på DENNA server
#'
#' Skannar `www/nedladdning/` för alla aktiva appar och synkar mot
#' `adminshiny.landningssida_nedladdning`. Körs lokalt.
#'
#' @param target `"publik"` eller `"intern"`.
#' @param shiny_rot Katalog att skanna.
#'
#' @return Osynligt: en lista med `funna`, `borttagna`.
#' @export
landningssida_synka_nedladdning <- function(target = c("publik", "intern"),
                                            shiny_rot = "/srv/shiny-server") {
  target <- intern_validera_target(match.arg(target))
  con <- intern_con_skriv(); on.exit(DBI::dbDisconnect(con), add = TRUE)

  aktiva <- DBI::dbGetQuery(con, "
    SELECT namn FROM adminshiny.landningssida_app
    WHERE server = $1 AND typ = 'app' AND status = 'aktiv'", params = list(target))$namn

  hittade <- list()
  for (app in aktiva) {
    mapp <- file.path(shiny_rot, app, "www", "nedladdning")
    if (!dir.exists(mapp)) next
    filer <- list.files(mapp, full.names = FALSE, no.. = TRUE)
    filer <- filer[!file.info(file.path(mapp, filer))$isdir]
    if (length(filer) == 0) next
    info <- file.info(file.path(mapp, filer))
    hittade[[length(hittade) + 1]] <- data.frame(
      app = app, filnamn = filer,
      storlek_bytes = info$size, senast_andrad = info$mtime,
      stringsAsFactors = FALSE)
  }

  funna <- if (length(hittade)) do.call(rbind, hittade) else
    data.frame(app = character(0), filnamn = character(0),
               storlek_bytes = numeric(0), senast_andrad = as.POSIXct(character(0)))

  for (i in seq_len(nrow(funna))) {
    DBI::dbExecute(con, "
      INSERT INTO adminshiny.landningssida_nedladdning
        (server, app, filnamn, storlek_bytes, senast_andrad, uppdaterad_tid)
      VALUES ($1, $2, $3, $4, $5, now())
      ON CONFLICT (server, app, filnamn) DO UPDATE
        SET storlek_bytes = EXCLUDED.storlek_bytes,
            senast_andrad = EXCLUDED.senast_andrad, uppdaterad_tid = now()",
      params = list(target, funna$app[i], funna$filnamn[i],
                    funna$storlek_bytes[i], funna$senast_andrad[i]))
  }

  befintliga <- DBI::dbGetQuery(con, "
    SELECT app, filnamn FROM adminshiny.landningssida_nedladdning WHERE server = $1",
    params = list(target))

  borttagna <- 0
  if (nrow(befintliga) > 0) {
    b_nyckel <- paste(befintliga$app, befintliga$filnamn, sep = "/")
    f_nyckel <- if (nrow(funna)) paste(funna$app, funna$filnamn, sep = "/") else character(0)
    for (i in which(!b_nyckel %in% f_nyckel)) {
      DBI::dbExecute(con, "
        DELETE FROM adminshiny.landningssida_nedladdning
        WHERE server = $1 AND app = $2 AND filnamn = $3",
        params = list(target, befintliga$app[i], befintliga$filnamn[i]))
      borttagna <- borttagna + 1
    }
  }

  message("Nedladdning-synk (", target, "): ", nrow(funna), " fil(er) funna, ",
          borttagna, " borttagna.")
  invisible(list(funna = nrow(funna), borttagna = borttagna))
}

#' Aggregerad översikt över nedladdningsfiler (en rad per app)
#'
#' @param target `"publik"` eller `"intern"`.
#' @return En `data.frame` med `app`, `antal_filer`, `senaste_andring`,
#'   `total_storlek`.
#' @export
landningssida_nedladdning_lista <- function(target = c("publik", "intern")) {
  target <- intern_validera_target(match.arg(target))
  con <- intern_con_las(); on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbGetQuery(con, "
    SELECT app, count(*) AS antal_filer, max(senast_andrad) AS senaste_andring,
           sum(storlek_bytes) AS total_storlek
    FROM adminshiny.landningssida_nedladdning
    WHERE server = $1 GROUP BY app ORDER BY app", params = list(target))
}

#' Detaljlista över nedladdningsfiler för en app
#'
#' @param target `"publik"` eller `"intern"`.
#' @param app Appnamnet.
#' @return En `data.frame` med `filnamn`, `storlek_bytes`, `senast_andrad`.
#' @export
landningssida_nedladdning_lista_filer <- function(target = c("publik", "intern"), app) {
  target <- intern_validera_target(match.arg(target))
  intern_validera_namn(app)
  con <- intern_con_las(); on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbGetQuery(con, "
    SELECT filnamn, storlek_bytes, senast_andrad
    FROM adminshiny.landningssida_nedladdning
    WHERE server = $1 AND app = $2 ORDER BY filnamn", params = list(target, app))
}

#' Namn på appens aktiva cron-jobb (för varningstext innan radering)
#'
#' @param target `"publik"` eller `"intern"`.
#' @param app Appnamnet.
#' @return Teckenvektor med jobbnamn (tom om inga).
#' @export
landningssida_nedladdning_har_aktivt_cronjobb <- function(target = c("publik", "intern"), app) {
  target <- intern_validera_target(match.arg(target))
  intern_validera_namn(app)
  con <- tryCatch(intern_con_las(), error = function(e) NULL)
  if (is.null(con)) return(character(0))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbGetQuery(con, "
    SELECT namn FROM adminshiny.cron_jobb
    WHERE server = $1 AND app = $2 AND aktiv = TRUE", params = list(target, app))$namn
}

# Lokal radering (körs på "intern" direkt, "publik" via lyssnarens kommandokö).
intern_ta_bort_fil <- function(app, filnamn, shiny_rot = "/srv/shiny-server") {
  intern_validera_namn(app)
  intern_validera_filnamn(filnamn)
  mapp <- file.path(shiny_rot, app, "www", "nedladdning")
  fil  <- file.path(mapp, filnamn)
  if (!file.exists(fil)) stop("Filen finns inte: ", fil, call. = FALSE)
  if (!file.remove(fil)) stop("Kunde inte ta bort filen: ", fil, call. = FALSE)
  tom <- length(list.files(mapp)) == 0
  if (tom) unlink(mapp, recursive = TRUE)
  invisible(tom)
}

intern_ta_bort_mapp <- function(app, shiny_rot = "/srv/shiny-server") {
  intern_validera_namn(app)
  mapp <- file.path(shiny_rot, app, "www", "nedladdning")
  if (dir.exists(mapp)) unlink(mapp, recursive = TRUE)
  invisible(TRUE)
}

#' Ta bort en enskild nedladdningsfil (och mappen om den blir tom)
#'
#' @param target `"publik"` eller `"intern"`.
#' @param app Appnamnet.
#' @param filnamn Filnamnet.
#' @param andrad_av Valfri identifierare (bara `"publik"`).
#' @return Osynligt `TRUE`.
#' @export
landningssida_nedladdning_ta_bort_fil <- function(target = c("publik", "intern"), app,
                                                  filnamn, andrad_av = NA_character_) {
  target <- intern_validera_target(match.arg(target))
  intern_validera_namn(app)
  intern_validera_filnamn(filnamn)

  con <- intern_con_skriv(); on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (target == "intern") {
    tom <- intern_ta_bort_fil(app, filnamn)
    DBI::dbExecute(con, "
      DELETE FROM adminshiny.landningssida_nedladdning
      WHERE server = $1 AND app = $2 AND filnamn = $3",
      params = list(target, app, filnamn))
    message("Filen borttagen (intern)",
            if (tom) " - mappen var tom och togs också bort." else ".")
  } else {
    DBI::dbExecute(con, "
      INSERT INTO adminshiny.kommando (server, typ, payload, skapad_av)
      VALUES ('publik', 'ta_bort_nedladdningsfil', $1, $2)",
      params = list(jsonlite::toJSON(list(app = app, filnamn = filnamn), auto_unbox = TRUE),
                    andrad_av))
    message("Borttagning köad (publik) - klar inom några sekunder.")
  }
  invisible(TRUE)
}

#' Ta bort hela nedladdningsmappen för en app
#'
#' @param target `"publik"` eller `"intern"`.
#' @param app Appnamnet.
#' @param andrad_av Valfri identifierare (bara `"publik"`).
#' @return Osynligt `TRUE`.
#' @export
landningssida_nedladdning_ta_bort_mapp <- function(target = c("publik", "intern"), app,
                                                   andrad_av = NA_character_) {
  target <- intern_validera_target(match.arg(target))
  intern_validera_namn(app)

  con <- intern_con_skriv(); on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (target == "intern") {
    intern_ta_bort_mapp(app)
    DBI::dbExecute(con, "
      DELETE FROM adminshiny.landningssida_nedladdning WHERE server = $1 AND app = $2",
      params = list(target, app))
    message("Nedladdningsmappen borttagen (intern).")
  } else {
    DBI::dbExecute(con, "
      INSERT INTO adminshiny.kommando (server, typ, payload, skapad_av)
      VALUES ('publik', 'ta_bort_nedladdningsmapp', $1, $2)",
      params = list(jsonlite::toJSON(list(app = app), auto_unbox = TRUE), andrad_av))
    message("Borttagning köad (publik) - klar inom några sekunder.")
  }
  invisible(TRUE)
}
