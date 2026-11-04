# Admin-styrda beskrivningsoverrides för landningssidans kort. En rad här
# vinner alltid över APP_BESKRIVNING/<meta description> i appens/rapportens
# egen kod - se generera_landningssida.R i repot landningssida. Används
# bl.a. för appar importerade från externa repon (t.ex. via git subtree,
# som ncdsim) där det inte går att - eller är olämpligt att - ändra
# källkoden för att få en beskrivning på landningssidan.

#' Lista aktuella beskrivningsoverrides för en server
#'
#' @param target `"publik"` eller `"intern"`.
#' @return En `data.frame` med `namn` och `beskrivning`.
#' @export
landningssida_beskrivning_lista <- function(target = c("publik", "intern")) {
  target <- intern_validera_target(match.arg(target))
  con <- intern_con_las(); on.exit(DBI::dbDisconnect(con), add = TRUE)

  df <- DBI::dbGetQuery(con, "
    SELECT namn, beskrivning FROM adminshiny.landningssida_beskrivning
    WHERE server = $1 ORDER BY namn", params = list(target))

  if (nrow(df) == 0) message("Inga manuella beskrivningskopplingar för ", target, ".")
  df
}

#' Sätt eller ändra en beskrivning för en app eller rapport
#'
#' @param target `"publik"` eller `"intern"`.
#' @param namn Mappnamnet.
#' @param beskrivning Fri text - en mening om vad appen/rapporten visar.
#' @param andrad_av Valfri identifierare (loggas i DB, bara `"publik"`).
#' @return Osynligt `TRUE`.
#' @export
landningssida_beskrivning_koppla <- function(target = c("publik", "intern"), namn, beskrivning,
                                             andrad_av = NA_character_) {
  target <- intern_validera_target(match.arg(target))
  stopifnot(is.character(namn), length(namn) == 1, nzchar(namn))
  stopifnot(is.character(beskrivning), length(beskrivning) == 1)
  beskrivning <- trimws(beskrivning)
  if (!nzchar(beskrivning)) {
    stop("Beskrivningen kan inte vara tom - använd landningssida_beskrivning_ta_bort_koppling() för att ta bort overriden.",
         call. = FALSE)
  }

  con <- intern_con_skriv(); on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "
    INSERT INTO adminshiny.landningssida_beskrivning (server, namn, beskrivning, satt_av)
    VALUES ($1, $2, $3, $4)
    ON CONFLICT (server, namn) DO UPDATE SET beskrivning = EXCLUDED.beskrivning, satt_tid = now()",
    params = list(target, namn, beskrivning, andrad_av))

  intern_efter_andring(target,
    paste0("Beskrivning kopplad (intern): ", namn, "."),
    paste0("Beskrivning kopplad (publik): ", namn, "."))
}

#' Ta bort en manuell beskrivningskoppling
#'
#' Återgår till appens/rapportens egen beskrivning i koden, om någon finns.
#'
#' @param target `"publik"` eller `"intern"`.
#' @param namn Mappnamnet (eller flera) vars koppling ska tas bort.
#' @return Osynligt `TRUE`.
#' @export
landningssida_beskrivning_ta_bort_koppling <- function(target = c("publik", "intern"), namn) {
  target <- intern_validera_target(match.arg(target))
  intern_validera_namn(namn)

  con <- intern_con_skriv(); on.exit(DBI::dbDisconnect(con), add = TRUE)
  borttagna <- 0
  for (n in namn) {
    borttagna <- borttagna + DBI::dbExecute(con, "
      DELETE FROM adminshiny.landningssida_beskrivning
      WHERE server = $1 AND namn = $2",
      params = list(target, n))
  }

  intern_efter_andring(target,
    paste0(borttagna, " beskrivningskoppling(ar) borttagna (intern)."),
    paste0(borttagna, " beskrivningskoppling(ar) borttagna (publik)."))
}
