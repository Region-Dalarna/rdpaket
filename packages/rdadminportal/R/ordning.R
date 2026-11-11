# Ordningen på landningssidan: en handpåförd ordning inom appar respektive webbrapporter. Poster som saknar
# ordning kommer efter de ordnade, i alfabetisk ordning. PDF-rapporter sorteras på publiceringsdatum och
# påverkas inte. Tabell: adminshiny.landningssida_ordning (server, namn, ordning, andrad_av, andrad_tid).

# TRUE om tabellen finns. Översikten och generatorn ska fungera även innan tabellen skapats.
#' @noRd
intern_ordning_tabell_finns <- function(con) {
  isTRUE(DBI::dbGetQuery(con, "SELECT to_regclass('adminshiny.landningssida_ordning') IS NOT NULL AS finns")$finns)
}

# Låter den publika serverns lyssnare regenerera landningssidan (samma kanal och payload som databasens
# triggers använder för övriga landningssida-tabeller).
#' @noRd
intern_notifiera_publik <- function(con, tabell = "landningssida_ordning") {
  DBI::dbGetQuery(con, "SELECT pg_notify($1, $2)", params = list("adminportal_publik", tabell))
  invisible(TRUE)
}

#' Lista den handpåförda ordningen på landningssidan
#'
#' @param target `"publik"` eller `"intern"`.
#' @return En `data.frame` med `namn` och `ordning`, sorterad på `ordning`.
#'   Tom om ingen ordning är satt (eller tabellen saknas).
#' @export
landningssida_ordning_lista <- function(target = c("publik", "intern")) {
  target <- intern_validera_target(match.arg(target))
  con <- intern_con_las(); on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (!intern_ordning_tabell_finns(con)) return(data.frame(namn = character(0), ordning = integer(0)))
  DBI::dbGetQuery(con, "
    SELECT namn, ordning FROM adminshiny.landningssida_ordning
    WHERE server = $1 ORDER BY ordning, namn", params = list(target))
}

#' Sätt ordningen för en grupp av poster på landningssidan
#'
#' Posterna får ordningsnummer 1, 2, 3 ... i den ordning de anges. Ange hela
#' gruppen (alla appar, eller alla webbrapporter) i den ordning du vill ha dem,
#' eftersom numreringen börjar om från 1 för varje anrop. Poster som inte
#' nämns kommer efter de ordnade, alfabetiskt.
#'
#' @param target `"publik"` eller `"intern"`.
#' @param namn Teckenvektor med postnamn i önskad ordning.
#' @param andrad_av Valfri identifierare för vem som gjorde ändringen.
#' @return Osynligt `TRUE`.
#' @export
landningssida_ordning_satt <- function(target = c("publik", "intern"), namn,
                                       andrad_av = NA_character_) {
  target <- intern_validera_target(match.arg(target))
  intern_validera_landningssidenamn(namn)
  if (anyDuplicated(namn)) stop("namn får inte innehålla dubbletter.", call. = FALSE)

  con <- intern_con_skriv(); on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWithTransaction(con, {
    for (i in seq_along(namn)) {
      DBI::dbExecute(con, "
        INSERT INTO adminshiny.landningssida_ordning (server, namn, ordning, andrad_av, andrad_tid)
        VALUES ($1, $2, $3, $4, now())
        ON CONFLICT (server, namn) DO UPDATE
          SET ordning = EXCLUDED.ordning, andrad_av = EXCLUDED.andrad_av, andrad_tid = now()",
        params = list(target, namn[i], as.integer(i), andrad_av))
    }
  })

  if (target == "publik") intern_notifiera_publik(con)
  intern_efter_andring(target,
    paste0("Ordningen sparades för ", length(namn), " poster (intern)."),
    paste0("Ordningen sparades för ", length(namn), " poster (publik)."))
}

#' Ta bort den handpåförda ordningen
#'
#' @param target `"publik"` eller `"intern"`.
#' @param namn Poster att ta bort ordningen för, eller `NULL` (standard) för
#'   alla poster på servern. Posterna hamnar då i alfabetisk ordning.
#' @return Osynligt `TRUE`.
#' @export
landningssida_ordning_aterstall <- function(target = c("publik", "intern"), namn = NULL) {
  target <- intern_validera_target(match.arg(target))
  if (!is.null(namn)) intern_validera_landningssidenamn(namn)

  con <- intern_con_skriv(); on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (is.null(namn)) {
    DBI::dbExecute(con, "DELETE FROM adminshiny.landningssida_ordning WHERE server = $1", params = list(target))
  } else {
    for (n in namn) {
      DBI::dbExecute(con, "DELETE FROM adminshiny.landningssida_ordning WHERE server = $1 AND namn = $2",
                     params = list(target, n))
    }
  }

  if (target == "publik") intern_notifiera_publik(con)
  intern_efter_andring(target,
    "Ordningen återställdes (intern).",
    "Ordningen återställdes (publik).")
}
