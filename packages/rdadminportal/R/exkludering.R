# Exkludering: vilka appar/rapporter som INTE ska visas på landningssidan.

#' Lista exkluderade appar/rapporter
#'
#' @param target `"publik"` eller `"intern"`.
#' @return Teckenvektor med mappnamn (osynligt tom vektor om listan är tom).
#' @export
landningssida_lista_exkluderade <- function(target = c("publik", "intern")) {
  target <- intern_validera_target(match.arg(target))

  rader <- if (target == "publik") {
    con <- intern_con_las(); on.exit(DBI::dbDisconnect(con), add = TRUE)
    DBI::dbGetQuery(con, "
      SELECT namn FROM adminshiny.landningssida_exkludering
      WHERE server = 'publik' ORDER BY namn")$namn
  } else {
    r <- intern_lokalt_anrop("/usr/local/bin/hantera_exkludering.sh", "lista")
    r[nzchar(trimws(r))]
  }

  if (length(rader) == 0) {
    message("Exkluderingslistan för ", target, " är tom.")
    return(invisible(character(0)))
  }
  rader
}

#' Lägg till appar/rapporter i exkluderingslistan
#'
#' @param target `"publik"` eller `"intern"`.
#' @param namn Teckenvektor med mappnamn som ska exkluderas.
#' @param andrad_av Valfri identifierare för vem som gjorde ändringen (loggas i
#'   DB, bara `"publik"`).
#' @return Osynligt `TRUE`.
#' @export
landningssida_exkludera <- function(target = c("publik", "intern"), namn,
                                    andrad_av = NA_character_) {
  target <- intern_validera_target(match.arg(target))
  intern_validera_namn(namn)

  con <- intern_con_skriv(); on.exit(DBI::dbDisconnect(con), add = TRUE)
  for (n in namn) {
    DBI::dbExecute(con, "
      INSERT INTO adminshiny.landningssida_exkludering (server, namn, tillagd_av)
      VALUES ($1, $2, $3)
      ON CONFLICT (server, namn) DO NOTHING",
      params = list(target, n, andrad_av))
  }

  intern_efter_andring(target,
    paste0(length(namn), " app(ar) tillagda i exkluderingslistan (intern)."),
    paste0(length(namn), " app(ar) tillagda i exkluderingslistan (publik)."))
}

#' Ta bort appar/rapporter från exkluderingslistan
#'
#' @inheritParams landningssida_exkludera
#' @return Osynligt `TRUE`.
#' @export
landningssida_inkludera <- function(target = c("publik", "intern"), namn) {
  target <- intern_validera_target(match.arg(target))
  intern_validera_namn(namn)

  con <- intern_con_skriv(); on.exit(DBI::dbDisconnect(con), add = TRUE)
  borttagna <- 0
  for (n in namn) {
    borttagna <- borttagna + DBI::dbExecute(con, "
      DELETE FROM adminshiny.landningssida_exkludering
      WHERE server = $1 AND namn = $2",
      params = list(target, n))
  }

  intern_efter_andring(target,
    paste0(borttagna, " app(ar) borttagna från exkluderingslistan (intern)."),
    paste0(borttagna, " app(ar) borttagna från exkluderingslistan (publik)."))
}
