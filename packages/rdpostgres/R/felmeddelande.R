#' Tolka ett PostgreSQL-fel till ett begripligt meddelande
#'
#' Känner igen behörighetsfel och föreslår en administrativ uppkoppling.
#'
#' @param e Ett fångat fel (condition).
#' @param atgard Kort beskrivning av vad som försöktes, t.ex.
#'   `"skapa schemat 'foo'"`.
#'
#' @return En teckensträng.
#' @export
postgres_felmeddelande <- function(e, atgard) {
  feltext <- conditionMessage(e)
  behorighet <- c("permission denied", "must have admin option", "must be member of role",
                  "not permitted", "saknar behörighet", "åtkomst nekas", "måste vara medlem")
  if (any(vapply(behorighet, function(m) grepl(m, feltext, ignore.case = TRUE), logical(1)))) {
    return(paste0(
      "Uppkopplingen saknar sannolikt behörighet för att ", atgard, ".\n\n",
      "PostgreSQL meddelade:\n", feltext, "\n\n",
      "Prova en administrativ uppkoppling, t.ex. uppkoppling_adm()."))
  }
  paste0("Ett fel uppstod vid försök att ", atgard, ".\n\nPostgreSQL meddelade:\n", feltext)
}

#' Logga en händelse till en loggfil
#'
#' @param meddelande Text att logga.
#' @param log_file Sökväg till loggfilen (rader läggs till).
#'
#' @return Osynligt `NULL`.
#' @export
logga_event <- function(meddelande, log_file) {
  rad <- paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), meddelande, sep = " - ")
  cat(rad, "\n", file = log_file, append = TRUE)
  invisible(NULL)
}
