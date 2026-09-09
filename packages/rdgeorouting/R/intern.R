# Delade hjälpare.
#
# Ruttanalyserna körs mot Region Dalarnas databas "ruttanalyser" (pgRouting)
# respektive "geodata"/"sekretess". con kan skickas som ett DBIConnection-objekt
# eller som ett databasnamn (sträng) - i det senare fallet kopplas det upp via
# rdpostgres och keyring-service "rd_geodata".

intern_krav <- function(paket) {
  if (!requireNamespace(paket, quietly = TRUE)) {
    stop("Paketet '", paket, "' krävs för den här funktionen.", call. = FALSE)
  }
}

# Dölj ett specifikt (ofarligt) varningsmeddelande.
intern_tyst_varning <- function(expr, monster) {
  withCallingHandlers(expr, warning = function(w) {
    if (grepl(monster, conditionMessage(w), fixed = TRUE)) invokeRestart("muffleWarning")
  })
}

# Löser upp con: DBIConnection -> använd; NA/NULL -> standarddatabas; sträng ->
# koppla upp via rdpostgres. Returnerar list(con, egen).
intern_rutt_con <- function(con, adm = TRUE, standard_db = "ruttanalyser") {
  if (inherits(con, "DBIConnection")) return(list(con = con, egen = FALSE))

  db <- if (is.null(con) || (length(con) == 1 && is.character(con) && !nzchar(con)) ||
            (length(con) == 1 && !is.character(con) && is.na(con))) {
    standard_db
  } else {
    con
  }
  ny <- if (adm) {
    rdpostgres::uppkoppling_adm(db)
  } else {
    rdpostgres::uppkoppling_db(db_name = db, service_name = "rd_geodata")
  }
  if (is.null(ny)) {
    stop("Kunde inte ansluta till databasen '", db, "'.", call. = FALSE)
  }
  list(con = ny, egen = TRUE)
}

intern_stang <- function(c) {
  if (isTRUE(c$egen) && DBI::dbIsValid(c$con)) DBI::dbDisconnect(c$con)
}

# Mutabelt tillstånd för metadata-loggning genom tryCatch (ersätter <<- mot
# global environment i originalet).
intern_meta_state <- function() {
  e <- new.env(parent = emptyenv())
  e$lyckad <- FALSE
  e$kommentar <- NA_character_
  e
}

# Kort versionsstämpel, t.ex. "9sep2026_1430".
intern_ver_stampel <- function(tid = Sys.time()) {
  paste0(as.integer(format(tid, "%d")),
         tolower(format(tid, "%b%Y")), "_",
         format(tid, "%H%M"))
}
