# Generell administration av PostGIS-tabeller och databastillägg.

intern_postgis_con <- function(con = "default") {
  if (inherits(con, "DBIConnection")) return(list(con = con, egen = FALSE))
  db <- if (is.null(con) ||
            (length(con) == 1 && is.character(con) && !nzchar(con)) ||
            (length(con) == 1 && !is.character(con) && is.na(con))) {
    "geodata"
  } else {
    con
  }
  ny <- uppkoppling_db(db_name = db, service_name = "rd_geodata")
  if (is.null(ny)) {
    stop("Kunde inte ansluta till databasen '", db, "'.", call. = FALSE)
  }
  list(con = ny, egen = TRUE)
}

#' Aktivera PostGIS-tillägget i en databas
#'
#' @param con En aktiv `DBIConnection`.
#' @return Osynligt `NULL`.
#' @export
postgis_aktivera_i_postgres_db <- function(con) {
  tryCatch({
    DBI::dbExecute(con, "CREATE EXTENSION IF NOT EXISTS postgis;")
    message("PostGIS-tillägget har aktiverats i databasen.")
  }, error = function(e) message("Kunde inte aktivera PostGIS-tillägget: ", conditionMessage(e)))
  invisible(NULL)
}

#' @rdname postgis_aktivera_i_postgres_db
#' @export
postgis_installera_i_postgres_db <- postgis_aktivera_i_postgres_db

#' Kopiera en tabell inom en PostGIS-databas
#'
#' @param con En `DBIConnection` eller `"default"`.
#' @param schema_fran,tabell_fran Källa.
#' @param schema_till,tabell_till Mål.
#' @param skriv_over Skriv över måltabellen om den finns.
#'
#' @return Osynligt `NULL`.
#' @export
postgis_kopiera_tabell <- function(con = "default", schema_fran, tabell_fran,
                                   schema_till, tabell_till, skriv_over = FALSE) {
  cc <- intern_postgis_con(con)
  on.exit(intern_stang(cc), add = TRUE)
  con <- cc$con

  if (!postgres_tabell_finns(con = con, schema = schema_fran, tabell = tabell_fran)) {
    stop("Tabellen ", schema_fran, ".", tabell_fran, " finns inte.", call. = FALSE)
  }
  if (postgres_tabell_finns(con = con, schema = schema_till, tabell = tabell_till)) {
    if (skriv_over) {
      postgres_tabell_ta_bort(con = con, schema = schema_till, tabell = tabell_till)
    } else {
      stop("Tabellen ", schema_till, ".", tabell_till, " finns redan (sätt skriv_over = TRUE).", call. = FALSE)
    }
  }
  DBI::dbExecute(con, glue::glue_sql(
    "CREATE TABLE {`schema_till`}.{`tabell_till`} (LIKE {`schema_fran`}.{`tabell_fran`} INCLUDING ALL);", .con = con))
  DBI::dbExecute(con, glue::glue_sql(
    "INSERT INTO {`schema_till`}.{`tabell_till`} SELECT * FROM {`schema_fran`}.{`tabell_fran`};", .con = con))
  invisible(NULL)
}

#' Flytta en tabell från ett schema till ett annat
#'
#' @param con En `DBIConnection` eller `"default"`.
#' @param schema_fran,tabell_fran Källa.
#' @param schema_till Målschema.
#'
#' @return Osynligt `NULL`.
#' @export
postgis_flytta_tabell <- function(con = "default", schema_fran, tabell_fran, schema_till) {
  cc <- intern_postgis_con(con)
  on.exit(intern_stang(cc), add = TRUE)
  DBI::dbExecute(cc$con, glue::glue_sql(
    "ALTER TABLE {`schema_fran`}.{`tabell_fran`} SET SCHEMA {`schema_till`};", .con = cc$con))
  invisible(NULL)
}
