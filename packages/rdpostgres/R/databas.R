# Skapa/ta bort databaser, scheman och tabeller.

#' Skapa en ny databas
#' @param con En aktiv `DBIConnection`.
#' @param databasnamn Namn på den nya databasen.
#' @return Osynligt `NULL`.
#' @export
postgres_databas_skapa <- function(con, databasnamn) {
  tryCatch({
    DBI::dbExecute(con, glue::glue("CREATE DATABASE {DBI::dbQuoteIdentifier(con, databasnamn)};"))
    message("Databasen '", databasnamn, "' har skapats.")
  }, error = function(e) message("Kunde inte skapa databasen: ", conditionMessage(e)))
  invisible(NULL)
}

#' Ta bort en hel databas (kräver interaktiv bekräftelse)
#' @param con En aktiv `DBIConnection`.
#' @param databasnamn Databas att ta bort.
#' @return Osynligt `FALSE` om avbruten, annars `NULL`.
#' @export
postgres_databas_ta_bort <- function(con, databasnamn) {
  svar <- readline(prompt = paste0(
    "Är du säker på att du vill ta bort databasen '", databasnamn,
    "'? Skriv 'ja' för att bekräfta: "))
  if (tolower(svar) != "ja") {
    message("Åtgärden avbröts - databasen togs inte bort.")
    return(invisible(FALSE))
  }
  tryCatch({
    DBI::dbExecute(con, glue::glue("DROP DATABASE IF EXISTS {DBI::dbQuoteIdentifier(con, databasnamn)};"))
    message("Databasen '", databasnamn, "' har tagits bort.")
  }, error = function(e) message("Kunde inte ta bort databasen: ", conditionMessage(e)))
  invisible(NULL)
}

#' Finns schemat?
#' @param con En aktiv `DBIConnection`.
#' @param schema_namn Schemanamn.
#' @return `TRUE`/`FALSE`.
#' @export
postgres_schema_finns <- function(con, schema_namn) {
  DBI::dbGetQuery(con, glue::glue_sql(
    "SELECT EXISTS (SELECT 1 FROM information_schema.schemata WHERE schema_name = {schema_namn}) AS finns;",
    .con = con))$finns[1]
}

#' Skapa ett schema om det inte redan finns
#' @param schema_namn Schemanamn.
#' @param con En `DBIConnection` eller `"default"`.
#' @return Osynligt `NULL`.
#' @export
postgres_schema_skapa_om_inte_finns <- function(schema_namn, con = "default") {
  c <- intern_con(con); on.exit(intern_stang(c), add = TRUE)
  DBI::dbExecute(c$con, glue::glue_sql(
    "CREATE SCHEMA IF NOT EXISTS {`schema_namn`};", .con = c$con))
  invisible(NULL)
}

#' Ta bort ett tomt schema
#'
#' Vägrar om schemat innehåller tabeller (skriver ut vilka).
#'
#' @param con En aktiv `DBIConnection`.
#' @param schema Schemanamn.
#' @return Osynligt `NULL`.
#' @export
postgres_schema_ta_bort <- function(con, schema) {
  if (!postgres_schema_finns(con, schema)) {
    message("Schemat '", schema, "' existerar inte. Ingen åtgärd vidtogs.")
    return(invisible(NULL))
  }
  tabeller <- DBI::dbGetQuery(con, glue::glue_sql(
    "SELECT table_name FROM information_schema.tables WHERE table_schema = {schema};", .con = con))
  if (nrow(tabeller) > 0) {
    message("Schemat '", schema, "' innehåller tabeller och kan inte tas bort:")
    print(tabeller)
    return(invisible(NULL))
  }
  DBI::dbExecute(con, glue::glue("DROP SCHEMA {DBI::dbQuoteIdentifier(con, schema)};"))
  message("Schemat '", schema, "' har tagits bort.")
  invisible(NULL)
}

#' Finns tabellen (eller vyn)?
#' @param con En `DBIConnection` eller `"default"`.
#' @param schema,tabell Schema och tabell.
#' @return `TRUE`/`FALSE`.
#' @export
postgres_tabell_finns <- function(con = "default", schema, tabell) {
  c <- intern_con(con); on.exit(intern_stang(c), add = TRUE)
  DBI::dbExistsTable(c$con, DBI::Id(schema = schema, table = tabell))
}

#' Ta bort en tabell, vy eller materialiserad vy
#' @param con En `DBIConnection` eller `"default"`.
#' @param schema,tabell Schema och objektnamn.
#' @param drop_cascade Lägg till `CASCADE`.
#' @return Osynligt `NULL`.
#' @export
postgres_tabell_ta_bort <- function(con = "default", schema, tabell, drop_cascade = FALSE) {
  c <- intern_con(con); on.exit(intern_stang(c), add = TRUE)
  schema_tabell <- paste0(schema, ".", tabell)

  ar_vy <- nrow(DBI::dbGetQuery(c$con, glue::glue_sql(
    "SELECT 1 FROM information_schema.views WHERE table_schema = {schema} AND table_name = {tabell}",
    .con = c$con))) > 0
  ar_mat_vy <- nrow(DBI::dbGetQuery(c$con, glue::glue_sql(
    "SELECT 1 FROM pg_matviews WHERE schemaname = {schema} AND matviewname = {tabell}",
    .con = c$con))) > 0

  if (ar_vy)          { finns <- TRUE; drop_typ <- "VIEW";              typ_text <- "Vyn" }
  else if (ar_mat_vy) { finns <- TRUE; drop_typ <- "MATERIALIZED VIEW"; typ_text <- "Den materialiserade vyn" }
  else                { finns <- DBI::dbExistsTable(c$con, DBI::Id(schema = schema, table = tabell))
                        drop_typ <- "TABLE"; typ_text <- "Tabellen" }

  if (!finns) {
    message(typ_text, " '", schema_tabell, "' existerar inte. Ingen åtgärd vidtogs.")
    return(invisible(NULL))
  }

  sql <- paste0("DROP ", drop_typ, " ",
                DBI::dbQuoteIdentifier(c$con, schema), ".",
                DBI::dbQuoteIdentifier(c$con, tabell),
                if (drop_cascade) " CASCADE;" else ";")
  DBI::dbExecute(c$con, sql)
  message(typ_text, " '", schema_tabell, "' har tagits bort",
          if (drop_cascade) " med CASCADE." else ".")
  invisible(NULL)
}

#' Kontrollera att schema, tabell och kolumner finns
#'
#' @param con En `DBIConnection` eller `"default"`.
#' @param schema,tabell Schema och tabell.
#' @param kolumner Teckenvektor med kolumnnamn (`NULL` = kontrollera inte).
#' @param stoppa_vid_fel `TRUE` = `stop()` vid fel; `FALSE` = returnera en lista
#'   med `allt_finns` och `felmeddelande`.
#'
#' @return Osynligt `NULL` (om allt finns och `stoppa_vid_fel`), annars en lista.
#' @export
postgres_finns_schema_tabell_kolumner <- function(con = "default", schema, tabell,
                                                  kolumner = NULL, stoppa_vid_fel = TRUE) {
  if (missing(schema) || missing(tabell)) {
    stop("Parametrarna schema och tabell måste anges.", call. = FALSE)
  }
  c <- intern_con(con); on.exit(intern_stang(c), add = TRUE)

  fel_retur <- function(fel) {
    if (stoppa_vid_fel) stop(fel, call. = FALSE)
    list(allt_finns = FALSE, felmeddelande = fel)
  }

  if (!postgres_schema_finns(c$con, schema)) {
    return(fel_retur(glue::glue("Schemat '{schema}' finns inte.")))
  }
  tabell_finns <- DBI::dbGetQuery(c$con, glue::glue_sql(
    "SELECT EXISTS (SELECT 1 FROM information_schema.tables
     WHERE table_schema = {schema} AND table_name = {tabell}) AS finns;", .con = c$con))$finns
  if (!tabell_finns) {
    return(fel_retur(glue::glue("Tabellen '{tabell}' i schemat '{schema}' finns inte.")))
  }
  if (!is.null(kolumner)) {
    fanns <- DBI::dbGetQuery(c$con, glue::glue_sql(
      "SELECT column_name FROM information_schema.columns
       WHERE table_schema = {schema} AND table_name = {tabell}", .con = c$con))$column_name
    saknade <- setdiff(kolumner, fanns)
    if (length(saknade) > 0) {
      return(fel_retur(glue::glue(
        "Kolumn(er) '{paste(saknade, collapse = \"', '\")}' i {schema}.{tabell} finns inte.")))
    }
  }
  if (stoppa_vid_fel) invisible(NULL) else list(allt_finns = TRUE, felmeddelande = NULL)
}
