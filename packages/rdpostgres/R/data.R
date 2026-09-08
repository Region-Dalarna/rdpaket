# Läsa och skriva tabeller.

#' Läs en tabell från ett schema till en data.frame
#'
#' @param con En `DBIConnection` eller `"default"`.
#' @param schema,tabell Schema och tabell.
#' @param query Valfri SQL-svans som läggs efter `SELECT * FROM schema.tabell`,
#'   t.ex. `"WHERE ar = 2024"`.
#' @param meddelande_info Skriv ut att tabellen lästs in.
#'
#' @return En `data.frame`, eller `NULL` vid fel.
#' @export
postgres_tabell_till_df <- function(con = "default", schema, tabell,
                                    query = NA, meddelande_info = FALSE) {
  c <- intern_con(con); on.exit(intern_stang(c), add = TRUE)
  sql <- paste0("SELECT * FROM ", schema, ".", tabell,
                if (!is.na(query)) paste0(" ", query) else "")
  tryCatch({
    df <- DBI::dbGetQuery(c$con, sql)
    if (meddelande_info) message("Tabellen ", tabell, " från schemat ", schema, " har lästs in.")
    df
  }, error = function(e) {
    message("Kunde inte läsa tabellen ", tabell, " från schemat ", schema, ": ", conditionMessage(e))
    NULL
  })
}

#' Skriv en data.frame till en PostgreSQL-tabell
#'
#' Skapar schemat om det saknas. Utan `addera_data` skrivs tabellen över
#' (`TRUNCATE` om den finns, så struktur och behörigheter behålls).
#'
#' @param con En `DBIConnection` eller `"default"`.
#' @param inlas_df Data att skriva.
#' @param schema,tabell Målschema och måltabell.
#' @param id_kol Kolumn som ska bli primärnyckel (`NA` = ingen).
#' @param addera_data `TRUE` = lägg till rader i stället för att skriva över.
#' @param tabellnamn_till_gemener,kolumnnamn_till_gemener Gör namn till gemener.
#'
#' @return Osynligt `NULL`.
#' @export
postgres_df_till_postgrestabell <- function(con = "default", inlas_df, schema, tabell,
                                            id_kol = NA, addera_data = FALSE,
                                            tabellnamn_till_gemener = TRUE,
                                            kolumnnamn_till_gemener = TRUE) {
  c <- intern_con(con); on.exit(intern_stang(c), add = TRUE)
  con <- c$con

  if (kolumnnamn_till_gemener) names(inlas_df) <- tolower(names(inlas_df))
  if (tabellnamn_till_gemener) tabell <- tolower(tabell)

  if (!postgres_schema_finns(con, schema)) {
    DBI::dbExecute(con, glue::glue_sql("CREATE SCHEMA IF NOT EXISTS {`schema`};", .con = con))
  }

  tabell_finns <- DBI::dbExistsTable(con, DBI::Id(schema = schema, table = tabell))
  if (!tabell_finns) {
    append_mode <- FALSE
  } else if (addera_data) {
    append_mode <- TRUE
  } else {
    DBI::dbExecute(con, glue::glue_sql("TRUNCATE TABLE {`schema`}.{`tabell`};", .con = con))
    append_mode <- FALSE
  }

  DBI::dbWriteTable(con, DBI::Id(schema = schema, table = tabell), inlas_df,
                    append = append_mode, overwrite = !append_mode && tabell_finns)

  if (!is.na(id_kol)) {
    DBI::dbExecute(con, glue::glue_sql(
      "ALTER TABLE {`schema`}.{`tabell`} ADD PRIMARY KEY ({`id_kol`});", .con = con))
  }
  invisible(NULL)
}

#' Hämta data ur Region Dalarnas öppna data-databas
#'
#' Utan `schema`/`tabell` listas scheman och tabeller. `schema` kan skickas som
#' `"schema.tabell"`.
#'
#' @param schema Schemanamn, eller `"schema.tabell"`.
#' @param tabell Tabellnamn.
#' @param query Valfri SQL-svans.
#'
#' @return En `data.frame`, eller (utan schema/tabell) en lista över scheman
#'   och tabeller.
#' @export
oppnadata_hamta <- function(schema = NA, tabell = NA, query = NA) {
  postgres_hamta_oppnadata(schema = schema, tabell = tabell, query = query)
}

#' @rdname oppnadata_hamta
#' @param meddelande_info Skriv ut att tabellen lästs in.
#' @export
postgres_hamta_oppnadata <- function(schema = NA, tabell = NA, query = NA,
                                     meddelande_info = FALSE) {
  if (is.na(schema) && is.na(tabell)) {
    return(postgres_lista_scheman_tabeller(con = uppkoppling_db(db_name = "oppna_data")))
  }
  if (!is.na(schema) && grepl("\\.", schema)) {
    if (lengths(regmatches(schema, gregexpr("\\.", schema))) > 1) {
      stop("Det får max vara en punkt, som skiljer schema från tabell.", call. = FALSE)
    }
    delar <- strsplit(schema, "\\.")[[1]]
    schema <- delar[1]; tabell <- delar[2]
  }

  st <- postgres_lista_scheman_tabeller(uppkoppling_db(db_name = "oppna_data"))
  if (!schema %in% names(st)) stop("Schemat '", schema, "' finns inte i oppna_data.", call. = FALSE)
  if (!tabell %in% st[[schema]]) stop("Tabellen '", tabell, "' finns inte i schemat '", schema, "'.", call. = FALSE)

  postgres_tabell_till_df(con = uppkoppling_db(db_name = "oppna_data"),
                          schema = schema, tabell = tabell, query = query,
                          meddelande_info = meddelande_info)
}
