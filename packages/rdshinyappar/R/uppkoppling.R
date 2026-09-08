# Databasuppkopplingar för Shiny-appar i drift.

intern_shiny_uppkoppling <- function(db_name, db_host, db_port, db_options, db_user) {
  tryCatch(
    DBI::dbConnect(
      RPostgres::Postgres(),
      bigint   = "integer",
      user     = db_user,
      password = shiny_get_password(db_user),
      host     = db_host,
      port     = db_port,
      dbname   = db_name,
      options  = db_options
    ),
    error = function(e) {
      message("Ett fel inträffade vid anslutning till databasen: ", conditionMessage(e))
      NULL
    }
  )
}

#' Koppla upp mot databasen med skrivrättigheter
#'
#' Öppnar en `RPostgres`-anslutning som användaren `shiny_skriv` (eller angiven
#' `db_user`). Lösenordet hämtas med [shiny_get_password()].
#'
#' @param db_name Databasnamn.
#' @param db_host Värdnamn.
#' @param db_port Port.
#' @param db_options `options`-sträng till `dbConnect()`.
#' @param db_user Databasanvändare (och nyckel för lösenordet).
#'
#' @return Ett `DBIConnection`-objekt, eller `NULL` om anslutningen misslyckas.
#' @export
shiny_uppkoppling_skriv <- function(
    db_name    = "geodata",
    db_host    = "WFALMITVS526.ltdalarna.se",
    db_port    = 5432,
    db_options = "-c search_path=public",
    db_user    = "shiny_skriv"
) {
  intern_shiny_uppkoppling(db_name, db_host, db_port, db_options, db_user)
}

#' Koppla upp mot databasen med läsrättigheter
#'
#' Öppnar en `RPostgres`-anslutning som användaren `shiny_las` (eller angiven
#' `db_user`). Används av andra funktioner som standard om ingen egen
#' anslutning skickas med. Lösenordet hämtas med [shiny_get_password()].
#'
#' @inheritParams shiny_uppkoppling_skriv
#'
#' @return Ett `DBIConnection`-objekt, eller `NULL` om anslutningen misslyckas.
#' @export
shiny_uppkoppling_las <- function(
    db_name    = "geodata",
    db_host    = "WFALMITVS526.ltdalarna.se",
    db_port    = 5432,
    db_options = "-c search_path=public",
    db_user    = "shiny_las"
) {
  intern_shiny_uppkoppling(db_name, db_host, db_port, db_options, db_user)
}

#' Lista tabeller och vyer i databasen
#'
#' Frågar `information_schema` efter tabeller/vyer med valfri filtrering på
#' schema, tabellnamn och förekomst av geometrikolumn.
#'
#' @param con En aktiv `DBIConnection`.
#' @param include_views Ta med vyer (`VIEW`), inte bara `BASE TABLE`.
#' @param only_with_geometry Bara tabeller som har en geometrikolumn (PostGIS).
#' @param schema_like `ILIKE`-mönster för schemanamn, t.ex. `"karta%"`. `NULL` =
#'   ingen filtrering.
#' @param table_like `ILIKE`-mönster för tabellnamn, t.ex. `"%kommun%"`. `NULL` =
#'   ingen filtrering.
#' @param exclude_schemas Scheman som alltid utesluts.
#' @param include_rowcount_est Ta med uppskattat radantal från `pg_catalog`
#'   (snabbt men ungefärligt).
#'
#' @return En `data.frame` med kolumnerna `schema`, `table`, `type`,
#'   `geometry_columns` och ev. `rowcount_est`.
#' @export
shiny_db_list <- function(
    con,
    include_views        = TRUE,
    only_with_geometry   = FALSE,
    schema_like          = NULL,
    table_like           = NULL,
    exclude_schemas      = c("pg_catalog", "information_schema", "public"),
    include_rowcount_est = FALSE
) {
  stopifnot(DBI::dbIsValid(con))

  where_clauses <- character()

  if (length(exclude_schemas)) {
    excl <- paste(DBI::dbQuoteLiteral(con, exclude_schemas), collapse = ", ")
    where_clauses <- c(where_clauses, paste0("t.table_schema NOT IN (", excl, ")"))
  }

  if (isTRUE(include_views)) {
    where_clauses <- c(where_clauses, "t.table_type IN ('BASE TABLE','VIEW')")
  } else {
    where_clauses <- c(where_clauses, "t.table_type = 'BASE TABLE'")
  }

  if (!is.null(schema_like)) {
    where_clauses <- c(
      where_clauses,
      paste0("t.table_schema ILIKE ", DBI::dbQuoteLiteral(con, schema_like))
    )
  }
  if (!is.null(table_like)) {
    where_clauses <- c(
      where_clauses,
      paste0("t.table_name ILIKE ", DBI::dbQuoteLiteral(con, table_like))
    )
  }

  if (isTRUE(only_with_geometry)) {
    where_clauses <- c(
      where_clauses,
      paste(
        "EXISTS (",
        " SELECT 1",
        " FROM information_schema.columns c",
        " WHERE c.table_schema = t.table_schema",
        "   AND c.table_name   = t.table_name",
        "   AND c.udt_name     = 'geometry'",
        ")",
        sep = "\n"
      )
    )
  }

  where_sql <- paste(where_clauses, collapse = " AND ")
  if (!nzchar(where_sql)) where_sql <- "TRUE"

  rowcount_cols <- ""
  rowcount_join <- ""
  if (isTRUE(include_rowcount_est)) {
    rowcount_cols <- paste(
      "",
      ", CASE",
      "    WHEN pc.reltuples IS NULL THEN NULL",
      "    ELSE GREATEST(pc.reltuples::bigint, 0)",
      "  END AS rowcount_est",
      sep = "\n"
    )
    rowcount_join <- paste(
      "LEFT JOIN pg_catalog.pg_namespace pn",
      "  ON pn.nspname = t.table_schema",
      "LEFT JOIN pg_catalog.pg_class pc",
      "  ON pc.relnamespace = pn.oid",
      " AND pc.relname      = t.table_name",
      " AND pc.relkind IN ('r','m','v')",
      sep = "\n"
    )
  }

  geomname_cols <- paste(
    "",
    ", (",
    "    SELECT array_agg(c.column_name ORDER BY c.ordinal_position)",
    "    FROM information_schema.columns c",
    "    WHERE c.table_schema = t.table_schema",
    "      AND c.table_name   = t.table_name",
    "      AND c.udt_name     = 'geometry'",
    "  ) AS geometry_columns",
    sep = "\n"
  )

  sql <- paste(
    "SELECT",
    "  t.table_schema AS schema,",
    "  t.table_name   AS table,",
    "  t.table_type   AS type",
    geomname_cols,
    rowcount_cols,
    "FROM information_schema.tables t",
    rowcount_join,
    paste("WHERE", where_sql),
    "ORDER BY t.table_schema, t.table_name;",
    sep = "\n"
  )

  DBI::dbGetQuery(con, sql)
}
