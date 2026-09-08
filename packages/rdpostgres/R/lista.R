# Listnings- och översiktsfunktioner.

#' Lista databaser på servern
#'
#' @param con En `DBIConnection`, eller `"default"` för en tillfällig
#'   standarduppkoppling.
#' @return En `data.frame` med kolumnen `databas`.
#' @export
postgres_lista_databaser <- function(con = "default") {
  c <- intern_con(con); on.exit(intern_stang(c), add = TRUE)
  res <- DBI::dbGetQuery(c$con,
    "SELECT datname FROM pg_database WHERE datistemplate = false;")
  dplyr::rename(res, databas = "datname")
}

#' Lista scheman och deras tabeller/vyer
#'
#' @param con En `DBIConnection` eller `"default"`.
#' @param visa_system_tabeller Ta med `public`, `information_schema` och `pg_*`.
#' @return En namngiven lista: ett element per schema med en teckenvektor av
#'   tabell-, vy- och matvy-namn.
#' @export
postgres_lista_scheman_tabeller <- function(con = "default",
                                            visa_system_tabeller = FALSE) {
  c <- intern_con(con); on.exit(intern_stang(c), add = TRUE)

  scheman <- DBI::dbGetQuery(c$con,
    "SELECT schema_name FROM information_schema.schemata")$schema_name
  if (!visa_system_tabeller) {
    scheman <- scheman[!grepl("pg_", scheman) &
                       !scheman %in% c("public", "information_schema")]
  }

  out <- list()
  for (s in scheman) {
    tab <- DBI::dbGetQuery(c$con, sprintf(
      "SELECT table_name FROM information_schema.tables
       WHERE table_schema = '%s' AND table_type IN ('BASE TABLE', 'VIEW', 'MATERIALIZED VIEW')", s))$table_name
    mv <- DBI::dbGetQuery(c$con, sprintf(
      "SELECT matviewname FROM pg_matviews WHERE schemaname = '%s'", s))$matviewname
    out[[s]] <- unique(c(tab, mv))
  }
  out
}

#' Lista roller och användare
#' @param con En `DBIConnection` eller `"default"`.
#' @return En `data.frame` från `pg_roles`.
#' @export
postgres_lista_roller_anvandare <- function(con = "default") {
  c <- intern_con(con); on.exit(intern_stang(c), add = TRUE)
  DBI::dbGetQuery(c$con, "
    SELECT rolname, rolsuper, rolinherit, rolcreaterole, rolcreatedb, rolcanlogin
    FROM pg_roles;")
}

#' Lista roller/användares behörighet per schema (read/write/no access)
#' @param con En `DBIConnection` eller `"default"`.
#' @return En `data.frame` med `role_or_user`, `table_schema`, `access_level`.
#' @export
postgres_lista_behorighet_till_scheman <- function(con = "default") {
  c <- intern_con(con); on.exit(intern_stang(c), add = TRUE)
  DBI::dbGetQuery(c$con, "
    WITH privilege_summary AS (
      SELECT grantee AS role_or_user, table_schema,
        CASE
          WHEN STRING_AGG(privilege_type, ',') LIKE '%INSERT%' OR
               STRING_AGG(privilege_type, ',') LIKE '%UPDATE%' OR
               STRING_AGG(privilege_type, ',') LIKE '%CREATE%' OR
               STRING_AGG(privilege_type, ',') LIKE '%DELETE%' THEN 'write'
          WHEN STRING_AGG(privilege_type, ',') LIKE '%SELECT%' THEN 'read'
          ELSE 'no access'
        END AS access_type
      FROM information_schema.role_table_grants
      GROUP BY grantee, table_schema
    )
    SELECT role_or_user, table_schema, MAX(access_type) AS access_level
    FROM privilege_summary
    GROUP BY role_or_user, table_schema
    ORDER BY role_or_user, table_schema;")
}

#' Lista kolumnnamn i alla tabeller i ett schema
#' @param con En `DBIConnection` eller `"default"`.
#' @param schema Schemanamn.
#' @return En `data.frame` med `table_name`, `column_name`, `data_type`.
#' @export
postgres_lista_kolumnnamn_i_schema <- function(con = "default", schema) {
  if (missing(schema) || is.na(schema)) stop("Schema måste anges.", call. = FALSE)
  c <- intern_con(con); on.exit(intern_stang(c), add = TRUE)
  DBI::dbGetQuery(c$con, glue::glue("
    SELECT table_name, column_name, data_type
    FROM information_schema.columns
    WHERE table_schema = '{schema}'
    ORDER BY table_name, ordinal_position;"))
}

#' Lista rollmedlemskap (vem ärver vilken roll)
#' @param con En `DBIConnection` eller `"default"`.
#' @return En `data.frame` med `user_or_role`, `inherited_role`.
#' @export
postgres_lista_rollmedlemskap <- function(con = "default") {
  c <- intern_con(con); on.exit(intern_stang(c), add = TRUE)
  DBI::dbGetQuery(c$con, "
    SELECT member.rolname AS user_or_role, role.rolname AS inherited_role
    FROM pg_auth_members m
    JOIN pg_roles member ON m.member = member.oid
    JOIN pg_roles role   ON m.roleid = role.oid
    ORDER BY member.rolname, role.rolname;")
}
