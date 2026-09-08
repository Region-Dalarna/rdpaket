# Användare, roller och rättigheter.

#' Giltiga PostgreSQL-rättigheter med beskrivning
#' @return En `data.frame` med `Rattighet` och `Beskrivning`.
#' @export
postgres_lista_giltiga_rattigheter <- function() {
  data.frame(
    Rattighet = c("CONNECT", "SELECT", "INSERT", "UPDATE", "DELETE",
                  "TRUNCATE", "REFERENCES", "USAGE", "EXECUTE", "CREATE", "TEMP"),
    Beskrivning = c(
      "Ansluta till en specifik databas.",
      "Läsa från tabeller och vyer (SELECT).",
      "Lägga till nya rader i en tabell.",
      "Uppdatera befintliga data i en tabell eller kolumner.",
      "Ta bort rader från en tabell.",
      "Tömma en tabell helt utan att utlösa triggers.",
      "Skapa foreign keys som refererar till en annan tabell.",
      "Använda objekt som sekvenser, scheman eller typer.",
      "Köra lagrade procedurer eller funktioner.",
      "Skapa nya objekt (t.ex. tabeller) i ett schema.",
      "Skapa temporära tabeller i databasen."),
    stringsAsFactors = FALSE)
}

#' Lägg till en användare med lösenord
#' @param con En `DBIConnection` eller `"default"`.
#' @param anvandarnamn,losenord Uppgifter för den nya användaren.
#' @return Osynligt `NULL`.
#' @export
postgres_anvandare_lagg_till <- function(con = "default", anvandarnamn, losenord) {
  c <- intern_con(con); on.exit(intern_stang(c), add = TRUE)
  tryCatch(
    DBI::dbExecute(c$con, glue::glue_sql(
      "CREATE USER {`anvandarnamn`} WITH PASSWORD {losenord};", .con = c$con)),
    error = function(e) message("Användaren finns redan eller annat fel: ", conditionMessage(e)))
  invisible(NULL)
}

#' Ta bort en användare/roll från servern
#' @param con En `DBIConnection` eller `"default"`.
#' @param anvandarnamn Användare att ta bort.
#' @return Osynligt `NULL`.
#' @export
postgres_anvandare_ta_bort <- function(con = "default", anvandarnamn) {
  c <- intern_con(con); on.exit(intern_stang(c), add = TRUE)
  antal <- DBI::dbGetQuery(c$con, glue::glue_sql(
    "SELECT COUNT(*) AS antal FROM pg_authid WHERE rolname = {anvandarnamn};", .con = c$con))$antal
  if (antal == 0) {
    message("Användaren ", anvandarnamn, " finns inte på servern.")
    return(invisible(NULL))
  }
  tryCatch({
    DBI::dbExecute(c$con, glue::glue("DROP ROLE {DBI::dbQuoteIdentifier(c$con, anvandarnamn)};"))
    message("Användaren ", anvandarnamn, " har tagits bort.")
  }, error = function(e) message("Kunde inte ta bort användaren ", anvandarnamn, ": ", conditionMessage(e)))
  invisible(NULL)
}

#' Skapa en roll (NOLOGIN) om den inte finns
#' @param con En `DBIConnection` eller `"default"`.
#' @param rollnamn Rollnamn.
#' @return Osynligt `NULL`.
#' @export
postgres_roll_lagg_till <- function(con = "default", rollnamn) {
  c <- intern_con(con); on.exit(intern_stang(c), add = TRUE)
  DBI::dbExecute(c$con, glue::glue("
    DO $$ BEGIN
      IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = '{rollnamn}') THEN
        CREATE ROLE {rollnamn} NOLOGIN;
      END IF;
    END $$;"))
  invisible(NULL)
}

#' Ta bort en roll
#' @param con En `DBIConnection` eller `"default"`.
#' @param rollnamn Rollnamn.
#' @return Osynligt `NULL`.
#' @export
postgres_roll_ta_bort <- function(con = "default", rollnamn) {
  c <- intern_con(con); on.exit(intern_stang(c), add = TRUE)
  tryCatch({
    DBI::dbExecute(c$con, glue::glue("DROP ROLE {DBI::dbQuoteIdentifier(c$con, rollnamn)};"))
    message("Rollen ", rollnamn, " har tagits bort.")
  }, error = function(e) message("Kunde inte ta bort rollen ", rollnamn, ": ", conditionMessage(e)))
  invisible(NULL)
}

#' Tilldela en roll till en användare
#' @param con En `DBIConnection` eller `"default"`.
#' @param rollnamn,anvandarnamn Roll och mottagande användare.
#' @return Osynligt `TRUE`.
#' @export
postgres_roll_tilldela_till_anvandare <- function(con = "default", rollnamn, anvandarnamn) {
  if (missing(rollnamn) || !nzchar(rollnamn)) stop("'rollnamn' måste anges.", call. = FALSE)
  if (missing(anvandarnamn) || !nzchar(anvandarnamn)) stop("'anvandarnamn' måste anges.", call. = FALSE)
  c <- intern_con(con); on.exit(intern_stang(c), add = TRUE)
  tryCatch({
    DBI::dbExecute(c$con, glue::glue_sql("GRANT {`rollnamn`} TO {`anvandarnamn`};", .con = c$con))
    message("Rollen ", rollnamn, " har tilldelats användaren ", anvandarnamn, ".")
  }, error = function(e) message(postgres_felmeddelande(e,
    glue::glue("tilldela rollen '{rollnamn}' till användaren '{anvandarnamn}'"))))
  invisible(TRUE)
}

#' Ta bort en roll från en användare
#' @param con En `DBIConnection` eller `"default"`.
#' @param rollnamn,anvandarnamn Roll och användare.
#' @return Osynligt `TRUE`.
#' @export
postgres_roll_ta_bort_fran_anvandare <- function(con = "default", rollnamn, anvandarnamn) {
  if (missing(rollnamn) || !nzchar(rollnamn)) stop("'rollnamn' måste anges.", call. = FALSE)
  if (missing(anvandarnamn) || !nzchar(anvandarnamn)) stop("'anvandarnamn' måste anges.", call. = FALSE)
  c <- intern_con(con); on.exit(intern_stang(c), add = TRUE)
  tryCatch({
    DBI::dbExecute(c$con, glue::glue_sql("REVOKE {`rollnamn`} FROM {`anvandarnamn`};", .con = c$con))
    message("Rollen ", rollnamn, " har tagits bort från användaren ", anvandarnamn, ".")
  }, error = function(e) message(postgres_felmeddelande(e,
    glue::glue("ta bort rollen '{rollnamn}' från användaren '{anvandarnamn}'"))))
  invisible(TRUE)
}

#' Byt lösenord för en användare
#' @param con En `DBIConnection` eller `"default"`.
#' @param anvandarnamn,nytt_losenord Användare och nytt lösenord.
#' @return Osynligt `NULL`.
#' @export
postgres_losenord_byt_for_anvandare <- function(con = "default", anvandarnamn, nytt_losenord) {
  c <- intern_con(con); on.exit(intern_stang(c), add = TRUE)
  tryCatch({
    DBI::dbExecute(c$con, glue::glue_sql(
      "ALTER ROLE {`anvandarnamn`} WITH PASSWORD {nytt_losenord};", .con = c$con))
    message("Lösenordet har ändrats för användaren ", anvandarnamn, ".")
  }, error = function(e) message("Kunde inte ändra lösenordet för ", anvandarnamn, ": ", conditionMessage(e)))
  invisible(NULL)
}

#' Lägg till rättigheter för en användare i ett eller alla scheman
#'
#' @param con En `DBIConnection` eller `"default"`.
#' @param anvandarnamn Användare.
#' @param schema `"alla"` eller en vektor av schemanamn.
#' @param rattigheter Vektor av rättigheter, eller `"alla"`. `c("CONNECT",
#'   "SELECT", "USAGE")` ger läsrättigheter.
#' @param meddelande_rattigheter Skriv ut varje beviljad rättighet.
#'
#' @return Osynligt `NULL`.
#' @export
postgres_rattigheter_anvandare_lagg_till <- function(con = "default", anvandarnamn,
                                                     schema = "alla",
                                                     rattigheter = c("CONNECT", "SELECT", "USAGE"),
                                                     meddelande_rattigheter = TRUE) {
  c <- intern_con(con); on.exit(intern_stang(c), add = TRUE)
  con <- c$con
  db <- DBI::dbGetInfo(con)$dbname
  if (all(rattigheter == "alla")) rattigheter <- postgres_lista_giltiga_rattigheter()$Rattighet
  giltiga <- postgres_lista_giltiga_rattigheter()$Rattighet

  msg <- function(...) if (meddelande_rattigheter) message(...)

  if ("CONNECT" %in% rattigheter) {
    tryCatch({
      DBI::dbExecute(con, glue::glue("GRANT CONNECT ON DATABASE {DBI::dbQuoteIdentifier(con, db)} TO {DBI::dbQuoteIdentifier(con, anvandarnamn)};"))
      msg("CONNECT tillagd för ", anvandarnamn, " till databasen ", db)
    }, error = function(e) message("Kunde inte ge CONNECT: ", conditionMessage(e)))
  }

  scheman <- names(postgres_lista_scheman_tabeller(con = con))
  if (!all(schema == "alla")) scheman <- scheman[scheman %in% schema]
  if (length(scheman) < 1) stop("Angivna scheman finns inte i databasen.", call. = FALSE)

  for (s in scheman) {
    sq <- DBI::dbQuoteIdentifier(con, s)
    aq <- DBI::dbQuoteIdentifier(con, anvandarnamn)
    if ("USAGE" %in% rattigheter) {
      tryCatch({ DBI::dbExecute(con, glue::glue("GRANT USAGE ON SCHEMA {sq} TO {aq};"))
        msg("USAGE tillagd på ", s, " för ", anvandarnamn) },
        error = function(e) message("Kunde inte ge USAGE på ", s, ": ", conditionMessage(e)))
    }
    if ("CREATE" %in% rattigheter) {
      tryCatch({ DBI::dbExecute(con, glue::glue("GRANT CREATE ON SCHEMA {sq} TO {aq};"))
        msg("CREATE tillagd på ", s, " för ", anvandarnamn) },
        error = function(e) message("Kunde inte ge CREATE på ", s, ": ", conditionMessage(e)))
    }
    matviews <- DBI::dbGetQuery(con, glue::glue_sql(
      "SELECT quote_ident(nspname) || '.' || quote_ident(relname) AS full_name
       FROM pg_class JOIN pg_namespace ON pg_namespace.oid = pg_class.relnamespace
       WHERE relkind = 'm' AND nspname = {s}", .con = con))$full_name

    for (r in setdiff(rattigheter, c("CONNECT", "USAGE", "CREATE"))) {
      if (!r %in% giltiga) { message("Ogiltig rättighet: ", r); next }
      tryCatch({
        DBI::dbExecute(con, glue::glue("GRANT {r} ON ALL TABLES IN SCHEMA {sq} TO {aq};"))
        msg("Rättigheten ", r, " tillagd för ", anvandarnamn, " i schemat ", s)
        for (mv in matviews) {
          DBI::dbExecute(con, glue::glue("GRANT {r} ON {mv} TO {aq};"))
        }
      }, error = function(e) message("Kunde inte ge ", r, " i ", s, ": ", conditionMessage(e)))
    }
  }
  invisible(NULL)
}

#' Ta bort rättigheter från en användare
#'
#' @inheritParams postgres_rattigheter_anvandare_lagg_till
#' @param rattigheter Vektor av rättigheter, eller `"alla"`.
#' @return Osynligt `NULL`.
#' @export
postgres_rattigheter_anvandare_ta_bort <- function(con = "default", anvandarnamn,
                                                   schema = "alla", rattigheter = "alla",
                                                   meddelande_rattigheter = TRUE) {
  c <- intern_con(con); on.exit(intern_stang(c), add = TRUE)
  con <- c$con
  if (all(rattigheter == "alla")) rattigheter <- postgres_lista_giltiga_rattigheter()$Rattighet
  db <- DBI::dbGetInfo(con)$dbname
  aq <- DBI::dbQuoteIdentifier(con, anvandarnamn)
  msg <- function(...) if (meddelande_rattigheter) message(...)

  scheman <- names(postgres_lista_scheman_tabeller(con = con))
  if (!all(schema == "alla")) scheman <- scheman[scheman %in% schema]
  if (length(scheman) < 1) stop("Angivna scheman finns inte i databasen.", call. = FALSE)

  for (r in intersect(rattigheter, c("CONNECT", "TEMP"))) {
    DBI::dbExecute(con, glue::glue("REVOKE {r} ON DATABASE {DBI::dbQuoteIdentifier(con, db)} FROM {aq};"))
    msg("Rättigheten ", r, " borttagen från ", anvandarnamn, " i databasen ", db, ".")
  }
  for (s in scheman) {
    sq <- DBI::dbQuoteIdentifier(con, s)
    for (r in intersect(rattigheter, c("USAGE", "CREATE"))) {
      DBI::dbExecute(con, glue::glue("REVOKE {r} ON SCHEMA {sq} FROM {aq};"))
      msg("Rättigheten ", r, " borttagen från ", anvandarnamn, " på schemat ", s, ".")
    }
    for (r in intersect(rattigheter, c("SELECT", "INSERT", "UPDATE", "DELETE", "TRUNCATE", "REFERENCES"))) {
      DBI::dbExecute(con, glue::glue("REVOKE {r} ON ALL TABLES IN SCHEMA {sq} FROM {aq};"))
      msg("Rättigheten ", r, " borttagen från ", anvandarnamn, " i schemat ", s, ".")
    }
  }
  invisible(NULL)
}

#' Alla effektiva rättigheter per schema i den anslutna databasen
#'
#' Räknar in ärvda roller (rekursivt) och superuser-status.
#'
#' @param con En `DBIConnection` eller `"default"`.
#' @param anvandarnamn Filtrera på en användare (`NULL` = alla).
#' @return En `data.frame` med `role_or_user`, `schema_name`, `access_level`.
#' @export
postgres_alla_rattigheter <- function(con = "default", anvandarnamn = NULL) {
  c <- intern_con(con); on.exit(intern_stang(c), add = TRUE)
  con <- c$con
  filter_anv <- if (!is.null(anvandarnamn)) {
    glue::glue_sql("WHERE role_or_user = {anvandarnamn}", .con = con)
  } else ""

  DBI::dbGetQuery(con, glue::glue("
    WITH RECURSIVE role_closure AS (
      SELECT r.oid AS principal_oid, r.rolname AS role_or_user,
             r.oid AS grant_role_oid, r.rolname AS grant_role, r.rolsuper AS rolsuper
      FROM pg_roles r
      UNION ALL
      SELECT rc.principal_oid, rc.role_or_user, parent_role.oid, parent_role.rolname, rc.rolsuper
      FROM role_closure rc
      JOIN pg_auth_members m ON m.member = rc.grant_role_oid
      JOIN pg_roles parent_role ON parent_role.oid = m.roleid
    ),
    all_schemas AS (
      SELECT schema_name FROM information_schema.schemata
      WHERE schema_name NOT LIKE 'pg_%' AND schema_name != 'information_schema'
    ),
    table_privileges AS (
      SELECT grantee AS role_or_user, table_schema, STRING_AGG(privilege_type, ',') AS privileges
      FROM information_schema.role_table_grants GROUP BY grantee, table_schema
    ),
    schema_create_privileges AS (
      SELECT r.rolname AS role_or_user, n.nspname AS table_schema, 'CREATE' AS privileges
      FROM pg_roles r CROSS JOIN pg_namespace n
      WHERE has_schema_privilege(r.rolname, n.nspname, 'CREATE')
    ),
    merged_privileges AS (
      SELECT role_or_user, table_schema, privileges FROM table_privileges
      UNION ALL
      SELECT role_or_user, table_schema, privileges FROM schema_create_privileges
    ),
    access_levels AS (
      SELECT role_or_user, table_schema,
        CASE
          WHEN STRING_AGG(privileges, ',') LIKE '%INSERT%' OR STRING_AGG(privileges, ',') LIKE '%UPDATE%'
            OR STRING_AGG(privileges, ',') LIKE '%DELETE%' OR STRING_AGG(privileges, ',') LIKE '%CREATE%' THEN 'write'
          WHEN STRING_AGG(privileges, ',') LIKE '%SELECT%' THEN 'read'
          ELSE 'no access'
        END AS access_type
      FROM merged_privileges GROUP BY role_or_user, table_schema
    ),
    effective_access AS (
      SELECT rc.role_or_user, s.schema_name,
        CASE
          WHEN BOOL_OR(rc.rolsuper) THEN 'write'
          WHEN MAX(CASE WHEN al.access_type = 'write' THEN 2 WHEN al.access_type = 'read' THEN 1 ELSE 0 END) = 2 THEN 'write'
          WHEN MAX(CASE WHEN al.access_type = 'write' THEN 2 WHEN al.access_type = 'read' THEN 1 ELSE 0 END) = 1 THEN 'read'
          ELSE 'no access'
        END AS access_level
      FROM role_closure rc
      CROSS JOIN all_schemas s
      LEFT JOIN access_levels al ON al.role_or_user = rc.grant_role AND al.table_schema = s.schema_name
      GROUP BY rc.role_or_user, s.schema_name
    )
    SELECT role_or_user, schema_name, access_level
    FROM effective_access
    {filter_anv}
    ORDER BY role_or_user, schema_name;"))
}

#' En användares rättigheter över alla databaser på servern
#'
#' @param anvandarnamn Användare.
#' @param con En admin-`DBIConnection` eller `"default"` (använder
#'   [uppkoppling_adm()]).
#' @param databaser Begränsa till dessa databaser (`NULL` = alla).
#' @param visa_databaser_utan_connect Ta med databaser användaren inte kan
#'   ansluta till.
#' @param visa_alla_scheman `TRUE` = en rad per databas+schema; `FALSE` =
#'   sammanräkning per databas och access-nivå.
#'
#' @return En `data.frame`.
#' @export
postgres_alla_rattigheter_server <- function(anvandarnamn, con = "default",
                                             databaser = NULL,
                                             visa_databaser_utan_connect = TRUE,
                                             visa_alla_scheman = FALSE) {
  if (missing(anvandarnamn) || is.null(anvandarnamn) || !nzchar(anvandarnamn)) {
    stop("Parametern 'anvandarnamn' måste anges.", call. = FALSE)
  }
  cs <- intern_con(con, adm = TRUE); on.exit(intern_stang(cs), add = TRUE)
  con_server <- cs$con

  db_connect <- DBI::dbGetQuery(con_server, glue::glue_sql("
    SELECT datname AS databas,
           has_database_privilege({anvandarnamn}, datname, 'CONNECT') AS connect,
           has_database_privilege({anvandarnamn}, datname, 'CREATE') AS database_create,
           has_database_privilege({anvandarnamn}, datname, 'TEMPORARY') AS database_temp
    FROM pg_database WHERE datistemplate = false ORDER BY datname;", .con = con_server))
  if (!is.null(databaser)) db_connect <- dplyr::filter(db_connect, .data$databas %in% databaser)

  resultat <- purrr::map_dfr(seq_len(nrow(db_connect)), function(i) {
    db <- db_connect$databas[i]
    if (!isTRUE(db_connect$connect[i])) {
      if (!visa_databaser_utan_connect) return(tibble::tibble())
      return(tibble::tibble(databas = db, connect = FALSE,
                            database_create = db_connect$database_create[i],
                            database_temp = db_connect$database_temp[i],
                            role_or_user = anvandarnamn, schema_name = NA_character_,
                            access_level = "no access", felmeddelande = NA_character_))
    }
    con_db <- NULL
    ratt <- tryCatch({
      con_db <- uppkoppling_adm(db)
      if (is.null(con_db)) stop("Kunde inte ansluta med uppkoppling_adm().")
      postgres_alla_rattigheter(con = con_db, anvandarnamn = anvandarnamn)
    }, error = function(e) tibble::tibble(role_or_user = anvandarnamn, schema_name = NA_character_,
                                          access_level = NA_character_, felmeddelande = e$message),
       finally = if (!is.null(con_db) && DBI::dbIsValid(con_db)) DBI::dbDisconnect(con_db))
    if (!"felmeddelande" %in% names(ratt)) ratt$felmeddelande <- NA_character_
    if (nrow(ratt) == 0) ratt <- tibble::tibble(role_or_user = anvandarnamn, schema_name = NA_character_,
                                                access_level = "no access", felmeddelande = NA_character_)
    dplyr::mutate(ratt, databas = db, connect = TRUE,
                  database_create = db_connect$database_create[i],
                  database_temp = db_connect$database_temp[i], .before = 1)
  })

  retur <- dplyr::arrange(resultat, .data$databas, .data$schema_name)
  if (visa_alla_scheman) retur else dplyr::count(retur, .data$databas, .data$connect, .data$access_level)
}
