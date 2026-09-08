# Automatiska grants via event triggers: nya tabeller/scheman får rättigheter
# för angivna läs- och skrivroller automatiskt.

#' Installera automatiska grants (event triggers)
#'
#' Skapar `public.auto_grant()` och event triggers som ger `lasroller` SELECT
#' och `skrivroller` SELECT/INSERT/UPDATE/DELETE på nya tabeller, vyer,
#' materialiserade vyer och scheman.
#'
#' @param con En aktiv `DBIConnection`.
#' @param remove_old Ta bort befintliga `auto_grant_*`-triggers först.
#' @param rattigheter_pa_befintliga Kör även
#'   [postgres_grants_pa_befintliga_objekt()] efteråt.
#' @param lasroller,skrivroller Roller som ska få läs- resp. skrivrättigheter.
#' @param lasroll_tilldela,skrivroll_tilldela Användare att tilldela roll(erna)
#'   till (`NULL` = ingen).
#'
#' @return Osynligt `TRUE`.
#' @export
postgres_grants_auto_skapa <- function(con, remove_old = TRUE,
                                       rattigheter_pa_befintliga = TRUE,
                                       lasroller = "lasroll", skrivroller = "skrivroll",
                                       lasroll_tilldela = NULL, skrivroll_tilldela = NULL) {
  lasroller <- unique(lasroller)
  skrivroller <- unique(skrivroller)
  if (length(lasroller) < 1 && length(skrivroller) < 1) {
    stop("Minst en läsroll eller skrivroll måste anges.", call. = FALSE)
  }

  gamla <- DBI::dbGetQuery(con,
    "SELECT evtname FROM pg_event_trigger WHERE evtname LIKE 'auto_grant%';")
  if (nrow(gamla) > 0 && remove_old) {
    DBI::dbExecute(con, "DROP EVENT TRIGGER IF EXISTS auto_grant_tables;")
    DBI::dbExecute(con, "DROP EVENT TRIGGER IF EXISTS auto_grant_schemas;")
  }

  lasroller_sql   <- paste(DBI::dbQuoteString(con, lasroller), collapse = ", ")
  skrivroller_sql <- paste(DBI::dbQuoteString(con, skrivroller), collapse = ", ")

  DBI::dbExecute(con, glue::glue("
    CREATE OR REPLACE FUNCTION public.auto_grant()
    RETURNS event_trigger LANGUAGE plpgsql AS $$
    DECLARE
      obj record; schema_name text; lasroll text; skrivroll text;
      lasroller text[] := ARRAY[{lasroller_sql}];
      skrivroller text[] := ARRAY[{skrivroller_sql}];
    BEGIN
      FOR obj IN SELECT * FROM pg_event_trigger_ddl_commands() LOOP
        BEGIN
          IF obj.object_type IN ('table', 'view', 'materialized view') THEN
            FOREACH lasroll IN ARRAY lasroller LOOP
              EXECUTE format('GRANT SELECT ON %s TO %I;', obj.object_identity, lasroll);
            END LOOP;
            FOREACH skrivroll IN ARRAY skrivroller LOOP
              EXECUTE format('GRANT SELECT, INSERT, UPDATE, DELETE ON %s TO %I;', obj.object_identity, skrivroll);
            END LOOP;
          ELSIF obj.object_type = 'schema' THEN
            schema_name := obj.object_identity;
            FOREACH lasroll IN ARRAY lasroller LOOP
              EXECUTE format('GRANT USAGE ON SCHEMA %I TO %I;', schema_name, lasroll);
              EXECUTE format('ALTER DEFAULT PRIVILEGES IN SCHEMA %I GRANT SELECT ON TABLES TO %I;', schema_name, lasroll);
            END LOOP;
            FOREACH skrivroll IN ARRAY skrivroller LOOP
              EXECUTE format('GRANT USAGE, CREATE ON SCHEMA %I TO %I;', schema_name, skrivroll);
              EXECUTE format('ALTER DEFAULT PRIVILEGES IN SCHEMA %I GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO %I;', schema_name, skrivroll);
            END LOOP;
          END IF;
        EXCEPTION WHEN OTHERS THEN
          RAISE WARNING 'Fel vid grant för % (type: %): %', obj.object_identity, obj.object_type, SQLERRM;
        END;
      END LOOP;
    END; $$;"))

  DBI::dbExecute(con, "DROP EVENT TRIGGER IF EXISTS auto_grant_tables;")
  DBI::dbExecute(con, "
    CREATE EVENT TRIGGER auto_grant_tables ON ddl_command_end
      WHEN TAG IN ('CREATE TABLE', 'CREATE VIEW', 'CREATE MATERIALIZED VIEW')
      EXECUTE FUNCTION public.auto_grant();")
  DBI::dbExecute(con, "DROP EVENT TRIGGER IF EXISTS auto_grant_schemas;")
  DBI::dbExecute(con, "
    CREATE EVENT TRIGGER auto_grant_schemas ON ddl_command_end
      WHEN TAG IN ('CREATE SCHEMA') EXECUTE FUNCTION public.auto_grant();")

  message("Triggers och funktion installerade.")

  if (isTRUE(rattigheter_pa_befintliga)) {
    postgres_grants_pa_befintliga_objekt(con = con, lasroller = lasroller, skrivroller = skrivroller)
  }

  for (anv in lasroll_tilldela) for (roll in lasroller) {
    tryCatch(DBI::dbExecute(con, glue::glue_sql("GRANT {`roll`} TO {`anv`};", .con = con)),
             error = function(e) message("Kunde inte tilldela ", roll, " till ", anv, ": ", conditionMessage(e)))
  }
  for (anv in skrivroll_tilldela) for (roll in skrivroller) {
    tryCatch(DBI::dbExecute(con, glue::glue_sql("GRANT {`roll`} TO {`anv`};", .con = con)),
             error = function(e) message("Kunde inte tilldela ", roll, " till ", anv, ": ", conditionMessage(e)))
  }
  invisible(TRUE)
}

#' Ge läs-/skrivroller rättigheter på alla befintliga objekt
#'
#' @param con En aktiv `DBIConnection`.
#' @param lasroller,skrivroller Roller.
#' @return Osynligt `TRUE`.
#' @export
postgres_grants_pa_befintliga_objekt <- function(con, lasroller = "lasroll", skrivroller = "skrivroll") {
  lasroller <- unique(lasroller); skrivroller <- unique(skrivroller)
  utan_system <- "NOT IN ('pg_catalog', 'information_schema', 'pg_toast') AND %s NOT LIKE 'pg_%%'"

  scheman <- DBI::dbGetQuery(con, sprintf(
    "SELECT schema_name FROM information_schema.schemata WHERE schema_name %s ORDER BY schema_name;",
    sprintf(utan_system, "schema_name")))$schema_name
  tabeller <- DBI::dbGetQuery(con, sprintf(
    "SELECT table_schema, table_name FROM information_schema.tables WHERE table_schema %s ORDER BY 1, 2;",
    sprintf(utan_system, "table_schema")))
  matviews <- DBI::dbGetQuery(con, sprintf(
    "SELECT schemaname, matviewname FROM pg_matviews WHERE schemaname %s ORDER BY 1, 2;",
    sprintf(utan_system, "schemaname")))

  for (s in scheman) {
    for (r in lasroller) {
      DBI::dbExecute(con, glue::glue_sql("GRANT USAGE ON SCHEMA {`s`} TO {`r`};", .con = con))
      DBI::dbExecute(con, glue::glue_sql(
        "ALTER DEFAULT PRIVILEGES IN SCHEMA {`s`} GRANT SELECT ON TABLES TO {`r`};", .con = con))
    }
    for (r in skrivroller) {
      DBI::dbExecute(con, glue::glue_sql("GRANT USAGE, CREATE ON SCHEMA {`s`} TO {`r`};", .con = con))
      DBI::dbExecute(con, glue::glue_sql(
        "ALTER DEFAULT PRIVILEGES IN SCHEMA {`s`} GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO {`r`};", .con = con))
    }
  }
  bevilja <- function(schema_namn, objekt_namn) {
    for (r in lasroller) DBI::dbExecute(con, glue::glue_sql(
      "GRANT SELECT ON {`schema_namn`}.{`objekt_namn`} TO {`r`};", .con = con))
    for (r in skrivroller) DBI::dbExecute(con, glue::glue_sql(
      "GRANT SELECT, INSERT, UPDATE, DELETE ON {`schema_namn`}.{`objekt_namn`} TO {`r`};", .con = con))
  }
  for (i in seq_len(nrow(tabeller))) bevilja(tabeller$table_schema[i], tabeller$table_name[i])
  for (i in seq_len(nrow(matviews))) bevilja(matviews$schemaname[i], matviews$matviewname[i])

  message("Rättigheter till befintliga objekt har uppdaterats.")
  invisible(TRUE)
}

#' Visa befintliga event triggers och default privileges
#'
#' @param con En aktiv `DBIConnection`.
#' @return Osynligt: en lista med `event_triggers` och `default_privs`.
#' @export
postgres_grants_auto_visa <- function(con) {
  event_triggers <- DBI::dbGetQuery(con, "
    SELECT evtname AS trigger_name, evtevent AS event, evtenabled AS enabled,
           evtowner::regrole AS owner
    FROM pg_event_trigger ORDER BY evtname;")
  default_privs <- DBI::dbGetQuery(con, "
    SELECT defaclrole::regrole AS role_owner, defaclnamespace::regnamespace AS schema,
           defaclobjtype AS object_type, defaclacl AS privileges
    FROM pg_default_acl ORDER BY defaclrole, defaclnamespace;")

  if (nrow(event_triggers) == 0) message("Inga event triggers hittades.") else {
    message("Event triggers:"); print(tibble::as_tibble(event_triggers))
  }
  if (nrow(default_privs) == 0) message("Inga default privileges hittades.") else {
    parsed <- default_privs |>
      dplyr::mutate(privileges = as.character(.data$privileges)) |>
      tidyr::separate_rows("privileges", sep = ",") |>
      dplyr::mutate(
        privileges = stringr::str_remove_all(.data$privileges, "[{}\"]"),
        grantee    = stringr::str_extract(.data$privileges, "^[^=]+"),
        rights     = stringr::str_extract(.data$privileges, "(?<==)[^/]+"),
        granted_by = stringr::str_extract(.data$privileges, "(?<=/).*"),
        rights_verbose = dplyr::case_when(
          .data$rights == "r" ~ "SELECT only",
          .data$rights == "arwd" ~ "ALL (SELECT, INSERT, UPDATE, DELETE)",
          TRUE ~ .data$rights)) |>
      dplyr::select("role_owner", "schema", "object_type", "grantee", "rights_verbose", "granted_by")
    print(tibble::as_tibble(parsed))
  }
  invisible(list(event_triggers = event_triggers, default_privs = default_privs))
}

#' Testa att auto_grant-triggarna fungerar
#'
#' Skapar ett tillfälligt testschema med tabell/vy/matvy och kontrollerar att
#' `lasroll` och `skrivroll` fått rätt rättigheter automatiskt.
#'
#' @param con En aktiv `DBIConnection`.
#' @return Osynligt: en `data.frame` med kontroller och status.
#' @export
postgres_grants_auto_testa <- function(con) {
  results <- tibble::tibble(kontroll = character(), status = character(), kommentar = character())
  radd <- function(res, k, s, kom) tibble::add_row(res, kontroll = k, status = s, kommentar = kom)

  triggers <- DBI::dbGetQuery(con, "
    SELECT evtname, evtenabled FROM pg_event_trigger
    WHERE evtname IN ('auto_grant_tables', 'auto_grant_schemas');")
  results <- if (nrow(triggers) == 2 && all(triggers$evtenabled == "O")) {
    radd(results, "Event triggers aktiva", "OK", "Båda triggers finns och är aktiva")
  } else {
    radd(results, "Event triggers aktiva", "FAIL", "En eller båda triggers saknas/inaktiva")
  }

  defaults <- DBI::dbGetQuery(con, "
    SELECT defaclacl AS privileges FROM pg_default_acl;")
  har_def <- any(grepl("lasroll", defaults$privileges)) && any(grepl("skrivroll", defaults$privileges))
  results <- radd(results, "Default privileges", if (har_def) "OK" else "FAIL",
                  if (har_def) "lasroll och skrivroll ingår" else "Hittade inga för lasroll/skrivroll")

  ts <- "test_auto_grant"
  try({
    DBI::dbExecute(con, glue::glue("DROP SCHEMA IF EXISTS {ts} CASCADE;"))
    DBI::dbExecute(con, glue::glue("CREATE SCHEMA {ts};"))
    DBI::dbExecute(con, glue::glue("CREATE TABLE {ts}.tabell_test (id serial, namn text);"))
    DBI::dbExecute(con, glue::glue("CREATE VIEW {ts}.vy_test AS SELECT * FROM {ts}.tabell_test;"))
    DBI::dbExecute(con, glue::glue("CREATE MATERIALIZED VIEW {ts}.matvy_test AS SELECT * FROM {ts}.tabell_test;"))
    grants <- DBI::dbGetQuery(con, glue::glue("
      SELECT table_name, grantee, privilege_type FROM information_schema.role_table_grants
      WHERE table_schema = '{ts}' AND grantee IN ('lasroll', 'skrivroll');"))
    for (obj in c("tabell_test", "vy_test", "matvy_test")) {
      sub <- grants[grants$table_name == obj, ]
      las_ok <- "SELECT" %in% sub$privilege_type[sub$grantee == "lasroll"]
      skriv_ok <- all(c("SELECT", "INSERT", "UPDATE", "DELETE") %in% sub$privilege_type[sub$grantee == "skrivroll"])
      results <- radd(results, glue::glue("Trigger-test ({obj})"),
                      if (las_ok && skriv_ok) "OK" else "FAIL",
                      if (las_ok && skriv_ok) "Rättigheter satta automatiskt" else "Rättigheter saknas/felaktiga")
    }
    DBI::dbExecute(con, glue::glue("DROP SCHEMA IF EXISTS {ts} CASCADE;"))
  }, silent = TRUE)

  print(results)
  invisible(results)
}
