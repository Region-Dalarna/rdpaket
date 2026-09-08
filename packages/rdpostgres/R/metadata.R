# Metadata om när tabeller senast uppdaterades (metadata.uppdateringar).

#' Läs metadata-tabellen
#'
#' @param con En `DBIConnection` eller `"default"`.
#' @param tabell,schema Metadata-tabell och -schema.
#' @param query Valfri SQL-svans.
#'
#' @return En `data.frame`, eller `NULL` vid fel.
#' @export
postgres_meta <- function(con = "default", tabell = "aktuell_version",
                          schema = "metadata", query = NA) {
  c <- intern_con(con); on.exit(intern_stang(c), add = TRUE)
  sql <- paste0("SELECT * FROM ", schema, ".", tabell,
                if (!is.na(query)) paste0(" ", query) else "")
  tryCatch(DBI::dbGetQuery(c$con, sql), error = function(e) {
    message("Kunde inte läsa ", schema, ".", tabell, ": ", conditionMessage(e)); NULL
  })
}

#' Skapa vyn `metadata.aktuell_version`
#'
#' Vyn visar den senaste raden per schema/tabell ur `metadata.uppdateringar`.
#'
#' @param con En aktiv `DBIConnection`.
#' @return Osynligt `NULL`.
#' @export
postgres_meta_skapa_vy_aktuell_version <- function(con) {
  if ("aktuell_version" %in% postgres_lista_scheman_tabeller(con = con)$metadata) {
    stop("Vyn metadata.aktuell_version finns redan.", call. = FALSE)
  }
  tryCatch({
    DBI::dbExecute(con, "
      CREATE OR REPLACE VIEW metadata.aktuell_version AS
      SELECT * FROM (
        SELECT DISTINCT ON (schema, tabell)
          id, schema, tabell, version_datum, version_tid,
          uppdaterad_datum, uppdaterad_tid, lyckad_uppdatering, kommentar
        FROM metadata.uppdateringar
        ORDER BY schema, tabell, uppdaterad_datum DESC, uppdaterad_tid DESC
      ) subquery
      ORDER BY uppdaterad_datum ASC, uppdaterad_tid ASC;")
    message("Vyn 'metadata.aktuell_version' skapades.")
  }, error = function(e) message("Ett fel uppstod: ", conditionMessage(e)))
  invisible(NULL)
}

#' Lägg till en rad i metadata.uppdateringar
#'
#' Skapar schemat `metadata` och tabellen `uppdateringar` om de saknas.
#'
#' @param con En aktiv `DBIConnection`.
#' @param schema,tabell Vilket objekt raden gäller.
#' @param version_datum,version_tid Versionens datum/tid (`NA` = samma som
#'   `uppdaterad_*`).
#' @param uppdaterad_datum,uppdaterad_tid När uppdateringen gjordes.
#' @param lyckad_uppdatering `TRUE`/`FALSE`.
#' @param kommentar Fritextkommentar.
#'
#' @return Osynligt `NULL`.
#' @export
postgres_metadata_uppdatera <- function(con, schema, tabell,
                                        version_datum = NA, version_tid = NA,
                                        uppdaterad_datum = Sys.Date(),
                                        uppdaterad_tid = format(Sys.time(), "%H:%M:%S"),
                                        lyckad_uppdatering, kommentar = NA) {
  if (missing(schema) || missing(tabell)) {
    stop("Parametrarna 'schema' och 'tabell' är obligatoriska.", call. = FALSE)
  }
  if (is.na(version_datum)) version_datum <- uppdaterad_datum
  if (is.na(version_tid))   version_tid   <- uppdaterad_tid

  if (!postgres_schema_finns(con, "metadata")) {
    DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS metadata;")
  }
  tabell_finns <- DBI::dbGetQuery(con, "
    SELECT EXISTS (SELECT 1 FROM information_schema.tables
    WHERE table_schema = 'metadata' AND table_name = 'uppdateringar') AS finns;")$finns
  if (!tabell_finns) {
    DBI::dbExecute(con, "
      CREATE TABLE metadata.uppdateringar (
        id SERIAL PRIMARY KEY, schema TEXT NOT NULL, tabell TEXT NOT NULL,
        version_datum DATE, version_tid TIME,
        uppdaterad_datum DATE DEFAULT CURRENT_DATE,
        uppdaterad_tid TIME DEFAULT CURRENT_TIME,
        lyckad_uppdatering BOOLEAN, kommentar TEXT);")
  }

  DBI::dbExecute(con, "
    INSERT INTO metadata.uppdateringar
      (id, schema, tabell, version_datum, version_tid, uppdaterad_datum,
       uppdaterad_tid, lyckad_uppdatering, kommentar)
    VALUES ((SELECT COALESCE(MAX(id), 0) + 1 FROM metadata.uppdateringar),
            $1, $2, $3, $4, $5, $6, $7, $8);",
    params = list(schema, tabell, version_datum, version_tid,
                  uppdaterad_datum, uppdaterad_tid, lyckad_uppdatering, kommentar))
  message("Metadata har lagts till för tabellen: ", schema, ".", tabell)
  invisible(NULL)
}

#' Skriv en data.frame till en tabell och logga i metadata
#'
#' Wrapar [postgres_df_till_postgrestabell()] i `tryCatch` och skriver alltid en
#' rad till `metadata.uppdateringar` (även vid fel).
#'
#' @param con En aktiv `DBIConnection`.
#' @param inlas_df Data att skriva.
#' @param schema,tabell Målschema och måltabell.
#' @param id_kol Primärnyckelkolumn (`NA` = ingen).
#' @param addera_data `TRUE` = lägg till rader i stället för att skriva över.
#' @param felmeddelande_medskickat Om datahämtningen redan misslyckats: texten
#'   loggas och ingen skrivning görs.
#' @param kommentar_metadata Kommentar att spara i metadata vid lyckad skrivning.
#' @param version_datum,version_tid Versionens datum/tid.
#'
#' @return Osynligt `NULL`.
#' @export
postgres_databas_skriv_med_metadata <- function(con, inlas_df, schema, tabell,
                                                id_kol = NA, addera_data = FALSE,
                                                felmeddelande_medskickat = NA,
                                                kommentar_metadata = NA,
                                                version_datum = NA, version_tid = NA) {
  resultat <- NA_character_
  lyckad <- FALSE
  tryCatch({
    if (!is.na(felmeddelande_medskickat)) {
      resultat <- felmeddelande_medskickat
    } else {
      postgres_df_till_postgrestabell(con = con, inlas_df = inlas_df, schema = schema,
                                      tabell = tabell, id_kol = id_kol, addera_data = addera_data)
      resultat <- if (!is.na(kommentar_metadata)) kommentar_metadata else NA_character_
      lyckad <- TRUE
    }
  }, error = function(e) {
    resultat <<- conditionMessage(e)
    message("Data kunde inte läggas till i databasen. Felmeddelande: ", resultat)
  }, finally = {
    postgres_metadata_uppdatera(con = con, schema = schema, tabell = tabell,
                                lyckad_uppdatering = lyckad, kommentar = resultat,
                                version_datum = version_datum, version_tid = version_tid)
  })
  invisible(NULL)
}

#' När uppdaterades tabellen senast? (ISO 8601-sträng)
#'
#' Läser `metadata.uppdateringar`. `schema` kan skickas som `"schema.tabell"`.
#'
#' @param con En `DBIConnection` eller `"default"`.
#' @param schema Schema, eller `"schema.tabell"`.
#' @param tabell Tabell (kan utelämnas om `schema` innehåller punkt).
#'
#' @return En sträng som `"2024-04-12T06:00:00Z"`, eller `NA` om metadata saknas.
#' @export
postgres_tabell_uppdaterades <- function(con = "default", schema, tabell = NULL) {
  if (grepl("\\.", schema)) {
    if (lengths(regmatches(schema, gregexpr("\\.", schema))) > 1) {
      stop("Det får max vara en punkt, som skiljer schema från tabell.", call. = FALSE)
    }
    delar <- strsplit(schema, "\\.")[[1]]
    schema <- delar[1]; tabell <- delar[2]
  }
  if (is.null(tabell)) stop("Parametern 'tabell' saknas.", call. = FALSE)

  c <- intern_con(con); on.exit(intern_stang(c), add = TRUE)
  res <- DBI::dbGetQuery(c$con, glue::glue_sql("
    SELECT version_datum, version_tid FROM metadata.uppdateringar
    WHERE schema = {schema} AND tabell = {tabell}
    ORDER BY version_datum DESC, version_tid DESC LIMIT 1;", .con = c$con))

  if (nrow(res) == 0) {
    message("Ingen metadata hittades för '", schema, ".", tabell, "'.")
    return(NA_character_)
  }
  paste0(format(as.Date(res$version_datum), "%Y-%m-%d"), "T",
         substr(as.character(res$version_tid), 1, 8), "Z")
}

#' När uppdaterades filen senast? (ISO 8601-sträng)
#'
#' @param filnamn_med_sokvag Sökväg till filen.
#' @return En sträng som `"2024-04-12T06:00:00Z"`.
#' @export
fil_dataset_uppdaterades <- function(filnamn_med_sokvag) {
  mtime <- file.info(filnamn_med_sokvag)$mtime
  paste0(format(as.Date(mtime), "%Y-%m-%d"), "T",
         substr(as.character(mtime), 12, 19), "Z")
}

#' Dela upp en ISO 8601-uppdateringssträng i datum och tid
#'
#' @param datum_tid_txt Sträng som `"2024-04-12T06:00:00Z"`.
#' @return En lista med `datum` och `tid` (teckensträngar).
#' @export
uppdaterad_till_text_datum_tid <- function(datum_tid_txt) {
  list(
    datum = as.character(as.Date(datum_tid_txt)),
    tid   = format(as.POSIXct(datum_tid_txt, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), "%H:%M:%S")
  )
}

#' Hämta en PxWeb-tabell och skriv den till en PostgreSQL-tabell
#'
#' Kontrollerar via `pxweb2r` om PxWeb-tabellen uppdaterats sedan
#' PostgreSQL-tabellen senast skrevs, och skriver bara om det behövs (eller om
#' `uppdatering_tvinga = TRUE`).
#'
#' @param tabell_id_pxweb2 Tabell-id i PxWeb.
#' @param db_namn Databasnamn i PostgreSQL.
#' @param schema_db,tabell_db Målschema och måltabell.
#' @param hamta_data_funktion Funktion `function(tabell_id_pxweb2)` som
#'   returnerar data.
#' @param id_kol Primärnyckelkolumn (`NA` = ingen).
#' @param kommentar_metadata Kommentar att spara i metadata.
#' @param uppdatering_tvinga Uppdatera även om det inte behövs.
#' @param verbose Skriv ut vad som händer.
#'
#' @return Osynligt: en lista som beskriver om uppdatering gjordes.
#' @export
postgres_pxweb2_uppdatera_tabell <- function(tabell_id_pxweb2, db_namn, schema_db, tabell_db,
                                             hamta_data_funktion, id_kol = NA,
                                             kommentar_metadata = NA,
                                             uppdatering_tvinga = FALSE, verbose = TRUE) {
  if (!requireNamespace("pxweb2r", quietly = TRUE)) {
    stop("Paketet 'pxweb2r' krävs för postgres_pxweb2_uppdatera_tabell().", call. = FALSE)
  }
  con <- uppkoppling_adm(db_namn)
  on.exit(if (!is.null(con) && DBI::dbIsValid(con)) DBI::dbDisconnect(con), add = TRUE)

  if (postgres_tabell_finns(con, schema_db, tabell_db)) {
    db_uppdaterad <- postgres_tabell_uppdaterades(con = con, schema = schema_db, tabell = tabell_db)
    behover <- pxweb2r::pxweb2_table_needs_update(tabell_id_pxweb2, db_uppdaterad)
  } else {
    behover <- TRUE
    if (verbose) message(schema_db, ".", tabell_db, " finns inte och skapas nu.")
  }

  if (!behover && !uppdatering_tvinga) {
    msg <- paste0("Tabell ", tabell_id_pxweb2, " har inte uppdaterats sedan ",
                  schema_db, ".", tabell_db, " senast skrevs. Ingen uppdatering behövs.")
    if (verbose) message(msg)
    return(invisible(list(uppdaterad = FALSE, tabell_id = tabell_id_pxweb2,
                          schema_db = schema_db, tabell_db = tabell_db, meddelande = msg)))
  }

  if (verbose) message("Hämtar data för tabell ", tabell_id_pxweb2, " ...")
  inlas_df <- hamta_data_funktion(tabell_id_pxweb2)
  datum_tid <- uppdaterad_till_text_datum_tid(pxweb2r::pxweb2_table_updated(tabell_id_pxweb2))

  if (verbose) message("Skriver data till ", schema_db, ".", tabell_db, " ...")
  postgres_databas_skriv_med_metadata(con = con, inlas_df = inlas_df, schema = schema_db,
                                      tabell = tabell_db, id_kol = id_kol,
                                      kommentar_metadata = kommentar_metadata,
                                      version_datum = datum_tid$datum, version_tid = datum_tid$tid)

  invisible(list(uppdaterad = TRUE, tabell_id = tabell_id_pxweb2, schema_db = schema_db,
                 tabell_db = tabell_db, antal_rader = nrow(inlas_df),
                 version_datum = datum_tid$datum, version_tid = datum_tid$tid))
}
