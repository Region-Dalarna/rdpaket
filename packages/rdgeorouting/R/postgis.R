# PostGIS: aktivera tillägget, skriva/läsa sf-tabeller, kopiera/flytta tabeller.

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

#' Skriv ett sf-objekt till en PostGIS-tabell
#'
#' Skapar schemat om det saknas, hanterar spatialt index och primärnyckel.
#'
#' @param con En `DBIConnection` eller `"default"` (geodata).
#' @param inlas_sf sf-objektet som skrivs.
#' @param schema,tabell Målschema och måltabell.
#' @param postgistabell_id_kol Kolumn som ska bli primärnyckel (`NA` = ingen).
#' @param postgistabell_geo_kol Geometrikolumnens namn (krävs för spatialt index).
#' @param skapa_spatialt_index Skapa GIST-index på geometrikolumnen.
#' @param addera_data `TRUE` = lägg till rader i stället för att skriva över.
#' @param skriv_over_tabell_om_finns `TRUE` = ta bort och skapa om (t.ex. vid
#'   kolumnändringar); `FALSE` behåller strukturen.
#' @param tabellnamn_till_gemener,kolumnnamn_till_gemener Gör namn till gemener.
#'
#' @return Osynligt `NULL`.
#' @export
postgis_sf_till_postgistabell <- function(con = "default", inlas_sf,
                                          schema = "karta", tabell,
                                          postgistabell_id_kol = NA,
                                          postgistabell_geo_kol = NA,
                                          skapa_spatialt_index = TRUE,
                                          addera_data = FALSE,
                                          skriv_over_tabell_om_finns = FALSE,
                                          tabellnamn_till_gemener = TRUE,
                                          kolumnnamn_till_gemener = TRUE) {
  intern_krav("sf")
  if (all(is.na(postgistabell_geo_kol)) && skapa_spatialt_index) {
    stop("postgistabell_geo_kol måste anges när skapa_spatialt_index = TRUE.", call. = FALSE)
  }
  cc <- intern_rutt_con(con, adm = FALSE, standard_db = "geodata")
  on.exit(intern_stang(cc), add = TRUE)
  con <- cc$con

  if (kolumnnamn_till_gemener) names(inlas_sf) <- tolower(names(inlas_sf))
  if (tabellnamn_till_gemener) tabell <- tolower(tabell)

  if (!rdpostgres::postgres_schema_finns(con, schema)) {
    DBI::dbExecute(con, glue::glue_sql("CREATE SCHEMA IF NOT EXISTS {`schema`};", .con = con))
  }

  tabell_finns <- DBI::dbExistsTable(con, DBI::Id(schema = schema, table = tabell))
  if (!tabell_finns) {
    append_mode <- FALSE
  } else if (addera_data) {
    append_mode <- TRUE
  } else {
    DBI::dbExecute(con, glue::glue_sql("TRUNCATE TABLE {`schema`}.{`tabell`};", .con = con))
    append_mode <- !skriv_over_tabell_om_finns
  }

  sf::st_write(obj = inlas_sf, dsn = con,
               layer = DBI::Id(schema = schema, table = tabell),
               append = append_mode, quiet = TRUE)

  if (skapa_spatialt_index && !all(is.na(postgistabell_geo_kol)) && length(postgistabell_geo_kol) > 0) {
    for (geokol in postgistabell_geo_kol) {
      idx <- paste0(geokol, "_idx")
      DBI::dbExecute(con, glue::glue_sql("DROP INDEX IF EXISTS {`schema`}.{`idx`};", .con = con))
      DBI::dbExecute(con, glue::glue_sql(
        "CREATE INDEX {`idx`} ON {`schema`}.{`tabell`} USING GIST ({`geokol`});", .con = con))
    }
  }

  if (!is.na(postgistabell_id_kol)) {
    pk_finns <- nrow(DBI::dbGetQuery(con, glue::glue_sql(
      "SELECT 1 FROM information_schema.table_constraints
       WHERE table_schema = {schema} AND table_name = {tabell} AND constraint_type = 'PRIMARY KEY'",
      .con = con))) > 0
    if (!pk_finns) {
      DBI::dbExecute(con, glue::glue_sql(
        "ALTER TABLE {`schema`}.{`tabell`} ADD PRIMARY KEY ({`postgistabell_id_kol`});", .con = con))
    }
  }
  invisible(NULL)
}

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
  cc <- intern_rutt_con(con, adm = FALSE, standard_db = "geodata")
  on.exit(intern_stang(cc), add = TRUE)
  con <- cc$con

  if (!rdpostgres::postgres_tabell_finns(con = con, schema = schema_fran, tabell = tabell_fran)) {
    stop("Tabellen ", schema_fran, ".", tabell_fran, " finns inte.", call. = FALSE)
  }
  if (rdpostgres::postgres_tabell_finns(con = con, schema = schema_till, tabell = tabell_till)) {
    if (skriv_over) {
      rdpostgres::postgres_tabell_ta_bort(con = con, schema = schema_till, tabell = tabell_till)
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

#' Kopiera en tabell mellan två PostGIS-databaser
#'
#' Läser via `sf`, skriver till målet, uppdaterar SRID och loggar i
#' `metadata.uppdateringar` (tar versionsdatum/-tid från källans metadata om det
#' finns).
#'
#' @param con_fran_databas,con_till_databas `DBIConnection`-objekt (eller
#'   `"default"` för källan).
#' @param schema_fran,tabell_fran Källa.
#' @param schema_till,tabell_till Mål.
#'
#' @return Osynligt `NULL`.
#' @export
postgis_kopiera_tabell_mellan_databaser <- function(con_fran_databas,
                                                    con_till_databas,
                                                    schema_fran, tabell_fran,
                                                    schema_till, tabell_till) {
  intern_krav("sf")
  cf <- intern_rutt_con(con_fran_databas, adm = FALSE, standard_db = "geodata")
  on.exit(intern_stang(cf), add = TRUE)
  con_fran <- cf$con

  st <- intern_meta_state()
  st$ver_datum <- NA
  st$ver_tid <- NA

  tryCatch({
    dat <- sf::st_read(con_fran, layer = DBI::Id(schema = schema_fran, table = tabell_fran), quiet = TRUE)
    sf::st_write(dsn = con_till_databas, obj = dat,
                 layer = DBI::Id(schema = schema_till, table = tabell_till),
                 append = FALSE, quiet = TRUE)

    srid <- sf::st_crs(dat)$epsg
    geom_kol <- attr(dat, "sf_column")
    if (!is.na(srid) && !is.null(geom_kol)) {
      DBI::dbExecute(con_till_databas, glue::glue(
        "SELECT UpdateGeometrySRID('{schema_till}', '{tabell_till}', '{geom_kol}', {srid});"))
    }

    meta_fran <- rdpostgres::postgres_meta(
      con = con_fran,
      query = glue::glue("WHERE schema = '{schema_fran}' AND tabell = '{tabell_fran}'"))

    if (is.null(meta_fran) || nrow(meta_fran) == 0) {
      nu <- Sys.time()
      st$ver_datum <- format(nu, "%Y-%m-%d")
      st$ver_tid <- format(nu, "%H:%M")
      st$kommentar <- glue::glue("{schema_fran}.{tabell_fran} ver: {intern_ver_stampel(nu)}")
    } else {
      st$ver_datum <- meta_fran$version_datum[1]
      st$ver_tid <- meta_fran$version_tid[1]
      st$kommentar <- meta_fran$kommentar[1]
    }
    st$lyckad <- TRUE
  }, error = function(e) {
    st$kommentar <- conditionMessage(e)
    st$lyckad <- FALSE
    message("Kunde inte kopiera tabellen: ", st$kommentar)
  }, finally = {
    rdpostgres::postgres_metadata_uppdatera(
      con = con_till_databas, schema = schema_till, tabell = tabell_till,
      version_datum = st$ver_datum, version_tid = st$ver_tid,
      lyckad_uppdatering = st$lyckad, kommentar = st$kommentar)
  })
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
  cc <- intern_rutt_con(con, adm = FALSE, standard_db = "geodata")
  on.exit(intern_stang(cc), add = TRUE)
  DBI::dbExecute(cc$con, glue::glue_sql(
    "ALTER TABLE {`schema_fran`}.{`tabell_fran`} SET SCHEMA {`schema_till`};", .con = cc$con))
  invisible(NULL)
}

#' Skriv ett sf-objekt (eller data.frame) till PostGIS och logga i metadata
#'
#' Wrapar [postgis_sf_till_postgistabell()] i `tryCatch` och skriver alltid en
#' rad till `metadata.uppdateringar`.
#'
#' @param con En aktiv `DBIConnection`.
#' @param inlas_sf sf-objektet (eller data.frame).
#' @param schema,tabell Målschema och måltabell.
#' @param postgistabell_geo_kol Geometrikolumn (`NA` = ingen geometri/index).
#' @param postgistabell_id_kol Primärnyckelkolumn (`NA` = ingen).
#' @param postgis_addera_data `TRUE` = lägg till rader.
#' @param skriv_over_tabell_om_finns Se [postgis_sf_till_postgistabell()].
#' @param felmeddelande_medskickat Om datahämtningen redan misslyckats: loggas,
#'   ingen skrivning görs.
#' @param kommentar_metadata Kommentar vid lyckad skrivning.
#' @param tabellnamn_till_gemener,kolumnnamn_till_gemener Gör namn till gemener.
#' @param version_datum,version_tid Versionens datum/tid.
#'
#' @return Osynligt `NULL`.
#' @export
postgis_databas_skriv_med_metadata <- function(con, inlas_sf, schema, tabell,
                                               postgistabell_geo_kol = "geometry",
                                               postgistabell_id_kol = NA,
                                               postgis_addera_data = FALSE,
                                               skriv_over_tabell_om_finns = TRUE,
                                               felmeddelande_medskickat = NA,
                                               kommentar_metadata = NA,
                                               tabellnamn_till_gemener = TRUE,
                                               kolumnnamn_till_gemener = TRUE,
                                               version_datum = NA, version_tid = NA) {
  st <- intern_meta_state()
  tryCatch({
    if (!is.na(felmeddelande_medskickat)) {
      st$kommentar <- felmeddelande_medskickat
      st$lyckad <- FALSE
    } else {
      intern_tyst_varning(
        postgis_sf_till_postgistabell(
          con = con, inlas_sf = inlas_sf, schema = schema, tabell = tabell,
          postgistabell_geo_kol = postgistabell_geo_kol,
          postgistabell_id_kol = postgistabell_id_kol,
          skapa_spatialt_index = !all(is.na(postgistabell_geo_kol)),
          addera_data = postgis_addera_data,
          skriv_over_tabell_om_finns = skriv_over_tabell_om_finns,
          tabellnamn_till_gemener = tabellnamn_till_gemener,
          kolumnnamn_till_gemener = kolumnnamn_till_gemener),
        "Invalid time zone 'UTC', falling back to local time.")
      st$kommentar <- if (!is.na(kommentar_metadata)) kommentar_metadata else NA_character_
      st$lyckad <- TRUE
    }
  }, error = function(e) {
    st$kommentar <- conditionMessage(e)
    st$lyckad <- FALSE
    message("Data kunde inte läggas till i databasen. Felmeddelande: ", st$kommentar)
  }, finally = {
    rdpostgres::postgres_metadata_uppdatera(
      con = con, schema = schema, tabell = tabell,
      lyckad_uppdatering = st$lyckad, kommentar = st$kommentar,
      version_datum = version_datum, version_tid = version_tid)
  })
  invisible(NULL)
}
