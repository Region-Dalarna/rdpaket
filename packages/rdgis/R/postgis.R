# PostGIS-geometri, tabellreferenser och metadata.

#' Referera till en tabell i en PostGIS-databas
#'
#' Skapar en lättviktig referens till en tabell i databasen. Öppnar ingen
#' anslutning och läser ingen data.
#'
#' @param con En `DBIConnection`, eller `NA` (standard) för Region Dalarnas
#'   databas.
#' @param schema,tabell Schema och tabellnamn.
#'
#' @return Ett objekt av klass `postgis_tabell`.
#' @export
postgis_tabell <- function(con = NA, schema, tabell) {
  structure(list(con = con, schema = schema, tabell = tabell), class = "postgis_tabell")
}

#' @export
print.postgis_tabell <- function(x, ...) {
  cat("<postgis_tabell>", x$schema, ".", x$tabell, "\n", sep = "")
  invisible(x)
}

# Anslutningar som skickats in ägs av anroparen; automatiska anslutningar
# använder tjänsten rd_geodata.
intern_postgis_con <- function(con = "default") {
  if (inherits(con, "DBIConnection")) return(list(con = con, egen = FALSE))
  db <- if (is.null(con) ||
            (length(con) == 1 && is.character(con) && !nzchar(con)) ||
            (length(con) == 1 && !is.character(con) && is.na(con))) {
    "geodata"
  } else {
    con
  }
  ny <- rdpostgres::uppkoppling_db(db_name = db, service_name = "rd_geodata")
  if (is.null(ny)) {
    stop("Kunde inte ansluta till databasen '", db, "'.", call. = FALSE)
  }
  list(con = ny, egen = TRUE)
}

intern_postgis_stang <- function(c) {
  if (isTRUE(c$egen) && DBI::dbIsValid(c$con)) DBI::dbDisconnect(c$con)
}

intern_postgis_meta_state <- function() {
  e <- new.env(parent = emptyenv())
  e$lyckad <- FALSE
  e$kommentar <- NA_character_
  e
}

intern_postgis_ver_stampel <- function(tid = Sys.time()) {
  paste0(as.integer(format(tid, "%d")),
         tolower(format(tid, "%b%Y")), "_",
         format(tid, "%H%M"))
}

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
  cc <- intern_postgis_con(con)
  on.exit(intern_postgis_stang(cc), add = TRUE)
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
  cf <- intern_postgis_con(con_fran_databas)
  on.exit(intern_postgis_stang(cf), add = TRUE)
  con_fran <- cf$con

  st <- intern_postgis_meta_state()
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
      st$kommentar <- glue::glue("{schema_fran}.{tabell_fran} ver: {intern_postgis_ver_stampel(nu)}")
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

#' Skriv ett sf-objekt (eller data.frame) till PostGIS och logga metadata
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
  st <- intern_postgis_meta_state()
  tryCatch({
    if (!is.na(felmeddelande_medskickat)) {
      st$kommentar <- felmeddelande_medskickat
      st$lyckad <- FALSE
    } else {
      withCallingHandlers(
        postgis_sf_till_postgistabell(
          con = con, inlas_sf = inlas_sf, schema = schema, tabell = tabell,
          postgistabell_geo_kol = postgistabell_geo_kol,
          postgistabell_id_kol = postgistabell_id_kol,
          skapa_spatialt_index = !all(is.na(postgistabell_geo_kol)),
          addera_data = postgis_addera_data,
          skriv_over_tabell_om_finns = skriv_over_tabell_om_finns,
          tabellnamn_till_gemener = tabellnamn_till_gemener,
          kolumnnamn_till_gemener = kolumnnamn_till_gemener),
        warning = function(w) {
          if (grepl("Invalid time zone 'UTC', falling back to local time.",
                    conditionMessage(w), fixed = TRUE)) {
            invokeRestart("muffleWarning")
          }
        })
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

#' Skapa en områdesnyckelstabell eller -vy
#'
#' Varje ruta kopplas till det område den överlappar mest (`DISTINCT ON` +
#' `ST_Area(ST_Intersection(...))`).
#'
#' @param con En aktiv `DBIConnection`.
#' @param omrades_schema,omrades_tabell Områdeslagret.
#' @param omrade_urval_kolumner Kolumner att ta med från områdeslagret (`NULL` =
#'   alla).
#' @param rut_schema,rut_tabell,rut_id_kol Rutlagret.
#' @param rut_urval_kolumner Kolumner att ta med från rutlagret (`NULL` = alla).
#' @param utdata_schema,utdata_vy Målschema och -namn (`NULL` = samma som
#'   `omrades_tabell`).
#' @param fran_rutid_till_x_y_kol Namngiven lista med två element `namn = c(start,
#'   slut)` för att skapa x/y-kolumner ur rut-id:t. `NULL` = behåll bara rutid.
#' @param databas_typ `"tabell"`, `"materialiserad_vy"` eller `"vy"`.
#' @param skriv_over_befintlig_tabell_vy Ta bort befintlig tabell/vy först.
#'
#' @return Osynligt: namnet på den skapade tabellen/vyn.
#' @export
postgis_skapa_omradesnyckel_tabell_vy <- function(con, omrades_schema, omrades_tabell,
                                                  omrade_urval_kolumner = NULL,
                                                  rut_schema = "rutor",
                                                  rut_tabell = "bo_syss_100m_alla_ar",
                                                  rut_id_kol = "rutid",
                                                  rut_urval_kolumner = NULL,
                                                  utdata_schema = "omradesnycklar",
                                                  utdata_vy = NULL,
                                                  fran_rutid_till_x_y_kol = list("Ruta100swX" = c(7, 13), "Ruta100swY" = c(1, 6)),
                                                  databas_typ = "vy",
                                                  skriv_over_befintlig_tabell_vy = TRUE) {
  if (is.null(utdata_vy)) utdata_vy <- omrades_tabell

  if (!DBI::dbExistsTable(con, DBI::Id(schema = rut_schema, table = rut_tabell))) {
    stop("Rutlagret '", rut_schema, ".", rut_tabell, "' hittades inte.", call. = FALSE)
  }
  if (!DBI::dbExistsTable(con, DBI::Id(schema = omrades_schema, table = omrades_tabell))) {
    stop("Områdeslagret '", omrades_schema, ".", omrades_tabell, "' hittades inte.", call. = FALSE)
  }

  vy_typ <- switch(databas_typ,
    "tabell" = "TABLE", "materialiserad_vy" = "MATERIALIZED VIEW", "vy" = "VIEW",
    stop("databas_typ måste vara 'tabell', 'materialiserad_vy' eller 'vy'.", call. = FALSE))
  or_replace <- if (databas_typ == "vy") "OR REPLACE " else ""

  rdpostgres::postgres_schema_skapa_om_inte_finns(con = con, schema_namn = utdata_schema)

  geom_kol_for <- function(s, t) {
    DBI::dbGetQuery(con, glue::glue_sql(
      "SELECT f_geometry_column FROM geometry_columns
       WHERE f_table_schema = {s} AND f_table_name = {t}", .con = con))$f_geometry_column
  }
  rut_geom_kol <- geom_kol_for(rut_schema, rut_tabell)
  omrades_geom_kol <- geom_kol_for(omrades_schema, omrades_tabell)

  rut_kolumner <- DBI::dbGetQuery(con, glue::glue_sql(
    "SELECT column_name FROM information_schema.columns
     WHERE table_schema = {rut_schema} AND table_name = {rut_tabell}
       AND column_name != {rut_geom_kol}", .con = con))$column_name
  if (!is.null(rut_urval_kolumner)) rut_kolumner <- intersect(rut_kolumner, rut_urval_kolumner)
  if (length(rut_kolumner) == 0) stop("Inga giltiga kolumner valda för rutlagret.", call. = FALSE)

  omrades_kolumner <- DBI::dbGetQuery(con, glue::glue_sql(
    "SELECT column_name FROM information_schema.columns
     WHERE table_schema = {omrades_schema} AND table_name = {omrades_tabell}
       AND column_name != {omrades_geom_kol}", .con = con))$column_name
  if (!is.null(omrade_urval_kolumner)) omrades_kolumner <- intersect(omrades_kolumner, omrade_urval_kolumner)
  if (length(omrades_kolumner) == 0) stop("Inga giltiga kolumner valda för områdeslagret.", call. = FALSE)

  giltig_split <- !is.null(fran_rutid_till_x_y_kol) &&
    length(fran_rutid_till_x_y_kol) == 2 &&
    !is.null(names(fran_rutid_till_x_y_kol)) && all(nzchar(names(fran_rutid_till_x_y_kol))) &&
    all(vapply(fran_rutid_till_x_y_kol, function(x) is.numeric(x) && length(x) == 2, logical(1)))
  if (!is.null(fran_rutid_till_x_y_kol) && !giltig_split) {
    message("fran_rutid_till_x_y_kol är ogiltig - kör utan uppdelning av rutid.")
  }

  rut_select <- paste(paste0("r.", rut_kolumner), collapse = ",\n      ")
  omrades_select <- paste(paste0("o.", omrades_kolumner), collapse = ",\n      ")
  rutid_split <- if (giltig_split) {
    paste(mapply(function(namn, pos) glue::glue(
      ",\n      substring(r.{rut_id_kol}, {pos[1]}, {pos[2] - pos[1] + 1})::numeric AS {namn}"),
      names(fran_rutid_till_x_y_kol), fran_rutid_till_x_y_kol), collapse = "")
  } else ""

  select_sql <- glue::glue(
    'SELECT DISTINCT ON (r.{rut_id_kol})
      {rut_select},
      {omrades_select}{rutid_split}
      FROM {rut_schema}."{rut_tabell}" r
      JOIN {omrades_schema}."{omrades_tabell}" o
        ON ST_Intersects(r.{rut_geom_kol}, o.{omrades_geom_kol})
      ORDER BY r.{rut_id_kol},
        ST_Area(ST_Intersection(r.{rut_geom_kol}, o.{omrades_geom_kol})) DESC')

  vy_sql <- if (databas_typ == "tabell") {
    glue::glue('CREATE TABLE {utdata_schema}."{utdata_vy}" AS {select_sql};')
  } else {
    glue::glue('CREATE {or_replace}{vy_typ} {utdata_schema}."{utdata_vy}" AS {select_sql};')
  }

  if (skriv_over_befintlig_tabell_vy) {
    befintlig <- DBI::dbGetQuery(con, glue::glue_sql(
      "SELECT CASE
         WHEN EXISTS (SELECT 1 FROM pg_matviews WHERE schemaname = {utdata_schema} AND matviewname = {utdata_vy}) THEN 'materialiserad_vy'
         WHEN EXISTS (SELECT 1 FROM pg_views WHERE schemaname = {utdata_schema} AND viewname = {utdata_vy}) THEN 'vy'
         WHEN EXISTS (SELECT 1 FROM pg_tables WHERE schemaname = {utdata_schema} AND tablename = {utdata_vy}) THEN 'tabell'
         ELSE NULL END AS typ", .con = con))$typ
    if (!is.na(befintlig)) {
      drop_sql <- switch(befintlig,
        "tabell" = glue::glue('DROP TABLE {utdata_schema}."{utdata_vy}";'),
        "materialiserad_vy" = glue::glue('DROP MATERIALIZED VIEW {utdata_schema}."{utdata_vy}";'),
        "vy" = glue::glue('DROP VIEW {utdata_schema}."{utdata_vy}";'))
      message("Tar bort befintlig ", befintlig, ": ", utdata_schema, ".", utdata_vy)
      DBI::dbExecute(con, drop_sql)
    }
  }

  message("Skapar ", databas_typ, ": ", utdata_schema, ".", utdata_vy)
  DBI::dbExecute(con, vy_sql)
  message("Klart: ", utdata_schema, ".", utdata_vy)
  invisible(utdata_vy)
}
