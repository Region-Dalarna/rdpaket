gtfs_db_qname <- function(con, schema, tabell) {
  paste(
    as.character(DBI::dbQuoteIdentifier(con, schema)),
    as.character(DBI::dbQuoteIdentifier(con, tabell)),
    sep = "."
  )
}

synka_historisk_kolumner <- function(con, schema, schema_historisk, tabell) {
  kall_typer <- DBI::dbGetQuery(con, glue::glue(
    "SELECT column_name, data_type
     FROM information_schema.columns
     WHERE table_schema = {DBI::dbQuoteString(con, schema)}
       AND table_name = {DBI::dbQuoteString(con, tabell)};"
  ))

  target_kolumner <- DBI::dbListFields(con, DBI::Id(schema = schema_historisk, table = tabell))
  saknade <- kall_typer[!kall_typer$column_name %in% target_kolumner, , drop = FALSE]

  for (i in seq_len(nrow(saknade))) {
    historisk_tabell <- gtfs_db_qname(con, schema_historisk, tabell)
    kolumn <- as.character(DBI::dbQuoteIdentifier(con, saknade$column_name[i]))
    DBI::dbExecute(con, glue::glue(
      "ALTER TABLE {historisk_tabell} ADD COLUMN {kolumn} {saknade$data_type[i]};"
    ))
    message(glue::glue(
      "Lade till ny kolumn i {schema_historisk}.{tabell}: ",
      "{saknade$column_name[i]} ({saknade$data_type[i]})"
    ))
  }

  invisible(TRUE)
}

hamta_gemensamma_kolumner <- function(con, schema, schema_historisk, tabell) {
  kall_kolumner <- DBI::dbListFields(con, DBI::Id(schema = schema, table = tabell))
  target_kolumner <- DBI::dbListFields(con, DBI::Id(schema = schema_historisk, table = tabell))

  gemensamma <- setdiff(intersect(kall_kolumner, target_kolumner), "version")
  if (length(gemensamma) == 0) stop(glue::glue("Inga gemensamma kolumner hittades för tabellen {tabell}"))
  gemensamma
}

#' Version GTFS database contents
#'
#' Archives the current database feed when the incoming calendar has a new end date.
#' @describeIn rdgtfs-api Version database feeds
#' @param con A DBI database connection.
#' @param gtfs_data A named list of incoming GTFS tables.
#' @param schema Database schema for current GTFS data.
#' @param vy_operatorer Controls historical views created after an archive; `NULL` disables them.
#' @return `TRUE` invisibly when a version is created, otherwise `FALSE`.
#' @export
versionshantering <- function(con, gtfs_data, schema = "gtfs", vy_operatorer = NA) {
  schema_h <- gtfs_historiskt_schema(schema)
  versions_tabell <- gtfs_db_qname(con, schema_h, "versions")
  calendar_dates <- gtfs_db_qname(con, schema, "calendar_dates")

  tryCatch({
    DBI::dbExecute(con, glue::glue("
      CREATE TABLE IF NOT EXISTS {versions_tabell} (
        version INTEGER PRIMARY KEY, start_date DATE, end_date DATE
      );"))

    sista_datum_db <- as.Date(DBI::dbGetQuery(con, glue::glue(
      "SELECT MAX(date) AS sista_datum FROM {calendar_dates};"
    ))$sista_datum[1])
    sista_datum_gtfs_data <- max(gtfs_text_till_datum(gtfs_data$calendar_dates$date))

    if (!is.na(sista_datum_db) && sista_datum_db == sista_datum_gtfs_data) {
      return(invisible(FALSE))
    }

    senaste_version <- DBI::dbGetQuery(con, glue::glue(
      "SELECT MAX(version) AS senaste_version FROM {versions_tabell};"
    ))$senaste_version[1]
    ny_version <- if (is.na(senaste_version)) 1L else as.integer(senaste_version) + 1L

    start_datum_gtfs_data <- if (is.na(sista_datum_db)) {
      min(gtfs_text_till_datum(gtfs_data$calendar_dates$date))
    } else {
      sista_datum_db + 1
    }

    tabeller <- c(
      "agency", "routes", "calendar", "calendar_dates", "shapes_line",
      "stops", "trips", "stop_times", "linjeklassificering",
      gtfs_valfria_tabeller_finns(con, schema, schema_h)
    )

    gtfs_transaktion(con, {
      if (!is.na(senaste_version)) {
        agency_historisk <- gtfs_db_qname(con, schema_h, "agency")
        agency_finns <- DBI::dbGetQuery(con, glue::glue(
          "SELECT EXISTS (SELECT 1 FROM {agency_historisk} WHERE version = {as.integer(senaste_version)});"
        ))$exists[1]

        if (agency_finns) {
          warning(glue::glue(
            "Version {senaste_version} verkar redan vara arkiverad i {schema_h} ",
            "(hittade rader i agency med version = {senaste_version}). ",
            "Hoppar över arkiveringssteget men fortsätter skapa ny version {ny_version}."
          ))
        } else {
          DBI::dbExecute(con, glue::glue(
            "UPDATE {versions_tabell}
             SET end_date = {DBI::dbQuoteString(con, as.character(sista_datum_db))}
             WHERE version = {as.integer(senaste_version)};"
          ))

          for (tabell in tabeller) {
            synka_historisk_kolumner(con, schema, schema_h, tabell)
            kolumner <- hamta_gemensamma_kolumner(con, schema, schema_h, tabell)
            kolumner_q <- paste(
              as.character(DBI::dbQuoteIdentifier(con, kolumner)),
              collapse = ", "
            )
            aktuell_tabell <- gtfs_db_qname(con, schema, tabell)
            historisk_tabell <- gtfs_db_qname(con, schema_h, tabell)
            DBI::dbExecute(con, glue::glue(
              "INSERT INTO {historisk_tabell} ({kolumner_q}, version)
               SELECT {kolumner_q}, {as.integer(senaste_version)} FROM {aktuell_tabell}
               ON CONFLICT DO NOTHING;"
            ))
          }
        }
      }

      DBI::dbExecute(con, glue::glue(
        "INSERT INTO {versions_tabell} (version, start_date)
         VALUES ({ny_version}, {DBI::dbQuoteString(con, as.character(start_datum_gtfs_data))})
         ON CONFLICT (version) DO NOTHING;"
      ))
    })

    if (!is.na(senaste_version)) {
      for (op in vy_operatorer) {
        if (is.na(op)) {
          skapa_vyer_historisk_hallplats(con, schema = schema)
          skapa_vyer_historisk_linjer(con, schema = schema)
        } else {
          skapa_vyer_historisk_hallplats(con, schema = schema, agency_namn = op)
          skapa_vyer_historisk_linjer(con, schema = schema, agency_namn = op)
        }
      }
    }

    invisible(TRUE)
  }, error = function(e) {
    stop(glue::glue("Ett fel inträffade vid versionshanteringen: {conditionMessage(e)}"), call. = FALSE)
  })
}

#' Delete old GTFS versions
#'
#' Removes historical versions whose end date is older than the requested age.
#' @describeIn rdgtfs-api Delete old history
#' @param con A DBI database connection.
#' @param antal_ar Age threshold in years.
#' @param schema Base database schema.
#' @return Deleted version numbers, invisibly.
#' @export
radera_gamla_versioner <- function(con, antal_ar, schema = "gtfs") {
  schema_h <- gtfs_historiskt_schema(schema)
  versions_tabell <- gtfs_db_qname(con, schema_h, "versions")

  tryCatch({
    grans <- Sys.Date() - round(antal_ar * 365)
    gamla_versioner <- DBI::dbGetQuery(con, glue::glue(
      "SELECT version FROM {versions_tabell}
       WHERE end_date < {DBI::dbQuoteString(con, as.character(grans))};"
    ))$version

    if (length(gamla_versioner) == 0) return(invisible(integer(0)))
    versioner <- paste(as.integer(gamla_versioner), collapse = ", ")
    tabeller <- c(
      gtfs_valfria_tabeller_finns(con, schema, schema_h),
      "stop_times", "trips", "stops", "shapes_line", "calendar_dates",
      "calendar", "routes", "agency", "linjeklassificering"
    )

    gtfs_transaktion(con, {
      for (tabell in tabeller) {
        tabell_q <- gtfs_db_qname(con, schema_h, tabell)
        DBI::dbExecute(con, glue::glue(
          "DELETE FROM {tabell_q} WHERE version IN ({versioner});"
        ))
      }
      DBI::dbExecute(con, glue::glue(
        "DELETE FROM {versions_tabell} WHERE version IN ({versioner});"
      ))
    })

    invisible(gamla_versioner)
  }, error = function(e) {
    stop(glue::glue("Ett fel inträffade vid radering av gamla versioner: {conditionMessage(e)}"), call. = FALSE)
  })
}

#' Upload GTFS data to the database
#'
#' Replaces current feed tables and writes stop and shape geometries to PostGIS.
#' @describeIn rdgtfs-api Upload a GTFS feed
#' @param con A DBI database connection.
#' @param gtfs_data A named list of GTFS tables.
#' @param schema Target database schema.
#' @param gtfs_dataset Dataset kind, `regional` or `sweden_3`.
#' @param transaktion Run truncate and upload in a transaction.
#' @return `TRUE` invisibly.
#' @export
ladda_upp_till_databas <- function(con, gtfs_data, schema = "gtfs",
                                   gtfs_dataset = "regional", transaktion = FALSE) {
  sweden_3 <- gtfs_kontrollera_dataset(gtfs_dataset)
  tabeller_att_truncate <- c(
    "stop_times", "trips", "routes", "agency", "stops",
    "shapes_line", "calendar", "calendar_dates"
  )
  tabeller_bas <- c("agency", "routes", "calendar", "calendar_dates", "trips", "stop_times")
  if (sweden_3) {
    tabeller_att_truncate <- c(
      tabeller_att_truncate, "shapes", "transfers", "attributions", "feed_info"
    )
    tabeller_bas <- c(tabeller_bas, "shapes", "transfers", "attributions", "feed_info")
  }

  jobb <- function() {
    truncate_lista <- paste(
      vapply(tabeller_att_truncate, function(tabell) gtfs_db_qname(con, schema, tabell), character(1)),
      collapse = ", "
    )
    DBI::dbExecute(con, glue::glue(
      "TRUNCATE TABLE {truncate_lista} RESTART IDENTITY CASCADE;"
    ))
    message("Tabeller truncade.")

    if (!is.null(gtfs_data$stops)) {
      sf_stops <- sf::st_as_sf(
        as.data.frame(gtfs_data$stops),
        coords = c("stop_lon", "stop_lat"), crs = 4326, remove = FALSE
      ) |>
        sf::st_transform(3006)
      sf_stops <- gtfs_begransa_till_tabell(con, schema, "stops", sf_stops)
      sf::st_write(
        obj = sf_stops, dsn = con, DBI::Id(schema = schema, table = "stops"),
        geomtype = "POINT", delete_layer = FALSE, append = TRUE
      )
      rm(sf_stops)
      message("stops uppladdad.")
    }

    if (!is.null(gtfs_data$shapes)) {
      sf_shapes_line <- gtfs_shapes_till_linjer(gtfs_data$shapes, crs = 4326) |>
        sf::st_transform(3006)
      sf::st_write(
        obj = sf_shapes_line, dsn = con, DBI::Id(schema = schema, table = "shapes_line"),
        geomtype = "LINESTRING", delete_layer = FALSE, append = TRUE
      )
      rm(sf_shapes_line)
      message("shapes_line uppladdad.")
    }

    for (tabell in tabeller_bas) {
      if (!is.null(gtfs_data[[tabell]])) {
        df <- gtfs_begransa_till_tabell(
          con, schema, tabell, as.data.frame(gtfs_data[[tabell]])
        )
        DBI::dbWriteTable(
          con, DBI::Id(schema = schema, table = tabell), df,
          append = TRUE, row.names = FALSE
        )
        message(glue::glue("{tabell} uppladdad."))
      } else {
        message(glue::glue("{tabell} saknas i gtfs_data — hoppas över."))
      }
    }
  }

  tryCatch({
    if (transaktion) gtfs_transaktion(con, jobb()) else jobb()
  }, error = function(e) {
    stop(paste("Ett fel inträffade vid uppladdningen av data till databasen:", conditionMessage(e)),
         call. = FALSE)
  })

  invisible(TRUE)
}

#' Populate Dalarna route classifications
#'
#' Rebuilds the route classification table using Dalarna-specific line-number rules.
#' @describeIn rdgtfs-api Create Dalarna route classes
#' @param con A DBI database connection.
#' @param schema Database schema containing routes and classification tables.
#' @return `TRUE` invisibly.
#' @export
skapa_tabell_linjeklassificering <- function(con, schema = "gtfs") {
  routes_tabell <- gtfs_db_qname(con, schema, "routes")
  klassificering_tabell <- gtfs_db_qname(con, schema, "linjeklassificering")

  tryCatch({
    gtfs_transaktion(con, {
      DBI::dbExecute(con, glue::glue("TRUNCATE {klassificering_tabell};"))
      DBI::dbExecute(con, glue::glue("
        INSERT INTO {klassificering_tabell} (route_short_name, klassificering)
        SELECT DISTINCT ON (route_short_name) route_short_name,
               CASE
                 WHEN route_short_name ~ '^[0-9]{{1,6}}$' THEN
                   CASE
                     WHEN CAST(route_short_name AS INTEGER) = 39 THEN 'Landsbygdstrafik'
                     WHEN CAST(route_short_name AS INTEGER) IN (42, 46, 241, 360, 500) THEN 'Utomlänstrafik'
                     WHEN CAST(route_short_name AS INTEGER) BETWEEN 1 AND 99 THEN 'Stadstrafik'
                     WHEN CAST(route_short_name AS INTEGER) BETWEEN 151 AND 154 THEN 'Stadstrafik'
                     WHEN CAST(route_short_name AS INTEGER) BETWEEN 101 AND 199 THEN 'Stråktrafik'
                     WHEN CAST(route_short_name AS INTEGER) BETWEEN 100 AND 400 THEN 'Landsbygdstrafik'
                     WHEN CAST(route_short_name AS INTEGER) BETWEEN 430 AND 800 THEN 'Flextrafik'
                     WHEN CAST(route_short_name AS INTEGER) BETWEEN 900 AND 1000 THEN 'Stängd skoltrafik'
                     ELSE 'Okänd'
                   END
                 WHEN route_short_name = 'Tåg' THEN 'Tåg'
                 ELSE 'Okänd'
               END AS klassificering
        FROM {routes_tabell}
        ORDER BY route_short_name, route_id;
      "))
    })
  }, error = function(e) {
    stop(paste("Ett fel inträffade vid skapandet av linjeklassificeringen:", conditionMessage(e)),
         call. = FALSE)
  })

  invisible(TRUE)
}

#' Look up a GTFS agency ID by name
#'
#' Matches a case-insensitive substring and errors if the match is missing or ambiguous.
#' @describeIn rdgtfs-api Look up an agency ID
#' @param con A DBI database connection.
#' @param schema Database schema containing the agency table.
#' @param agency_namn Agency name substring to match.
#' @return The matching agency ID.
#' @export
hamta_agency_id <- function(con, schema = "gtfs", agency_namn) {
  agency_tabell <- gtfs_db_qname(con, schema, "agency")
  namn <- as.character(DBI::dbQuoteString(con, paste0("%", agency_namn, "%")))
  traff <- DBI::dbGetQuery(con, glue::glue("
    SELECT agency_id::text AS agency_id, agency_name
    FROM {agency_tabell}
    WHERE agency_name ILIKE {namn}
  "))

  if (nrow(traff) == 0) {
    stop(glue::glue("Ingen operatör matchande '{agency_namn}' hittades i {schema}.agency."), call. = FALSE)
  }
  if (nrow(traff) > 1) {
    stop(glue::glue(
      "Flera operatörer matchar '{agency_namn}': {paste(traff$agency_name, collapse = ', ')}. ",
      "Skriv en mer specifik del av namnet."
    ), call. = FALSE)
  }

  message(glue::glue(
    "Matchade '{agency_namn}' -> agency_id = '{traff$agency_id}' ({traff$agency_name})"
  ))
  traff$agency_id
}
