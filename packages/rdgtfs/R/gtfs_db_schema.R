gtfs_historiskt_schema <- function(schema) paste0(schema, "_historisk")

gtfs_transaktion <- function(con, kod) {
  egen <- tryCatch({ DBI::dbBegin(con); TRUE }, error = function(e) FALSE)
  if (!egen) return(kod)

  tryCatch({
    resultat <- kod
    DBI::dbCommit(con)
    resultat
  }, error = function(e) {
    try(DBI::dbRollback(con), silent = TRUE)
    stop(e)
  })
}

gtfs_valfria_tabeller_finns <- function(con, schema, schema_h) {
  valfria <- c("transfers", "attributions", "feed_info")
  finns <- vapply(valfria, function(t) {
    DBI::dbExistsTable(con, DBI::Id(schema = schema, table = t)) &&
      DBI::dbExistsTable(con, DBI::Id(schema = schema_h, table = t))
  }, logical(1))
  valfria[finns]
}

gtfs_fordonstyp_sql <- function() {
  "CASE
     WHEN r.route_type BETWEEN 100 AND 199 OR r.route_type = 2 THEN 'Tåg'
     WHEN r.route_type BETWEEN 400 AND 499 OR r.route_type = 1 THEN 'Tunnelbana'
     WHEN r.route_type BETWEEN 200 AND 299 OR r.route_type BETWEEN 700 AND 799 OR r.route_type = 3 THEN 'Buss'
     WHEN r.route_type BETWEEN 900 AND 999 OR r.route_type = 0 THEN 'Spårvagn'
     WHEN r.route_type BETWEEN 1000 AND 1099 OR r.route_type = 4 THEN 'Färja'
     WHEN r.route_type BETWEEN 1500 AND 1599 THEN 'Förbeställningstrafik'
     ELSE 'Övrigt'
   END"
}

gtfs_vy_parametrar <- function(con, schema, agency_id, agency_namn, vy_suffix) {
  if (length(schema) != 1 || is.na(schema) ||
      !grepl("^[A-Za-z_][A-Za-z0-9_]*$", schema)) {
    stop("schema måste vara ett giltigt SQL-schemaidentifierare.", call. = FALSE)
  }
  if (!is.null(agency_namn)) {
    if (!is.null(agency_id)) stop("Ange antingen agency_id eller agency_namn, inte båda.", call. = FALSE)
    agency_id <- hamta_agency_id(con, schema, agency_namn)
  }
  if (!is.null(vy_suffix)) {
    if (length(vy_suffix) != 1 || is.na(vy_suffix)) {
      stop("vy_suffix måste vara ett enda värde.", call. = FALSE)
    }
    vy_suffix <- gsub("[^a-zA-Z0-9_]", "_", vy_suffix)
  }
  if (!is.null(agency_id) && is.null(vy_suffix)) {
    vy_suffix <- paste0("_", gsub("[^a-z0-9_]", "_", tolower(agency_id)))
  }
  if (is.null(vy_suffix)) vy_suffix <- ""

  agency_filter <- if (!is.null(agency_id)) {
    glue::glue("AND r.agency_id = {DBI::dbQuoteString(con, agency_id)}")
  } else {
    ""
  }
  list(vy_suffix = vy_suffix, agency_filter = agency_filter)
}

gtfs_skapa_geomindex <- function(con, schema, vy) {
  schema_q <- as.character(DBI::dbQuoteIdentifier(con, schema))
  vy_q <- as.character(DBI::dbQuoteIdentifier(con, vy))
  index_q <- as.character(DBI::dbQuoteIdentifier(con, paste0("idx_", vy, "_geom")))
  DBI::dbExecute(con, glue::glue(
    "CREATE INDEX {index_q} ON {schema_q}.{vy_q} USING GIST (geometry);"
  ))
}

gtfs_begransa_till_tabell <- function(con, schema, tabell, df) {
  tabellkolumner <- DBI::dbListFields(con, DBI::Id(schema = schema, table = tabell))
  saknas <- setdiff(names(df), tabellkolumner)
  if (length(saknas) > 0) {
    message(glue::glue("{tabell}: hoppar över kolumner som saknas i databastabellen: {paste(saknas, collapse = ', ')}"))
    df <- df[, setdiff(names(df), saknas), drop = FALSE]
  }
  df
}

#' Create current and historical GTFS tables
#'
#' Creates the PostGIS tables and indexes needed to load GTFS feeds and their history.
#' @describeIn rdgtfs-api Create database tables
#' @param con A DBI database connection.
#' @param schema Database schema for current GTFS data.
#' @param gtfs_dataset Dataset kind, `regional` or `sweden_3`.
#' @return `TRUE` invisibly.
#' @export
skapa_tabeller <- function(con, schema = "gtfs", gtfs_dataset = "regional") {
  sweden_3 <- gtfs_kontrollera_dataset(gtfs_dataset)
  schema_h <- gtfs_historiskt_schema(schema)
  schema_q <- as.character(DBI::dbQuoteIdentifier(con, schema))
  schema_h_q <- as.character(DBI::dbQuoteIdentifier(con, schema_h))
  kor <- function(sql) DBI::dbExecute(con, glue::glue(sql))

  aktuella <- c(
    "CREATE TABLE IF NOT EXISTS {schema_q}.agency (
       agency_id VARCHAR PRIMARY KEY, agency_name VARCHAR NOT NULL,
       agency_url VARCHAR NOT NULL, agency_timezone VARCHAR NOT NULL,
       agency_lang VARCHAR, agency_fare_url VARCHAR
     );",
    "CREATE TABLE IF NOT EXISTS {schema_q}.routes (
       route_id VARCHAR PRIMARY KEY, agency_id VARCHAR REFERENCES {schema_q}.agency(agency_id),
       route_short_name VARCHAR NOT NULL, route_long_name VARCHAR NOT NULL,
       route_desc VARCHAR, route_type INTEGER NOT NULL
     );",
    "CREATE INDEX IF NOT EXISTS idx_routes_route_short_name ON {schema_q}.routes (route_short_name);",
    "CREATE TABLE IF NOT EXISTS {schema_q}.calendar (
       service_id VARCHAR PRIMARY KEY, monday INTEGER, tuesday INTEGER, wednesday INTEGER,
       thursday INTEGER, friday INTEGER, saturday INTEGER, sunday INTEGER,
       start_date DATE, end_date DATE
     );",
    "CREATE TABLE IF NOT EXISTS {schema_q}.calendar_dates (
       service_id VARCHAR, date DATE, exception_type INTEGER, PRIMARY KEY (service_id, date)
     );",
    "CREATE TABLE IF NOT EXISTS {schema_q}.shapes_line (
       shape_id VARCHAR PRIMARY KEY, geometry GEOMETRY(Linestring, 3006),
       antal_punkter INTEGER, max_dist FLOAT
     );",
    "CREATE INDEX IF NOT EXISTS idx_shapes_line_geometry ON {schema_q}.shapes_line USING GIST (geometry);",
    "CREATE TABLE IF NOT EXISTS {schema_q}.shapes (
       shape_id VARCHAR, shape_pt_lat FLOAT, shape_pt_lon FLOAT,
       shape_pt_sequence INTEGER, shape_dist_traveled FLOAT,
       PRIMARY KEY (shape_id, shape_pt_sequence)
     );",
    "CREATE INDEX IF NOT EXISTS idx_shapes_shape_id ON {schema_q}.shapes (shape_id);"
  )

  trips <- if (sweden_3) {
    "CREATE TABLE IF NOT EXISTS {schema_q}.trips (
       trip_id VARCHAR PRIMARY KEY, route_id VARCHAR REFERENCES {schema_q}.routes(route_id),
       service_id VARCHAR NOT NULL, trip_headsign VARCHAR, trip_short_name VARCHAR,
       direction_id INTEGER, shape_id VARCHAR, samtrafiken_internal_trip_number VARCHAR
     );"
  } else {
    "CREATE TABLE IF NOT EXISTS {schema_q}.trips (
       trip_id VARCHAR PRIMARY KEY, route_id VARCHAR REFERENCES {schema_q}.routes(route_id),
       service_id VARCHAR NOT NULL, trip_headsign VARCHAR, direction_id INTEGER, shape_id VARCHAR
     );"
  }

  aktuella <- c(
    aktuella,
    trips,
    "CREATE INDEX IF NOT EXISTS idx_trips_shape_id ON {schema_q}.trips (shape_id);",
    "CREATE INDEX IF NOT EXISTS idx_trips_route_id ON {schema_q}.trips (route_id);",
    "CREATE INDEX IF NOT EXISTS idx_trips_service_id ON {schema_q}.trips (service_id);",
    "CREATE TABLE IF NOT EXISTS {schema_q}.stops (
       stop_id VARCHAR PRIMARY KEY, hpl_id VARCHAR, stop_name VARCHAR NOT NULL,
       stop_lat FLOAT NOT NULL, stop_lon FLOAT NOT NULL, location_type INTEGER,
       parent_station VARCHAR, platform_code VARCHAR, geometry GEOMETRY(Point, 3006)
     );",
    "CREATE INDEX IF NOT EXISTS idx_stops_geometry ON {schema_q}.stops USING GIST (geometry);",
    "CREATE TABLE IF NOT EXISTS {schema_q}.stop_times (
       trip_id VARCHAR, arrival_time VARCHAR, departure_time VARCHAR, stop_id VARCHAR,
       stop_sequence INTEGER, stop_headsign VARCHAR, pickup_type INTEGER, drop_off_type INTEGER,
       pickup_booking_rule_id VARCHAR, drop_off_booking_rule_id VARCHAR,
       shape_dist_traveled FLOAT, timepoint INTEGER,
       PRIMARY KEY (trip_id, stop_id, stop_sequence),
       FOREIGN KEY (stop_id) REFERENCES {schema_q}.stops(stop_id),
       FOREIGN KEY (trip_id) REFERENCES {schema_q}.trips(trip_id)
     );",
    "CREATE INDEX IF NOT EXISTS idx_stop_times_trip_id ON {schema_q}.stop_times (trip_id);",
    "CREATE INDEX IF NOT EXISTS idx_stop_times_stop_id ON {schema_q}.stop_times (stop_id);",
    "CREATE TABLE IF NOT EXISTS {schema_q}.linjeklassificering (
       route_short_name VARCHAR PRIMARY KEY, klassificering VARCHAR NOT NULL
     );"
  )

  if (sweden_3) {
    aktuella <- c(
      aktuella,
      "CREATE TABLE IF NOT EXISTS {schema_q}.transfers (
         from_stop_id VARCHAR, to_stop_id VARCHAR, transfer_type INTEGER,
         min_transfer_time INTEGER, from_trip_id VARCHAR, to_trip_id VARCHAR
       );",
      "CREATE INDEX IF NOT EXISTS idx_transfers_from_stop ON {schema_q}.transfers (from_stop_id);",
      "CREATE INDEX IF NOT EXISTS idx_transfers_to_stop ON {schema_q}.transfers (to_stop_id);",
      "CREATE TABLE IF NOT EXISTS {schema_q}.attributions (
         trip_id VARCHAR, organization_name VARCHAR NOT NULL, is_operator INTEGER
       );",
      "CREATE INDEX IF NOT EXISTS idx_attributions_trip_id ON {schema_q}.attributions (trip_id);",
      "CREATE TABLE IF NOT EXISTS {schema_q}.feed_info (
         feed_id VARCHAR PRIMARY KEY, feed_publisher_name VARCHAR,
         feed_publisher_url VARCHAR, feed_lang VARCHAR, feed_version VARCHAR
       );"
    )
  }

  historiska <- c(
    "CREATE TABLE IF NOT EXISTS {schema_h_q}.agency (
       agency_id VARCHAR, agency_name VARCHAR NOT NULL, agency_url VARCHAR NOT NULL,
       agency_timezone VARCHAR NOT NULL, agency_lang VARCHAR, agency_fare_url VARCHAR,
       version INTEGER, PRIMARY KEY (agency_id, version)
     );",
    "CREATE TABLE IF NOT EXISTS {schema_h_q}.routes (
       route_id VARCHAR, agency_id VARCHAR, route_short_name VARCHAR NOT NULL,
       route_long_name VARCHAR NOT NULL, route_desc VARCHAR, route_type INTEGER NOT NULL,
       version INTEGER, PRIMARY KEY (route_id, version),
       FOREIGN KEY (agency_id, version) REFERENCES {schema_h_q}.agency(agency_id, version)
     );",
    "CREATE TABLE IF NOT EXISTS {schema_h_q}.calendar (
       service_id VARCHAR, monday INTEGER, tuesday INTEGER, wednesday INTEGER, thursday INTEGER,
       friday INTEGER, saturday INTEGER, sunday INTEGER, start_date DATE, end_date DATE,
       version INTEGER, PRIMARY KEY (service_id, version)
     );",
    "CREATE TABLE IF NOT EXISTS {schema_h_q}.calendar_dates (
       service_id VARCHAR, date DATE, exception_type INTEGER, version INTEGER,
       PRIMARY KEY (service_id, version, date)
     );",
    "CREATE TABLE IF NOT EXISTS {schema_h_q}.shapes_line (
       shape_id VARCHAR, geometry GEOMETRY(Linestring, 3006), antal_punkter INTEGER,
       max_dist FLOAT, version INTEGER, PRIMARY KEY (shape_id, version)
     );",
    "CREATE INDEX IF NOT EXISTS idx_shapes_line_geometry_historisk ON {schema_h_q}.shapes_line USING GIST (geometry);"
  )

  trips_h <- if (sweden_3) {
    "CREATE TABLE IF NOT EXISTS {schema_h_q}.trips (
       trip_id VARCHAR, route_id VARCHAR, service_id VARCHAR NOT NULL, trip_headsign VARCHAR,
       trip_short_name VARCHAR, direction_id INTEGER, shape_id VARCHAR,
       samtrafiken_internal_trip_number VARCHAR, version INTEGER,
       PRIMARY KEY (trip_id, version),
       FOREIGN KEY (route_id, version) REFERENCES {schema_h_q}.routes(route_id, version)
     );"
  } else {
    "CREATE TABLE IF NOT EXISTS {schema_h_q}.trips (
       trip_id VARCHAR, route_id VARCHAR, service_id VARCHAR NOT NULL, trip_headsign VARCHAR,
       direction_id INTEGER, shape_id VARCHAR, version INTEGER,
       PRIMARY KEY (trip_id, version),
       FOREIGN KEY (route_id, version) REFERENCES {schema_h_q}.routes(route_id, version)
     );"
  }

  historiska <- c(
    historiska,
    trips_h,
    "CREATE INDEX IF NOT EXISTS idx_trips_route_id_historisk ON {schema_h_q}.trips (route_id, version);",
    "CREATE INDEX IF NOT EXISTS idx_trips_shape_id_historisk ON {schema_h_q}.trips (shape_id);",
    "CREATE INDEX IF NOT EXISTS idx_trips_service_id_historisk ON {schema_h_q}.trips (service_id);",
    "CREATE TABLE IF NOT EXISTS {schema_h_q}.stops (
       stop_id VARCHAR, hpl_id VARCHAR, stop_name VARCHAR NOT NULL, stop_lat FLOAT NOT NULL,
       stop_lon FLOAT NOT NULL, location_type INTEGER, parent_station VARCHAR,
       platform_code VARCHAR, geometry GEOMETRY(Point, 3006), version INTEGER,
       PRIMARY KEY (stop_id, version)
     );",
    "CREATE INDEX IF NOT EXISTS idx_stops_geometry_historisk ON {schema_h_q}.stops USING GIST (geometry);",
    "CREATE TABLE IF NOT EXISTS {schema_h_q}.stop_times (
       trip_id VARCHAR, arrival_time VARCHAR, departure_time VARCHAR, stop_id VARCHAR,
       stop_sequence INTEGER, stop_headsign VARCHAR, pickup_type INTEGER, drop_off_type INTEGER,
       shape_dist_traveled FLOAT, pickup_booking_rule_id VARCHAR, drop_off_booking_rule_id VARCHAR,
       timepoint INTEGER, version INTEGER, PRIMARY KEY (trip_id, version, stop_sequence),
       FOREIGN KEY (trip_id, version) REFERENCES {schema_h_q}.trips(trip_id, version),
       FOREIGN KEY (stop_id, version) REFERENCES {schema_h_q}.stops(stop_id, version)
     );",
    "CREATE INDEX IF NOT EXISTS idx_stop_times_trip_id_historisk ON {schema_h_q}.stop_times (trip_id, version);",
    "CREATE INDEX IF NOT EXISTS idx_stop_times_stop_id_historisk ON {schema_h_q}.stop_times (stop_id, version);",
    "CREATE TABLE IF NOT EXISTS {schema_h_q}.linjeklassificering (
       route_short_name VARCHAR, klassificering VARCHAR NOT NULL, version INTEGER,
       PRIMARY KEY (route_short_name, version)
     );",
    "CREATE TABLE IF NOT EXISTS {schema_h_q}.versions (
       version INTEGER PRIMARY KEY, start_date DATE, end_date DATE
     );"
  )

  if (sweden_3) {
    historiska <- c(
      historiska,
      "CREATE TABLE IF NOT EXISTS {schema_h_q}.transfers (
         from_stop_id VARCHAR, to_stop_id VARCHAR, transfer_type INTEGER,
         min_transfer_time INTEGER, from_trip_id VARCHAR, to_trip_id VARCHAR, version INTEGER
       );",
      "CREATE INDEX IF NOT EXISTS idx_transfers_from_stop_historisk ON {schema_h_q}.transfers (from_stop_id, version);",
      "CREATE TABLE IF NOT EXISTS {schema_h_q}.attributions (
         trip_id VARCHAR, organization_name VARCHAR NOT NULL, is_operator INTEGER, version INTEGER
       );",
      "CREATE INDEX IF NOT EXISTS idx_attributions_trip_id_historisk ON {schema_h_q}.attributions (trip_id, version);",
      "CREATE TABLE IF NOT EXISTS {schema_h_q}.feed_info (
         feed_id VARCHAR, feed_publisher_name VARCHAR, feed_publisher_url VARCHAR,
         feed_lang VARCHAR, feed_version VARCHAR, version INTEGER,
         PRIMARY KEY (feed_id, version)
       );"
    )
  }

  tryCatch({
    gtfs_transaktion(con, {
      kor("CREATE SCHEMA IF NOT EXISTS {schema_q};")
      kor("CREATE SCHEMA IF NOT EXISTS {schema_h_q};")
      for (sql in c(aktuella, historiska)) kor(sql)
    })
  }, error = function(e) {
    stop(glue::glue("Ett fel inträffade vid skapandet av tabeller: {conditionMessage(e)}"), call. = FALSE)
  })

  invisible(TRUE)
}
