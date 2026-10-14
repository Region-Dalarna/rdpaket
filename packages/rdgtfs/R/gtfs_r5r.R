#' Validate a GTFS feed
#'
#' Runs the Google GTFS validator and returns its JSON report.
#' @describeIn rdgtfs-api Validate a feed
#' @param gtfs GTFS object or path accepted by `gtfstools`.
#' @param output_path Directory where the validator writes its report.
#' @param validator_path Path to the GTFS validator JAR.
#' @return The validation report, invisibly.
#' @export
gtfs_validera <- function(gtfs,
                          output_path    = "validering",
                          validator_path = file.path(getwd(), "gtfstools", "gtfs-validator-v6.0.0.jar")) {
  if (is.list(gtfs) && !inherits(gtfs, "dt_gtfs")) gtfs <- gtfstools::as_dt_gtfs(gtfs)

  gtfstools::validate_gtfs(gtfs, output_path = output_path, validator_path = validator_path)

  rapport <- jsonlite::read_json(file.path(output_path, "report.json"))
  for (notice in rapport$notices) {
    if (notice$severity %in% c("ERROR", "WARNING")) {
      message(notice$severity, " - ", notice$code, " - ", notice$totalNotices, " fel")
    }
  }
  invisible(rapport)
}

#' Normalize GTFS data before database upload
#'
#' Fixes upload-relevant types and normalizes stop sequence numbers.
#' @describeIn rdgtfs-api Normalize GTFS for database upload
#' @param gtfs A named list of GTFS tables.
#' @param gtfs_dataset Dataset kind, `regional` or `sweden_3`.
#' @return The normalized GTFS object.
#' @export
gtfs_rensa_for_db <- function(gtfs, gtfs_dataset = "regional") {
  sweden_3 <- gtfs_kontrollera_dataset(gtfs_dataset)

  gtfs$stops <- gtfs_konvertera(gtfs$stops, decimal = c("stop_lat", "stop_lon"))

  st <- gtfs$stop_times
  st <- st[order(st$trip_id, st$stop_sequence, method = "radix"), ]
  st$stop_sequence <- sequence(rle(st$trip_id)$lengths)
  gtfs$stop_times <- st

  if (sweden_3) {
    if (!is.null(gtfs$transfers)) {
      gtfs$transfers <- gtfs$transfers |>
        gtfs_tom_till_na() |>
        gtfs_konvertera(heltal = c("transfer_type", "min_transfer_time"))
    }
    if (!is.null(gtfs$attributions)) {
      gtfs$attributions <- gtfs$attributions |>
        gtfs_tom_till_na() |>
        gtfs_konvertera(heltal = "is_operator")
    }
    if (!is.null(gtfs$calendar)) {
      gtfs$calendar <- gtfs$calendar |>
        gtfs_tom_till_na() |>
        gtfs_konvertera(heltal = .gtfs_veckodagar)
    }
    if (!is.null(gtfs$feed_info)) {
      gtfs$feed_info <- gtfs_tom_till_na(gtfs$feed_info)
    }
  }

  gtfs
}

#' Clean GTFS data for r5r
#'
#' Removes unsupported records and cascade-filters dependent tables.
#' @describeIn rdgtfs-api Clean GTFS for r5r
#' @param gtfs A named list of GTFS tables.
#' @return A GTFS object cleaned for r5r.
#' @export
gtfs_rensa_for_r5r <- function(gtfs) {
  gtfs$routes <- gtfs$routes |>
    dplyr::filter(route_type != 1501L) |>
    dplyr::mutate(route_type = dplyr::if_else(route_type %in% c(100L, 103L, 106L),
                                              2L, as.integer(route_type)))
  gtfs$trips <- dplyr::filter(gtfs$trips, route_id %in% gtfs$routes$route_id)

  gtfs$stops <- gtfs$stops |>
    dplyr::filter(is.na(location_type) | location_type == 0,
                  dplyr::between(stop_lat, 55, 70),
                  dplyr::between(stop_lon, 10, 25)) |>
    dplyr::select(-dplyr::any_of(c("location_type", "parent_station", "platform_code", "hpl_id")))

  gtfs$stop_times <- gtfs$stop_times |>
    dplyr::filter(stop_id %in% gtfs$stops$stop_id, trip_id %in% gtfs$trips$trip_id) |>
    dplyr::select(-dplyr::any_of("shape_dist_traveled"))
  if (!is.null(gtfs$shapes)) {
    gtfs$shapes <- dplyr::select(gtfs$shapes, -dplyr::any_of("shape_dist_traveled"))
  }

  gtfs$attributions <- NULL
  gtfs$transfers    <- NULL

  gtfs$trips <- dplyr::filter(gtfs$trips, trip_id %in% gtfs$stop_times$trip_id)
  gtfs$routes <- dplyr::filter(gtfs$routes, route_id %in% gtfs$trips$route_id)

  kvarv_service <- unique(gtfs$trips$service_id)
  if (!is.null(gtfs$calendar)) {
    gtfs$calendar <- dplyr::filter(gtfs$calendar, service_id %in% kvarv_service)
  }
  gtfs$calendar_dates <- dplyr::filter(gtfs$calendar_dates, service_id %in% kvarv_service)

  if (!is.null(gtfs$shapes)) {
    gtfs$shapes <- dplyr::filter(gtfs$shapes, shape_id %in% gtfs$trips$shape_id)
  }
  if (!is.null(gtfs$agency) && "agency_id" %in% names(gtfs$routes) &&
      any(!is.na(gtfs$routes$agency_id))) {
    gtfs$agency <- dplyr::filter(gtfs$agency, agency_id %in% gtfs$routes$agency_id)
  }

  if (!is.null(gtfs$feed_info) && nrow(gtfs$feed_info) > 0 && nrow(gtfs$calendar_dates) > 0) {
    gtfs$feed_info$feed_start_date <- gtfs_datum_till_text(min(gtfs$calendar_dates$date, na.rm = TRUE))
    gtfs$feed_info$feed_end_date   <- gtfs_datum_till_text(max(gtfs$calendar_dates$date, na.rm = TRUE))
  }

  gtfs
}

#' Read and prepare database GTFS for r5r
#'
#' Reads GTFS tables from a database, cleans them, and optionally writes a GTFS ZIP.
#' @describeIn rdgtfs-api Prepare database GTFS for r5r
#' @param con A DBI database connection.
#' @param schema Database schema containing GTFS tables.
#' @param output_zip Optional path for the generated GTFS ZIP.
#' @param datum Optional start date; only services active on or after this date remain.
#' @return The cleaned GTFS object, invisibly if a ZIP is written.
#' @export
gtfs_hamta_for_r5r <- function(con,
                               schema     = "gtfs",
                               output_zip = NULL,
                               datum      = NULL) {
  las <- function(tabell) {
    id <- DBI::Id(schema = schema, table = tabell)
    if (!DBI::dbExistsTable(con, id)) return(NULL)
    DBI::dbReadTable(con, id)
  }

  las_stops <- function() {
    id <- DBI::Id(schema = schema, table = "stops")
    if (!DBI::dbExistsTable(con, id)) return(NULL)
    kol <- setdiff(DBI::dbListFields(con, id), "geometry")
    DBI::dbGetQuery(con, paste0(
      "SELECT ", paste(DBI::dbQuoteIdentifier(con, kol), collapse = ", "),
      " FROM ", DBI::dbQuoteIdentifier(con, id)))
  }

  gtfs <- list(
    routes         = las("routes"),
    trips          = las("trips"),
    stop_times     = las("stop_times"),
    stops          = las_stops(),
    calendar       = las("calendar"),
    calendar_dates = las("calendar_dates"),
    shapes         = las("shapes"),
    agency         = las("agency"),
    feed_info      = las("feed_info"),
    transfers      = las("transfers"),
    attributions   = las("attributions")
  )
  gtfs <- gtfs[!vapply(gtfs, is.null, logical(1))]

  if (!is.null(gtfs$shapes) && nrow(gtfs$shapes) == 0) {
    gtfs$shapes <- NULL
    gtfs$trips$shape_id <- NULL
    message("Tabellen shapes är tom - shapes utelämnas och shape_id tas bort från trips.")
  }

  if (!is.null(datum)) {
    datum  <- as.Date(datum)
    aktiva <- gtfs$calendar_dates |>
      dplyr::filter(as.Date(date) >= datum, exception_type == 1L) |>
      dplyr::pull(service_id) |>
      unique()
    gtfs$trips <- dplyr::filter(gtfs$trips, service_id %in% aktiva)
  }

  gtfs <- gtfs_rensa_for_r5r(gtfs)

  if (!is.null(output_zip)) {
    skriv <- gtfs
    if (!is.null(skriv$calendar)) {
      skriv$calendar <- dplyr::mutate(skriv$calendar,
                                      dplyr::across(dplyr::any_of(c("start_date", "end_date")), gtfs_datum_till_text))
    }
    skriv$calendar_dates <- dplyr::mutate(skriv$calendar_dates, date = gtfs_datum_till_text(date))

    gtfs_dt <- gtfstools::as_dt_gtfs(skriv)
    gtfstools::write_gtfs(gtfs_dt, output_zip)
    message("Körklar GTFS-zip skriven: ", output_zip)
    return(invisible(gtfs))
  }

  gtfs
}
