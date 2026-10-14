#' Display GTFS shapes on an interactive map
#'
#' Uses the optional `mapview` package to render GTFS shape lines.
#' @describeIn rdgtfs-api Display GTFS shapes
#' @param gtfs_data GTFS data list containing a `shapes` table.
#' @return A `mapview` map widget.
#' @export
mapview_gtfs <- function(gtfs_data) {
  if (!requireNamespace("mapview", quietly = TRUE)) {
    stop("Paketet 'mapview' krävs för mapview_gtfs() men är inte installerat.", call. = FALSE)
  }
  if (is.null(gtfs_data$shapes)) {
    stop("GTFS-datat innehåller ingen shapes-information. Kan inte skapa karta.", call. = FALSE)
  }

  shapes_lines <- gtfs_shapes_till_linjer(gtfs_data$shapes, crs = 4326)

  mapview::mapview(shapes_lines, zcol = "shape_id", legend = TRUE)
}

#' Filter GTFS data geographically
#'
#' Retains complete trips whose stops intersect a polygon and all dependent rows.
#' @describeIn rdgtfs-api Filter a feed geographically
#' @param gtfs_data GTFS object or a `gtfstools` `dt_gtfs` object.
#' @param polygon An `sf` or `sfc` polygon with a defined coordinate reference system.
#' @param buffert_km Optional buffer distance around the polygon, in kilometres.
#' @param gtfs_dataset Dataset kind, `regional` or `sweden_3`.
#' @param fullstandiga_operatorer Optional agency-name substrings whose complete networks are retained.
#' @return A filtered GTFS object with related tables kept consistent.
#' @export
gtfs_filtrera_geografiskt <- function(gtfs_data, polygon, buffert_km = 0,
                                      gtfs_dataset = "regional",
                                      fullstandiga_operatorer = NULL) {
  gtfs_kontrollera_dataset(gtfs_dataset)
  if (!inherits(polygon, "sf") && !inherits(polygon, "sfc")) {
    stop("polygon måste vara ett sf- eller sfc-objekt", call. = FALSE)
  }
  if (is.na(sf::st_crs(polygon))) {
    stop("polygon saknar CRS - sätt det med sf::st_set_crs() först", call. = FALSE)
  }
  if (!inherits(gtfs_data, "dt_gtfs")) {
    message("gtfs_data är inte av typen dt_gtfs — konverterar med as_dt_gtfs()")
    gtfs_data <- gtfstools::as_dt_gtfs(gtfs_data)
  }

  rader <- function(df, lgl) df[lgl, ]

  if (buffert_km > 0) {
    polygon <- polygon |>
      sf::st_transform(3006) |>
      sf::st_buffer(dist = buffert_km * 1000)
    message(glue::glue("Polygon buffrad med {buffert_km} km"))
  }
  polygon_union <- sf::st_union(sf::st_transform(polygon, 4326))

  stops <- as.data.frame(gtfs_data$stops)
  stops_sf <- sf::st_as_sf(stops[, c("stop_id", "stop_lon", "stop_lat")],
                           coords = c("stop_lon", "stop_lat"), crs = 4326,
                           na.fail = FALSE)
  inom_polygon <- lengths(sf::st_intersects(stops_sf, polygon_union)) > 0
  stops_ids_geografi <- stops$stop_id[inom_polygon]
  if (length(stops_ids_geografi) == 0) {
    stop("Inga stops hittades inom angiven polygon — kontrollera CRS och polygonens utbredning", call. = FALSE)
  }
  message(glue::glue("{length(stops_ids_geografi)} stops hittades inom polygon"))

  stop_times <- gtfs_data$stop_times
  trips      <- gtfs_data$trips
  routes     <- gtfs_data$routes

  trips_ids_geografi <- unique(stop_times$trip_id[stop_times$stop_id %in% stops_ids_geografi])
  route_ids_geografi <- unique(trips$route_id[trips$trip_id %in% trips_ids_geografi])

  route_ids_fullstandiga <- character(0)
  if (length(fullstandiga_operatorer) > 0) {
    op_txt <- paste(fullstandiga_operatorer, collapse = ", ")

    if (is.null(gtfs_data$agency)) {
      warning(glue::glue(
        "gtfs_data$agency saknas (NULL) — kan inte matcha operatörerna '{op_txt}'. ",
        "Inget undantag tillämpas, endast geografisk filtrering körs."
      ))
    } else {
      agency <- as.data.frame(gtfs_data$agency)
      namn_lc <- tolower(agency$agency_name)
      traff <- Reduce(`|`, lapply(tolower(fullstandiga_operatorer),
                                  function(n) grepl(n, namn_lc, fixed = TRUE)))
      agency_ids_fullstandiga <- as.character(agency$agency_id[traff])

      if (length(agency_ids_fullstandiga) == 0) {
        warning(glue::glue(
          "Ingen operatör matchande '{op_txt}' hittades i gtfs_data$agency (tillgängliga namn: ",
          "{paste(unique(agency$agency_name), collapse = ', ')}) — inget undantag tillämpas."
        ))
      } else {
        route_ids_fullstandiga <- routes$route_id[as.character(routes$agency_id) %in% agency_ids_fullstandiga]

        if (length(route_ids_fullstandiga) == 0) {
          warning(glue::glue(
            "Operatör(er) '{op_txt}' hittades i agency-tabellen ",
            "(agency_id: {paste(agency_ids_fullstandiga, collapse = ', ')}) men INGA routes matchade ",
            "i gtfs_data$routes$agency_id. Kontrollera att agency_id-typerna stämmer överens ",
            "mellan agency- och routes-tabellerna."
          ))
        } else {
          message(glue::glue(
            "{length(route_ids_fullstandiga)} routes tas med i sin helhet för: ",
            "{paste(unique(agency$agency_name[traff]), collapse = ', ')}"
          ))
        }
      }
    }
  }

  route_ids <- union(route_ids_geografi, route_ids_fullstandiga)
  trips_filtrerade <- rader(trips, trips$route_id %in% route_ids)
  trips_ids        <- unique(trips_filtrerade$trip_id)

  stop_times_filtrerade <- rader(stop_times, stop_times$trip_id %in% trips_ids)
  stops_ids <- union(stops_ids_geografi, unique(stop_times_filtrerade$stop_id))

  service_ids <- unique(trips_filtrerade$service_id)
  shape_ids   <- unique(trips_filtrerade$shape_id)
  agency_ids  <- unique(routes$agency_id[routes$route_id %in% route_ids])

  message(glue::glue(
    "{length(trips_ids)} trips | {length(route_ids)} routes | ",
    "{length(service_ids)} service_ids | {length(shape_ids)} shapes | ",
    "{length(stops_ids)} stops totalt (inkl. fullständiga operatörer)"
  ))

  gtfs_filtrerat <- gtfs_data
  gtfs_filtrerat$stops      <- rader(gtfs_data$stops, gtfs_data$stops$stop_id %in% stops_ids)
  gtfs_filtrerat$stop_times <- stop_times_filtrerade
  gtfs_filtrerat$trips      <- trips_filtrerade
  gtfs_filtrerat$routes     <- rader(routes, routes$route_id %in% route_ids)

  if (!is.null(gtfs_data$agency)) {
    gtfs_filtrerat$agency <- rader(gtfs_data$agency, gtfs_data$agency$agency_id %in% agency_ids)
  }
  if (!is.null(gtfs_data$calendar_dates)) {
    gtfs_filtrerat$calendar_dates <- rader(gtfs_data$calendar_dates,
                                           gtfs_data$calendar_dates$service_id %in% service_ids)
  }
  if (!is.null(gtfs_data$calendar)) {
    gtfs_filtrerat$calendar <- rader(gtfs_data$calendar,
                                     gtfs_data$calendar$service_id %in% service_ids)
  }
  if (!is.null(gtfs_data$shapes)) {
    gtfs_filtrerat$shapes <- rader(gtfs_data$shapes, gtfs_data$shapes$shape_id %in% shape_ids)
  }
  if (!is.null(gtfs_data$transfers)) {
    gtfs_filtrerat$transfers <- rader(gtfs_data$transfers,
                                      gtfs_data$transfers$from_stop_id %in% stops_ids &
                                        gtfs_data$transfers$to_stop_id %in% stops_ids)
  }
  if (!is.null(gtfs_data$attributions)) {
    gtfs_filtrerat$attributions <- rader(gtfs_data$attributions,
                                         gtfs_data$attributions$trip_id %in% c(trips_ids, "", NA))
  }

  gtfs_filtrerat
}
