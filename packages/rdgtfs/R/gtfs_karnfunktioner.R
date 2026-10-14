.gtfs_veckodagar <- c("monday", "tuesday", "wednesday", "thursday",
                      "friday", "saturday", "sunday")
.gtfs_veckodagar_w <- c("sunday", "monday", "tuesday", "wednesday",
                        "thursday", "friday", "saturday")

gtfs_kontrollera_dataset <- function(gtfs_dataset) {
  if (length(gtfs_dataset) != 1 || !gtfs_dataset %in% c("regional", "sweden_3")) {
    stop("gtfs_dataset måste vara 'regional' eller 'sweden_3'", call. = FALSE)
  }
  gtfs_dataset == "sweden_3"
}

gtfs_text_till_datum <- function(x) {
  if (inherits(x, "Date")) return(x)
  as.Date(as.character(x), format = "%Y%m%d")
}

gtfs_datum_till_text <- function(x) {
  if (inherits(x, "Date")) format(x, "%Y%m%d") else as.character(x)
}

gtfs_konvertera <- function(df, heltal = character(), decimal = character()) {
  for (k in intersect(heltal, names(df))) df[[k]] <- suppressWarnings(as.integer(df[[k]]))
  for (k in intersect(decimal, names(df))) df[[k]] <- suppressWarnings(as.numeric(df[[k]]))
  df
}

gtfs_tom_till_na <- function(df) {
  for (k in names(df)[vapply(df, is.character, logical(1))]) {
    df[[k]][which(df[[k]] == "")] <- NA_character_
  }
  df
}

gtfs_las_fil <- function(mapp, filnamn, obligatorisk = FALSE) {
  sokvag <- file.path(mapp, filnamn)
  if (!file.exists(sokvag)) {
    if (obligatorisk) stop("Filen ", filnamn, " saknas i GTFS-datasetet.", call. = FALSE)
    return(NULL)
  }
  data.table::fread(sokvag, sep = ",", colClasses = "character",
                    encoding = "UTF-8", data.table = FALSE, showProgress = FALSE)
}

#' Build GTFS shape lines
#'
#' Converts ordered GTFS shape points into one `sf` linestring per shape.
#' @describeIn rdgtfs-api Build shape geometries
#' @param shapes GTFS `shapes` table with shape IDs, coordinates, and sequence.
#' @param crs Coordinate reference system for the returned geometries.
#' @return An `sf` object containing one linestring per shape.
#' @export
gtfs_shapes_till_linjer <- function(shapes, crs = 4326) {
  df <- data.frame(
    shape_id = as.character(shapes$shape_id),
    lon      = as.numeric(shapes$shape_pt_lon),
    lat      = as.numeric(shapes$shape_pt_lat),
    sekv     = as.integer(shapes$shape_pt_sequence),
    dist     = if ("shape_dist_traveled" %in% names(shapes)) as.numeric(shapes$shape_dist_traveled) else NA_real_,
    stringsAsFactors = FALSE
  )
  df <- df[stats::complete.cases(df[, c("shape_id", "lon", "lat")]), ]
  df <- df[order(df$shape_id, df$sekv, method = "radix"), ]

  idx <- split(seq_len(nrow(df)), df$shape_id)
  idx <- idx[lengths(idx) >= 2]
  if (length(idx) == 0) stop("Inga shapes med minst två punkter hittades.", call. = FALSE)

  linjer <- lapply(idx, function(i) sf::st_linestring(cbind(df$lon[i], df$lat[i])))
  max_dist <- vapply(idx, function(i) {
    d <- df$dist[i]
    if (all(is.na(d))) NA_real_ else max(c(0, diff(d)), na.rm = TRUE)
  }, numeric(1))

  sf::st_sf(shape_id      = names(idx),
            antal_punkter = as.integer(lengths(idx)),
            max_dist      = unname(max_dist),
            geometry      = sf::st_sfc(unname(linjer), crs = crs))
}

#' Derive GTFS calendar weekdays
#'
#' Creates a `calendar` table from active dates in `calendar_dates`.
#' @describeIn rdgtfs-api Build calendar weekdays
#' @param calendar_dates_df GTFS `calendar_dates` data frame.
#' @return A data frame with weekday flags and start/end dates per service.
#' @export
gtfs_fyll_calendar_dagar <- function(calendar_dates_df) {
  calendar_dates_df |>
    dplyr::mutate(datum    = gtfs_text_till_datum(date),
                  veckodag = .gtfs_veckodagar_w[as.integer(format(datum, "%w")) + 1L]) |>
    dplyr::group_by(service_id) |>
    dplyr::summarise(
      monday     = as.integer(any(veckodag == "monday"    & exception_type == 1L)),
      tuesday    = as.integer(any(veckodag == "tuesday"   & exception_type == 1L)),
      wednesday  = as.integer(any(veckodag == "wednesday" & exception_type == 1L)),
      thursday   = as.integer(any(veckodag == "thursday"  & exception_type == 1L)),
      friday     = as.integer(any(veckodag == "friday"    & exception_type == 1L)),
      saturday   = as.integer(any(veckodag == "saturday" & exception_type == 1L)),
      sunday     = as.integer(any(veckodag == "sunday"    & exception_type == 1L)),
      start_date = min(datum),
      end_date   = max(datum),
      .groups = "drop"
    )
}

#' Expand GTFS calendar to dates
#'
#' Creates active `calendar_dates` rows from weekday service periods.
#' @describeIn rdgtfs-api Expand calendar dates
#' @param calendar_df GTFS `calendar` data frame.
#' @param skickade_service_id Optional service IDs to expand; `NULL` expands all.
#' @param exception_kol Exception type to assign to generated dates.
#' @return A data frame with service ID, date, and exception type.
#' @export
gtfs_fyll_calendar_dates_fran_calendar <- function(calendar_df,
                                                   skickade_service_id = NULL,
                                                   exception_kol = 1L) {
  if (!is.null(skickade_service_id) && !all(is.na(skickade_service_id))) {
    calendar_df <- dplyr::filter(calendar_df, service_id %in% skickade_service_id)
  }

  aktiva_df <- calendar_df |>
    dplyr::select(service_id, dplyr::all_of(.gtfs_veckodagar)) |>
    tidyr::pivot_longer(-service_id, names_to = "veckodag", values_to = "aktiv") |>
    dplyr::filter(as.integer(aktiv) == 1L) |>
    dplyr::select(-aktiv)

  calendar_df |>
    dplyr::transmute(service_id,
                     start = gtfs_text_till_datum(start_date),
                     slut  = gtfs_text_till_datum(end_date)) |>
    dplyr::mutate(date = Map(function(a, b) seq.Date(a, b, by = "day"), start, slut)) |>
    tidyr::unnest(date) |>
    dplyr::mutate(veckodag = .gtfs_veckodagar[(as.integer(format(date, "%w")) + 6L) %% 7L + 1L]) |>
    dplyr::inner_join(aktiva_df, by = c("service_id", "veckodag")) |>
    dplyr::transmute(service_id, date, exception_type = as.integer(exception_kol))
}
