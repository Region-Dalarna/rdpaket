#' Add Dalatrafik stop identifiers to a Sweden 3 feed
#'
#' Matches stops served by Dalatrafik against the regional Dalatrafik feed by
#' rounded coordinates and adds the regional stop identifiers.
#' @describeIn rdgtfs-api Add alternative Dalatrafik stop IDs
#' @param gtfs_data_dalarna A Sweden 3 GTFS data list filtered to Dalarna.
#' @param agency_namn Agency-name pattern, matched case-insensitively.
#' @param decimaler Number of decimals used when rounding coordinates for matching.
#' @return The GTFS data list with character columns `dt_hpl_id` and
#'   `dt_stop_id` added to `stops`.
#' @export
lagg_till_alternativa_id_dalatrafik <- function(gtfs_data_dalarna,
                                                agency_namn = "Dalatrafik",
                                                decimaler = 5) {
  gtfs_regional <- rdverktyg::skriptrader_upprepa_om_fel(hamta_gtfs_data("dt"))

  # Hållplatser saknar agency_id, så operatören kopplas via
  # stop_times -> trips -> routes -> agency.
  agency_id_dalatrafik <- gtfs_data_dalarna$agency |>
    dplyr::filter(grepl(agency_namn, agency_name, ignore.case = TRUE)) |>
    dplyr::pull(agency_id)

  route_ids_dalatrafik <- gtfs_data_dalarna$routes |>
    dplyr::filter(agency_id %in% agency_id_dalatrafik) |>
    dplyr::pull(route_id)

  trip_ids_dalatrafik <- gtfs_data_dalarna$trips |>
    dplyr::filter(route_id %in% route_ids_dalatrafik) |>
    dplyr::pull(trip_id)

  dalatrafik_stop_ids <- gtfs_data_dalarna$stop_times |>
    dplyr::filter(trip_id %in% trip_ids_dalatrafik) |>
    dplyr::pull(stop_id) |>
    unique()

  koord_sweden3 <- gtfs_data_dalarna$stops |>
    dplyr::mutate(
      koord_sträng = paste(round(stop_lon, decimaler), round(stop_lat, decimaler))
    )

  # Första raden per koordinat behålls så att delade koordinater inte ger fler rader.
  koord_regional <- gtfs_regional$stops |>
    dplyr::mutate(
      koord_sträng = paste(round(stop_lon, decimaler), round(stop_lat, decimaler))
    ) |>
    dplyr::distinct(koord_sträng, .keep_all = TRUE) |>
    dplyr::select(koord_sträng, dt_hpl_id = hpl_id, dt_stop_id = stop_id) |>
    dplyr::mutate(
      dt_hpl_id = as.character(dt_hpl_id),
      dt_stop_id = as.character(dt_stop_id)
    )

  # Hållplatser som inte trafikeras av Dalatrafik får NA, så att delade
  # bytespunkter inte får en annan operatörs ID.
  gtfs_data_dalarna$stops <- koord_sweden3 |>
    dplyr::left_join(koord_regional, by = "koord_sträng") |>
    dplyr::mutate(
      dt_hpl_id = dplyr::if_else(stop_id %in% dalatrafik_stop_ids, dt_hpl_id, NA_character_),
      dt_stop_id = dplyr::if_else(stop_id %in% dalatrafik_stop_ids, dt_stop_id, NA_character_)
    ) |>
    dplyr::select(-koord_sträng)

  gtfs_data_dalarna
}
