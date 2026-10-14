#' Fetch Trafiklab's Swedish operator lookup table
#'
#' Retrieves regional operators and optionally joins Region Dalarna's county names.
#' @describeIn rdgtfs-api Fetch the operator lookup
#' @param tabort_na Remove rows without a mapped region code.
#' @param med_regionnamn Join county names using `rdverktyg`.
#' @return A data frame containing operator, operator code, and region code.
#' @export
gtfs_operatorer_sverige_nyckeltabell_hamta <- function(tabort_na = TRUE,
                                                       med_regionnamn = TRUE) {
  operatorkod_till_regionkod <- c(
    sl = "01", ul = "03", sormland = "04", otraf = "05", jlt = "06",
    krono = "07", klt = "08", gotland = "09", blekinge = "10", skane = "12",
    halland = "13", vt = "14", varm = "17", orebro = "18", vastmanland = "19",
    dt = "20", xt = "21", dintur = "22", jamtland = "23", vasterbotten = "24",
    norrbotten = "25"
  )

  url_trafiklab <- "https://www.trafiklab.se/sv/api/gtfs-datasets/gtfs-regional/"

  retur_df <- rvest::read_html(url_trafiklab) |>
    rvest::html_element("table") |>
    rvest::html_table() |>
    dplyr::select(operator = Operator, operatorkod = Abbreviation) |>
    dplyr::mutate(regionkod = unname(operatorkod_till_regionkod[operatorkod]))

  if (med_regionnamn) {
    regionnyckel <- rdverktyg::hamtaregtab() |>
      dplyr::filter(nchar(regionkod) == 2, regionkod != "00") |>
      dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region))
    retur_df <- dplyr::left_join(retur_df, regionnyckel, by = "regionkod")
  }

  if (tabort_na) retur_df <- dplyr::filter(retur_df, !is.na(regionkod))

  retur_df
}

#' Download and parse a Trafiklab GTFS feed
#'
#' Downloads a selected regional or national feed using a token stored in keyring.
#' @describeIn rdgtfs-api Download and parse a GTFS feed
#' @param gtfs_dataset County code, operator code, `sverige_2`, or `sweden_3`.
#' @param spara_filmap Optional directory for downloaded ZIP and extracted files.
#' @param test_mode Only check that the feed URL responds; do not download data.
#' @return A named list of GTFS tables, or `TRUE` invisibly in test mode.
#' @export
hamta_gtfs_data <- function(gtfs_dataset = "20",
                            spara_filmap = NA,
                            test_mode = FALSE) {
  if (length(gtfs_dataset) != 1) {
    stop("Parametern gtfs_dataset kan för närvarande bara ha ett värde. Korrigera parametern och försök igen.")
  }
  gtfs_dataset <- as.character(gtfs_dataset)
  dataset_lc   <- tolower(gtfs_dataset)

  tryCatch({
    datum <- format(Sys.Date(), "%Y%m%d")

    if (dataset_lc %in% c("sweden_3", "sweden3")) {
      api_key  <- keyring::key_get("API_trafiklab_token", "GTFS_Sweden_3")
      url      <- paste0("https://opendata.samtrafiken.se/gtfs-sweden/sweden.zip?key=", api_key)
      kan_head <- TRUE
    } else if (dataset_lc %in% c("sverige_2", "sverige2")) {
      api_key  <- keyring::key_get("API_trafiklab_token", "GTFS_Sverige_2")
      url      <- paste0("https://api.resrobot.se/gtfs/sweden.zip?key=", api_key)
      kan_head <- FALSE
    } else {
      if (grepl("^[0-9]+$", gtfs_dataset)) gtfs_dataset <- sprintf("%02d", as.integer(gtfs_dataset))

      regionnyckel <- gtfs_operatorer_sverige_nyckeltabell_hamta(med_regionnamn = FALSE)
      rkm <- if (dataset_lc %in% regionnyckel$operatorkod) dataset_lc else
        regionnyckel$operatorkod[regionnyckel$regionkod == gtfs_dataset]
      if (length(rkm) != 1) {
        stop("Hittade inte exakt en regional operatör för '", gtfs_dataset,
             "'. Ange en giltig länskod eller operatörskod.")
      }

      api_key  <- keyring::key_get("API_trafiklab_token", "GTFS_Regional")
      url      <- paste0("https://opendata.samtrafiken.se/gtfs/", rkm, "/", rkm, ".zip?key=", api_key)
      kan_head <- TRUE
    }

    if (test_mode) {
      svar <- if (kan_head) httr::HEAD(url) else
        httr::GET(url, httr::add_headers(Range = "bytes=0-0"))
      if (!httr::status_code(svar) %in% c(200L, 206L)) {
        stop("URL:en fungerar inte, felkod: ", httr::status_code(svar))
      }
      message("Test framgångsrikt: URL:en finns och fungerar.")
      return(invisible(TRUE))
    }

    spara <- !is.null(spara_filmap) && !is.na(spara_filmap)
    if (spara) {
      if (!dir.exists(spara_filmap)) dir.create(spara_filmap, recursive = TRUE)
      gtfs_fil  <- file.path(spara_filmap, paste0("trafiklab_", gtfs_dataset, "_", datum, ".zip"))
      unzip_dir <- file.path(spara_filmap, paste0(gtfs_dataset, "_", datum))
    } else {
      gtfs_fil  <- tempfile(fileext = ".zip")
      unzip_dir <- tempfile("gtfs_")
      on.exit(unlink(c(gtfs_fil, unzip_dir), recursive = TRUE), add = TRUE)
    }

    svar <- httr::GET(url, httr::write_disk(gtfs_fil, overwrite = TRUE))
    httr::stop_for_status(svar, task = "ladda ned GTFS-datasetet")
    utils::unzip(gtfs_fil, exdir = unzip_dir)

    las <- function(fil, obligatorisk = FALSE) gtfs_las_fil(unzip_dir, fil, obligatorisk)

    gtfs_lista <- list()

    gtfs_lista$routes <- las("routes.txt", TRUE) |>
      gtfs_konvertera(heltal = "route_type")

    stops <- las("stops.txt", TRUE) |>
      gtfs_konvertera(heltal = "location_type", decimal = c("stop_lat", "stop_lon"))
    stops$hpl_id <- substr(stops$stop_id, 8, 13)
    gtfs_lista$stops <- stops

    stop_times <- las("stop_times.txt", TRUE) |>
      gtfs_konvertera(heltal  = c("stop_sequence", "pickup_type", "drop_off_type", "timepoint"),
                      decimal = "shape_dist_traveled")
    if (!"shape_dist_traveled" %in% names(stop_times)) stop_times$shape_dist_traveled <- NA_real_
    if (!"timepoint" %in% names(stop_times))           stop_times$timepoint <- NA_integer_
    gtfs_lista$stop_times <- stop_times

    gtfs_lista$trips <- las("trips.txt", TRUE) |>
      gtfs_konvertera(heltal = "direction_id")
    if (!"direction_id" %in% names(gtfs_lista$trips)) gtfs_lista$trips$direction_id <- NA_integer_

    calendar_dates <- las("calendar_dates.txt")
    calendar       <- las("calendar.txt")

    if (!is.null(calendar_dates)) {
      calendar_dates <- calendar_dates |>
        dplyr::mutate(date = gtfs_text_till_datum(date),
                      exception_type = as.integer(exception_type))
    }
    if (!is.null(calendar)) {
      calendar <- calendar |>
        gtfs_konvertera(heltal = .gtfs_veckodagar) |>
        dplyr::mutate(start_date = gtfs_text_till_datum(start_date),
                      end_date   = gtfs_text_till_datum(end_date))
      if (sum(unlist(calendar[.gtfs_veckodagar]), na.rm = TRUE) == 0) calendar <- NULL
    }
    if (is.null(calendar_dates) && !is.null(calendar)) {
      calendar_dates <- gtfs_fyll_calendar_dates_fran_calendar(calendar)
    }
    if (is.null(calendar_dates)) {
      stop("Datasetet saknar både användbar calendar.txt och calendar_dates.txt.")
    }
    if (is.null(calendar)) calendar <- gtfs_fyll_calendar_dagar(calendar_dates)

    gtfs_lista$calendar_dates <- calendar_dates
    gtfs_lista$calendar       <- calendar

    shapes <- las("shapes.txt")
    if (!is.null(shapes)) {
      shapes <- gtfs_konvertera(shapes,
                                heltal  = "shape_pt_sequence",
                                decimal = c("shape_pt_lat", "shape_pt_lon", "shape_dist_traveled"))
      if (!"shape_dist_traveled" %in% names(shapes)) shapes$shape_dist_traveled <- NA_real_
      gtfs_lista$shapes <- shapes
    }

    for (fil in c("agency", "feed_info", "attributions", "transfers")) {
      x <- las(paste0(fil, ".txt"))
      if (!is.null(x)) gtfs_lista[[fil]] <- x
    }

    gtfs_lista
  }, error = function(e) {
    stop(paste("Följande fel har uppstått under nedladdning och bearbetning av gtfs-datasetet: ", conditionMessage(e)),
         call. = FALSE)
  })
}
