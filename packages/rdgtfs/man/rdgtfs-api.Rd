\name{rdgtfs-api}
\alias{rdgtfs-api}
\alias{gtfs_fyll_calendar_dagar}
\alias{gtfs_fyll_calendar_dates_fran_calendar}
\alias{gtfs_filtrera_geografiskt}
\alias{gtfs_hamta_for_r5r}
\alias{gtfs_operatorer_sverige_nyckeltabell_hamta}
\alias{gtfs_rensa_for_db}
\alias{gtfs_rensa_for_r5r}
\alias{gtfs_shapes_till_linjer}
\alias{gtfs_validera}
\alias{hamta_agency_id}
\alias{hamta_gtfs_data}
\alias{ladda_upp_till_databas}
\alias{mapview_gtfs}
\alias{radera_gamla_versioner}
\alias{skapa_tabell_linjeklassificering}
\alias{skapa_tabeller}
\alias{skapa_vyer_hallplats}
\alias{skapa_vyer_historisk_hallplats}
\alias{skapa_vyer_historisk_linjer}
\alias{skapa_vyer_linjer}
\alias{versionshantering}
\title{GTFS functions for Region Dalarna}
\description{
Tools for downloading, validating, transforming, storing, and preparing
General Transit Feed Specification data for analysis.
}
\usage{
gtfs_fyll_calendar_dagar(calendar_dates_df)

gtfs_fyll_calendar_dates_fran_calendar(calendar_df,
  skickade_service_id = NULL, exception_kol = 1L)

gtfs_filtrera_geografiskt(gtfs_data, polygon, buffert_km = 0,
  gtfs_dataset = "regional", fullstandiga_operatorer = NULL)

gtfs_hamta_for_r5r(con, schema = "gtfs", output_zip = NULL, datum = NULL)

gtfs_operatorer_sverige_nyckeltabell_hamta(tabort_na = TRUE,
  med_regionnamn = TRUE)

gtfs_rensa_for_db(gtfs, gtfs_dataset = "regional")

gtfs_rensa_for_r5r(gtfs)

gtfs_shapes_till_linjer(shapes, crs = 4326)

gtfs_validera(gtfs, output_path = "validering",
  validator_path = file.path(getwd(), "gtfstools",
    "gtfs-validator-v6.0.0.jar"))

hamta_agency_id(con, schema = "gtfs", agency_namn)

hamta_gtfs_data(gtfs_dataset = "20", spara_filmap = NA,
  test_mode = FALSE)

ladda_upp_till_databas(con, gtfs_data, schema = "gtfs",
  gtfs_dataset = "regional", transaktion = FALSE)

mapview_gtfs(gtfs_data)

radera_gamla_versioner(con, antal_ar, schema = "gtfs")

skapa_tabell_linjeklassificering(con, schema = "gtfs")

skapa_tabeller(con, schema = "gtfs", gtfs_dataset = "regional")

skapa_vyer_hallplats(con, schema = "gtfs", agency_id = NULL,
  agency_namn = NULL, vy_suffix = NULL)

skapa_vyer_historisk_hallplats(con, schema = "gtfs",
  agency_id = NULL, agency_namn = NULL, vy_suffix = NULL)

skapa_vyer_historisk_linjer(con, schema = "gtfs", agency_id = NULL,
  agency_namn = NULL, vy_suffix = NULL)

skapa_vyer_linjer(con, schema = "gtfs", agency_id = NULL,
  agency_namn = NULL, vy_suffix = NULL)

versionshantering(con, gtfs_data, schema = "gtfs", vy_operatorer = NA)
}
\arguments{
\item{gtfs}{A named list of GTFS tables, a \code{gtfstools} object, or
an input accepted by the named function.}
\item{gtfs_data}{GTFS data object or named list of GTFS tables.}
\item{calendar_dates_df}{GTFS \code{calendar_dates} data frame.}
\item{calendar_df}{GTFS \code{calendar} data frame.}
\item{skickade_service_id}{Optional service IDs to expand; \code{NULL}
expands all services.}
\item{exception_kol}{Exception type assigned to generated dates.}
\item{shapes}{GTFS \code{shapes} table.}
\item{crs}{Coordinate reference system for generated geometries.}
\item{gtfs_dataset}{Dataset kind or identifier. Database cleaners accept
\code{"regional"} or \code{"sweden_3"}.}
\item{output_path}{Directory where validator output is written.}
\item{validator_path}{Path to the GTFS validator JAR.}
\item{con}{A DBI database connection.}
\item{schema}{Database schema containing or receiving GTFS tables.}
\item{output_zip}{Optional output path for a GTFS ZIP.}
\item{datum}{Optional date; r5r preparation retains services active on
or after this date.}
\item{antal_ar}{Age threshold in years for deleting historical versions.}
\item{vy_operatorer}{Controls which operator-specific historical views
are built after a version transition.}
\item{transaktion}{Whether upload truncation and writing run in a transaction.}
\item{agency_id}{Optional agency identifier filter.}
\item{agency_namn}{Agency-name substring, matched case-insensitively.}
\item{vy_suffix}{Optional suffix for generated materialized view names.}
\item{tabort_na}{Whether to remove operators with no mapped region code.}
\item{med_regionnamn}{Whether to join county names from \code{rdverktyg}.}
\item{spara_filmap}{Optional directory for downloaded and extracted feed files.}
\item{test_mode}{Check feed URL availability without downloading the feed.}
\item{gtfs_data}{GTFS object to display, filter, upload, or version.}
\item{polygon}{An \code{sf} or \code{sfc} polygon with a defined CRS.}
\item{buffert_km}{Optional buffer around the polygon, in kilometres.}
\item{fullstandiga_operatorer}{Optional agency-name substrings whose complete
networks should be retained.}
}
\value{
GTFS tables, filtered GTFS objects, map widgets, agency identifiers, or
status values as appropriate; database functions generally return
\code{TRUE} invisibly.
}
\details{
Functions requiring optional packages or external services will report
those requirements when called. Database functions require a compatible
PostgreSQL/PostGIS connection and schemas.
}
