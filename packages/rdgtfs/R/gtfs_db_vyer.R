#' Create current stop-level materialized views
#'
#' Builds stop and stop-position departure views for the current GTFS feed.
#' @describeIn rdgtfs-api Create current stop views
#' @param con A DBI database connection.
#' @param schema Database schema containing current GTFS tables.
#' @param agency_id Optional agency ID filter.
#' @param agency_namn Optional agency-name substring filter.
#' @param vy_suffix Optional suffix for generated view names.
#' @return `TRUE` invisibly.
#' @export
skapa_vyer_hallplats <- function(con, schema = "gtfs", agency_id = NULL,
                                 agency_namn = NULL, vy_suffix = NULL) {
  # Om agency_id (eller agency_namn) anges filtreras allt till den operatören,
  # annars skapas vyer för samtliga operatörer. vy_suffix styr vynamnen
  # (t.ex. "_dt"); utan vy_suffix används agency_id.

  p <- gtfs_vy_parametrar(con, schema, agency_id, agency_namn, vy_suffix)
  agency_filter  <- p$agency_filter
  fordonstyp_sql <- gtfs_fordonstyp_sql()
  vy_hallplatslage <- paste0("vy_hallplatslage_avgangar", p$vy_suffix)
  vy_hallplats     <- paste0("vy_hallplats_avgangar", p$vy_suffix)

  tryCatch({
    gtfs_transaktion(con, {
      DBI::dbExecute(con, glue::glue("DROP MATERIALIZED VIEW IF EXISTS {schema}.{vy_hallplats} CASCADE;"))
      DBI::dbExecute(con, glue::glue("DROP MATERIALIZED VIEW IF EXISTS {schema}.{vy_hallplatslage} CASCADE;"))

      # Hållplatslägen med avgångar, linjer och klassificeringar.
      # stopp_tur = unika (hållplats, tur) - DISTINCT behövs för turer som passerar
      # samma hållplats flera gånger. Veckan med flest avgångar räknas som "normalvecka".
      DBI::dbExecute(con, glue::glue("
        CREATE MATERIALIZED VIEW {schema}.{vy_hallplatslage} AS
        WITH stopp_tur AS (
            SELECT DISTINCT st.stop_id, st.trip_id, t.route_id, t.service_id
            FROM {schema}.stop_times st
            JOIN {schema}.trips t ON st.trip_id = t.trip_id
            JOIN {schema}.routes r ON t.route_id = r.route_id
            WHERE 1 = 1
            {agency_filter}
        ),
        avgangar_per_service AS (
            SELECT stop_id, service_id, COUNT(*) AS antal_turer
            FROM stopp_tur
            GROUP BY stop_id, service_id
        ),
        dagliga_avgangar AS (
            SELECT a.stop_id, cd.date, SUM(a.antal_turer)::BIGINT AS antal_avgangar
            FROM avgangar_per_service a
            JOIN {schema}.calendar_dates cd ON a.service_id = cd.service_id
            WHERE cd.exception_type = 1
            GROUP BY a.stop_id, cd.date
        ),
        avgangar_vecka AS (
            SELECT
                stop_id,
                DATE_TRUNC('week', date) AS vecka_start,
                SUM(CASE WHEN EXTRACT(DOW FROM date) BETWEEN 1 AND 5 THEN antal_avgangar ELSE 0 END) AS total_veckodag_avgangar,
                SUM(CASE WHEN EXTRACT(DOW FROM date) = 6 THEN antal_avgangar ELSE 0 END) AS total_lordag_avgangar,
                SUM(CASE WHEN EXTRACT(DOW FROM date) = 0 THEN antal_avgangar ELSE 0 END) AS total_sondag_avgangar,
                SUM(antal_avgangar) AS total_veckans_avgangar
            FROM dagliga_avgangar
            GROUP BY stop_id, DATE_TRUNC('week', date)
        ),
        normal_vecka AS (
            SELECT
                stop_id, vecka_start, total_veckans_avgangar, total_veckodag_avgangar,
                total_lordag_avgangar, total_sondag_avgangar,
                ROW_NUMBER() OVER (PARTITION BY stop_id ORDER BY total_veckans_avgangar DESC) AS rank
            FROM avgangar_vecka
        ),
        routes_per_stop AS (
            SELECT
                sr.stop_id,
                STRING_AGG(DISTINCT r.route_short_name::TEXT, ',') AS linjer,
                STRING_AGG(DISTINCT lc.klassificering, ',') AS linjetyper,
                STRING_AGG(DISTINCT ag.agency_name, ',') AS operator,
                STRING_AGG(DISTINCT {fordonstyp_sql}, ',') AS fordonstyp,
                STRING_AGG(DISTINCT r.route_type::TEXT, ',') AS route_type
            FROM (SELECT DISTINCT stop_id, route_id FROM stopp_tur) sr
            JOIN {schema}.routes r ON sr.route_id = r.route_id
            LEFT JOIN {schema}.linjeklassificering lc ON r.route_short_name = lc.route_short_name
            LEFT JOIN {schema}.agency ag ON r.agency_id = ag.agency_id
            GROUP BY sr.stop_id
        )
        SELECT
            st.stop_id,
            st.hpl_id,
            st.stop_name,
            st.platform_code,
            st.parent_station,
            ROUND(nv.total_veckodag_avgangar / 5.0, 2) AS genomsnitt_veckodag_avgangar,
            nv.total_lordag_avgangar AS antal_lordag_avgangar,
            nv.total_sondag_avgangar AS antal_sondag_avgangar,
            ROUND(nv.total_veckans_avgangar / 7.0, 2) AS genomsnitt_dagliga_avgangar,
            rs.linjer,
            rs.linjetyper,
            rs.operator,
            rs.fordonstyp,
            rs.route_type,
            st.geometry
        FROM normal_vecka nv
        JOIN {schema}.stops st ON nv.stop_id = st.stop_id
        JOIN routes_per_stop rs ON st.stop_id = rs.stop_id
        WHERE nv.rank = 1;
      "))
      gtfs_skapa_geomindex(con, schema, vy_hallplatslage)

      # Hållplats = alla hållplatslägen som hör till samma parent_station.
      # LEFT JOIN: annars tappas hållplatsläget om parent_station saknas eller
      # inte matchar något stop_id i stops.
      DBI::dbExecute(con, glue::glue("
        CREATE MATERIALIZED VIEW {schema}.{vy_hallplats} AS
        SELECT
            COALESCE(h.stop_id, nva.stop_id) AS stop_id,
            COALESCE(h.hpl_id, nva.hpl_id) AS hpl_id,
            COALESCE(h.stop_name, nva.stop_name) AS stop_name,
            SUM(nva.genomsnitt_veckodag_avgangar) AS genomsnitt_veckodag_avgangar,
            SUM(nva.antal_lordag_avgangar) AS antal_lordag_avgangar,
            SUM(nva.antal_sondag_avgangar) AS antal_sondag_avgangar,
            SUM(nva.genomsnitt_dagliga_avgangar) AS genomsnitt_dagliga_avgangar,
            ARRAY_TO_STRING(ARRAY(SELECT DISTINCT unnest(string_to_array(STRING_AGG(DISTINCT nva.linjer, ',' ORDER BY nva.linjer), ',')) ORDER BY unnest), ', ') AS linjer,
            ARRAY_TO_STRING(ARRAY(SELECT DISTINCT unnest(string_to_array(STRING_AGG(DISTINCT nva.linjetyper, ',' ORDER BY nva.linjetyper), ',')) ORDER BY unnest), ', ') AS linjetyper,
            ARRAY_TO_STRING(ARRAY(SELECT DISTINCT unnest(string_to_array(STRING_AGG(DISTINCT nva.operator, ',' ORDER BY nva.operator), ',')) ORDER BY unnest), ', ') AS operator,
            ARRAY_TO_STRING(ARRAY(SELECT DISTINCT unnest(string_to_array(STRING_AGG(DISTINCT nva.fordonstyp, ',' ORDER BY nva.fordonstyp), ',')) ORDER BY unnest), ', ') AS fordonstyp,
            ARRAY_TO_STRING(ARRAY(SELECT DISTINCT unnest(string_to_array(STRING_AGG(DISTINCT nva.route_type, ',' ORDER BY nva.route_type), ',')) ORDER BY unnest), ', ') AS route_type,
            COALESCE(h.geometry, nva.geometry) AS geometry
        FROM {schema}.{vy_hallplatslage} nva
        LEFT JOIN {schema}.stops h ON nva.parent_station = h.stop_id
        GROUP BY
            COALESCE(h.stop_id, nva.stop_id), COALESCE(h.hpl_id, nva.hpl_id),
            COALESCE(h.stop_name, nva.stop_name), COALESCE(h.geometry, nva.geometry);
      "))
      gtfs_skapa_geomindex(con, schema, vy_hallplats)
    })
  }, error = function(e) {
    stop(paste("Ett fel inträffade vid skapandet av vyer för hållplats:", conditionMessage(e)), call. = FALSE)
  })

  invisible(TRUE)
}


#' Create historical stop-level materialized views
#'
#' Builds version-aware stop and stop-position departure views.
#' @describeIn rdgtfs-api Create historical stop views
#' @param con A DBI database connection.
#' @param schema Base schema; historical data is read from its `_historisk` schema.
#' @param agency_id Optional agency ID filter.
#' @param agency_namn Optional agency-name substring filter.
#' @param vy_suffix Optional suffix for generated view names.
#' @return `TRUE` invisibly.
#' @export
skapa_vyer_historisk_hallplats <- function(con, schema = "gtfs", agency_id = NULL,
                                           agency_namn = NULL, vy_suffix = NULL) {
  # Historisk motsvarighet till skapa_vyer_hallplats(). schema = grundschemat
  # ("gtfs"); vyerna skapas i {schema}_historisk. agency_id antas vara stabilt
  # över versioner.

  p <- gtfs_vy_parametrar(con, schema, agency_id, agency_namn, vy_suffix)
  agency_filter  <- p$agency_filter
  fordonstyp_sql <- gtfs_fordonstyp_sql()
  s <- gtfs_historiskt_schema(schema)   # schema för SQL nedan
  vy_hallplatslage <- paste0("vy_historisk_hallplatslage_avgangar", p$vy_suffix)
  vy_hallplats     <- paste0("vy_historisk_hallplats_avgangar", p$vy_suffix)

  tryCatch({
    gtfs_transaktion(con, {
      DBI::dbExecute(con, glue::glue("DROP MATERIALIZED VIEW IF EXISTS {s}.{vy_hallplats} CASCADE;"))
      DBI::dbExecute(con, glue::glue("DROP MATERIALIZED VIEW IF EXISTS {s}.{vy_hallplatslage} CASCADE;"))

      DBI::dbExecute(con, glue::glue("
        CREATE MATERIALIZED VIEW {s}.{vy_hallplatslage} AS
        WITH stopp_tur AS (
            SELECT DISTINCT st.stop_id, st.trip_id, st.version, t.route_id, t.service_id
            FROM {s}.stop_times st
            JOIN {s}.trips t ON st.trip_id = t.trip_id AND st.version = t.version
            JOIN {s}.routes r ON t.route_id = r.route_id AND t.version = r.version
            WHERE 1 = 1
            {agency_filter}
        ),
        avgangar_per_service AS (
            SELECT stop_id, version, service_id, COUNT(*) AS antal_turer
            FROM stopp_tur
            GROUP BY stop_id, version, service_id
        ),
        dagliga_avgangar AS (
            SELECT a.stop_id, a.version, cd.date, SUM(a.antal_turer)::BIGINT AS antal_avgangar
            FROM avgangar_per_service a
            JOIN {s}.calendar_dates cd ON a.service_id = cd.service_id AND a.version = cd.version
            WHERE cd.exception_type = 1
            GROUP BY a.stop_id, a.version, cd.date
        ),
        avgangar_vecka AS (
            SELECT
                stop_id,
                version,
                DATE_TRUNC('week', date) AS vecka_start,
                SUM(CASE WHEN EXTRACT(DOW FROM date) BETWEEN 1 AND 5 THEN antal_avgangar ELSE 0 END) AS total_veckodag_avgangar,
                SUM(CASE WHEN EXTRACT(DOW FROM date) = 6 THEN antal_avgangar ELSE 0 END) AS total_lordag_avgangar,
                SUM(CASE WHEN EXTRACT(DOW FROM date) = 0 THEN antal_avgangar ELSE 0 END) AS total_sondag_avgangar,
                SUM(antal_avgangar) AS total_veckans_avgangar
            FROM dagliga_avgangar
            GROUP BY stop_id, version, DATE_TRUNC('week', date)
        ),
        normal_vecka AS (
            SELECT
                stop_id, version, vecka_start, total_veckans_avgangar, total_veckodag_avgangar,
                total_lordag_avgangar, total_sondag_avgangar,
                ROW_NUMBER() OVER (PARTITION BY stop_id, version ORDER BY total_veckans_avgangar DESC) AS rank
            FROM avgangar_vecka
        ),
        routes_per_stop AS (
            SELECT
                sr.stop_id,
                sr.version,
                STRING_AGG(DISTINCT r.route_short_name::TEXT, ', ') AS linjer,
                STRING_AGG(DISTINCT lc.klassificering, ', ') AS linjetyper,
                STRING_AGG(DISTINCT ag.agency_name, ', ') AS operator,
                STRING_AGG(DISTINCT {fordonstyp_sql}, ', ') AS fordonstyp,
                STRING_AGG(DISTINCT r.route_type::TEXT, ', ') AS route_type
            FROM (SELECT DISTINCT stop_id, version, route_id FROM stopp_tur) sr
            JOIN {s}.routes r ON sr.route_id = r.route_id AND sr.version = r.version
            LEFT JOIN {s}.linjeklassificering lc ON r.route_short_name = lc.route_short_name AND r.version = lc.version
            LEFT JOIN {s}.agency ag ON r.agency_id = ag.agency_id AND r.version = ag.version
            GROUP BY sr.stop_id, sr.version
        )
        SELECT
            CONCAT(st.stop_id, '_', nv.version) AS unique_id,
            st.stop_id,
            st.hpl_id,
            st.stop_name,
            st.platform_code,
            st.parent_station,
            ROUND(nv.total_veckodag_avgangar / 5.0, 2) AS genomsnitt_veckodag_avgangar,
            nv.total_lordag_avgangar AS antal_lordag_avgangar,
            nv.total_sondag_avgangar AS antal_sondag_avgangar,
            ROUND(nv.total_veckans_avgangar / 7.0, 2) AS genomsnitt_dagliga_avgangar,
            rs.linjer,
            rs.linjetyper,
            rs.operator,
            rs.fordonstyp,
            rs.route_type,
            ST_SetSRID(st.geometry, 3006) AS geometry,
            v.start_date AS startdatum,
            v.end_date AS slutdatum,
            nv.version
        FROM normal_vecka nv
        JOIN {s}.stops st ON nv.stop_id = st.stop_id AND nv.version = st.version
        JOIN routes_per_stop rs ON st.stop_id = rs.stop_id AND nv.version = rs.version
        JOIN {s}.versions v ON nv.version = v.version
        WHERE nv.rank = 1;
      "))
      gtfs_skapa_geomindex(con, s, vy_hallplatslage)

      DBI::dbExecute(con, glue::glue("
        CREATE MATERIALIZED VIEW {s}.{vy_hallplats} AS
        SELECT
            CONCAT(COALESCE(h.stop_id, nva.stop_id), '_', nva.version) AS unique_id,
            COALESCE(h.stop_id, nva.stop_id) AS stop_id,
            COALESCE(h.hpl_id, nva.hpl_id) AS hpl_id,
            COALESCE(h.stop_name, nva.stop_name) AS stop_name,
            nva.startdatum,
            nva.slutdatum,
            nva.version,
            SUM(nva.genomsnitt_veckodag_avgangar) AS genomsnitt_veckodag_avgangar,
            SUM(nva.antal_lordag_avgangar) AS antal_lordag_avgangar,
            SUM(nva.antal_sondag_avgangar) AS antal_sondag_avgangar,
            SUM(nva.genomsnitt_dagliga_avgangar) AS genomsnitt_dagliga_avgangar,
            ARRAY_TO_STRING(ARRAY(SELECT DISTINCT unnest(string_to_array(STRING_AGG(DISTINCT nva.linjer, ', ' ORDER BY nva.linjer), ', ')) ORDER BY unnest), ', ') AS linjer,
            ARRAY_TO_STRING(ARRAY(SELECT DISTINCT unnest(string_to_array(STRING_AGG(DISTINCT nva.linjetyper, ', ' ORDER BY nva.linjetyper), ', ')) ORDER BY unnest), ', ') AS linjetyper,
            ARRAY_TO_STRING(ARRAY(SELECT DISTINCT unnest(string_to_array(STRING_AGG(DISTINCT nva.operator, ', ' ORDER BY nva.operator), ', ')) ORDER BY unnest), ', ') AS operator,
            ARRAY_TO_STRING(ARRAY(SELECT DISTINCT unnest(string_to_array(STRING_AGG(DISTINCT nva.fordonstyp, ', ' ORDER BY nva.fordonstyp), ', ')) ORDER BY unnest), ', ') AS fordonstyp,
            ARRAY_TO_STRING(ARRAY(SELECT DISTINCT unnest(string_to_array(STRING_AGG(DISTINCT nva.route_type, ', ' ORDER BY nva.route_type), ', ')) ORDER BY unnest), ', ') AS route_type,
            COALESCE(ST_SetSRID(h.geometry, 3006), nva.geometry) AS geometry
        FROM {s}.{vy_hallplatslage} nva
        LEFT JOIN {s}.stops h ON nva.parent_station = h.stop_id AND nva.version = h.version
        GROUP BY
            COALESCE(h.stop_id, nva.stop_id), COALESCE(h.hpl_id, nva.hpl_id),
            COALESCE(h.stop_name, nva.stop_name), nva.startdatum, nva.slutdatum,
            COALESCE(ST_SetSRID(h.geometry, 3006), nva.geometry), nva.version;
      "))
      gtfs_skapa_geomindex(con, s, vy_hallplats)
    })

    message("Materialiserade vyer skapade framgångsrikt.")
  }, error = function(e) {
    stop(glue::glue("Ett fel inträffade vid skapandet av materialiserade vyer: {conditionMessage(e)}"), call. = FALSE)
  })

  invisible(TRUE)
}


#' Create current route materialized views
#'
#' Builds spatial route summaries for the current GTFS feed.
#' @describeIn rdgtfs-api Create current route views
#' @param con A DBI database connection.
#' @param schema Database schema containing current GTFS tables.
#' @param agency_id Optional agency ID filter.
#' @param agency_namn Optional agency-name substring filter.
#' @param vy_suffix Optional suffix for generated view names.
#' @return `TRUE` invisibly.
#' @export
skapa_vyer_linjer <- function(con, schema = "gtfs", agency_id = NULL,
                              agency_namn = NULL, vy_suffix = NULL) {
  # Kolumnen "operator" är operatörsnamnet för linjen, "route_type" den råa
  # GTFS-koden och "fordonstyp" en läsbar grupp härledd från den.
  #
  # OBS antal_turer räknas nu per (shape, route). Tidigare joinades alla turer på
  # shape_id oavsett route, så en shape som delas av flera linjer fick samma
  # (för höga) antal_turer på alla linjer.

  p <- gtfs_vy_parametrar(con, schema, agency_id, agency_namn, vy_suffix)
  agency_filter  <- p$agency_filter
  fordonstyp_sql <- gtfs_fordonstyp_sql()
  vy_alla       <- paste0("vy_linjer_avgangar_alla", p$vy_suffix)
  vy_vanligaste <- paste0("vy_linjer_avgangar_vanligaste", p$vy_suffix)

  tryCatch({
    gtfs_transaktion(con, {
      DBI::dbExecute(con, glue::glue("DROP MATERIALIZED VIEW IF EXISTS {schema}.{vy_alla} CASCADE;"))
      DBI::dbExecute(con, glue::glue("DROP MATERIALIZED VIEW IF EXISTS {schema}.{vy_vanligaste} CASCADE;"))

      DBI::dbExecute(con, glue::glue("
        CREATE MATERIALIZED VIEW {schema}.{vy_alla} AS
        WITH linje_tur AS (
            SELECT t.trip_id, t.shape_id, t.route_id, t.service_id
            FROM {schema}.trips t
            JOIN {schema}.routes r ON t.route_id = r.route_id
            WHERE t.shape_id IS NOT NULL
            {agency_filter}
        ),
        turer_per_service AS (
            SELECT shape_id, route_id, service_id, COUNT(*) AS antal_turer
            FROM linje_tur
            GROUP BY shape_id, route_id, service_id
        ),
        dagliga_avgangar AS (
            SELECT s.shape_id, s.route_id, cd.date, SUM(s.antal_turer)::BIGINT AS antal_avgangar
            FROM turer_per_service s
            JOIN {schema}.calendar_dates cd ON s.service_id = cd.service_id
            WHERE cd.exception_type = 1
            GROUP BY s.shape_id, s.route_id, cd.date
        ),
        avgangar_vecka AS (
            SELECT
                shape_id,
                route_id,
                DATE_TRUNC('week', date) AS vecka_start,
                SUM(CASE WHEN EXTRACT(DOW FROM date) BETWEEN 1 AND 5 THEN antal_avgangar ELSE 0 END) AS total_veckodag_avgangar,
                SUM(CASE WHEN EXTRACT(DOW FROM date) = 6 THEN antal_avgangar ELSE 0 END) AS total_lordag_avgangar,
                SUM(CASE WHEN EXTRACT(DOW FROM date) = 0 THEN antal_avgangar ELSE 0 END) AS total_sondag_avgangar,
                SUM(antal_avgangar) AS total_veckans_avgangar
            FROM dagliga_avgangar
            GROUP BY shape_id, route_id, DATE_TRUNC('week', date)
        ),
        normal_vecka AS (
            SELECT
                shape_id, route_id, vecka_start, total_veckans_avgangar, total_veckodag_avgangar,
                total_lordag_avgangar, total_sondag_avgangar,
                ROW_NUMBER() OVER (PARTITION BY shape_id, route_id ORDER BY total_veckans_avgangar DESC) AS rank
            FROM avgangar_vecka
        ),
        turer_totalt AS (
            SELECT shape_id, route_id, SUM(antal_turer)::BIGINT AS antal_turer
            FROM turer_per_service
            GROUP BY shape_id, route_id
        )
        SELECT
            sl.shape_id,
            r.route_short_name AS linjenummer,
            r.route_long_name,
            lc.klassificering,
            ag.agency_name AS operator,
            r.route_type,
            {fordonstyp_sql} AS fordonstyp,
            sl.antal_punkter,
            sl.max_dist AS max_avstand_punkter,
            sl.geometry,
            ROUND(nv.total_veckodag_avgangar / 5.0, 2) AS genomsnitt_veckodag_avgangar,
            nv.total_lordag_avgangar AS antal_lordag_avgangar,
            nv.total_sondag_avgangar AS antal_sondag_avgangar,
            ROUND(nv.total_veckans_avgangar / 7.0, 2) AS genomsnitt_veckans_avgangar,
            tt.antal_turer
        FROM normal_vecka nv
        JOIN turer_totalt tt ON nv.shape_id = tt.shape_id AND nv.route_id = tt.route_id
        JOIN {schema}.shapes_line sl ON nv.shape_id = sl.shape_id
        JOIN {schema}.routes r ON nv.route_id = r.route_id
        LEFT JOIN {schema}.linjeklassificering lc ON r.route_short_name = lc.route_short_name
        LEFT JOIN {schema}.agency ag ON r.agency_id = ag.agency_id
        WHERE nv.rank = 1;
      "))
      gtfs_skapa_geomindex(con, schema, vy_alla)

      # Vanligaste linjen (flest turer) per linjenummer
      DBI::dbExecute(con, glue::glue("
        CREATE MATERIALIZED VIEW {schema}.{vy_vanligaste} AS
        WITH vanligaste_linje_per_route AS (
            SELECT
                linjenummer, klassificering, operator, route_type, fordonstyp, shape_id,
                antal_turer, genomsnitt_veckodag_avgangar, antal_lordag_avgangar,
                antal_sondag_avgangar, genomsnitt_veckans_avgangar, antal_punkter,
                max_avstand_punkter, geometry,
                ROW_NUMBER() OVER (PARTITION BY linjenummer ORDER BY antal_turer DESC) AS rank
            FROM {schema}.{vy_alla}
        )
        SELECT
            linjenummer, klassificering, operator, route_type, fordonstyp, shape_id,
            antal_turer, genomsnitt_veckodag_avgangar, antal_lordag_avgangar,
            antal_sondag_avgangar, genomsnitt_veckans_avgangar, antal_punkter,
            max_avstand_punkter, geometry
        FROM vanligaste_linje_per_route
        WHERE rank = 1;
      "))
      gtfs_skapa_geomindex(con, schema, vy_vanligaste)
    })

    message(glue::glue("Materialiserad vy '{schema}.{vy_alla}' skapad framgångsrikt."))
  }, error = function(e) {
    stop(paste("Ett fel inträffade vid skapandet av vyn:", conditionMessage(e)), call. = FALSE)
  })

  invisible(TRUE)
}


#' Create historical route materialized views
#'
#' Builds version-aware spatial route summaries.
#' @describeIn rdgtfs-api Create historical route views
#' @param con A DBI database connection.
#' @param schema Base schema; historical data is read from its `_historisk` schema.
#' @param agency_id Optional agency ID filter.
#' @param agency_namn Optional agency-name substring filter.
#' @param vy_suffix Optional suffix for generated view names.
#' @return `TRUE` invisibly.
#' @export
skapa_vyer_historisk_linjer <- function(con, schema = "gtfs", agency_id = NULL,
                                        agency_namn = NULL, vy_suffix = NULL) {
  # Historisk motsvarighet till skapa_vyer_linjer(). Se den för anmärkning om antal_turer.

  p <- gtfs_vy_parametrar(con, schema, agency_id, agency_namn, vy_suffix)
  agency_filter  <- p$agency_filter
  fordonstyp_sql <- gtfs_fordonstyp_sql()
  s <- gtfs_historiskt_schema(schema)
  vy_alla       <- paste0("vy_historisk_linjer_avgangar_alla", p$vy_suffix)
  vy_vanligaste <- paste0("vy_historisk_linjer_avgangar_vanligaste", p$vy_suffix)

  tryCatch({
    gtfs_transaktion(con, {
      DBI::dbExecute(con, glue::glue("DROP MATERIALIZED VIEW IF EXISTS {s}.{vy_alla} CASCADE;"))
      DBI::dbExecute(con, glue::glue("DROP MATERIALIZED VIEW IF EXISTS {s}.{vy_vanligaste} CASCADE;"))

      DBI::dbExecute(con, glue::glue("
        CREATE MATERIALIZED VIEW {s}.{vy_alla} AS
        WITH linje_tur AS (
            SELECT t.trip_id, t.shape_id, t.route_id, t.service_id, t.version
            FROM {s}.trips t
            JOIN {s}.routes r ON t.route_id = r.route_id AND t.version = r.version
            WHERE t.shape_id IS NOT NULL
            {agency_filter}
        ),
        turer_per_service AS (
            SELECT shape_id, route_id, service_id, version, COUNT(*) AS antal_turer
            FROM linje_tur
            GROUP BY shape_id, route_id, service_id, version
        ),
        dagliga_avgangar AS (
            SELECT s.shape_id, s.route_id, s.version, cd.date, SUM(s.antal_turer)::BIGINT AS antal_avgangar
            FROM turer_per_service s
            JOIN {s}.calendar_dates cd ON s.service_id = cd.service_id AND s.version = cd.version
            WHERE cd.exception_type = 1
            GROUP BY s.shape_id, s.route_id, s.version, cd.date
        ),
        avgangar_vecka AS (
            SELECT
                shape_id,
                route_id,
                version,
                DATE_TRUNC('week', date) AS vecka_start,
                SUM(CASE WHEN EXTRACT(DOW FROM date) BETWEEN 1 AND 5 THEN antal_avgangar ELSE 0 END) AS total_veckodag_avgangar,
                SUM(CASE WHEN EXTRACT(DOW FROM date) = 6 THEN antal_avgangar ELSE 0 END) AS total_lordag_avgangar,
                SUM(CASE WHEN EXTRACT(DOW FROM date) = 0 THEN antal_avgangar ELSE 0 END) AS total_sondag_avgangar,
                SUM(antal_avgangar) AS total_veckans_avgangar
            FROM dagliga_avgangar
            GROUP BY shape_id, route_id, version, DATE_TRUNC('week', date)
        ),
        normal_vecka AS (
            SELECT
                shape_id, route_id, version, vecka_start, total_veckans_avgangar, total_veckodag_avgangar,
                total_lordag_avgangar, total_sondag_avgangar,
                ROW_NUMBER() OVER (PARTITION BY shape_id, route_id, version ORDER BY total_veckans_avgangar DESC) AS rank
            FROM avgangar_vecka
        ),
        turer_totalt AS (
            SELECT shape_id, route_id, version, SUM(antal_turer)::BIGINT AS antal_turer
            FROM turer_per_service
            GROUP BY shape_id, route_id, version
        )
        SELECT
            CONCAT(sl.shape_id, '_', nv.version) AS unique_id,
            sl.shape_id,
            r.route_short_name AS linjenummer,
            r.route_long_name,
            lc.klassificering,
            ag.agency_name AS operator,
            r.route_type,
            {fordonstyp_sql} AS fordonstyp,
            sl.antal_punkter,
            sl.max_dist AS max_avstand_punkter,
            sl.geometry,
            ROUND(nv.total_veckodag_avgangar / 5.0, 2) AS genomsnitt_veckodag_avgangar,
            nv.total_lordag_avgangar AS antal_lordag_avgangar,
            nv.total_sondag_avgangar AS antal_sondag_avgangar,
            ROUND(nv.total_veckans_avgangar / 7.0, 2) AS genomsnitt_veckans_avgangar,
            tt.antal_turer,
            nv.version,
            v.start_date AS startdatum,
            v.end_date AS slutdatum
        FROM normal_vecka nv
        JOIN turer_totalt tt ON nv.shape_id = tt.shape_id AND nv.route_id = tt.route_id AND nv.version = tt.version
        JOIN {s}.shapes_line sl ON nv.shape_id = sl.shape_id AND nv.version = sl.version
        JOIN {s}.routes r ON nv.route_id = r.route_id AND nv.version = r.version
        LEFT JOIN {s}.linjeklassificering lc ON r.route_short_name = lc.route_short_name AND r.version = lc.version
        LEFT JOIN {s}.agency ag ON r.agency_id = ag.agency_id AND r.version = ag.version
        JOIN {s}.versions v ON nv.version = v.version
        WHERE nv.rank = 1;
      "))
      gtfs_skapa_geomindex(con, s, vy_alla)

      DBI::dbExecute(con, glue::glue("
        CREATE MATERIALIZED VIEW {s}.{vy_vanligaste} AS
        WITH vanligaste_linje_per_route AS (
            SELECT
                unique_id, linjenummer, klassificering, operator, route_type, fordonstyp, shape_id,
                antal_turer, genomsnitt_veckodag_avgangar, antal_lordag_avgangar,
                antal_sondag_avgangar, genomsnitt_veckans_avgangar, antal_punkter,
                max_avstand_punkter, geometry, version, startdatum, slutdatum,
                ROW_NUMBER() OVER (PARTITION BY linjenummer, version ORDER BY antal_turer DESC) AS rank
            FROM {s}.{vy_alla}
        )
        SELECT
            unique_id, linjenummer, klassificering, operator, route_type, fordonstyp, shape_id,
            antal_turer, genomsnitt_veckodag_avgangar, antal_lordag_avgangar,
            antal_sondag_avgangar, genomsnitt_veckans_avgangar, antal_punkter,
            max_avstand_punkter, geometry, version, startdatum, slutdatum
        FROM vanligaste_linje_per_route
        WHERE rank = 1;
      "))
      gtfs_skapa_geomindex(con, s, vy_vanligaste)
    })

    message("Materialiserade vyer skapade framgångsrikt.")
  }, error = function(e) {
    stop(glue::glue("Ett fel inträffade vid skapandet av materialiserade vyer: {conditionMessage(e)}"), call. = FALSE)
  })

  invisible(TRUE)
}
