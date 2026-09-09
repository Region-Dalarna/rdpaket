# Pendlingsanalyser: pendlingsnätverk, kraftfält och in-/utpendling på ruta.
#
# Kräver att vägnätet redan finns i schemat "grafer" (tabellerna nvdb_noded,
# nvdb_noded_vertices_pgr och tatort).

intern_utskriftsmapp <- function() {
  if (requireNamespace("rdverktyg", quietly = TRUE)) rdverktyg::utskriftsmapp() else getwd()
}

intern_pendling_con <- function(con) {
  if (inherits(con, "DBIConnection")) return(list(con = con, egen = FALSE))
  ny <- rdpostgres::uppkoppling_db(db_name = "geodata", service_name = "rd_geodata")
  if (is.null(ny)) stop("Kunde inte ansluta till databasen.", call. = FALSE)
  list(con = ny, egen = TRUE)
}

#' Bygg ett pendlingsnätverk och beräkna antal pendlare per vägsträcka
#'
#' @param tabell_pend_relation Data.frame med kolumnerna `from_id`, `to_id`,
#'   `n` (pendlingsrelationer, hämtas t.ex. på MONA).
#' @param con En `DBIConnection`, eller `NA` för Region Dalarnas databas.
#' @param dist Max avstånd (m) från tätort till närmaste vägnod.
#' @param skriv_till_gpkg Skriv resultatet till GeoPackage i stället för att
#'   returnera det.
#' @param ut_mapp,gpkg_namn Målmapp och filnamn för GeoPackage.
#'
#' @return Ett `sf`-objekt, eller osynligt `NULL` om `skriv_till_gpkg`.
#' @export
pendling_natverk <- function(tabell_pend_relation, con = NA, dist = 2000,
                             skriv_till_gpkg = FALSE, ut_mapp = NA, gpkg_namn = NA) {
  intern_krav("sf")
  if (!all(c("from_id", "to_id", "n") %in% colnames(tabell_pend_relation))) {
    stop("`tabell_pend_relation` måste innehålla kolumnerna 'from_id', 'to_id', 'n'.", call. = FALSE)
  }
  if (is.na(ut_mapp)) ut_mapp <- intern_utskriftsmapp()
  if (is.na(gpkg_namn)) gpkg_namn <- "pendling_natverk.gpkg"

  cc <- intern_pendling_con(con)
  on.exit(intern_stang(cc), add = TRUE)
  con <- cc$con

  DBI::dbExecute(con, "SET search_path TO grafer, public;")
  kant_tabell <- "nvdb_noded"
  nod_tabell <- "grafer.nvdb_noded_vertices_pgr"
  kost_kol <- "dist_cost"; omvand_kost_kol <- "dist_reverse_cost"

  DBI::dbWriteTable(con, "data", tabell_pend_relation, overwrite = TRUE, temporary = FALSE)

  DBI::dbExecute(con, "DROP TABLE IF EXISTS tatort_vertex;")
  DBI::dbExecute(con, glue::glue("
    CREATE TEMP TABLE tatort_vertex AS
    SELECT t.tokod, e.id, e.dist
    FROM tatort t
    JOIN lateral(
      SELECT id, e.geom <-> t.geom as dist FROM {nod_tabell} e
      ORDER BY t.geom <-> e.geom LIMIT 1
    ) AS e ON true
    WHERE dist < {dist};"))

  DBI::dbExecute(con, "DROP TABLE IF EXISTS combinations;")
  DBI::dbExecute(con, "
    CREATE TEMP TABLE combinations AS
    SELECT c.*, t.id AS start_vid, f.id AS end_vid
    FROM data c
    JOIN tatort_vertex t ON c.from_id = t.tokod
    JOIN tatort_vertex f ON c.to_id = f.tokod
    WHERE c.from_id <> c.to_id;")

  query <- glue::glue("
    WITH astar AS (
      SELECT * FROM pgr_aStar(
        'SELECT id, source, target, {kost_kol} AS cost, {omvand_kost_kol} AS reverse_cost, x1, y1, x2, y2 FROM {kant_tabell}',
        'SELECT start_vid AS source, end_vid AS target FROM combinations'
      )
    ), edges AS (
      SELECT edge as edge_id, sum(n) AS antal_pend
      FROM astar a JOIN combinations d ON a.start_vid = d.start_vid AND a.end_vid = d.end_vid
      WHERE edge <> -1 GROUP BY edge
    )
    SELECT * FROM edges e JOIN {kant_tabell} r ON e.edge_id = r.id;")

  natverk <- sf::st_read(con, query = query, quiet = TRUE)

  if (skriv_till_gpkg) {
    sokvag <- file.path(ut_mapp, gpkg_namn)
    sf::st_write(natverk, sokvag, delete_dsn = TRUE, quiet = TRUE)
    message("Resultat skrivet till ", sokvag)
    return(invisible(NULL))
  }
  natverk
}

#' Beräkna kraftfält (LA-områden) utifrån pendlingsrelationer mellan tätorter
#'
#' Klassificerar tätorter som LA/solitär/satellit/gemensamt LA och bygger
#' kraftfältspolygoner genom att buffra rutterna mellan satelliter och deras LA.
#'
#' @param tabell_pend_relation Data.frame med `from_id`, `to_id`, `n`.
#' @param ut_mapp,gpkg_namn Målmapp/filnamn för GeoPackage.
#' @param enskilt_troskelvarde,totalt_troskelvarde Tröskelvärden (%) för
#'   klassificeringen.
#' @param primar_la_buffer,sekundar_la_buffer,gemensam_la_buffer Buffertavstånd (m).
#' @param con En `DBIConnection`, eller `NA` för Region Dalarnas databas.
#' @param dist Max avstånd (m) från tätort till vägnod.
#' @param skriv_till_gpkg Skriv lagren till GeoPackage i stället för att
#'   returnera dem.
#'
#' @return En namngiven lista med `sf`-objekt (`la`, `solitary`, `common_la`,
#'   `satellites`, `routes`, `secla_areas`, `primla_areas`, `commonla_areas`),
#'   eller osynligt `NULL` om `skriv_till_gpkg`.
#' @export
pendling_kraftfalt <- function(tabell_pend_relation, ut_mapp = NA, gpkg_namn = NA,
                               enskilt_troskelvarde = 20, totalt_troskelvarde = 35,
                               primar_la_buffer = 2000, sekundar_la_buffer = 1000,
                               gemensam_la_buffer = 5000, con = NA, dist = 2000,
                               skriv_till_gpkg = FALSE) {
  intern_krav("sf")
  if (!all(c("from_id", "to_id", "n") %in% colnames(tabell_pend_relation))) {
    stop("`tabell_pend_relation` måste innehålla kolumnerna 'from_id', 'to_id', 'n'.", call. = FALSE)
  }
  if (is.na(ut_mapp)) ut_mapp <- intern_utskriftsmapp()
  if (is.na(gpkg_namn)) gpkg_namn <- "kraftfalt.gpkg"

  cc <- intern_pendling_con(con)
  on.exit(intern_stang(cc), add = TRUE)
  con <- cc$con

  DBI::dbExecute(con, "SET search_path TO grafer, public;")
  kant_tabell <- "nvdb_noded"; nod_tabell <- "grafer.nvdb_noded_vertices_pgr"
  kost_kol <- "dist_cost"; omvand_kost_kol <- "dist_reverse_cost"
  et <- enskilt_troskelvarde; tt <- totalt_troskelvarde

  DBI::dbWriteTable(con, "data", tabell_pend_relation, overwrite = TRUE, temporary = FALSE)

  kor <- function(drop, sql) { DBI::dbExecute(con, drop); DBI::dbExecute(con, sql) }

  kor("DROP TABLE IF EXISTS tatort_vertex;", glue::glue("
    CREATE TEMP TABLE tatort_vertex AS
    SELECT t.tokod, e.id, e.dist FROM tatort t
    JOIN lateral(SELECT id, e.geom <-> t.geom as dist FROM {nod_tabell} e
                 ORDER BY t.geom <-> e.geom LIMIT 1) AS e ON true
    WHERE dist < {dist};"))

  kor("DROP TABLE IF EXISTS commute_combinations;", glue::glue("
    CREATE TABLE commute_combinations AS
    WITH temp_pendlings_data AS (
      SELECT from_id, to_id, n,
        sum(n) OVER w AS totalworkers,
        sum(n) FILTER (WHERE from_id=to_id) OVER w AS localworkers
      FROM grafer.data
      WHERE from_id IN (SELECT tokod FROM tatort_vertex)
      WINDOW w AS (PARTITION BY from_id)
    )
    SELECT from_id, to_id, n, totalworkers, localworkers,
      100*n::DECIMAL/totalworkers AS perc,
      100-100*localworkers/totalworkers as perc_total_commuters,
      CASE WHEN from_id=to_id THEN 0
           ELSE count(*) FILTER (WHERE from_id <> to_id) OVER wd END AS ranking
    FROM temp_pendlings_data
    WHERE to_id IN (SELECT tokod FROM tatort_vertex)
    WINDOW wd AS (PARTITION BY from_id ORDER BY n DESC)
    ORDER BY from_id, n desc;"))

  kor("DROP TABLE IF EXISTS la;", glue::glue("
    CREATE TEMP TABLE la AS
    SELECT c.from_id AS id, c.totalworkers, c.localworkers, c.perc_total_commuters,
           t.tobeteckn, t.lan, t.kommun, t.kommunnamn, t.geom
    FROM commute_combinations c JOIN grafer.tatort t ON t.tokod = c.from_id
    WHERE ranking = 1 AND perc <= {et} AND perc_total_commuters <= {tt};"))

  kor("DROP TABLE IF EXISTS solitary;", glue::glue("
    CREATE TEMP TABLE solitary AS
    SELECT c.from_id as id, c.totalworkers, c.localworkers, c.perc_total_commuters,
           t.tobeteckn, t.lan, t.kommun, t.kommunnamn, t.geom
    FROM commute_combinations c JOIN grafer.tatort t ON t.tokod=c.from_id
    WHERE ranking=1 AND perc <= {et} AND perc_total_commuters > {tt};"))

  kor("DROP TABLE IF EXISTS common_la;", glue::glue("
    CREATE TEMP TABLE common_la AS
    WITH sats AS (SELECT * FROM commute_combinations WHERE ranking > 1 AND perc > {et}),
    common_la_cte AS (
      SELECT a.* FROM sats a JOIN sats b ON a.from_id = b.to_id AND a.to_id = b.from_id
    )
    SELECT c.from_id AS id, c.to_id AS common_la, c.totalworkers, c.localworkers, c.perc_total_commuters,
           t.tobeteckn, t.lan, t.kommun, t.kommunnamn, t.geom
    FROM common_la_cte c JOIN grafer.tatort t ON t.tokod = c.from_id;"))

  kor("DROP TABLE IF EXISTS satellites;", glue::glue("
    CREATE TEMP TABLE satellites AS
    WITH sats AS (
      SELECT * FROM commute_combinations
      WHERE ranking IN (1, 2) AND perc > {et} AND from_id NOT IN (SELECT id FROM common_la)
    ), la_id AS (SELECT id FROM la UNION SELECT id FROM common_la),
    agg AS (
      SELECT from_id,
        array_agg(to_id ORDER BY ranking) AS id_array_all,
        array_agg(to_id ORDER BY ranking) FILTER (WHERE to_id IN (SELECT id FROM la_id)) AS id_array
      FROM sats GROUP BY from_id
    )
    SELECT a.from_id AS id, id_array[1] AS primary_la, id_array[2] AS secondary_la,
           id_array_all[1] AS primary_destination, id_array_all[2] AS secondary_destination,
           s.totalworkers, s.localworkers, s.perc_total_commuters,
           t.tobeteckn, t.lan, t.kommun, t.kommunnamn, t.geom
    FROM agg a JOIN sats s ON a.from_id = s.from_id
    JOIN grafer.tatort t ON t.tokod = s.from_id;"))

  kor("DROP TABLE IF EXISTS route_combinations;", glue::glue("
    CREATE TEMP TABLE route_combinations AS
    WITH sats AS (SELECT from_id, to_id, ranking FROM commute_combinations WHERE ranking in (1,2) AND perc > {et})
    SELECT s.*, f.id as start_vid, t.id as end_vid
    FROM sats s JOIN tatort_vertex f ON f.tokod=s.from_id JOIN tatort_vertex t ON t.tokod=s.to_id;"))

  kor("DROP TABLE IF EXISTS routes;", glue::glue("
    CREATE TEMP TABLE routes AS
    WITH astar AS (
      SELECT * FROM pgr_aStar(
        'SELECT id, source, target, {kost_kol} AS cost, {omvand_kost_kol} AS reverse_cost, x1, y1, x2, y2 FROM grafer.{kant_tabell}',
        'SELECT start_vid AS source, end_vid AS target FROM route_combinations'
      )
    ), paths AS (
      SELECT a.start_vid, a.end_vid, sum(a.cost) AS cost, max(a.agg_cost) AS agg_cost,
             st_LineMerge(st_union(r.geom)) AS geom
      FROM astar a JOIN grafer.{kant_tabell} r ON a.edge=r.id
      GROUP BY a.start_vid, a.end_vid
    )
    SELECT c.*, p.agg_cost, p.geom
    FROM route_combinations c JOIN paths p ON c.start_vid=p.start_vid AND c.end_vid=p.end_vid;"))

  kor("DROP TABLE IF EXISTS secla_areas;", glue::glue("
    CREATE TEMP TABLE secla_areas AS
    SELECT to_id, MAX(ranking) AS max_ranking, st_buffer(st_collect(r.geom), {sekundar_la_buffer}) as geom
    FROM satellites s JOIN routes r ON s.id=r.from_id AND s.secondary_la=r.to_id
    GROUP BY to_id;"))

  kor("DROP TABLE IF EXISTS primla_areas;", glue::glue("
    CREATE TEMP TABLE primla_areas AS
    SELECT to_id, max(ranking), st_buffer(st_collect(r.geom), {primar_la_buffer}) as geom
    FROM satellites s JOIN routes r ON s.id=r.from_id AND s.primary_la=r.to_id
    GROUP BY to_id;"))

  kor("DROP TABLE IF EXISTS commonla_areas;", glue::glue("
    CREATE TEMP TABLE commonla_areas AS
    SELECT from_id, to_id, ranking, st_buffer(r.geom, {gemensam_la_buffer}) as geom
    FROM common_la s JOIN routes r ON s.id=r.from_id AND s.common_la=r.to_id;"))

  lager <- c("la", "solitary", "common_la", "satellites", "routes",
             "secla_areas", "primla_areas", "commonla_areas")

  las_lager <- function(namn) {
    finns <- DBI::dbGetQuery(con, glue::glue(
      "SELECT EXISTS (SELECT FROM information_schema.tables WHERE table_name = '{namn}')"))[1, 1]
    if (!isTRUE(finns)) return(NULL)
    lyr <- tryCatch(sf::st_read(con, query = glue::glue("SELECT * FROM {namn}"), quiet = TRUE),
                    error = function(e) NULL)
    if (is.null(lyr) || nrow(lyr) == 0) NULL else lyr
  }

  if (skriv_till_gpkg) {
    sokvag <- file.path(ut_mapp, gpkg_namn)
    purrr::walk(lager, function(namn) {
      lyr <- las_lager(namn)
      if (!is.null(lyr)) sf::st_write(lyr, sokvag, namn, append = TRUE, quiet = TRUE)
    })
    message("Lager skrivna till ", sokvag)
    return(invisible(NULL))
  }
  stats::setNames(lapply(lager, las_lager), lager)
}

#' In- och utpendling för rutor inom en polygon
#'
#' @param version `"PostGIS"` (snabbare, kräver `con`) eller `"R"`.
#' @param con En `DBIConnection`, eller `NA` för Region Dalarnas databas
#'   (används av `"PostGIS"`-versionen).
#' @param tabell_pend_relation_ruta Data.frame med `boruta`, `arbruta`,
#'   `antalpend`.
#' @param rutor `sf`-objekt med rutorna (måste ha `rut_id`).
#' @param polygon `sf`-objekt (punkt/linje/polygon) som avgränsar området.
#' @param ut_mapp Målmapp för GeoPackage.
#' @param grid_epsg EPSG-kod (inte använd i nuläget, behålls för kompatibilitet).
#' @param skriv_till_gpkg Skriv resultatet till GeoPackage.
#' @param gpkg_namn Filnamn (inte använt - namnet sätts av versionen).
#'
#' @return En lista med `utvalda_rutor`, `in_pendling`, `ut_pendling`.
#' @export
pendling_ruta <- function(version = c("PostGIS", "R"), con = NA,
                          tabell_pend_relation_ruta, rutor, polygon,
                          ut_mapp = NA, grid_epsg = 3006,
                          skriv_till_gpkg = FALSE, gpkg_namn = NA) {
  intern_krav("sf")
  version <- match.arg(version)

  if (!inherits(rutor, "sf")) stop("`rutor` måste vara ett sf-objekt.", call. = FALSE)
  if (!inherits(polygon, "sf")) stop("`polygon` måste vara ett sf-objekt.", call. = FALSE)
  if (!all(c("boruta", "arbruta", "antalpend") %in% colnames(tabell_pend_relation_ruta))) {
    stop("`tabell_pend_relation_ruta` måste innehålla 'boruta', 'arbruta', 'antalpend'.", call. = FALSE)
  }
  if (is.na(ut_mapp)) ut_mapp <- intern_utskriftsmapp()

  cc <- NULL
  if (version == "PostGIS") {
    cc <- intern_pendling_con(con)
    con <- cc$con
    if (!DBI::dbIsValid(con)) stop("Databasuppkopplingen är inte giltig.", call. = FALSE)
  }
  on.exit(if (!is.null(cc)) intern_stang(cc), add = TRUE)

  if (sf::st_crs(polygon) != sf::st_crs(rutor)) polygon <- sf::st_transform(polygon, sf::st_crs(rutor))
  vald_polygon <- sf::st_intersection(rutor, polygon)
  if (nrow(vald_polygon) == 0) stop("Ingen överlappning mellan polygon och rutor.", call. = FALSE)

  if (version == "PostGIS") {
    DBI::dbWriteTable(con, "ruta", rutor, overwrite = TRUE, temporary = TRUE)
    DBI::dbWriteTable(con, "rutpendling", tabell_pend_relation_ruta, overwrite = TRUE, temporary = TRUE)

    utvalda_rutor <- sf::st_filter(sf::st_read(con, layer = "ruta", quiet = TRUE), vald_polygon)
    ids <- paste(utvalda_rutor$rut_id, collapse = ", ")

    in_pendling <- sf::st_read(con, query = glue::glue("
      WITH commuters_in AS (
        SELECT boruta, sum(antalpend) AS total FROM rutpendling
        WHERE arbruta::BIGINT IN ({ids}) AND boruta::BIGINT NOT IN ({ids})
        GROUP BY boruta
      )
      SELECT c.*, r.geom FROM commuters_in c JOIN ruta r ON c.boruta = r.rut_id"), quiet = TRUE)
    ut_pendling <- sf::st_read(con, query = glue::glue("
      WITH commuters_out AS (
        SELECT arbruta, sum(antalpend) AS total FROM rutpendling
        WHERE boruta::BIGINT IN ({ids}) AND arbruta::BIGINT NOT IN ({ids})
        GROUP BY arbruta
      )
      SELECT c.*, r.geom FROM commuters_out c JOIN ruta r ON c.arbruta = r.rut_id"), quiet = TRUE)
  } else {
    utvalda_rutor <- sf::st_filter(rutor, vald_polygon)
    fran <- tabell_pend_relation_ruta[
      tabell_pend_relation_ruta$boruta %in% utvalda_rutor$rut_id &
      !tabell_pend_relation_ruta$arbruta %in% utvalda_rutor$rut_id, ]
    till <- tabell_pend_relation_ruta[
      tabell_pend_relation_ruta$arbruta %in% utvalda_rutor$rut_id &
      !tabell_pend_relation_ruta$boruta %in% utvalda_rutor$rut_id, ]

    from_c <- stats::aggregate(antalpend ~ boruta, data = till, FUN = sum)
    names(from_c) <- c("rut_id", "pendlare_fran")
    to_c <- stats::aggregate(antalpend ~ arbruta, data = fran, FUN = sum)
    names(to_c) <- c("rut_id", "pendlare_till")

    full <- merge(from_c, to_c, by = "rut_id", all = TRUE)
    result <- merge(rutor, full, by = "rut_id")
    in_pendling <- result[!is.na(result$pendlare_fran), setdiff(names(result), "pendlare_till")]
    ut_pendling <- result[!is.na(result$pendlare_till), setdiff(names(result), "pendlare_fran")]
  }

  if (skriv_till_gpkg) {
    filnamn <- if (version == "PostGIS") "rut_pendling_pg.gpkg" else "rut_pendlingR.gpkg"
    sokvag <- file.path(ut_mapp, filnamn)
    sf::st_write(utvalda_rutor, sokvag, "omrade", delete_dsn = TRUE, append = FALSE, quiet = TRUE)
    sf::st_write(in_pendling, sokvag, "in_pend", append = FALSE, quiet = TRUE)
    sf::st_write(ut_pendling, sokvag, "ut_pend", append = FALSE, quiet = TRUE)
  }

  list(utvalda_rutor = utvalda_rutor, in_pendling = in_pendling, ut_pendling = ut_pendling)
}
