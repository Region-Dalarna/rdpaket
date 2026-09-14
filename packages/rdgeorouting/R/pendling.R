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

# ---------------------------------------------------------------------------------------------
# Hjälpare för pendling_ruta(): rutor/fokusomrade kan var för sig ges som ett sf-objekt eller en
# postgis_tabell()-referens (se postgis.R). Vi behöver aldrig hela `rutor`-tabellen i minnet -
# bara (1) vilka rutor som ligger innanför fokusomrade och (2) geometrin för de specifika
# grannrutor som faktiskt dyker upp i den summerade in-/utpendlingen. Båda är riktade frågor/
# filter, oavsett om källan är ett sf-objekt eller en tabell i databasen.
# ---------------------------------------------------------------------------------------------

intern_krav_polygongeometri <- function(x, argnamn) {
  if (inherits(x, "postgis_tabell")) return(invisible(TRUE))
  if (!inherits(x, "sf")) {
    stop("`", argnamn, "` måste vara antingen ett sf-objekt (med polygon-/multipolygongeometri) ",
         "eller en postgis_tabell()-referens - inte t.ex. rasterdata.", call. = FALSE)
  }
  typer <- unique(as.character(sf::st_geometry_type(x)))
  if (!all(typer %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("`", argnamn, "` måste ha polygon- eller multipolygongeometri (hittade: ",
         paste(typer, collapse = ", "), ") - inte t.ex. punkter, linjer eller rasterdata.", call. = FALSE)
  }
  invisible(TRUE)
}

# fokusomrade är normalt en enda liten polygon - läses därför alltid in i sin helhet (ingen
# anledning att filtrera den i databasen innan den läses in).
intern_som_sf <- function(x, argnamn) {
  if (inherits(x, "sf")) return(x)
  cc <- intern_pendling_con(x$con)
  on.exit(intern_stang(cc), add = TRUE)
  omrade <- sf::st_read(cc$con, layer = DBI::Id(schema = x$schema, table = x$tabell), quiet = TRUE)
  intern_krav_polygongeometri(omrade, argnamn)
  omrade
}

# Geometrikolumnens namn och SRID för en postgis-tabell, avläst genom att hämta en enda rad -
# billigt även för en stor tabell, och gör att vi slipper anta att geometrikolumnen heter "geom".
intern_tabell_geominfo <- function(con, schema, tabell) {
  prov <- sf::st_read(con, query = glue::glue_sql(
    "SELECT * FROM {`schema`}.{`tabell`} LIMIT 1", .con = con), quiet = TRUE)
  srid <- sf::st_crs(prov)$epsg
  if (is.na(srid)) stop("Kunde inte avgöra SRID för ", schema, ".", tabell, ".", call. = FALSE)
  list(geom_kol = attr(prov, "sf_column"), srid = srid)
}

# Bygger ett gränssnitt mot `rutor`: $filtrera(fokusomrade) ger de rutor som ligger innanför
# fokusomrade, $id_geom(ids) ger geometrin för specifika rut_id. Databasaccelererat (ST_Intersects
# mot ett spatialt index) så fort en anslutning finns tillgänglig - för en postgis_tabell-referens
# frågas den befintliga tabellen direkt (ingen anledning att kopiera en tabell som redan ligger i
# databasen), för ett sf-objekt skrivs det i så fall till en temporär tabell med ett eget index
# först. Annars (inget con alls) görs allt i ren R utan databaskontakt.
intern_rutor_kalla <- function(rutor, con_arg) {

  if (inherits(rutor, "postgis_tabell")) {
    cc <- intern_pendling_con(rutor$con)
    info <- intern_tabell_geominfo(cc$con, rutor$schema, rutor$tabell)

    filtrera <- function(fokusomrade) {
      fokus_repr <- sf::st_transform(fokusomrade, info$srid)
      wkt <- sf::st_as_text(sf::st_union(sf::st_geometry(fokus_repr)))
      sf::st_read(cc$con, query = glue::glue_sql(
        "SELECT * FROM {`rutor$schema`}.{`rutor$tabell`} r
         WHERE ST_Intersects(r.{`info$geom_kol`}, ST_GeomFromText({wkt}, {info$srid}))",
        .con = cc$con), quiet = TRUE)
    }
    id_geom <- function(ids) {
      if (length(ids) == 0) {
        return(sf::st_read(cc$con, query = glue::glue_sql(
          "SELECT * FROM {`rutor$schema`}.{`rutor$tabell`} WHERE FALSE", .con = cc$con), quiet = TRUE))
      }
      sf::st_read(cc$con, query = glue::glue_sql(
        "SELECT * FROM {`rutor$schema`}.{`rutor$tabell`} WHERE rut_id IN ({ids*})", .con = cc$con), quiet = TRUE)
    }
    stang <- function() intern_stang(cc)

  } else {
    anvand_postgis <- inherits(con_arg, "DBIConnection")
    srid <- NULL

    if (anvand_postgis) {
      DBI::dbWriteTable(con_arg, "rutor_tmp", rutor, overwrite = TRUE, temporary = TRUE)
      DBI::dbExecute(con_arg, "CREATE INDEX ON rutor_tmp USING GIST (geom);")
      srid <- sf::st_crs(rutor)$epsg
      if (is.na(srid)) stop("Kunde inte avgöra SRID för `rutor`.", call. = FALSE)
    }

    filtrera <- function(fokusomrade) {
      if (anvand_postgis) {
        fokus_repr <- sf::st_transform(fokusomrade, srid)
        wkt <- sf::st_as_text(sf::st_union(sf::st_geometry(fokus_repr)))
        sf::st_read(con_arg, query = glue::glue_sql(
          "SELECT * FROM rutor_tmp r WHERE ST_Intersects(r.geom, ST_GeomFromText({wkt}, {srid}))",
          .con = con_arg), quiet = TRUE)
      } else {
        if (sf::st_crs(fokusomrade) != sf::st_crs(rutor)) {
          fokusomrade <- sf::st_transform(fokusomrade, sf::st_crs(rutor))
        }
        sf::st_filter(rutor, fokusomrade)
      }
    }
    id_geom <- function(ids) rutor[rutor$rut_id %in% ids, ]
    stang <- function() invisible(NULL)   # con_arg tillhör anroparen - stängs inte här
  }

  list(filtrera = filtrera, id_geom = id_geom, stang = stang)
}

#' In- och utpendling för rutor inom ett fokusområde
#'
#' Beräknar vilka rutor som pendlar in till respektive ut från ett angivet
#' fokusområde, utifrån en tabell med pendlingsrelationer mellan rutor.
#'
#' `rutor` och `fokusomrade` kan var för sig anges antingen som ett redan
#' inläst `sf`-objekt eller som en referens till en tabell i en PostGIS-
#' databas (skapad med [postgis_tabell()]) - funktionen avgör själv vilket,
#' och läser bara in det som faktiskt behövs (aldrig hela `rutor`, oavsett
#' hur stor den är). Beräkningen körs databasaccelererat så fort en
#' anslutning finns tillgänglig - antingen för att `rutor` eller
#' `fokusomrade` angetts som en [postgis_tabell()]-referens, eller för att
#' ett eget `con` skickas med trots att båda är `sf`-objekt. Ges ingen
#' anslutning alls (standardläget om båda är `sf`-objekt) körs allt i ren R
#' utan databaskontakt.
#'
#' Använd [pendling_ruta_karta()] för att göra resultatet till ett färdigt
#' `sf`-objekt (fokusområdets rutor svartmarkerade, in- eller
#' utpendlingsrutorna med antal pendlare) och/eller en GeoPackage-fil.
#'
#' @param tabell_pend_relation_ruta Data.frame med `boruta`, `arbruta`,
#'   `antalpend`.
#' @param rutor Ett `sf`-objekt med polygon-/multipolygongeometri och en
#'   `rut_id`-kolumn, eller en [postgis_tabell()]-referens till en
#'   motsvarande tabell i databasen. Rasterdata eller andra geometrityper
#'   (punkter, linjer) stöds inte.
#' @param fokusomrade Området pendlingen ska räknas mot - ett `sf`-objekt med
#'   polygon-/multipolygongeometri (t.ex. en tätortsgräns eller ett eget
#'   avgränsat område), eller en [postgis_tabell()]-referens. Rasterdata
#'   eller andra geometrityper stöds inte.
#' @param con En `DBIConnection`, eller `NA` (standard) för att inte ansluta
#'   till någon databas alls. Behövs bara om `rutor` och `fokusomrade` båda
#'   är `sf`-objekt och man ändå vill köra rutfiltreringen
#'   databasaccelererat - annars ignoreras den (om `rutor` eller
#'   `fokusomrade` redan är en [postgis_tabell()]-referens används dess
#'   egen anslutning i stället).
#' @param ut_mapp Målmapp för GeoPackage.
#' @param grid_epsg EPSG-kod (inte använd i nuläget, behålls för kompatibilitet).
#' @param skriv_till_gpkg Skriv resultatet till GeoPackage.
#' @param gpkg_namn Filnamn (`NA` = döps automatiskt).
#'
#' @return En lista med `utvalda_rutor`, `in_pendling` (med kolumnen
#'   `pendlare_fran`) och `ut_pendling` (med kolumnen `pendlare_till`).
#' @seealso [pendling_ruta_karta()] för att göra resultatet till en karta,
#'   [postgis_tabell()] för att referera till en tabell i databasen.
#' @export
pendling_ruta <- function(tabell_pend_relation_ruta, rutor, fokusomrade,
                          con = NA, ut_mapp = NA, grid_epsg = 3006,
                          skriv_till_gpkg = FALSE, gpkg_namn = NA) {
  intern_krav("sf")

  if (!all(c("boruta", "arbruta", "antalpend") %in% colnames(tabell_pend_relation_ruta))) {
    stop("`tabell_pend_relation_ruta` måste innehålla 'boruta', 'arbruta', 'antalpend'.", call. = FALSE)
  }
  intern_krav_polygongeometri(rutor, "rutor")
  intern_krav_polygongeometri(fokusomrade, "fokusomrade")
  if (is.na(ut_mapp)) ut_mapp <- intern_utskriftsmapp()

  fokus_sf <- intern_som_sf(fokusomrade, "fokusomrade")

  kalla <- intern_rutor_kalla(rutor, con)
  on.exit(kalla$stang(), add = TRUE)

  utvalda_rutor <- kalla$filtrera(fokus_sf)
  if (nrow(utvalda_rutor) == 0) stop("Ingen överlappning mellan fokusomrade och rutor.", call. = FALSE)
  ids <- utvalda_rutor$rut_id

  fran <- tabell_pend_relation_ruta[
    tabell_pend_relation_ruta$boruta %in% ids &
    !tabell_pend_relation_ruta$arbruta %in% ids, ]
  till <- tabell_pend_relation_ruta[
    tabell_pend_relation_ruta$arbruta %in% ids &
    !tabell_pend_relation_ruta$boruta %in% ids, ]

  from_c <- stats::aggregate(antalpend ~ boruta, data = till, FUN = sum)
  names(from_c) <- c("rut_id", "pendlare_fran")
  to_c <- stats::aggregate(antalpend ~ arbruta, data = fran, FUN = sum)
  names(to_c) <- c("rut_id", "pendlare_till")

  grannrutor <- kalla$id_geom(union(from_c$rut_id, to_c$rut_id))

  in_pendling <- merge(grannrutor, from_c, by = "rut_id")
  ut_pendling <- merge(grannrutor, to_c, by = "rut_id")

  if (skriv_till_gpkg) {
    filnamn <- if (!is.na(gpkg_namn)) gpkg_namn else "rut_pendling.gpkg"
    sokvag <- file.path(ut_mapp, filnamn)
    sf::st_write(utvalda_rutor, sokvag, "omrade", delete_dsn = TRUE, append = FALSE, quiet = TRUE)
    sf::st_write(in_pendling, sokvag, "in_pend", append = FALSE, quiet = TRUE)
    sf::st_write(ut_pendling, sokvag, "ut_pend", append = FALSE, quiet = TRUE)
  }

  list(utvalda_rutor = utvalda_rutor, in_pendling = in_pendling, ut_pendling = ut_pendling)
}

#' Karta (sf-objekt / GeoPackage) av resultatet från pendling_ruta()
#'
#' Slår ihop fokusområdets rutor (svartmarkerade) med antingen in- eller
#' utpendlingsrutorna från [pendling_ruta()]s resultat till ett enda
#' `sf`-objekt, klart att kartlägga eller skriva till en GeoPackage-fil.
#'
#' @param resultat Listan som returneras av [pendling_ruta()].
#' @param typ `"in"` eller `"ut"` - vilken pendling som ska visas
#'   tillsammans med fokusområdet.
#' @param farg_fokusomrade Färg för fokusområdets rutor (kolumnen `farg`,
#'   `NA` för pendlingsrutorna - dessa styrs lämpligare via kolumnen
#'   `antal_pendlare` med en färgskala när man kartlägger resultatet).
#' @param skriv_till_gpkg Skriv resultatet till en GeoPackage-fil.
#' @param ut_mapp Målmapp för GeoPackage.
#' @param gpkg_namn Filnamn för GeoPackage (`NA` = byggs automatiskt utifrån `typ`).
#'
#' @return Ett `sf`-objekt med kolumnerna `typ` (`"fokusomrade"`,
#'   `"in_pendling"` eller `"ut_pendling"`), `farg` och `antal_pendlare`
#'   (`NA` för fokusområdets rutor).
#' @seealso [pendling_ruta()]
#' @export
pendling_ruta_karta <- function(resultat, typ = c("in", "ut"),
                                farg_fokusomrade = "black",
                                skriv_till_gpkg = FALSE, ut_mapp = NA, gpkg_namn = NA) {
  intern_krav("sf")
  typ <- match.arg(typ)
  if (!all(c("utvalda_rutor", "in_pendling", "ut_pendling") %in% names(resultat))) {
    stop("`resultat` måste vara listan som pendling_ruta() returnerar.", call. = FALSE)
  }

  pendling_df <- if (typ == "in") resultat$in_pendling else resultat$ut_pendling
  pendlare_kol <- if (typ == "in") "pendlare_fran" else "pendlare_till"
  pendling_typ <- if (typ == "in") "in_pendling" else "ut_pendling"

  fokus <- resultat$utvalda_rutor
  fokus$typ <- "fokusomrade"
  fokus$farg <- farg_fokusomrade
  fokus$antal_pendlare <- NA_real_

  pend <- pendling_df
  pend$typ <- pendling_typ
  pend$farg <- NA_character_
  pend$antal_pendlare <- pend[[pendlare_kol]]

  # rutor och fokusomrade kan i teorin komma från källor med olika namn på geometrikolumnen
  # (t.ex. "geom" mot "geometry") - byt namn på pend:s geometrikolumn till fokus:s innan rbind.
  geom_kol <- attr(fokus, "sf_column")
  if (attr(pend, "sf_column") != geom_kol) names(pend)[names(pend) == attr(pend, "sf_column")] <- geom_kol
  sf::st_geometry(pend) <- geom_kol
  if (sf::st_crs(pend) != sf::st_crs(fokus)) pend <- sf::st_transform(pend, sf::st_crs(fokus))

  kolumner <- c("typ", "farg", "antal_pendlare", geom_kol)
  karta_sf <- rbind(fokus[, kolumner], pend[, kolumner])

  if (skriv_till_gpkg) {
    if (is.na(ut_mapp)) ut_mapp <- intern_utskriftsmapp()
    if (is.na(gpkg_namn)) gpkg_namn <- paste0("pendling_ruta_karta_", typ, ".gpkg")
    sf::st_write(karta_sf, file.path(ut_mapp, gpkg_namn), delete_dsn = TRUE, quiet = TRUE)
  }

  karta_sf
}
