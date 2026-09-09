# pgRouting: bygga grafer av vägnätet och köra ruttanalyser.
#
# Funktionerna körs mot databasen "ruttanalyser". SQL:n är bevarad ordagrant
# från func_GIS.R; det som ändrats är con-hanteringen (intern_rutt_con),
# borttagen tidtagningsboilerplate, och att metadata-loggningen genom tryCatch
# använder ett internt tillståndsobjekt i stället för <<- mot global miljö.

#' Installera pgRouting-tillägget i en PostGIS-databas
#' @param con En aktiv `DBIConnection`.
#' @return Osynligt `NULL`.
#' @export
pgrouting_installera_i_postgis_db <- function(con) {
  tryCatch({
    DBI::dbExecute(con, "CREATE EXTENSION IF NOT EXISTS pgrouting;")
    message("pgRouting-tillägget har installerats i databasen.")
  }, error = function(e) message("Kunde inte installera pgRouting-tillägget: ", conditionMessage(e)))
  invisible(NULL)
}

#' Standardhastigheter (km/h) för gång, cykel och elcykel
#' @return Ett tal.
#' @name pgrouting_hastighet
#' @export
pgrouting_hastighet_gang <- function() 5

#' @rdname pgrouting_hastighet
#' @export
pgrouting_hastighet_cykel <- function() 16

#' @rdname pgrouting_hastighet
#' @export
pgrouting_hastighet_elcykel <- function() 22

# Metadata-hämtning: version_datum/version_tid/kommentar för en tabell, eller NA.
intern_meta_rad <- function(con, schema, tabell) {
  m <- rdpostgres::postgres_meta(con = con,
    query = glue::glue("WHERE schema = '{schema}' AND tabell = '{tabell}'"))
  if (is.null(m) || nrow(m) == 0) {
    list(version_datum = NA, version_tid = NA, kommentar = NA_character_)
  } else {
    list(version_datum = m$version_datum[1], version_tid = m$version_tid[1],
         kommentar = m$kommentar[1])
  }
}

#' Klipp ett vägnät (NVDB) inom en buffrad regiongräns och skapa graf-tabell
#'
#' @param con En aktiv `DBIConnection` (skrivbehörighet).
#' @param con_till_databas `DBIConnection`, databasnamn, eller `NULL` (skriv till
#'   samma databas som `con`).
#' @param buffer_m Buffra regiongränsen så här många meter innan klippning.
#' @param natverk_schema,natverk_tabell,natverk_geokol Nätverkstabellen.
#' @param region_schema,region_tabell,regionkod_kol,regionkoder,region_geokol
#'   Regionlagret att klippa med.
#' @param output_schema,output_tabell Måltabellen.
#' @param urval_fran_natverk Valfri `WHERE`-sats för nätverket.
#'
#' @return Osynligt `NULL`.
#' @export
pgrouting_klipp_natverk_skapa_tabell <- function(con,
                                                 con_till_databas = "ruttanalyser",
                                                 buffer_m = 30000,
                                                 natverk_schema = "nvdb",
                                                 natverk_tabell = "dala_med_grannlan",
                                                 natverk_geokol = "geom",
                                                 region_schema = "karta",
                                                 region_tabell = "lan_lm",
                                                 regionkod_kol = "lankod",
                                                 regionkoder = "20",
                                                 region_geokol = "geom",
                                                 output_schema = "grafer",
                                                 output_tabell = "nvdb_alla",
                                                 urval_fran_natverk = "") {
  intern_krav("sf")
  if (is.null(con_till_databas)) {
    ct <- list(con = con, egen = FALSE)
  } else {
    ct <- intern_rutt_con(con_till_databas, adm = TRUE)
  }
  on.exit(intern_stang(ct), add = TRUE)
  con_till <- ct$con

  rdpostgres::postgres_finns_schema_tabell_kolumner(con, natverk_schema, natverk_tabell, natverk_geokol, stoppa_vid_fel = TRUE)
  rdpostgres::postgres_finns_schema_tabell_kolumner(con, region_schema, region_tabell, c(regionkod_kol, region_geokol), stoppa_vid_fel = TRUE)

  regionkoder_str <- paste0("'", paste(regionkoder, collapse = "', '"), "'")
  meta_nvdb <- intern_meta_rad(con, "nvdb", "dala_med_grannlan")
  st <- intern_meta_state()

  tryCatch({
    DBI::dbExecute(con, glue::glue(
      "CREATE INDEX IF NOT EXISTS {region_tabell}_geom_idx
       ON {region_schema}.{region_tabell} USING GIST({region_geokol});"))

    DBI::dbExecute(con_till, glue::glue("DROP TABLE IF EXISTS {output_schema}.{output_tabell};"))
    klippt <- DBI::dbGetQuery(con, glue::glue("
      WITH region_buffer AS (
        SELECT ST_Union(ST_Buffer({region_geokol}, {buffer_m})) AS {region_geokol}
        FROM {region_schema}.{region_tabell}
        WHERE {regionkod_kol} IN ({regionkoder_str})
      )
      SELECT l.* FROM {natverk_schema}.{natverk_tabell} l
      JOIN region_buffer d ON ST_Intersects(l.{natverk_geokol}, d.{region_geokol}){urval_fran_natverk};"))
    if (nrow(klippt) == 0) stop("Inga rader i det klippta nätverket. Kontrollera regionkoder och nätverkstabell.")

    sf::st_write(obj = klippt, dsn = con_till,
                 layer = DBI::Id(schema = output_schema, table = output_tabell),
                 append = FALSE, quiet = TRUE)
    message("Nätverket klippt.")

    DBI::dbExecute(con_till, glue::glue(
      "ALTER TABLE {output_schema}.{output_tabell} ALTER COLUMN geom
       TYPE geometry(LineString, 3006) USING ST_LineMerge(ST_Force2D({natverk_geokol}));"))
    idx <- glue::glue("{output_schema}_{output_tabell}_geom_idx")
    DBI::dbExecute(con_till, glue::glue("DROP INDEX IF EXISTS {idx};"))
    DBI::dbExecute(con_till, glue::glue("CREATE INDEX {idx} ON {output_schema}.{output_tabell} USING GIST({natverk_geokol});"))
    DBI::dbExecute(con_till, glue::glue("ANALYZE {output_schema}.{output_tabell};"))

    st$kommentar <- glue::glue("nvdb ver: {meta_nvdb$kommentar}")
    st$lyckad <- TRUE
  }, error = function(e) {
    st$kommentar <- conditionMessage(e)
    st$lyckad <- FALSE
    message("Fel: ", st$kommentar)
  }, finally = {
    rdpostgres::postgres_metadata_uppdatera(
      con = con_till, schema = output_schema, tabell = output_tabell,
      version_datum = meta_nvdb$version_datum, version_tid = meta_nvdb$version_tid,
      lyckad_uppdatering = st$lyckad, kommentar = st$kommentar)
  })
  message("Resultatet har sparats i ", output_schema, ".", output_tabell, ".")
  invisible(NULL)
}

#' Skapa ett nätverk med extra noder vid punkternas närmaste läge på nätverket
#'
#' Steg 2 i graf-uppbyggnaden: hitta närmaste punkt på nätverket för varje
#' adress, klustra dem, och dela nätverkssegmenten vid klusterpunkterna.
#'
#' @param con En `DBIConnection` eller databasnamn (`"ruttanalyser"`).
#' @param schema_punkter_fran,tabell_punkter_fran,geometri_kol_punkter_fran,id_kol_punkter_fran
#'   Punkttabellen.
#' @param schema_graf,tabell_graf,geometri_graf,id_graf Grafen.
#' @param tolerans_avstand Klustertolerans i meter.
#'
#' @return Osynligt `NULL`.
#' @export
pgrouting_hitta_narmaste_punkt_pa_natverk <- function(con = "ruttanalyser",
                                                      schema_punkter_fran = "punktlager",
                                                      tabell_punkter_fran = "adresser",
                                                      geometri_kol_punkter_fran = "geom",
                                                      id_kol_punkter_fran = "gml_id",
                                                      schema_graf = "grafer",
                                                      tabell_graf = "nvdb_alla",
                                                      geometri_graf = "geom",
                                                      id_graf = "rad_id",
                                                      tolerans_avstand = 3) {
  cc <- intern_rutt_con(con, adm = TRUE)
  on.exit(intern_stang(cc), add = TRUE)
  con <- cc$con
  tab_ny <- glue::glue("{tabell_graf}_{tabell_punkter_fran}")
  meta_graf <- intern_meta_rad(con, schema_graf, tabell_graf)
  st <- intern_meta_state()

  DBI::dbExecute(con, "BEGIN;")
  tryCatch({
    DBI::dbExecute(con, glue::glue("DROP TABLE IF EXISTS {schema_graf}.{tab_ny}_narmaste_punkt;"))
    DBI::dbExecute(con, glue::glue("
      CREATE TABLE {schema_graf}.{tab_ny}_narmaste_punkt AS
      WITH closest_points AS (
        SELECT f.{id_kol_punkter_fran} AS adress_id, g.{id_graf} AS graf_id,
          ST_ClosestPoint(g.{geometri_graf}, f.{geometri_kol_punkter_fran}) AS punkt_pa_natverk,
          ST_DWithin(ST_ClosestPoint(g.{geometri_graf}, f.{geometri_kol_punkter_fran}), ST_StartPoint(g.{geometri_graf}), {tolerans_avstand}) AS is_near_start_point,
          ST_DWithin(ST_ClosestPoint(g.{geometri_graf}, f.{geometri_kol_punkter_fran}), ST_EndPoint(g.{geometri_graf}), {tolerans_avstand}) AS is_near_end_point
        FROM {schema_punkter_fran}.{tabell_punkter_fran} f,
        LATERAL (
          SELECT {id_graf}, {geometri_graf} FROM {schema_graf}.{tabell_graf}
          ORDER BY f.{geometri_kol_punkter_fran} <-> {geometri_graf} LIMIT 1
        ) g
      )
      SELECT adress_id, graf_id, punkt_pa_natverk FROM closest_points
      WHERE NOT is_near_start_point AND NOT is_near_end_point;"))
    if (DBI::dbGetQuery(con, glue::glue("SELECT COUNT(*) c FROM {schema_graf}.{tab_ny}_narmaste_punkt"))$c == 0) {
      stop("Inga giltiga punkter hittades på nätverket.")
    }

    DBI::dbExecute(con, glue::glue("DROP TABLE IF EXISTS {schema_graf}.{tab_ny}_klusterpunkt;"))
    DBI::dbExecute(con, glue::glue("
      CREATE TABLE {schema_graf}.{tab_ny}_klusterpunkt AS
      WITH clusters AS (
        SELECT ST_ClusterDBSCAN(punkt_pa_natverk, eps := {tolerans_avstand}, minpoints := 1) OVER(PARTITION BY graf_id) AS cid,
               punkt_pa_natverk, graf_id
        FROM {schema_graf}.{tab_ny}_narmaste_punkt
      ), cluster_centroids AS (
        SELECT cid, graf_id, ST_Centroid(ST_Collect(punkt_pa_natverk)) AS centroid
        FROM clusters GROUP BY cid, graf_id
      ), representant_punkter AS (
        SELECT DISTINCT ON (c.cid, c.graf_id)
          c.cid, c.graf_id, c.punkt_pa_natverk AS representant_punkt,
          ST_LineLocatePoint(n.{geometri_graf}, c.punkt_pa_natverk) AS line_location
        FROM clusters c
        JOIN {schema_graf}.{tabell_graf} n ON c.graf_id = n.{id_graf}
        JOIN cluster_centroids cc ON c.cid = cc.cid AND c.graf_id = cc.graf_id
        ORDER BY c.cid, c.graf_id, ST_Distance(c.punkt_pa_natverk, cc.centroid)
      )
      SELECT cid, graf_id, representant_punkt, line_location
      FROM representant_punkter ORDER BY graf_id, line_location;"))

    DBI::dbExecute(con, glue::glue("DROP TABLE IF EXISTS {schema_graf}.{tab_ny};"))
    cols <- DBI::dbGetQuery(con, glue::glue("
      SELECT column_name FROM information_schema.columns
      WHERE table_schema = '{schema_graf}' AND table_name = '{tabell_graf}'
        AND column_name NOT IN ('{geometri_graf}', 'shape_length', 'rad_id', 'from_measure', 'to_measure');"))$column_name
    col_string <- paste0('n."', cols, '"', collapse = ", ")

    DBI::dbExecute(con, glue::glue("
      CREATE TABLE {schema_graf}.{tab_ny} AS
      WITH locus AS (
        SELECT {id_graf} AS gid, 0 AS l FROM {schema_graf}.{tabell_graf}
        UNION ALL SELECT {id_graf} AS gid, 1 AS l FROM {schema_graf}.{tabell_graf}
        UNION ALL SELECT graf_id AS gid, line_location AS l FROM {schema_graf}.{tab_ny}_klusterpunkt
      ),
      loc_with_idx AS (
        SELECT gid, l, RANK() OVER (PARTITION BY gid ORDER BY l) AS idx FROM locus
      ),
      segment AS (
        SELECT ROW_NUMBER() OVER (ORDER BY loc1.gid, loc1.idx) AS {id_graf},
          {col_string},
          ST_LineSubstring(n.{geometri_graf}, loc1.l, loc2.l) AS {geometri_graf},
          ST_Length(ST_LineSubstring(n.{geometri_graf}, loc1.l, loc2.l)) AS kostnad_meter
        FROM loc_with_idx loc1
        JOIN loc_with_idx loc2 USING (gid)
        JOIN {schema_graf}.{tabell_graf} n ON loc1.gid = n.{id_graf}
        WHERE loc2.idx = loc1.idx + 1
      )
      SELECT * FROM segment;"))
    idx <- glue::glue("{schema_graf}_{tab_ny}_geom_idx")
    DBI::dbExecute(con, glue::glue("DROP INDEX IF EXISTS {idx};"))
    DBI::dbExecute(con, glue::glue("CREATE INDEX {idx} ON {schema_graf}.{tab_ny} USING GIST({geometri_graf});"))
    DBI::dbExecute(con, glue::glue("ANALYZE {schema_graf}.{tab_ny};"))

    DBI::dbExecute(con, "COMMIT;")
    st$kommentar <- meta_graf$kommentar
    st$lyckad <- TRUE
    message("Nytt nätverk ", tab_ny, " skapat.")
  }, error = function(e) {
    DBI::dbExecute(con, "ROLLBACK;")
    st$kommentar <- conditionMessage(e)
    st$lyckad <- FALSE
    message("Fel i processen: ", st$kommentar)
  }, finally = {
    for (tb in c(tab_ny, paste0(tab_ny, "_narmaste_punkt"), paste0(tab_ny, "_klusterpunkt"))) {
      rdpostgres::postgres_metadata_uppdatera(
        con = con, schema = schema_graf, tabell = tb,
        version_datum = meta_graf$version_datum, version_tid = meta_graf$version_tid,
        lyckad_uppdatering = st$lyckad, kommentar = st$kommentar)
    }
  })
  invisible(NULL)
}

#' Bygg en pgRouting-topologi (source/target) på en linjetabell
#'
#' @param con En `DBIConnection` eller databasnamn.
#' @param schema_graf,tabell_graf,id_kol_graf,geom_kol_graf Tabellen som blir graf.
#' @param tolerans Anslutningstolerans för `pgr_createTopology()`.
#'
#' @return Osynligt `NULL`.
#' @export
pgrouting_tabell_till_pgrgraf <- function(con = "ruttanalyser",
                                          schema_graf = "grafer",
                                          tabell_graf = "nvdb_alla_adresser",
                                          id_kol_graf = "rad_id",
                                          geom_kol_graf = "geom",
                                          tolerans = 0.001) {
  cc <- intern_rutt_con(con, adm = TRUE)
  on.exit(intern_stang(cc), add = TRUE)
  con <- cc$con
  nu <- Sys.time()
  meta_graf <- intern_meta_rad(con, schema_graf, tabell_graf)
  st <- intern_meta_state()

  DBI::dbExecute(con, "BEGIN;")
  tryCatch({
    finns_vpgr <- nrow(DBI::dbGetQuery(con, glue::glue(
      "SELECT table_name FROM information_schema.tables
       WHERE table_schema = '{schema_graf}' AND table_name = '{tabell_graf}_vertices_pgr';"))) == 1
    if (finns_vpgr) DBI::dbExecute(con, glue::glue("DROP TABLE {schema_graf}.{tabell_graf}_vertices_pgr;"))

    DBI::dbExecute(con, glue::glue("ALTER TABLE {schema_graf}.{tabell_graf} ALTER COLUMN {id_kol_graf} TYPE integer USING {id_kol_graf}::integer;"))
    DBI::dbExecute(con, glue::glue("ALTER TABLE {schema_graf}.{tabell_graf} ADD COLUMN IF NOT EXISTS source integer;"))
    DBI::dbExecute(con, glue::glue("ALTER TABLE {schema_graf}.{tabell_graf} ADD COLUMN IF NOT EXISTS target integer;"))
    DBI::dbExecute(con, glue::glue("SELECT pgr_createTopology('{schema_graf}.{tabell_graf}', '{tolerans}', the_geom := '{geom_kol_graf}', id := '{id_kol_graf}');"))
    DBI::dbExecute(con, glue::glue("SELECT pgr_analyzeGraph('{schema_graf}.{tabell_graf}', {tolerans}, the_geom := '{geom_kol_graf}', id := '{id_kol_graf}');"))
    DBI::dbExecute(con, "COMMIT;")

    st$kommentar <- glue::glue("pgr_graf ver: {intern_ver_stampel(nu)}, {meta_graf$kommentar}")
    st$lyckad <- TRUE
    message("Topologi skapad för ", schema_graf, ".", tabell_graf, ".")
  }, error = function(e) {
    DBI::dbExecute(con, "ROLLBACK;")
    st$kommentar <- conditionMessage(e)
    st$lyckad <- FALSE
    message("Transaktionen misslyckades: ", st$kommentar)
  }, finally = {
    rdpostgres::postgres_metadata_uppdatera(
      con = con, schema = schema_graf, tabell = tabell_graf,
      version_datum = as.Date(nu), version_tid = format(nu, "%H:%M"),
      lyckad_uppdatering = st$lyckad, kommentar = st$kommentar)
  })
  invisible(NULL)
}

#' Koppla en punkttabell till en pgRouting-graf (närmaste nod-id)
#'
#' Lägger till kolumnen `nid_<tabell_natverk>` med id:t på närmaste graf-nod.
#'
#' @param con En `DBIConnection` eller databasnamn.
#' @param schema_punkter,tabell_punkter,geom_kol_punkter,id_kol_punkter Punkttabellen.
#' @param schema_natverk,tabell_natverk Grafen (`<tabell>_vertices_pgr` måste finnas).
#' @param generella_namn_prova Prova `"id"` resp. `"geom"`/`"geometry"` om de
#'   angivna kolumnnamnen inte finns.
#'
#' @return Osynligt `NULL`.
#' @export
pgrouting_punkttabell_koppla_till_pgr_graf <- function(con = "ruttanalyser",
                                                       schema_punkter = "punktlager",
                                                       tabell_punkter = "adresser",
                                                       geom_kol_punkter = "geom",
                                                       id_kol_punkter = "gml_id",
                                                       schema_natverk = "grafer",
                                                       tabell_natverk = "nvdb_alla_adresser",
                                                       generella_namn_prova = TRUE) {
  cc <- intern_rutt_con(con, adm = TRUE)
  on.exit(intern_stang(cc), add = TRUE)
  con <- cc$con

  har_kol <- function(k) rdpostgres::postgres_finns_schema_tabell_kolumner(
    con = con, schema = schema_punkter, tabell = tabell_punkter, kolumner = k,
    stoppa_vid_fel = FALSE)$allt_finns

  id_kol_finns <- har_kol(id_kol_punkter)
  geom_kol_finns <- har_kol(geom_kol_punkter)
  if (generella_namn_prova) {
    if (!id_kol_finns && har_kol("id")) { id_kol_punkter <- "id"; id_kol_finns <- TRUE }
    if (!geom_kol_finns && har_kol("geometry")) { geom_kol_punkter <- "geometry"; geom_kol_finns <- TRUE }
    if (!geom_kol_finns && har_kol("geom")) { geom_kol_punkter <- "geom"; geom_kol_finns <- TRUE }
  }
  if (!id_kol_finns)   stop("Angiven id-kolumn saknas i ", schema_punkter, ".", tabell_punkter, ".", call. = FALSE)
  if (!geom_kol_finns) stop("Angiven geometri-kolumn saknas i ", schema_punkter, ".", tabell_punkter, ".", call. = FALSE)

  meta_p <- intern_meta_rad(con, schema_punkter, tabell_punkter)
  meta_g <- intern_meta_rad(con, schema_natverk, tabell_natverk)
  st <- intern_meta_state()

  DBI::dbExecute(con, "BEGIN;")
  tryCatch({
    DBI::dbExecute(con, glue::glue("ALTER TABLE {schema_punkter}.{tabell_punkter} ADD COLUMN IF NOT EXISTS nid_{tabell_natverk} bigint;"))
    DBI::dbExecute(con, glue::glue("UPDATE {schema_punkter}.{tabell_punkter} SET nid_{tabell_natverk} = NULL;"))
    DBI::dbExecute(con, glue::glue("
      UPDATE {schema_punkter}.{tabell_punkter} AS f
      SET nid_{tabell_natverk} = n.id
      FROM (
        SELECT f2.{id_kol_punkter} AS punkt_id, v.id
        FROM {schema_punkter}.{tabell_punkter} AS f2
        JOIN LATERAL (
          SELECT id FROM {schema_natverk}.{tabell_natverk}_vertices_pgr AS v
          ORDER BY f2.{geom_kol_punkter} <-> v.the_geom LIMIT 1
        ) AS v ON TRUE
      ) AS n
      WHERE f.{id_kol_punkter} = n.punkt_id;"))
    DBI::dbExecute(con, "COMMIT;")

    delar <- unique(unlist(strsplit(paste0(meta_p$kommentar, ", ", meta_g$kommentar), ",\\s*")))
    st$kommentar <- paste(delar, collapse = ", ")
    st$lyckad <- TRUE
    message("Punkterna i ", schema_punkter, ".", tabell_punkter, " kopplade till grafen ",
            schema_natverk, ".", tabell_natverk, ".")
  }, error = function(e) {
    DBI::dbExecute(con, "ROLLBACK;")
    st$kommentar <- conditionMessage(e)
    st$lyckad <- FALSE
    message("Transaktionen misslyckades: ", st$kommentar)
  }, finally = {
    rdpostgres::postgres_metadata_uppdatera(
      con = con, schema = schema_punkter, tabell = tabell_punkter,
      version_datum = meta_p$version_datum, version_tid = meta_p$version_tid,
      lyckad_uppdatering = st$lyckad, kommentar = st$kommentar)
  })
  invisible(NULL)
}

#' Beräkna kostnadskolumner per transportsätt i en graf-tabell
#'
#' Lägger till `kostnad_gang_min`, `kostnad_cykel_min`, `kostnad_elcykel_min`,
#' `kostnad_bil_f_min`, `kostnad_bil_b_min` utifrån `kostnad_meter` och
#' hastighetsgränser.
#'
#' @param con En `DBIConnection` eller databasnamn.
#' @param schema_natverk,tabell_natverk Graf-tabellen.
#' @param kostnadskolumn_bil_f,kostnadskolumn_bil_b Hastighetskolumner (km/h).
#' @param kostnadskolumn_meter Kolumn med längd i meter.
#' @param berakna_kostnad_bil,berakna_kostnad_gang,berakna_kostnad_cykel,berakna_kostnad_elcykel
#'   Slå på/av respektive beräkning.
#'
#' @return Osynligt `NULL`.
#' @export
pgrouting_kostnadskolumner_transporttyp_graf <- function(con = "ruttanalyser",
                                                         schema_natverk = "grafer",
                                                         tabell_natverk = "nvdb_alla_adresser",
                                                         kostnadskolumn_bil_f = "hastighetsgrans_f",
                                                         kostnadskolumn_bil_b = "hastighetsgrans_b",
                                                         kostnadskolumn_meter = "kostnad_meter",
                                                         berakna_kostnad_bil = TRUE,
                                                         berakna_kostnad_gang = TRUE,
                                                         berakna_kostnad_cykel = TRUE,
                                                         berakna_kostnad_elcykel = TRUE) {
  cc <- intern_rutt_con(con, adm = TRUE)
  on.exit(intern_stang(cc), add = TRUE)
  con <- cc$con

  DBI::dbExecute(con, "BEGIN;")
  tryCatch({
    kol <- character(); ber <- character()
    lagg <- function(namn, uttryck) { kol[[length(kol) + 1]] <<- paste0("ADD COLUMN IF NOT EXISTS ", namn, " double precision"); ber[[length(ber) + 1]] <<- uttryck }

    if (berakna_kostnad_gang && !is.null(kostnadskolumn_meter))
      lagg("kostnad_gang_min", glue::glue("kostnad_gang_min = {kostnadskolumn_meter} / ({pgrouting_hastighet_gang()} / 3.6) / 60"))
    if (berakna_kostnad_cykel && !is.null(kostnadskolumn_meter))
      lagg("kostnad_cykel_min", glue::glue("kostnad_cykel_min = {kostnadskolumn_meter} / ({pgrouting_hastighet_cykel()} / 3.6) / 60"))
    if (berakna_kostnad_elcykel && !is.null(kostnadskolumn_meter))
      lagg("kostnad_elcykel_min", glue::glue("kostnad_elcykel_min = {kostnadskolumn_meter} / ({pgrouting_hastighet_elcykel()} / 3.6) / 60"))
    if (berakna_kostnad_bil && !is.null(kostnadskolumn_bil_f))
      lagg("kostnad_bil_f_min", glue::glue("kostnad_bil_f_min = {kostnadskolumn_meter} / (NULLIF({kostnadskolumn_bil_f}, 0) / 3.6) / 60"))
    if (berakna_kostnad_bil && !is.null(kostnadskolumn_bil_b))
      lagg("kostnad_bil_b_min", glue::glue("kostnad_bil_b_min = {kostnadskolumn_meter} / (NULLIF({kostnadskolumn_bil_b}, 0) / 3.6) / 60"))

    if (length(kol) > 0) {
      DBI::dbExecute(con, glue::glue("ALTER TABLE {schema_natverk}.{tabell_natverk}\n{paste(kol, collapse = ',\n')};"))
    }
    if (length(ber) > 0) {
      DBI::dbExecute(con, glue::glue("UPDATE {schema_natverk}.{tabell_natverk}\nSET {paste(ber, collapse = ',\n')}"))
    }
    DBI::dbExecute(con, "COMMIT;")
    message("Kostnadskolumner i ", schema_natverk, ".", tabell_natverk, " har beräknats.")
  }, error = function(e) {
    DBI::dbExecute(con, "ROLLBACK;")
    message("Transaktionen misslyckades: ", conditionMessage(e))
  })
  invisible(NULL)
}
