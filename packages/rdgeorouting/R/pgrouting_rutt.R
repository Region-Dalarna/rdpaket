# Ruttberäkning från alla från-punkter till närmaste till-punkt, samt
# orkestreringsfunktionen som bygger en helt ny graf vid ny NVDB-version.

#' Beräkna kortaste rutt från varje från-punkt till närmaste till-punkt
#'
#' Kör `pgr_dijkstraNear()` i batchar och bygger en geotabell med rutterna som
#' linjer plus en punkttabell (`_punkt`) med kostnaderna per från-punkt.
#'
#' @param con En `DBIConnection` eller databasnamn (`"ruttanalyser"`).
#' @param schema_fran,tabell_fran Från-punkterna (måste ha `nid_<tabell_graf>`).
#' @param schema_till,tabell_till,tabell_till_namnkol,tabell_till_idkol Till-punkterna.
#' @param schema_graf,tabell_graf Grafen.
#' @param schema_output Schema för resultattabellerna.
#' @param urval_till_tabell,urval_till_namn Valfritt `WHERE` på till-tabellen
#'   samt en namntagg för resultattabellen.
#' @param hastighet_gang,hastighet_cykel,hastighet_elcykel km/h (`NULL` =
#'   standardvärden).
#' @param batch_storlek Antal från-noder per batch.
#' @param antal_batcher_test `NULL` skarpt; heltal för att begränsa i test.
#'
#' @return Osynligt `NULL`.
#' @export
pgrouting_skapa_geotabell_rutt_fran_till <- function(con = "ruttanalyser",
                                                     schema_fran = "punktlager",
                                                     tabell_fran = "adresser",
                                                     schema_till, tabell_till,
                                                     schema_graf = "grafer",
                                                     tabell_graf = "nvdb_alla_adresser",
                                                     schema_output = "resultat",
                                                     tabell_till_namnkol = "mottagnings_namn",
                                                     tabell_till_idkol = "gml_id",
                                                     urval_till_tabell = NULL,
                                                     urval_till_namn = "",
                                                     hastighet_gang = NULL,
                                                     hastighet_cykel = NULL,
                                                     hastighet_elcykel = NULL,
                                                     batch_storlek = 1000,
                                                     antal_batcher_test = NULL) {
  cc <- intern_rutt_con(con, adm = TRUE)
  on.exit(intern_stang(cc), add = TRUE)
  con <- cc$con
  nu <- Sys.time()

  hg <- hastighet_gang %||% pgrouting_hastighet_gang()
  hc <- hastighet_cykel %||% pgrouting_hastighet_cykel()
  he <- hastighet_elcykel %||% pgrouting_hastighet_elcykel()

  st <- intern_meta_state()

  tryCatch({
    natverkstyp <- stringr::str_extract(tabell_graf, "alla|bil")
    if (length(urval_till_tabell) == 0) {
      tabell_ny <- glue::glue("{tabell_fran}_till_{tabell_till}_{natverkstyp}")
      urval_till_tabell <- ""
    } else {
      tabell_ny <- glue::glue("{tabell_fran}_till_{tabell_till}_{urval_till_namn}_{natverkstyp}")
    }

    frantabell_kolumner <- DBI::dbGetQuery(con, glue::glue("
      SELECT column_name, data_type FROM information_schema.columns
      WHERE table_schema = '{schema_fran}' AND table_name = '{tabell_fran}'
      ORDER BY ordinal_position;"))
    frantabell_kolumner <- frantabell_kolumner[
      !frantabell_kolumner$column_name %in% c("geom", "geometry") &
      !grepl("nid_", frantabell_kolumner$column_name), ]
    frantabell_kolumner$data_type <- ifelse(frantabell_kolumner$data_type == "USER-DEFINED", "geometry",
                                            ifelse(frantabell_kolumner$data_type == "text", "varchar",
                                                   frantabell_kolumner$data_type))
    frantabell_kolumner$definition <- paste(frantabell_kolumner$column_name, frantabell_kolumner$data_type)

    malpunkt_rutt_def <- c(
      "malpunkt_id varchar", "malpunkt_namn varchar", "start_vid int", "end_vid int",
      "kostnad_meter double precision", "kostnad_gang_min double precision",
      "kostnad_cykel_min double precision", "kostnad_elcykel_min double precision",
      "kostnad_bil_min double precision", "geom geometry")
    malpunkt_rutt_namn <- sub(" .*$", "", malpunkt_rutt_def)

    finns <- nrow(DBI::dbGetQuery(con, glue::glue(
      "SELECT table_name FROM information_schema.tables WHERE table_schema = '{schema_output}' AND table_name = '{tabell_ny}';"))) > 0
    if (!finns) {
      DBI::dbExecute(con, glue::glue("CREATE TABLE IF NOT EXISTS {schema_output}.{tabell_ny} (
        {paste(frantabell_kolumner$definition, collapse = ',\n')},
        {paste(malpunkt_rutt_def, collapse = ',\n')});"))
      message("Tabellen ", tabell_ny, " har skapats.")
    } else {
      DBI::dbExecute(con, glue::glue("TRUNCATE TABLE {schema_output}.{tabell_ny};"))
      message("Tabellen ", tabell_ny, " fanns redan och har tömts.")
    }

    temp_fran <- glue::glue("temp_{tabell_fran}"); temp_till <- glue::glue("temp_{tabell_till}")
    DBI::dbExecute(con, glue::glue("DROP TABLE IF EXISTS {temp_fran};"))
    DBI::dbExecute(con, glue::glue("DROP TABLE IF EXISTS {temp_till};"))
    DBI::dbExecute(con, glue::glue("CREATE TEMP TABLE {temp_fran} AS SELECT DISTINCT nid_{tabell_graf} FROM {schema_fran}.{tabell_fran};"))
    DBI::dbExecute(con, glue::glue("CREATE TEMP TABLE {temp_till} AS SELECT DISTINCT nid_{tabell_graf} FROM {schema_till}.{tabell_till};"))
    DBI::dbExecute(con, glue::glue("CREATE INDEX temp_fran_nidx ON {temp_fran}(nid_{tabell_graf});"))
    DBI::dbExecute(con, glue::glue("CREATE INDEX temp_till_nidx ON {temp_till}(nid_{tabell_graf});"))

    antal_noder <- DBI::dbGetQuery(con, glue::glue("SELECT COUNT(*) a FROM {temp_fran}"))$a
    antal_batcher <- ceiling(antal_noder / batch_storlek)
    if (!is.null(antal_batcher_test)) antal_batcher <- antal_batcher_test

    if (identical(natverkstyp, "bil")) {
      kostnad_berakning <- "CASE WHEN hastighetsgrans_f IS NOT NULL THEN kostnad_meter / (hastighetsgrans_f / 3.6) ELSE -1 END AS cost,
        CASE WHEN hastighetsgrans_b IS NOT NULL THEN kostnad_meter / (hastighetsgrans_b / 3.6) ELSE -1 END AS reverse_cost"
      kostnad_as <- " AS segment_tid_bil"; kostnad_sum <- "SUM(segment_tid_bil) AS kostnad_tid_bil,"
      vagnat_directed <- "true"
    } else {
      kostnad_berakning <- "kostnad_meter AS cost"
      kostnad_as <- ""; kostnad_sum <- "NULL AS kostnad_tid_bil,"
      vagnat_directed <- "false"
    }

    message("Letar kortaste vägen från ", antal_noder, " noder i ", antal_batcher, " batcher.")
    purrr::walk(seq_len(antal_batcher), function(i) {
      offset <- (i - 1) * batch_storlek
      sql <- glue::glue("
        INSERT INTO {schema_output}.{tabell_ny} (
          {paste(frantabell_kolumner$column_name, collapse = ',\n')},
          {paste(malpunkt_rutt_namn, collapse = ',\n')}
        )
        WITH dijkstra_raw AS (
          SELECT * FROM pgr_dijkstraNear(
            'SELECT rad_id AS id, source, target, {kostnad_berakning}
             FROM {schema_graf}.{tabell_graf} WHERE kostnad_meter IS NOT NULL',
            ARRAY(SELECT DISTINCT nid_{tabell_graf} FROM {schema_fran}.{tabell_fran} LIMIT {batch_storlek} OFFSET {offset})::INT[],
            ARRAY(SELECT DISTINCT nid_{tabell_graf} FROM {schema_till}.{tabell_till} {urval_till_tabell})::INT[],
            directed := {vagnat_directed}, cap := 1, global := false
          )
        ),
        rutter AS (
          SELECT d.path_seq, d.start_vid, d.end_vid, d.edge, v.kostnad_meter, d.cost{kostnad_as}, v.geom
          FROM dijkstra_raw d JOIN {schema_graf}.{tabell_graf} v ON d.edge = v.rad_id
        ),
        summerad_rutt AS (
          SELECT start_vid, end_vid, SUM(kostnad_meter) AS kostnad_meter, {kostnad_sum}
                 ST_LineMerge(ST_Union(ARRAY_AGG(geom ORDER BY path_seq))) AS rutt_geom
          FROM rutter WHERE edge != -1 GROUP BY start_vid, end_vid
        )
        SELECT
          {paste0('fran.', frantabell_kolumner$column_name, collapse = ',\n')},
          till.{tabell_till_idkol} AS malpunkt_id,
          till.{tabell_till_namnkol} AS malpunkt_namn,
          r.start_vid, r.end_vid, r.kostnad_meter,
          r.kostnad_meter / ({hg} / 3.6) / 60 AS kostnad_gang_min,
          r.kostnad_meter / ({hc} / 3.6) / 60 AS kostnad_cykel_min,
          r.kostnad_meter / ({he} / 3.6) / 60 AS kostnad_elcykel_min,
          CAST(r.kostnad_tid_bil AS double precision) / 60 AS kostnad_bil_min,
          r.rutt_geom AS geom
        FROM summerad_rutt r
        JOIN {schema_fran}.{tabell_fran} fran ON r.start_vid = fran.nid_{tabell_graf}
        JOIN {schema_till}.{tabell_till} till ON r.end_vid = till.nid_{tabell_graf}
        ORDER BY r.start_vid;")
      tryCatch(DBI::dbExecute(con, sql),
               error = function(e) message("Fel i batch ", i, ": ", conditionMessage(e)))
    }, .progress = TRUE)

    # noder där start = slut -> kostnad 0
    DBI::dbExecute(con, glue::glue("
      INSERT INTO {schema_output}.{tabell_ny} (start_vid, end_vid, kostnad_meter, kostnad_gang_min, kostnad_cykel_min, kostnad_elcykel_min, kostnad_bil_min)
      SELECT t.nid_{tabell_graf}, tt.nid_{tabell_graf}, 0, 0, 0, 0, 0
      FROM {temp_till} tt INNER JOIN {temp_fran} t ON tt.nid_{tabell_graf} = t.nid_{tabell_graf}
      WHERE NOT EXISTS (SELECT 1 FROM {schema_output}.{tabell_ny} ny WHERE ny.start_vid = t.nid_{tabell_graf});"))

    DBI::dbExecute(con, glue::glue("DROP TABLE IF EXISTS {schema_output}.{tabell_ny}_punkt;"))
    DBI::dbExecute(con, glue::glue("
      CREATE TABLE {schema_output}.{tabell_ny}_punkt AS
      SELECT a.*, r.malpunkt_id, r.malpunkt_namn, r.kostnad_meter,
             r.kostnad_gang_min, r.kostnad_cykel_min, r.kostnad_elcykel_min, r.kostnad_bil_min
      FROM {schema_fran}.{tabell_fran} a
      LEFT JOIN (SELECT DISTINCT ON (start_vid) * FROM {schema_output}.{tabell_ny}) r
        ON a.nid_{tabell_graf} = r.start_vid;"))
    DBI::dbExecute(con, glue::glue("CREATE INDEX ON {schema_output}.{tabell_ny}_punkt USING GIST (geom);"))
    DBI::dbExecute(con, glue::glue("DROP TABLE IF EXISTS {temp_fran};"))
    DBI::dbExecute(con, glue::glue("DROP TABLE IF EXISTS {temp_till};"))

    antal_rader <- DBI::dbGetQuery(con, glue::glue("SELECT COUNT(*) a FROM {schema_output}.{tabell_ny}"))$a
    message("Tabellen ", schema_output, ".", tabell_ny, " fylld med ", antal_rader, " rader (", antal_noder, " unika noder).")

    meta_g <- intern_meta_rad(con, schema_graf, tabell_graf)
    st$kommentar <- glue::glue("pgr_graf ver: {intern_ver_stampel(nu)}, {meta_g$kommentar}")
    st$lyckad <- TRUE
  }, error = function(e) {
    st$kommentar <- conditionMessage(e)
    st$lyckad <- FALSE
    message("Fel: ", st$kommentar)
  }, finally = {
    rdpostgres::postgres_metadata_uppdatera(
      con = con, schema = schema_graf, tabell = tabell_graf,
      version_datum = as.Date(nu), version_tid = format(nu, "%H:%M"),
      lyckad_uppdatering = st$lyckad, kommentar = st$kommentar)
  })
  invisible(NULL)
}

#' Kopiera en punkttabell mellan databaser och koppla den till alla grafer
#'
#' @param con_fran_databas,con_till_databas `DBIConnection`-objekt eller
#'   databasnamn (standard: geodata resp. ruttanalyser).
#' @param schema_fran,tabell_fran Källa.
#' @param schema_till,tabell_till Mål.
#' @param geom_kol_punkter,id_kol_punkter Kolumner i punkttabellen.
#' @param schema_natverk Schema där graferna ligger.
#'
#' @return Osynligt `NULL`.
#' @export
postgis_kopiera_punkttabell_koppla_till_pgr_graf <- function(con_fran_databas = "geodata",
                                                             con_till_databas = "ruttanalyser",
                                                             schema_fran, tabell_fran,
                                                             schema_till, tabell_till,
                                                             geom_kol_punkter = "geom",
                                                             id_kol_punkter = "id",
                                                             schema_natverk = "grafer") {
  cf <- intern_rutt_con(con_fran_databas, adm = FALSE, standard_db = "geodata")
  ct <- intern_rutt_con(con_till_databas, adm = TRUE, standard_db = "ruttanalyser")
  on.exit({ intern_stang(cf); intern_stang(ct) }, add = TRUE)

  postgis_kopiera_tabell_mellan_databaser(
    con_fran_databas = cf$con, con_till_databas = ct$con,
    schema_fran = schema_fran, tabell_fran = tabell_fran,
    schema_till = schema_till, tabell_till = tabell_till)

  grafer <- rdpostgres::postgres_lista_scheman_tabeller(con = ct$con)[[schema_natverk]]
  grafer <- grafer[!grepl("vertices_pgr", grafer)]

  purrr::walk(grafer, function(g) {
    pgrouting_punkttabell_koppla_till_pgr_graf(
      con = ct$con, schema_punkter = schema_till, tabell_punkter = tabell_till,
      geom_kol_punkter = geom_kol_punkter, id_kol_punkter = id_kol_punkter,
      schema_natverk = schema_natverk, tabell_natverk = g)
  })
  invisible(NULL)
}

#' Bygg om hela adressgrafen vid ny NVDB-version
#'
#' Orkestrerar hela kedjan: klipp nätverket, kopiera adresser till
#' ruttanalyser, hitta närmaste nod, bygg graf, beräkna kostnader, flytta
#' underlagstabeller och koppla alla punktlager till alla grafer.
#'
#' @param con_geodb,con_rutt `DBIConnection`-objekt eller databasnamn.
#' @param natverkstyp `"bil"` eller `"alla"`.
#' @param punkter_fran_schema,punkter_fran_tabell Adresskälla i geodata.
#' @param punkter_till_schema,punkter_till_tabell Adressmål i ruttanalyser.
#'
#' @return Osynligt `NULL`.
#' @export
pgrouting_skapa_ny_graf_nvdb_koppla_till_punkter <- function(con_geodb = "geodata",
                                                             con_rutt = "ruttanalyser",
                                                             natverkstyp = "bil",
                                                             punkter_fran_schema = "adresser",
                                                             punkter_fran_tabell = "dalarna",
                                                             punkter_till_tabell = "adresser",
                                                             punkter_till_schema = "punktlager") {
  cg <- intern_rutt_con(con_geodb, adm = TRUE, standard_db = "geodata")
  cr <- intern_rutt_con(con_rutt, adm = TRUE, standard_db = "ruttanalyser")
  on.exit({ intern_stang(cg); intern_stang(cr) }, add = TRUE)
  con_geodb <- cg$con; con_rutt <- cr$con

  natverk_urval <- if (identical(natverkstyp, "bil")) "WHERE vagtrafiknat_nattyp = 1" else ""

  pgrouting_klipp_natverk_skapa_tabell(
    con = con_geodb, con_till_databas = con_rutt, buffer_m = 30000,
    natverk_schema = "nvdb", natverk_tabell = "dala_med_grannlan", natverk_geokol = "geom",
    region_schema = "karta", region_tabell = "lan_lm", regionkod_kol = "lankod",
    regionkoder = "20", region_geokol = "geom", output_schema = "grafer",
    output_tabell = glue::glue("nvdb_{natverkstyp}"), urval_fran_natverk = natverk_urval)

  postgis_kopiera_tabell_mellan_databaser(
    con_fran_databas = con_geodb, con_till_databas = con_rutt,
    schema_fran = punkter_fran_schema, tabell_fran = punkter_fran_tabell,
    schema_till = punkter_till_schema, tabell_till = punkter_till_tabell)

  pgrouting_hitta_narmaste_punkt_pa_natverk(
    con = con_rutt, schema_punkter_fran = "punktlager",
    tabell_punkter_fran = punkter_till_tabell, geometri_kol_punkter_fran = "geom",
    id_kol_punkter_fran = "gml_id", schema_graf = "grafer",
    tabell_graf = glue::glue("nvdb_{natverkstyp}"), geometri_graf = "geom",
    id_graf = "rad_id", tolerans_avstand = 3)

  pgrouting_tabell_till_pgrgraf(
    con = con_rutt, schema_graf = "grafer",
    tabell_graf = glue::glue("nvdb_{natverkstyp}_{punkter_till_tabell}"),
    id_kol_graf = "rad_id", geom_kol_graf = "geom", tolerans = 0.001)

  pgrouting_kostnadskolumner_transporttyp_graf(
    con = con_rutt, schema_natverk = "grafer",
    tabell_natverk = glue::glue("nvdb_{natverkstyp}_{punkter_till_tabell}"),
    berakna_kostnad_bil = identical(natverkstyp, "bil"))

  tabeller_flytt <- c(
    glue::glue("nvdb_{natverkstyp}"),
    glue::glue("nvdb_{natverkstyp}_{punkter_till_tabell}_narmaste_punkt"),
    glue::glue("nvdb_{natverkstyp}_{punkter_till_tabell}_klusterpunkt"))
  purrr::walk(tabeller_flytt, function(tb) {
    postgis_kopiera_tabell(con = con_rutt, schema_fran = "grafer", tabell_fran = tb,
                           schema_till = "underlag_grafer", tabell_till = tb, skriv_over = TRUE)
    rdpostgres::postgres_tabell_ta_bort(con = con_rutt, schema = "grafer", tabell = tb)
    DBI::dbExecute(con_rutt, glue::glue("
      WITH senaste AS (
        SELECT id FROM metadata.uppdateringar
        WHERE tabell = '{tb}' AND schema = 'grafer'
        ORDER BY version_datum DESC, version_tid DESC LIMIT 1)
      UPDATE metadata.uppdateringar SET schema = 'underlag_grafer'
      WHERE id IN (SELECT id FROM senaste);"))
  })

  scheman <- rdpostgres::postgres_lista_scheman_tabeller(con = con_rutt)
  grafer <- scheman[["grafer"]][!grepl("vertices_pgr", scheman[["grafer"]])]
  punktlager <- scheman[["punktlager"]]

  for (pt in punktlager) for (g in grafer) {
    pgrouting_punkttabell_koppla_till_pgr_graf(
      con = con_rutt, schema_punkter = "punktlager", tabell_punkter = pt,
      geom_kol_punkter = "geom", id_kol_punkter = "gml_id",
      schema_natverk = "grafer", tabell_natverk = g)
  }
  invisible(NULL)
}
