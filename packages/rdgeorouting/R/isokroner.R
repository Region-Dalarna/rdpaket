# Isokroner (restids-/avståndsområden) från punkter längs en pgRouting-graf.

#' Skapa isokroner från punkter längs en pgRouting-graf
#'
#' Kör `pgr_drivingDistance()` från varje punkt och bygger polygoner
#' (concave/convex hull eller buffer) per kostnadsintervall.
#'
#' @param con En `DBIConnection` eller `"default"` (`ruttanalyser` via
#'   [rdpostgres::uppkoppling_adm()]).
#' @param punkter_sf sf-objekt med frånpunkter (alternativ till
#'   `schema_punkt`/`tabell_punkt`).
#' @param schema_punkt,tabell_punkt Punkttabell i databasen.
#' @param idkol_punkt,namnkol_punkt Id- och namnkolumn i punkttabellen.
#' @param nodkolumn_punkt Kolumn med förberäknad graf-nod (`NULL` = beräkna
#'   närmaste nod).
#' @param schema_graf,tabell_graf,idkol_graf Grafen.
#' @param kostnadskol_graf_f,kostnadskol_graf_b Kostnadskolumner framåt/bakåt
#'   (`NULL` bak = samma som fram).
#' @param intervall_varden Vektor av intervallgränser (t.ex. `c(15, 30, 45, 60)`).
#' @param kostnad_enhet `"auto"` (härleds ur kostnadskolumnens namn) eller
#'   annat.
#' @param restyp Etikett som skrivs som kolumn i resultatet.
#' @param spara_schema,spara_tabell Spara resultatet i databasen (`NULL` = spara
#'   inte).
#' @param returnera_sf Returnera resultatet som sf-objekt.
#' @param dela_upp_polygoner Klipp bort överlapp så polygonerna blir "ringar".
#' @param polygon_metod `"concave"`, `"buffer"` eller `"convex"`.
#' @param buffer_m,simplify_tol Parametrar för buffer-metoden.
#' @param visa_meddelanden Visa PostGIS NOTICE-meddelanden.
#'
#' @return Ett sf-objekt (om `returnera_sf`), annars osynligt `NULL`.
#' @export
postgis_isokroner_skapa <- function(con = "default",
                                    punkter_sf = NULL,
                                    schema_punkt = NULL, tabell_punkt = NULL,
                                    idkol_punkt = "id",
                                    nodkolumn_punkt = "nid_nvdb_alla_adresser",
                                    namnkol_punkt = "namn",
                                    schema_graf = "grafer",
                                    tabell_graf = "nvdb_alla_adresser",
                                    idkol_graf = "rad_id",
                                    kostnadskol_graf_f = "kostnad_meter",
                                    kostnadskol_graf_b = NULL,
                                    intervall_varden = c(5000, 10000, 20000, 30000),
                                    kostnad_enhet = "auto",
                                    restyp = "meter",
                                    spara_schema = NULL, spara_tabell = NULL,
                                    returnera_sf = TRUE,
                                    dela_upp_polygoner = TRUE,
                                    polygon_metod = "concave",
                                    buffer_m = 100, simplify_tol = 100,
                                    visa_meddelanden = FALSE) {
  intern_krav("sf")
  stopifnot(!is.null(punkter_sf) || (!is.null(schema_punkt) && !is.null(tabell_punkt)))
  stopifnot(returnera_sf || (!is.null(spara_schema) && !is.null(spara_tabell)))

  cc <- intern_rutt_con(con, adm = TRUE, standard_db = "ruttanalyser")
  on.exit(intern_stang(cc), add = TRUE)
  con <- cc$con

  if (!visa_meddelanden) DBI::dbExecute(con, "SET client_min_messages TO warning;")
  if (is.null(kostnadskol_graf_b)) kostnadskol_graf_b <- kostnadskol_graf_f

  if (!is.null(punkter_sf)) {
    geom_kol <- attr(punkter_sf, "sf_column")
    if (!identical(geom_kol, "geom")) names(punkter_sf)[names(punkter_sf) == geom_kol] <- "geom"
    punkter_sf <- sf::st_sf(punkter_sf, sf_column_name = "geom")
    rdpostgres::postgres_schema_skapa_om_inte_finns(con = con, schema_namn = "temp")
    sf::st_write(punkter_sf, con, DBI::Id(schema = "temp", table = "franpunkter"),
                 delete_layer = TRUE, quiet = TRUE)
    schema_punkt <- "temp"; tabell_punkt <- "franpunkter"
  }
  temptabell <- paste0(schema_punkt, ".", tabell_punkt)

  kolumner <- rdpostgres::postgres_lista_kolumnnamn_i_schema(con = con, schema = schema_punkt)
  har_kol <- function(k) any(kolumner$table_name == tabell_punkt & kolumner$column_name == k)

  namnkol_sql <- if (har_kol(namnkol_punkt)) paste0(', "', namnkol_punkt, '"') else ""
  nodkolumn_finns <- !is.null(nodkolumn_punkt) && har_kol(nodkolumn_punkt)

  if (!nodkolumn_finns) {
    DBI::dbExecute(con, glue::glue("ALTER TABLE {temptabell} ADD COLUMN IF NOT EXISTS toponode INTEGER;"))
    DBI::dbExecute(con, glue::glue("
      UPDATE {temptabell} p SET toponode = g.target
      FROM (
        SELECT p.{idkol_punkt}, g.target
        FROM {temptabell} p
        JOIN LATERAL (
          SELECT target FROM {schema_graf}.{tabell_graf}
          ORDER BY p.geom <-> geom LIMIT 1
        ) g ON true
      ) AS g
      WHERE p.{idkol_punkt} = g.{idkol_punkt};"))
    nodkolumn_punkt <- "toponode"
  }

  DBI::dbExecute(con, "DROP TABLE IF EXISTS noder_start;")
  DBI::dbExecute(con, glue::glue("
    CREATE TEMP TABLE noder_start AS
    SELECT {idkol_punkt}{namnkol_sql}, {nodkolumn_punkt} AS node
    FROM {temptabell} WHERE {nodkolumn_punkt} IS NOT NULL;"))
  stopifnot("noder_start" %in% DBI::dbListTables(con))

  from_vals <- c(0, utils::head(intervall_varden, -1) + 1)
  to_vals   <- intervall_varden
  max_cost  <- max(intervall_varden)

  enhet <- if (tolower(kostnad_enhet) == "auto") {
    if (grepl("meter", kostnadskol_graf_f)) " meter"
    else if (grepl("min", kostnadskol_graf_f)) " minuter"
    else if (grepl("sek", kostnadskol_graf_f)) " sekunder"
    else if (grepl("tim", kostnadskol_graf_f)) " timmar"
    else ""
  } else ""

  case_uttryck <- paste0(
    "CASE\n",
    paste(glue::glue("  WHEN d.agg_cost <= {to} THEN '{from}–{to}{enhet}'",
                     from = from_vals, to = to_vals), collapse = "\n"),
    glue::glue("\n  ELSE '> {max_cost}'\nEND AS kostnadsintervall"))

  noder <- DBI::dbReadTable(con, "noder_start")
  sql_per_punkt <- vapply(noder[[idkol_punkt]], function(pid) {
    node <- noder$node[noder[[idkol_punkt]] == pid]
    glue::glue("
    SELECT '{pid}' AS punkt_id, d.*, {case_uttryck}, e.*
    FROM pgr_drivingDistance(
      'SELECT {idkol_graf} AS id, source, target,
        CASE WHEN {kostnadskol_graf_f} IS NOT NULL THEN {kostnadskol_graf_f} ELSE -1 END AS cost,
        CASE WHEN {kostnadskol_graf_b} IS NOT NULL THEN {kostnadskol_graf_b} ELSE -1 END AS reverse_cost
       FROM {schema_graf}.{tabell_graf}',
      {node}, {max_cost}
    ) d
    JOIN {schema_graf}.{tabell_graf} e ON d.edge = e.{idkol_graf}")
  }, character(1))
  sql_union <- paste(sql_per_punkt, collapse = "\nUNION ALL\n")

  DBI::dbExecute(con, "DROP TABLE IF EXISTS isokron_edges;")
  DBI::dbExecute(con, glue::glue("CREATE TEMP TABLE isokron_edges AS\n{sql_union}"))

  DBI::dbExecute(con, "DROP TABLE IF EXISTS isokron_polygons;")
  poly_sql <- switch(polygon_metod,
    "concave" = glue::glue("
      CREATE TEMP TABLE isokron_polygons AS
      SELECT punkt_id{namnkol_sql}, e.kostnadsintervall,
             ST_ConcaveHull(ST_Collect(geom), 0.5) AS geom
      FROM isokron_edges AS e
      LEFT JOIN noder_start n ON e.punkt_id = n.{idkol_punkt}
      GROUP BY punkt_id{namnkol_sql}, e.kostnadsintervall;"),
    "buffer" = glue::glue("
      CREATE TEMP TABLE isokron_polygons AS
      SELECT punkt_id{namnkol_sql}, kostnadsintervall,
             ST_Buffer(ST_Union(ST_Simplify(geom, {simplify_tol})), {buffer_m}) AS geom
      FROM isokron_edges AS e
      LEFT JOIN noder_start n ON e.punkt_id = n.{idkol_punkt}
      GROUP BY punkt_id{namnkol_sql}, kostnadsintervall;"),
    "convex" = glue::glue("
      CREATE TEMP TABLE isokron_polygons AS
      SELECT punkt_id{namnkol_sql}, kostnadsintervall,
             ST_ConvexHull(ST_Collect(geom)) AS geom
      FROM isokron_edges AS e
      LEFT JOIN noder_start n ON e.punkt_id = n.{idkol_punkt}
      GROUP BY punkt_id{namnkol_sql}, kostnadsintervall;"),
    stop("polygon_metod måste vara 'concave', 'buffer' eller 'convex'.", call. = FALSE))
  DBI::dbExecute(con, poly_sql)

  iso <- sf::st_read(con, query = "SELECT * FROM isokron_polygons", quiet = TRUE)
  niva_ordning <- unique(iso$kostnadsintervall)[order(as.numeric(stringr::str_extract(unique(iso$kostnadsintervall), "^\\d+")))]
  iso$kostnadsintervall <- factor(iso$kostnadsintervall, levels = niva_ordning)
  iso <- iso[order(iso$punkt_id, -as.integer(iso$kostnadsintervall)), ]

  if (dela_upp_polygoner) {
    iso <- postgis_isokroner_dela_upp_polygoner(
      skickad_sf = iso, kategori_kol = "kostnadsintervall",
      isoid_kol = "punkt_id", geom_kol = attr(iso, "sf_column"))
  }

  if (!is.null(spara_schema) && !is.null(spara_tabell)) {
    sf::st_write(iso, dsn = con, layer = DBI::Id(schema = spara_schema, table = spara_tabell),
                 delete_layer = TRUE, quiet = TRUE)
  }

  if (restyp != "meter") {
    geom_kol <- attr(iso, "sf_column")
    iso$restyp <- restyp
    iso <- iso[, c(setdiff(names(iso), c("restyp", geom_kol)), "restyp", geom_kol)]
  }

  if (rdpostgres::postgres_schema_finns(con, "temp")) {
    suppressMessages(try(rdpostgres::postgres_tabell_ta_bort(con = con, schema = "temp", tabell = "franpunkter"), silent = TRUE))
    suppressMessages(try(rdpostgres::postgres_schema_ta_bort(con = con, schema = "temp"), silent = TRUE))
  }
  if (!visa_meddelanden) DBI::dbExecute(con, "SET client_min_messages TO notice;")

  if (returnera_sf) iso else invisible(NULL)
}

#' Klipp isokronpolygoner så att de inte överlappar
#'
#' @param skickad_sf sf-objekt med isokroner.
#' @param kategori_kol Kolumn med kostnadsintervall/nivå.
#' @param isoid_kol Kolumn med punkt-id (om flera punkter).
#' @param geom_kol Geometrikolumnens namn.
#'
#' @return Ett sf-objekt utan överlapp mellan nivåer.
#' @export
postgis_isokroner_dela_upp_polygoner <- function(skickad_sf, kategori_kol, isoid_kol, geom_kol) {
  intern_krav("sf")
  niva_ordning <- sort(unique(skickad_sf[[kategori_kol]]))

  iso_union <- do.call(rbind, lapply(
    split(skickad_sf, interaction(skickad_sf[[isoid_kol]], skickad_sf[[kategori_kol]], drop = TRUE)),
    function(grp) {
      g <- grp[1, ]
      sf::st_geometry(g) <- sf::st_union(sf::st_geometry(grp))
      g
    }))

  utan_overlapp <- vector("list", length(niva_ordning))
  for (i in seq_along(niva_ordning)) {
    aktuella <- iso_union[iso_union[[kategori_kol]] == niva_ordning[i], ]
    if (i == 1) {
      utan_overlapp[[i]] <- aktuella
      next
    }
    tidigare <- do.call(rbind, utan_overlapp[seq_len(i - 1)])
    klippt <- do.call(rbind, lapply(unique(aktuella[[isoid_kol]]), function(pid) {
      denna <- aktuella[aktuella[[isoid_kol]] == pid, ]
      tid_pid <- tidigare[tidigare[[isoid_kol]] == pid, ]
      if (nrow(tid_pid) == 0) return(denna)
      sf::st_geometry(denna) <- sf::st_difference(sf::st_geometry(denna),
                                                  sf::st_union(sf::st_geometry(tid_pid)))
      denna
    }))
    utan_overlapp[[i]] <- klippt
  }
  do.call(rbind, utan_overlapp)
}

# --- Tunna wrappers per transportsätt --------------------------------------

intern_isokron_wrapper <- function(..., tabell_graf, kostnadskol_graf_f, kostnadskol_graf_b, restyp,
                                   nodkolumn_punkt, intervall_varden) {
  postgis_isokroner_skapa(
    tabell_graf = tabell_graf, idkol_graf = "rad_id",
    kostnadskol_graf_f = kostnadskol_graf_f, kostnadskol_graf_b = kostnadskol_graf_b,
    restyp = restyp, nodkolumn_punkt = nodkolumn_punkt,
    intervall_varden = intervall_varden, kostnad_enhet = "auto", ...)
}

#' Isokroner per transportsätt
#'
#' Tunna wrappers runt [postgis_isokroner_skapa()] med rätt graf- och
#' kostnadskolumner.
#'
#' @param punkter_sf,schema_punkt,tabell_punkt,idkol_punkt,namnkol_punkt,spara_schema,spara_tabell,returnera_sf,dela_upp_polygoner
#'   Se [postgis_isokroner_skapa()].
#' @param nodkolumn_punkt Kolumn med förberäknad graf-nod.
#' @param intervall_varden Intervallgränser (minuter, resp. meter för `_meter`).
#'
#' @return Se [postgis_isokroner_skapa()].
#' @name postgis_isokroner
NULL

#' @rdname postgis_isokroner
#' @export
postgis_isokroner_bil <- function(punkter_sf = NULL, schema_punkt = NULL, tabell_punkt = NULL,
                                  idkol_punkt = "id", nodkolumn_punkt = "nid_nvdb_bil_adresser",
                                  namnkol_punkt = "namn", intervall_varden = c(15, 30, 45, 60),
                                  spara_schema = NULL, spara_tabell = NULL,
                                  returnera_sf = TRUE, dela_upp_polygoner = TRUE) {
  intern_isokron_wrapper(
    punkter_sf = punkter_sf, schema_punkt = schema_punkt, tabell_punkt = tabell_punkt,
    idkol_punkt = idkol_punkt, namnkol_punkt = namnkol_punkt,
    spara_schema = spara_schema, spara_tabell = spara_tabell,
    returnera_sf = returnera_sf, dela_upp_polygoner = dela_upp_polygoner,
    tabell_graf = "nvdb_bil_adresser",
    kostnadskol_graf_f = "kostnad_bil_f_min", kostnadskol_graf_b = "kostnad_bil_b_min",
    restyp = "bil", nodkolumn_punkt = nodkolumn_punkt, intervall_varden = intervall_varden)
}

#' @rdname postgis_isokroner
#' @export
postgis_isokroner_meter <- function(punkter_sf = NULL, schema_punkt = NULL, tabell_punkt = NULL,
                                    idkol_punkt = "id", nodkolumn_punkt = "nid_nvdb_alla_adresser",
                                    namnkol_punkt = "namn", intervall_varden = c(1000, 5000, 10000, 20000),
                                    spara_schema = NULL, spara_tabell = NULL,
                                    returnera_sf = TRUE, dela_upp_polygoner = TRUE) {
  intern_isokron_wrapper(
    punkter_sf = punkter_sf, schema_punkt = schema_punkt, tabell_punkt = tabell_punkt,
    idkol_punkt = idkol_punkt, namnkol_punkt = namnkol_punkt,
    spara_schema = spara_schema, spara_tabell = spara_tabell,
    returnera_sf = returnera_sf, dela_upp_polygoner = dela_upp_polygoner,
    tabell_graf = "nvdb_alla_adresser",
    kostnadskol_graf_f = "kostnad_meter", kostnadskol_graf_b = NULL,
    restyp = "meter", nodkolumn_punkt = nodkolumn_punkt, intervall_varden = intervall_varden)
}

intern_isokron_ttyp <- function(restyp, kostnadskol, ...) {
  args <- list(...)
  do.call(intern_isokron_wrapper, c(args, list(
    tabell_graf = "nvdb_alla_adresser", kostnadskol_graf_f = kostnadskol,
    kostnadskol_graf_b = NULL, restyp = restyp)))
}

#' @rdname postgis_isokroner
#' @export
postgis_isokroner_gang <- function(punkter_sf = NULL, schema_punkt = NULL, tabell_punkt = NULL,
                                   idkol_punkt = "id", nodkolumn_punkt = "nid_nvdb_alla_adresser",
                                   namnkol_punkt = "namn", intervall_varden = c(15, 30, 45, 60),
                                   spara_schema = NULL, spara_tabell = NULL,
                                   returnera_sf = TRUE, dela_upp_polygoner = TRUE) {
  intern_isokron_ttyp("till fots", "kostnad_gang_min",
    punkter_sf = punkter_sf, schema_punkt = schema_punkt, tabell_punkt = tabell_punkt,
    idkol_punkt = idkol_punkt, namnkol_punkt = namnkol_punkt,
    spara_schema = spara_schema, spara_tabell = spara_tabell,
    returnera_sf = returnera_sf, dela_upp_polygoner = dela_upp_polygoner,
    nodkolumn_punkt = nodkolumn_punkt, intervall_varden = intervall_varden)
}

#' @rdname postgis_isokroner
#' @export
postgis_isokroner_cykel <- function(punkter_sf = NULL, schema_punkt = NULL, tabell_punkt = NULL,
                                    idkol_punkt = "id", nodkolumn_punkt = "nid_nvdb_alla_adresser",
                                    namnkol_punkt = "namn", intervall_varden = c(15, 30, 45, 60),
                                    spara_schema = NULL, spara_tabell = NULL,
                                    returnera_sf = TRUE, dela_upp_polygoner = TRUE) {
  intern_isokron_ttyp("cykel", "kostnad_cykel_min",
    punkter_sf = punkter_sf, schema_punkt = schema_punkt, tabell_punkt = tabell_punkt,
    idkol_punkt = idkol_punkt, namnkol_punkt = namnkol_punkt,
    spara_schema = spara_schema, spara_tabell = spara_tabell,
    returnera_sf = returnera_sf, dela_upp_polygoner = dela_upp_polygoner,
    nodkolumn_punkt = nodkolumn_punkt, intervall_varden = intervall_varden)
}

#' @rdname postgis_isokroner
#' @export
postgis_isokroner_elcykel <- function(punkter_sf = NULL, schema_punkt = NULL, tabell_punkt = NULL,
                                      idkol_punkt = "id", nodkolumn_punkt = "nid_nvdb_alla_adresser",
                                      namnkol_punkt = "namn", intervall_varden = c(15, 30, 45, 60),
                                      spara_schema = NULL, spara_tabell = NULL,
                                      returnera_sf = TRUE, dela_upp_polygoner = TRUE) {
  intern_isokron_ttyp("elcykel", "kostnad_elcykel_min",
    punkter_sf = punkter_sf, schema_punkt = schema_punkt, tabell_punkt = tabell_punkt,
    idkol_punkt = idkol_punkt, namnkol_punkt = namnkol_punkt,
    spara_schema = spara_schema, spara_tabell = spara_tabell,
    returnera_sf = returnera_sf, dela_upp_polygoner = dela_upp_polygoner,
    nodkolumn_punkt = nodkolumn_punkt, intervall_varden = intervall_varden)
}
