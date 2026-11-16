# Bakgrundskarta (topografi) ur geodatabasen: bygg schemat bakgrundskarta_dalarna ur topografi_10 och hämta lagren.

# Lagren och deras källor/generalisering. Namnen är tabellnamn i schemat.
intern_bakgrund_lager <- function() {
  c("vatten", "skog", "bebyggelse", "vagar_stora", "vagar_mellan", "vagar_sma", "jarnvag",
    "ortnamn", "vattennamn", "sjoar")
}

# SQL-sats (CREATE TABLE ... AS <sats>) per lager för länsskala.
#' @noRd
intern_bakgrund_sql <- function(src = "topografi_10", mitt = "(ST_MaximumInscribedCircle(k.geom)).center") {
  # Slår ihop polygoner per klass, förenklar och tar bort små ytor. Ingen kantmatchning mellan klasser
  # behövs eftersom öppen mark och åker är bakgrund och ritas inte.
  yta <- function(villkor, tol, minyta) glue::glue("
    SELECT (d).geom::geometry(Polygon, 3006) AS geom
    FROM (SELECT ST_Dump(ST_SimplifyPreserveTopology(ST_UnaryUnion(ST_Collect(
                 ST_Force2D(ST_CollectionExtract(ST_MakeValid(geom), 3)))), {tol})) AS d
          FROM {src}.mark WHERE {villkor}) x
    WHERE ST_Area((d).geom) >= {minyta}")

  vag <- function(typer, tol) glue::glue("
    SELECT objekttyp, gastvag1nummer AS vagnummer,
           ST_SimplifyPreserveTopology(ST_Force2D(geom), {tol})::geometry(LineString, 3006) AS geom
    FROM {src}.vaglinje WHERE objekttyp IN ({typer})")

  # Ett namn kan vara delat på flera rader (tdelidx 1, 2, ...) med samma regtext och adat.
  text <- function(typer, minhojd) glue::glue("
    SELECT regtext AS namn, detaljtyp, thojd, min(tjust) AS tjust, count(*)::int AS delar,
           ST_Centroid(ST_Collect(ST_Force2D(geom)))::geometry(Point, 3006) AS geom,
           round(ST_MaxDistance(ST_Collect(geom), ST_Collect(geom))::numeric) AS spridning_m
    FROM {src}.textobjekt
    WHERE detaljtyp IN ({typer}) AND thojd >= {minhojd}
    GROUP BY regtext, detaljtyp, adat, thojd")

  list(
    vatten = yta("objekttyp IN ('Sjö', 'Vattendragsyta', 'Anlagt vatten')", 30, 20000),
    skog = yta("objekttyp IN ('Barr- och blandskog', 'Lövskog', 'Fjällbjörkskog')", 50, 50000),
    bebyggelse = yta(paste("objekttyp IN ('Låg bebyggelse', 'Hög bebyggelse', 'Sluten bebyggelse',",
                           "'Industri- och handelsbebyggelse')"), 20, 10000),
    vagar_stora = vag("'Motortrafikled', 'Mötesfri väg', 'Landsväg', 'Övergripande länk'", 10),
    vagar_mellan = vag("'Landsväg liten', 'Huvudgata', 'Lokalgata stor'", 10),
    vagar_sma = vag("'Lokalgata liten', 'Småväg'", 10),
    jarnvag = glue::glue("SELECT ST_SimplifyPreserveTopology(ST_Force2D(geom), 10)::geometry(LineString, 3006) AS geom
                          FROM {src}.ralstrafik WHERE status = 'Öppen'"),
    ortnamn = text("'BEBTÄTTX', 'BEBTX'", 14),
    vattennamn = text("'VATTTX', 'VATTDRTX', 'VATTDELTX'", 16),
    # En etikett per sjö, i mitten av sjöns största öppna vattenyta: sjöpolygoner över 0,25 km2 som
    # innehåller en sjötext. Namnet och rangen kommer från texten, arean (km2) från sjön.
    sjoar = glue::glue("
      SELECT k.regtext AS namn, k.thojd, round((ST_Area(k.geom) / 1e6)::numeric, 2) AS area_km2,
             ST_Force2D({mitt})::geometry(Point, 3006) AS geom
      FROM (SELECT DISTINCT ON (s.objektidentitet, t.regtext) s.objektidentitet, s.geom, t.regtext, t.thojd
            FROM {src}.mark s JOIN {src}.textobjekt t ON ST_Intersects(s.geom, t.geom)
            WHERE s.objekttyp = 'Sjö' AND ST_Area(s.geom) >= 250000
              AND t.detaljtyp = 'VATTTX' AND t.thojd >= 16
            ORDER BY s.objektidentitet, t.regtext, t.thojd DESC) k")
  )
}

#' Bygg bakgrundskartans lager i geodatabasen
#'
#' Skapar schemat `bakgrundskarta_dalarna` (länsskala) ur Lantmäteriets topografi i schemat
#' `topografi_10`: vatten, skog och bebyggelse som sammanslagna och förenklade ytor, vägar i tre
#' nivåer, järnväg samt ortnamn, vattennamn och sjönamn (en etikett per sjö, i sjöns mitt). Öppen mark och åker ritas inte, de är bakgrunden.
#' Lagren får rumsliga index och läses sedan med [hamta_bakgrundskarta()]. Funktionen ska köras
#' efter varje inläsning av `topografi_10`; den tar bort och bygger om varje lager.
#'
#' Skog är det tyngsta steget (flera minuter). Kräver en administratörsuppkoppling.
#'
#' @param con En `DBIConnection` med skrivrättigheter mot geodatabasen, eller `NULL` (standard) för
#'   `rdpostgres::uppkoppling_adm("geodata")`.
#' @param schema Schemat som skapas. Standard `"bakgrundskarta_dalarna"`.
#' @param kalla_schema Schemat med topografin. Standard `"topografi_10"`.
#' @param lager Vilka lager som ska byggas, `NULL` (standard) = alla.
#'   Möjliga namn: `vatten`, `skog`, `bebyggelse`, `vagar_stora`,
#'   `vagar_mellan`, `vagar_sma`, `jarnvag`, `ortnamn`, `vattennamn`, `sjoar`.
#'
#' @return En `data.frame` med lager, antal rader och storlek. Osynligt.
#' @seealso [hamta_bakgrundskarta()]
#' @export
skapa_bakgrundskarta_topografi <- function(con = NULL, schema = "bakgrundskarta_dalarna",
                                           kalla_schema = "topografi_10", lager = NULL) {
  okanda <- setdiff(lager, intern_bakgrund_lager())
  if (length(okanda) > 0) stop("Okända lager: ", paste(okanda, collapse = ", "), call. = FALSE)
  egen <- is.null(con)
  if (egen) {
    con <- rdpostgres::uppkoppling_adm("geodata")
    if (is.null(con)) stop("Kunde inte ansluta till geodatabasen.", call. = FALSE)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }
  # ST_MaximumInscribedCircle finns från PostGIS 3.1; annars används en punkt inuti sjön
  postgis_ver <- DBI::dbGetQuery(con, "SELECT PostGIS_Lib_Version() AS v")$v
  mitt <- if (numeric_version(sub("^([0-9]+\\.[0-9]+).*", "\\1", postgis_ver)) >= "3.1") {
    "(ST_MaximumInscribedCircle(k.geom)).center"
  } else {
    "ST_PointOnSurface(k.geom)"
  }
  sql <- intern_bakgrund_sql(kalla_schema, mitt)
  if (!is.null(lager)) sql <- sql[lager]

  DBI::dbExecute(con, glue::glue("CREATE SCHEMA IF NOT EXISTS {schema}"))
  DBI::dbExecute(con, "SET client_min_messages TO warning")

  for (namn in names(sql)) {
    t0 <- Sys.time()
    message("Bygger ", schema, ".", namn, " ...")
    DBI::dbWithTransaction(con, {
      DBI::dbExecute(con, glue::glue("DROP TABLE IF EXISTS {schema}.{namn}"))
      DBI::dbExecute(con, glue::glue("CREATE TABLE {schema}.{namn} AS {sql[[namn]]}"))
      DBI::dbExecute(con, glue::glue("CREATE INDEX {namn}_geom_idx ON {schema}.{namn} USING gist (geom)"))
      DBI::dbExecute(con, glue::glue("COMMENT ON TABLE {schema}.{namn} IS 'Genererad {Sys.Date()} ur {kalla_schema}'"))
    })
    DBI::dbExecute(con, glue::glue("ANALYZE {schema}.{namn}"))
    message("  klar på ", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), " s")
  }

  invisible(DBI::dbGetQuery(con, glue::glue("
    SELECT c.relname AS lager, c.reltuples::bigint AS rader,
           pg_size_pretty(pg_total_relation_size(c.oid)) AS storlek
    FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace
    WHERE n.nspname = '{schema}' AND c.relkind = 'r' ORDER BY 1")))
}

# SQL för ett bbox-urval (koordinater i SWEREF 99 TM)
#' @noRd
intern_bakgrund_urval_sql <- function(schema, tabell, bbox) {
  paste0("SELECT * FROM ", schema, ".", tabell,
         " WHERE geom && ST_MakeEnvelope(", paste(vapply(unname(bbox), format, "", scientific = FALSE, trim = TRUE),
                                                  collapse = ", "), ", 3006)")
}

#' Finns bakgrundskartan i geodatabasen?
#'
#' Kontrollerar att databasen går att nå och att alla lager i bakgrundskartan finns.
#' Ger ett meddelande om vad som saknas, och är avsedd som förkontroll i funktioner som ritar kartor
#' med bakgrund ur databasen.
#'
#' @param schema Schemat med bakgrundskartan. Standard `"bakgrundskarta_dalarna"`.
#' @param lager Vilka lager som måste finnas. Standard alla.
#' @param con En `DBIConnection`, eller `NULL` (standard) för en tillfällig uppkoppling.
#' @param tyst `TRUE` ger bara `TRUE`/`FALSE`, utan meddelande om vad som saknas.
#'
#' @return `TRUE` eller `FALSE`.
#' @export
bakgrundskarta_finns <- function(schema = "bakgrundskarta_dalarna", lager = intern_bakgrund_lager(),
                                 con = NULL, tyst = FALSE) {
  meddelande <- function(...) if (!tyst) message(...)
  egen <- is.null(con)
  if (egen) {
    con <- tryCatch(rdpostgres::uppkoppling_db(db_name = "geodata", service_name = "rd_geodata"),
                    error = function(e) NULL)
    if (is.null(con)) {
      meddelande("Ingen åtkomst till geodatabasen. Bakgrundskartan hämtas ur databasen och kräver uppkoppling.")
      return(FALSE)
    }
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }
  finns <- tryCatch(
    DBI::dbGetQuery(con, glue::glue("SELECT table_name FROM information_schema.tables
                                     WHERE table_schema = '{schema}'"))$table_name,
    error = function(e) character(0))
  saknas <- setdiff(lager, finns)
  if (length(saknas) > 0) {
    meddelande("Bakgrundskartan saknar lagret/lagren: ", paste(saknas, collapse = ", "),
               " i schemat '", schema, "'. Bygg dem med skapa_bakgrundskarta_topografi().")
    return(FALSE)
  }
  TRUE
}

#' Hämta bakgrundskartans lager ur geodatabasen
#'
#' Läser de förberedda topografilagren (se [skapa_bakgrundskarta_topografi()]) för ett utsnitt, som
#' en namngiven lista med `sf`-objekt i SWEREF 99 TM (EPSG:3006). Utsnittet bestäms av länet/länen i
#' `region_vekt`, eller av `bbox`. Funktionen stoppar med ett tydligt meddelande om databasen inte går
#' att nå eller om lagren inte byggts.
#'
#' @param region_vekt Länskod(er) eller kommunkod(er); utsnittet är omslutande rektangel för länet
#'   (kommunkoder ger länet kommunen ligger i). Standard `"20"` (Dalarna). Används inte om `bbox` anges.
#' @param lager Vilka lager som ska hämtas. Standard alla.
#' @param bbox Ett eget utsnitt: ett `sf::st_bbox()`-objekt eller en numerisk vektor
#'   `c(xmin, ymin, xmax, ymax)` i EPSG:3006. `NULL` (standard) = utifrån `region_vekt`.
#' @param marginal Andel av utsnittets storlek som läggs till på alla sidor. Standard 0,05.
#' @param schema Schemat med bakgrundskartan. Standard `"bakgrundskarta_dalarna"`.
#' @param con En `DBIConnection`, eller `NULL` (standard) för en tillfällig uppkoppling.
#'
#' @return En namngiven lista med `sf`-objekt, ett per lager.
#' @seealso [skapa_bakgrundskarta_topografi()], [bakgrundskarta_finns()]
#' @export
hamta_bakgrundskarta <- function(region_vekt = "20", lager = intern_bakgrund_lager(), bbox = NULL,
                                 marginal = 0.05, schema = "bakgrundskarta_dalarna", con = NULL) {
  intern_krav("sf")
  okanda <- setdiff(lager, intern_bakgrund_lager())
  if (length(okanda) > 0) stop("Okända lager: ", paste(okanda, collapse = ", "), call. = FALSE)

  egen <- is.null(con)
  if (egen) {
    con <- rdpostgres::uppkoppling_db(db_name = "geodata", service_name = "rd_geodata")
    if (is.null(con)) {
      stop("Kunde inte ansluta till geodatabasen. Bakgrundskartan hämtas ur databasen och kräver åtkomst.",
           call. = FALSE)
    }
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }
  if (!bakgrundskarta_finns(schema = schema, lager = lager, con = con, tyst = TRUE)) {
    stop("Bakgrundskartan saknas i geodatabasen (schema '", schema, "'). ",
         "Bygg den med skapa_bakgrundskarta_topografi().", call. = FALSE)
  }

  if (is.null(bbox)) {
    lan <- hamta_karta("lan", unique(substr(region_vekt, 1, 2)))
    if (is.null(lan) || nrow(lan) == 0) stop("Hittade inget län för region_vekt.", call. = FALSE)
    bbox <- sf::st_bbox(sf::st_transform(lan, 3006))
  }
  bbox <- as.numeric(bbox)[1:4]
  bredd <- bbox[3] - bbox[1]; hojd <- bbox[4] - bbox[2]
  bbox <- c(bbox[1] - marginal * bredd, bbox[2] - marginal * hojd,
            bbox[3] + marginal * bredd, bbox[4] + marginal * hojd)

  stats::setNames(lapply(lager, function(l) {
    sf::st_read(con, query = intern_bakgrund_urval_sql(schema, l, bbox), quiet = TRUE)
  }), lager)
}
