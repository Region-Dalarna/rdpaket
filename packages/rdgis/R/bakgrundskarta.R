# Bakgrundskarta (topografi) ur geodatabasen: bygg schemat bakgrundskarta_dalarna ur topografi_10 och hämta lagren.

# Lagren och deras källor/generalisering. Namnen är tabellnamn i schemat.
intern_bakgrund_lager <- function(kalla = "topografi_10") {
  if (kalla == "topografi_1m") {
    return(c("utland", "vatten", "skog", "fjall", "glaciar", "bebyggelse", "vattendrag",
             "vagar_stora", "vagar_mellan", "vagar_sma", "jarnvag", "ortnamn", "vattennamn", "terrangnamn", "hav"))
  }
  c("vatten", "skog", "bebyggelse", "vagar_stora", "vagar_mellan", "vagar_sma", "jarnvag",
    "ortnamn", "vattennamn", "sjoar")
}

# Vilken topografikälla ett bakgrundsschema är byggt ur (utifrån schemats namn) och tvärtom
#' @noRd
intern_bakgrund_kalla <- function(schema) {
  if (grepl("sverige", schema, fixed = TRUE)) "topografi_1m" else "topografi_10"
}
#' @noRd
intern_bakgrund_standardschema <- function(kalla_schema) {
  if (grepl("1m", kalla_schema, fixed = TRUE)) "bakgrundskarta_sverige" else "bakgrundskarta_dalarna"
}

# SQL per lager för hela Sverige ur Topografi 1M (SWEREF 99 TM). Underlaget är redan generaliserat och litet
# (några tiotal MB), så ytorna tas rakt av; MULTI-geometrier delas i enkla geometrier.
#' @noRd
intern_bakgrund_sql_1m <- function(src = "topografi_1m") {
  yta <- function(villkor) glue::glue("
    SELECT (d).geom::geometry(Polygon, 3006) AS geom
    FROM (SELECT ST_Dump(ST_Force2D(ST_CollectionExtract(ST_MakeValid(geom), 3))) AS d
          FROM {src}.mark WHERE {villkor}) x")
  linje <- function(tabell, villkor = "true", extra = "") glue::glue("
    SELECT {extra}(ST_Dump(ST_Force2D(geom))).geom::geometry(LineString, 3006) AS geom
    FROM {src}.{tabell} WHERE {villkor}")
  vag <- function(typer) linje("vaglinje", glue::glue("objekttyp IN ({typer})"),
                               "objekttyp, gastvag1nummer AS vagnummer, ")
  # Namnen ligger i textpunkt. textstrang är hela namnet och ett namn kan vara delat på två rader (textdelnr 1 och 2,
  # t.ex. "Drottning-" och "skär") som ligger nära varandra. Delarna slås ihop till en punkt per namn och kategori;
  # lika namn på olika platser (över 3 km ifrån varandra) hålls isär. klass = textstorleksklass (högre = större).
  text <- function(kategorier) glue::glue("
    SELECT t.textstrang AS namn, t.textkategori AS kategori, max(t.textstorleksklass::int) AS klass,
           ST_Centroid(ST_Collect(t.geom))::geometry(Point, 3006) AS geom,
           count(*)::int AS delar
    FROM (SELECT textstrang, textkategori, textstorleksklass, ST_Force2D(geom) AS geom,
                 ST_ClusterDBSCAN(ST_Force2D(geom), 3000, 1) OVER (PARTITION BY textstrang, textkategori) AS grupp
          FROM {src}.textpunkt WHERE textkategori IN ({kategorier})) t
    GROUP BY t.textstrang, t.textkategori, t.grupp")
  list(
    utland = yta("objekttyp = 'Ej karterat område'"),
    vatten = yta("objekttyp IN ('Vattenyta', 'Hav')"),
    # Bara havsytorna (svenskt territorialvatten); används för att klippa länsgränserna mot kusten
    hav = yta("objekttyp = 'Hav'"),
    skog = yta("objekttyp = 'Skog'"),
    fjall = yta("objekttyp = 'Kalfjäll'"),
    glaciar = yta("objekttyp = 'Glaciär'"),
    bebyggelse = yta("objekttyp = 'Bebyggelse'"),
    vattendrag = linje("hydrolinje"),
    vagar_stora = vag("'Motorväg', 'Motortrafikled', 'Mötesfri väg', 'Landsväg'"),
    vagar_mellan = vag("'Landsväg liten'"),
    vagar_sma = vag("'Småväg'"),
    jarnvag = linje("ralstrafik"),
    ortnamn = text("'Tätort', 'Bebyggelse'"),
    vattennamn = text("'Hydrografi'"),
    terrangnamn = text("'Terrängnamn'")
  )
}

# SQL-sats (CREATE TABLE ... AS <sats>) per lager för länsskala.
#' @noRd
intern_bakgrund_sql <- function(src = "topografi_10", mitt = "(ST_MaximumInscribedCircle(d.geom)).center") {
  if (grepl("1m", src, fixed = TRUE)) return(intern_bakgrund_sql_1m(src))
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
    # En etikett per sjö, i mitten av sjöns största sammanhängande vattenyta. Sjöpolygoner över 0,25 km2 som
    # innehåller en sjötext slås ihop per namn (en stor sjö kan bestå av flera polygoner) och delas sedan i
    # sammanhängande delar. Namn och rang kommer från texten, arean (km2) från den sammanhängande ytan.
    sjoar = glue::glue("
      SELECT d.namn, d.thojd, round((ST_Area(d.geom) / 1e6)::numeric, 2) AS area_km2,
             ST_Force2D({mitt})::geometry(Point, 3006) AS geom
      FROM (SELECT u.regtext AS namn, u.thojd, (ST_Dump(u.geom)).geom AS geom
            FROM (SELECT k.regtext, max(k.thojd) AS thojd, ST_UnaryUnion(ST_Collect(k.geom)) AS geom
                  FROM (SELECT DISTINCT ON (s.objektidentitet, t.regtext) s.geom, t.regtext, t.thojd
                        FROM {src}.mark s JOIN {src}.textobjekt t ON ST_Intersects(s.geom, t.geom)
                        WHERE s.objekttyp = 'Sjö' AND ST_Area(s.geom) >= 250000
                          AND t.detaljtyp = 'VATTTX' AND t.thojd >= 16
                        ORDER BY s.objektidentitet, t.regtext, t.thojd DESC) k
                  GROUP BY k.regtext) u) d")
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
#' @param schema Schemat som skapas. `NULL` (standard) ger `"bakgrundskarta_dalarna"` för `topografi_10` och
#'   `"bakgrundskarta_sverige"` för `topografi_1m`.
#' @param kalla_schema Schemat med topografin: `"topografi_10"` (standard, länsskala) eller `"topografi_1m"` (hela Sverige).
#' @param lager Vilka lager som ska byggas, `NULL` (standard) = alla.
#'   Ur `topografi_10`: `vatten`, `skog`, `bebyggelse`, `vagar_stora`, `vagar_mellan`, `vagar_sma`, `jarnvag`,
#'   `ortnamn`, `vattennamn`, `sjoar`. Ur `topografi_1m`: `utland`, `vatten` (sjöar och hav), `skog`, `fjall`,
#'   `glaciar`, `bebyggelse`, `vattendrag`, `vagar_stora`, `vagar_mellan`, `vagar_sma`, `jarnvag`, `ortnamn`,
#'   `vattennamn`, `terrangnamn`, `hav` (enbart havsytor, för att klippa gränser mot kusten).
#'
#' @return En `data.frame` med lager, antal rader och storlek. Osynligt.
#' @seealso [hamta_bakgrundskarta()]
#' @export
skapa_bakgrundskarta_topografi <- function(con = NULL, schema = NULL,
                                           kalla_schema = "topografi_10", lager = NULL) {
  if (is.null(schema)) schema <- intern_bakgrund_standardschema(kalla_schema)
  if (intern_bakgrund_kalla(schema) != if (grepl("1m", kalla_schema, fixed = TRUE)) "topografi_1m" else "topografi_10") {
    stop("Schemat '", schema, "' hör till en annan topografikälla än '", kalla_schema, "'. ",
         "Använd bakgrundskarta_sverige för topografi_1m och bakgrundskarta_dalarna för topografi_10.", call. = FALSE)
  }
  okanda <- setdiff(lager, intern_bakgrund_lager(intern_bakgrund_kalla(schema)))
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
    "(ST_MaximumInscribedCircle(d.geom)).center"
  } else {
    "ST_PointOnSurface(d.geom)"
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
#' @param schema Schemat med bakgrundskartan. Standard `"bakgrundskarta_dalarna"`; `"bakgrundskarta_sverige"` för hela Sverige.
#' @param lager Vilka lager som måste finnas. `NULL` (standard) = alla lager för schemat.
#' @param con En `DBIConnection`, eller `NULL` (standard) för en tillfällig uppkoppling.
#' @param tyst `TRUE` ger bara `TRUE`/`FALSE`, utan meddelande om vad som saknas.
#'
#' @return `TRUE` eller `FALSE`.
#' @export
bakgrundskarta_finns <- function(schema = "bakgrundskarta_dalarna", lager = NULL,
                                 con = NULL, tyst = FALSE) {
  if (is.null(lager)) lager <- intern_bakgrund_lager(intern_bakgrund_kalla(schema))
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
#' @param lager Vilka lager som ska hämtas. `NULL` (standard) = alla lager för schemat.
#' @param bbox Ett eget utsnitt: ett `sf::st_bbox()`-objekt eller en numerisk vektor
#'   `c(xmin, ymin, xmax, ymax)` i EPSG:3006. `NULL` (standard) = utifrån `region_vekt`.
#' @param marginal Andel av utsnittets storlek som läggs till på alla sidor. Standard 0,05.
#' @param schema Schemat med bakgrundskartan. Standard `"bakgrundskarta_dalarna"`; `"bakgrundskarta_sverige"` för hela Sverige.
#' @param con En `DBIConnection`, eller `NULL` (standard) för en tillfällig uppkoppling.
#'
#' @return En namngiven lista med `sf`-objekt, ett per lager.
#' @seealso [skapa_bakgrundskarta_topografi()], [bakgrundskarta_finns()]
#' @export
hamta_bakgrundskarta <- function(region_vekt = "20", lager = NULL, bbox = NULL,
                                 marginal = 0.05, schema = "bakgrundskarta_dalarna", con = NULL) {
  intern_krav("sf")
  alla_lager <- intern_bakgrund_lager(intern_bakgrund_kalla(schema))
  if (is.null(lager)) lager <- alla_lager
  # grannlander är ett frivilligt lager (skapa_grannlander()) och ingår därför inte i "alla lager"
  if (intern_bakgrund_kalla(schema) == "topografi_1m") alla_lager <- c(alla_lager, "grannlander")
  okanda <- setdiff(lager, alla_lager)
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
    x <- sf::st_read(con, query = intern_bakgrund_urval_sql(schema, l, bbox), quiet = TRUE)
    # ett tomt utsnitt (t.ex. glaciärer i Dalarna) kommer utan koordinatreferens
    if (is.na(sf::st_crs(x))) sf::st_crs(x) <- 3006
    x
  }), lager)
}
