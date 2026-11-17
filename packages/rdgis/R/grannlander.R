# Grannländer till bakgrundskartan över Sverige. Topografi 1M har ingen landyta utanför Sverige (grannländer och
# havet utanför territorialvattnet är samma yta, "Ej karterat område"), så landytorna hämtas ur Natural Earth.

#' @noRd
intern_grannlander_url <- function() {
  "https://naturalearth.s3.amazonaws.com/10m_cultural/ne_10m_admin_0_countries.zip"
}

# Läser Natural Earths länder, tar bort Sverige och beskär till Norden med omnejd innan ytorna projiceras till
# SWEREF 99 TM (hela världen går inte att projicera). Ger en sf-kolumn `geom`.
#' @noRd
intern_grannlander_sf <- function(fil) {
  intern_krav("sf")
  x <- sf::st_read(paste0("/vsizip/", fil), quiet = TRUE)
  x <- x[!(x$ADM0_A3 %in% "SWE"), ]
  ruta <- sf::st_as_sfc(sf::st_bbox(c(xmin = -5, ymin = 50, xmax = 45, ymax = 74), crs = sf::st_crs(4326)))
  g <- suppressWarnings(sf::st_intersection(sf::st_make_valid(sf::st_geometry(x)), ruta))
  g <- g[!sf::st_is_empty(g)]
  g <- sf::st_transform(suppressWarnings(sf::st_collection_extract(sf::st_make_valid(g), "POLYGON")), 3006)
  sf::st_sf(geom = sf::st_union(g))
}

#' Bygg lagret grannländer till bakgrundskartan över Sverige
#'
#' Lägger landytorna för Sveriges grannländer (Norge, Finland, Danmark, Tyskland m.fl.) som lagret
#' `grannlander` i schemat `bakgrundskarta_sverige`. Topografi 1M har inga landytor utanför Sverige, så ytorna
#' hämtas ur Natural Earth (10 m, public domain, ca 5 MB). Lagret begränsas till området utanför Sverige som
#' Topografi 1M ändå täcker (lagret `utland`), så det kan inte överlappa svenskt land. Kör
#' [skapa_bakgrundskarta_topografi()] först, och om lagret `utland` byggs om bör det här lagret byggas om efter.
#' Kartfunktionerna använder lagret om det finns och ritar annars utlandet som en enda yta.
#'
#' @param con En administratörsuppkoppling till `geodata`, eller `NULL` (standard) för en tillfällig.
#' @param fil Sökväg till en redan nedladdad `ne_10m_admin_0_countries.zip`. `NULL` (standard) laddar ner filen
#'   från Natural Earth.
#' @param schema Schemat. Standard `"bakgrundskarta_sverige"`.
#'
#' @return Antal ytor i lagret. Osynligt.
#' @seealso [skapa_bakgrundskarta_topografi()], [hamta_bakgrundskarta()]
#' @export
skapa_grannlander <- function(con = NULL, fil = NULL, schema = "bakgrundskarta_sverige") {
  intern_krav("sf")
  if (is.null(fil)) {
    fil <- tempfile(fileext = ".zip")
    on.exit(unlink(fil), add = TRUE)
    message("Laddar ner Natural Earth (ne_10m_admin_0_countries.zip, ca 5 MB) ...")
    utils::download.file(intern_grannlander_url(), fil, mode = "wb", quiet = TRUE)
  }
  if (!file.exists(fil)) stop("Hittar inte filen: ", fil, call. = FALSE)
  egen <- is.null(con)
  if (egen) {
    con <- rdpostgres::uppkoppling_adm("geodata")
    if (is.null(con)) stop("Kunde inte ansluta till geodatabasen.", call. = FALSE)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }
  if (!bakgrundskarta_finns(schema = schema, lager = "utland", con = con, tyst = TRUE)) {
    stop("Lagret 'utland' saknas i schemat '", schema, "'. Bygg bakgrundskartan med ",
         "skapa_bakgrundskarta_topografi(kalla_schema = \"topografi_1m\") först.", call. = FALSE)
  }

  grannar <- intern_grannlander_sf(fil)
  tmp <- "grannlander_ne"
  message("Bygger ", schema, ".grannlander ...")
  DBI::dbExecute(con, "SET client_min_messages TO warning")
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, glue::glue("DROP TABLE IF EXISTS {schema}.{tmp}"))
    sf::st_write(grannar, con, DBI::Id(schema = schema, table = tmp), quiet = TRUE)
    DBI::dbExecute(con, glue::glue("DROP TABLE IF EXISTS {schema}.grannlander"))
    # 2 km buffert täcker glipor mot Topografi 1M:s gräns; beskärningen mot utland ger att inget hamnar över svenskt land
    DBI::dbExecute(con, glue::glue("
      CREATE TABLE {schema}.grannlander AS
      SELECT (d).geom::geometry(Polygon, 3006) AS geom
      FROM (SELECT ST_Dump(ST_CollectionExtract(ST_MakeValid(ST_Intersection(
                     ST_Buffer(ST_MakeValid(n.geom), 2000), u.geom)), 3)) AS d
            FROM {schema}.{tmp} n,
                 (SELECT ST_UnaryUnion(ST_Collect(ST_MakeValid(geom))) AS geom FROM {schema}.utland) u) x"))
    DBI::dbExecute(con, glue::glue("CREATE INDEX grannlander_geom_idx ON {schema}.grannlander USING gist (geom)"))
    DBI::dbExecute(con, glue::glue("COMMENT ON TABLE {schema}.grannlander IS 'Genererad {Sys.Date()} ur Natural Earth 10m och {schema}.utland'"))
    DBI::dbExecute(con, glue::glue("DROP TABLE {schema}.{tmp}"))
  })
  DBI::dbExecute(con, glue::glue("ANALYZE {schema}.grannlander"))
  n <- DBI::dbGetQuery(con, glue::glue("SELECT count(*)::int AS n FROM {schema}.grannlander"))$n
  message("  klar, ", n, " ytor")
  invisible(n)
}
