# Hämta kartdata ur geodatabasens karta-schema.

#' Tabell över kartlager som går att hämta med `hamta_karta()`
#'
#' @return En `data.frame` med `namn`, `id_kol`, `lankol`, `kommunkol` och
#'   `sokord` (listkolumn).
#' @export
hamta_karttabell <- function() {
  tibble::tribble(
    ~namn,            ~id_kol,        ~lankol,       ~kommunkol,    ~sokord,
    "kommun_scb",     "knkod",        "lanskod_tx",  "knkod",       c("kommun", "kommuner", "kommunpolygoner"),
    "kommun_lm",      "kommunkod",    "lankod",      "kommunkod",   c("kommun_lm", "kommuner_lm", "kommunpolygoner_lm"),
    "lan_scb",        "lnkod",        "lnkod",       NA_character_, c("lan", "lanspolygoner"),
    "lan_lm",         "lankod",       "lankod",      NA_character_, c("lan_lm", "lanspolygoner_lm"),
    "rike_lm",        "landskod",     NA_character_, NA_character_, c("rike_lm", "rike", "riket", "Sverige"),
    "tatorter",       "tatortskod",   "lan",         "kommun",      c("tatort", "tatorter", "tatortspolygoner"),
    "tatortspunkter", "tatortskod",   "lan",         "kommun",      c("tatortspunkter"),
    "regso",          "regsokod",     "lanskod",     "kommunkod",   c("regso", "regsopolygoner"),
    "deso",           "deso",         "lanskod",     "kommunkod",   c("deso", "desopolygoner"),
    "distrikt",       "distriktskod", "lankod",      "kommunkod",   c("distrikt"),
    "nuts2",          "nuts_id",      "cntr_code",   "geo",         c("nuts2", "nuts2-omraden"),
    "nuts3",          "nuts_id",      "cntr_code",   "geo",         c("nuts3", "nuts3-omraden"),
    "laregion_scb",   "lakod",        "lan",         "kommun",      c("la", "laomraden", "la-omraden"),
    "varlden",        "Landskod",     NA_character_, NA_character_, c("varlden", "varldskarta"),
    "varldsdelar",    "Landskod",     NA_character_, NA_character_, c("varldsdelskarta", "varldsdelar")
  ) |> as.data.frame()
}

#' Läs en PostGIS-tabell direkt till ett sf-objekt
#'
#' @param con En `DBIConnection` eller `"default"` (använder
#'   `rdpostgres::uppkoppling_db()`).
#' @param schema,tabell Schema och tabell.
#' @param query Valfri SQL-svans efter `SELECT * FROM schema.tabell`.
#'
#' @return Ett `sf`-objekt.
#' @export
postgis_postgistabell_till_sf <- function(con = "default", schema, tabell, query = NA) {
  intern_krav("sf")
  egen <- is.character(con) && length(con) == 1 && con == "default"
  if (egen) {
    con <- rdpostgres::uppkoppling_db()
    on.exit(if (!is.null(con) && DBI::dbIsValid(con)) DBI::dbDisconnect(con), add = TRUE)
  }
  sql <- paste0("SELECT * FROM ", schema, ".", tabell,
                if (!is.na(query)) paste0(" ", query) else "")
  sf::st_read(con, query = sql, quiet = TRUE)
}

#' Hämta ett kartlager ur geodatabasens `karta`-schema
#'
#' @param karttyp Ett sökord ur [hamta_karttabell()], t.ex. `"kommun"`,
#'   `"lan"`, `"deso"`, `"tatorter"`.
#' @param regionkoder Vektor av läns- och/eller kommunkoder att begränsa till
#'   (`NA` = hela landet).
#'
#' @return Ett `sf`-objekt, eller `NULL` med en varning om karttypen inte finns.
#' @export
hamta_karta <- function(karttyp = "kommuner", regionkoder = NA) {
  tab <- hamta_karttabell()
  rad <- suppressWarnings(which(vapply(tab$sokord, function(s) karttyp %in% s, logical(1))))
  if (length(rad) == 0) {
    warning("Karttypen '", karttyp, "' finns inte i databasen.", call. = FALSE)
    return(NULL)
  }
  rad <- rad[1]
  pg_tabell <- tab$namn[rad]

  skickad_query <- NA
  if (all(!is.na(regionkoder)) && all(regionkoder != "00")) {
    lanskoder   <- regionkoder[nchar(regionkoder) == 2 & regionkoder != "00"]
    kommunkoder <- regionkoder[nchar(regionkoder) == 4]

    villkor <- character()
    if (length(lanskoder) > 0 && !is.na(tab$lankol[rad])) {
      villkor <- c(villkor, paste0(tab$lankol[rad], " IN (",
                                   paste0("'", lanskoder, "'", collapse = ", "), ")"))
    }
    if (length(kommunkoder) > 0 && !is.na(tab$kommunkol[rad])) {
      villkor <- c(villkor, paste0(tab$kommunkol[rad], " IN (",
                                   paste0("'", kommunkoder, "'", collapse = ", "), ")"))
    }
    if (length(villkor) > 0) skickad_query <- paste0("WHERE ", paste(villkor, collapse = " OR "))
  }

  postgis_postgistabell_till_sf(schema = "karta", tabell = pg_tabell, query = skickad_query)
}

#' Koppla ett dataset till rätt kartlager utifrån regionkodernas längd
#'
#' Gissar karttyp (län/kommun/nuts2/deso/regso/tätort) från längden och formen
#' på värdena i `geom_nyckel` och gör en `left_join` mot kartlagret.
#'
#' @param skickad_df En `data.frame` med en kolumn med regionkoder.
#' @param geom_nyckel Namnet på regionkodskolumnen.
#' @param tatortspunkter För tätorter: `TRUE` ger punkter, `FALSE` polygoner.
#'
#' @return Ett `sf`-objekt.
#' @export
kartifiera <- function(skickad_df, geom_nyckel, tatortspunkter = TRUE) {
  intern_krav("sf")
  koder <- unique(skickad_df[[geom_nyckel]])
  langder <- unique(nchar(koder))
  if (length(langder) > 1) {
    stop("Kolumnen '", geom_nyckel, "' innehåller koder av olika längd.", call. = FALSE)
  }

  karttyp <- if (langder == 2) {
    "lan"
  } else if (langder == 4 && all(grepl("^[0-9]+$", koder))) {
    "kommun"
  } else if (langder == 4) {
    "nuts2"
  } else if (langder == 9 && all(substr(koder, 5, 5) %in% c("A", "B", "C"))) {
    "deso"
  } else if (langder == 8 && all(substr(koder, 5, 5) == "R")) {
    "regso"
  } else if (langder == 9 && all(substr(koder, 5, 5) == "T")) {
    if (tatortspunkter) "tatortspunkter" else "tatorter"
  } else {
    stop("Kunde inte avgöra karttyp från koderna i '", geom_nyckel, "'.", call. = FALSE)
  }

  gis_lager <- hamta_karta(karttyp = karttyp, regionkoder = koder)
  tab <- hamta_karttabell()
  rad <- which(vapply(tab$sokord, function(s) karttyp %in% s, logical(1)))[1]
  id_kol <- tab$id_kol[rad]

  join <- dplyr::left_join(skickad_df, gis_lager,
                           by = stats::setNames(id_kol, geom_nyckel))
  sf::st_as_sf(join)
}
