# Övriga GIS-hjälpfunktioner.

#' Skapa (eller fyll på) ett sf-objekt av koordinatpar
#'
#' @param koordinater Teckenvektor med `"lat, lon"`-strängar.
#' @param malpunktsnamn Namn för varje punkt (samma längd som `koordinater`).
#' @param sf_obj Befintligt `sf`-objekt att lägga till punkterna i (`NULL` =
#'   skapa nytt).
#' @param vald_crs Koordinatsystem (EPSG). Standard 4326 (WGS84).
#'
#' @return Ett `sf`-objekt med punkter (`id`, `malpunkt_namn`, `lat`, `lon`).
#' @export
skapa_punkt_sf_av_koordinatpar <- function(koordinater, malpunktsnamn, sf_obj = NULL,
                                           vald_crs = 4326) {
  intern_krav("sf")
  if (length(koordinater) != length(malpunktsnamn)) {
    stop("Antalet koordinater måste vara samma som antalet målpunktsnamn.", call. = FALSE)
  }
  matris <- do.call(rbind, lapply(koordinater, function(k) as.numeric(strsplit(k, ",\\s*")[[1]])))
  df <- data.frame(
    id = seq_along(koordinater),
    malpunkt_namn = malpunktsnamn,
    lat = matris[, 1],
    lon = matris[, 2],
    stringsAsFactors = FALSE
  )
  punkter <- sf::st_as_sf(df, coords = c("lon", "lat"), crs = vald_crs)
  if (is.null(sf_obj)) return(punkter)
  if (!inherits(sf_obj, "sf")) stop("'sf_obj' måste vara ett sf-objekt.", call. = FALSE)
  dplyr::bind_rows(sf_obj, punkter)
}

#' Städa folkbokföringsadresser inför join
#'
#' Lägger till kolumnen `adress_join`: gemener, mellanslag mellan siffror och
#' bokstäver, och lägenhets-/våningsangivelser (t.ex. `" 1tr"`, `" lgh"`)
#' avklippta.
#'
#' @param skickad_df En `data.frame` med kolumnen `adress`.
#' @return `skickad_df` med kolumnen `adress_join` tillagd.
#' @export
adresser_inv_reg_folke_bearbeta <- function(skickad_df) {
  ta_bort <- c(" nb", " bv", " uv", " 1tr", " 1 tr", ",1tr", ", 1 tr", " 2 tr", " 2tr",
               " van1", " van 1", " van2", " van 2", " van3", " van 3", " 3tr", " 3 tr",
               " 4tr", " 4 tr", " van0", " van 0", " 6tr", " 6 tr", " 1/2tr", " 1/2 tr",
               " 2:a", " lgh", " lag", ",")
  monster <- paste0(ta_bort, collapse = "|")

  df <- skickad_df
  aj <- tolower(df$adress)
  aj <- gsub("(?<=[a-z])(?=[0-9])", " ", aj, perl = TRUE)
  aj <- gsub("(?<=[0-9])(?=[a-z])", " ", aj, perl = TRUE)
  aj <- trimws(gsub("\\s+", " ", aj))
  pos <- regexpr(monster, aj)
  aj <- ifelse(pos > 0, substr(aj, 1, pos - 1), aj)
  df$adress_join <- trimws(aj)
  df
}

#' Extrahera fullständiga kolumnnamn per lager ur en ESRI-geodatabas
#'
#' `sf::st_read()` trunkerar långa kolumnnamn. Den här funktionen kör
#' `ogrinfo` (GDAL, ingår i QGIS) och returnerar namngivna vektorer som kan
#' användas för att döpa om ett inläst sf-objekt.
#'
#' @param gdb_sokvag Sökväg till `.gdb`-mappen.
#' @param tabort_paranteser,byt_ut_slash_mot_understreck,byt_ut_mellanslag_mot_understreck,enbart_gemener,byt_ut_svenska_tecken
#'   Städsteg för kolumnnamnen.
#' @param nytt_namn_id_kol,nytt_namn_geo_kol Nya namn för FID- resp.
#'   geometrikolumnen (`NA` = behåll).
#'
#' @return En namngiven lista: ett element per lager med en namngiven
#'   teckenvektor `trunkerat_namn = fullständigt_namn`.
#' @export
gdb_extrahera_kolumnnamn_per_gislager <- function(gdb_sokvag,
                                                  tabort_paranteser = TRUE,
                                                  byt_ut_slash_mot_understreck = TRUE,
                                                  byt_ut_mellanslag_mot_understreck = TRUE,
                                                  enbart_gemener = TRUE,
                                                  byt_ut_svenska_tecken = TRUE,
                                                  nytt_namn_id_kol = "nvdb_id",
                                                  nytt_namn_geo_kol = "geom") {
  intern_krav("sf")
  ogr <- Sys.which("ogrinfo")
  if (!nzchar(ogr)) {
    qgis_dirs <- Filter(function(d) startsWith(basename(d), "QGIS"),
                        list.dirs(Sys.getenv("ProgramFiles"), recursive = FALSE))
    kandidater <- file.path(qgis_dirs, "bin", "ogrinfo.exe")
    ogr <- kandidater[file.exists(kandidater)][1]
    if (is.na(ogr)) stop("GDAL-programmet 'ogrinfo' krävs (ingår i QGIS).", call. = FALSE)
  }

  asciifiera <- function(v) {
    if (requireNamespace("rdverktyg", quietly = TRUE)) rdverktyg::byt_ut_svenska_tecken(v)
    else chartr("åäöÅÄÖ", "aaoAAO", v)
  }

  lager <- sf::st_layers(gdb_sokvag)$name
  stats::setNames(lapply(lager, function(l) {
    rader <- system(sprintf('"%s" %s %s -so', ogr, gdb_sokvag, l), intern = TRUE)
    id_kol  <- stringr::str_extract(stringr::str_subset(rader, "FID Column"), "(?<= = ).*")
    geo_kol <- stringr::str_extract(stringr::str_subset(rader, "Geometry Column"), "(?<= = ).*")
    start   <- which(grepl("Geometry Column = ", rader)) + 1
    fält    <- c(paste0(id_kol, ":"), rader[start:length(rader)], paste0(geo_kol, ":"))

    namn_ny <- stats::setNames(
      vapply(fält, function(f) {
        gammalt <- stringr::str_extract(f, "^[^:]+")
        nytt <- stringr::str_trim(stringr::str_remove(
          stringr::str_replace_all(
            stringr::str_extract(f, 'alternative name=\\"([^"]+)\\"') %||% NA_character_,
            '\\"', ""), "alternative name="))
        v <- if (is.na(nytt)) gammalt else nytt
        if (tabort_paranteser) v <- stringr::str_trim(stringr::str_remove_all(v, "\\(|\\)"))
        if (byt_ut_slash_mot_understreck) v <- gsub("/", "_", v)
        if (byt_ut_mellanslag_mot_understreck) v <- gsub(" ", "_", v)
        if (enbart_gemener) v <- tolower(v)
        if (byt_ut_svenska_tecken) v <- asciifiera(v)
        v
      }, character(1)),
      vapply(fält, function(f) stringr::str_extract(f, "^[^:]+"), character(1))
    )
    if (!is.null(id_kol) && !is.na(id_kol) && !is.na(nytt_namn_id_kol)) namn_ny[id_kol] <- nytt_namn_id_kol
    if (!is.null(geo_kol) && !is.na(geo_kol) && !is.na(nytt_namn_geo_kol)) namn_ny[geo_kol] <- nytt_namn_geo_kol
    namn_ny
  }), lager)
}

#' Vektorisera ett rasterlager till polygoner
#'
#' @param rasterlager Ett `terra::SpatRaster`.
#' @param filtrera_bort_na Ta bort celler med `NA`.
#' @param behall_varden_over Behåll bara polygoner med värde större än detta
#'   (`NA` = ingen filtrering).
#'
#' @return Ett `sf`-objekt med en kolumn `varde`.
#' @export
raster_till_vektor <- function(rasterlager, filtrera_bort_na = TRUE, behall_varden_over = 0) {
  intern_krav("sf")
  intern_krav("terra")
  poly <- terra::as.polygons(rasterlager, dissolve = FALSE, na.rm = filtrera_bort_na)
  ut <- sf::st_as_sf(poly)
  names(ut)[1] <- "varde"
  if (!is.na(behall_varden_over)) ut <- ut[ut$varde > behall_varden_over, ]
  ut
}

#' Skriv ett polygonlager som en `.poly`-fil (för osmosis)
#'
#' @param sf_obj Ett `sf`-objekt (polygon). Transformeras till WGS84.
#' @param file Sökväg till `.poly`-filen som skapas.
#' @param name Polygonets namn i filen.
#'
#' @return Osynligt `file`.
#' @export
sf_to_poly <- function(sf_obj, file, name = "polygon") {
  intern_krav("sf")
  if (is.na(sf::st_crs(sf_obj)) || !identical(sf::st_crs(sf_obj)$epsg, 4326L)) {
    sf_obj <- sf::st_transform(sf_obj, 4326)
  }
  if (as.character(sf::st_geometry_type(sf_obj, by_geometry = FALSE)) == "MULTIPOLYGON") {
    delar <- sf::st_cast(sf_obj, "POLYGON")
    delar$.area <- as.numeric(sf::st_area(delar))
    sf_obj <- delar[which.max(delar$.area), ]
  }
  coords <- sf::st_coordinates(sf_obj)
  if (nrow(coords) == 0) stop("Inga koordinater i sf-objektet.", call. = FALSE)

  con <- file(file, "w")
  on.exit(close(con), add = TRUE)
  writeLines(c(name, "1",
               sprintf("%.6f %.6f", coords[, 1], coords[, 2]),
               sprintf("%.6f %.6f", coords[1, 1], coords[1, 2]),
               "END", "END"), con)
  message("Skrev polyfil: ", file)
  invisible(file)
}
