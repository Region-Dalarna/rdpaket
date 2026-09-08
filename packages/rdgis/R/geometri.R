# Geometriska hjälpfunktioner.

#' Största ringen i varje (multi)polygon
#'
#' @param x Ett `sf`-objekt med polygoner.
#' @return Ett `sf`-objekt där varje rad ersatts med sin största delpolygon.
#' @seealso <https://github.com/r-spatial/sf/issues/1302>
#' @export
st_largest_ring <- function(x) {
  intern_krav("sf")
  if (nrow(x) < 1) return(x)
  delar <- lapply(seq_len(nrow(x)), function(i) {
    rad <- sf::st_cast(sf::st_set_agr(x[i, ], "identity"), "POLYGON")
    rad$.area <- as.numeric(sf::st_area(sf::st_geometry(rad)))
    rad <- rad[which.max(rad$.area), ]
    rad$.area <- NULL
    rad
  })
  do.call(rbind, delar)
}

#' Centroider som garanterat ligger inuti sin polygon
#'
#' @param x Ett `sf`-objekt med polygoner.
#' @param ensure_within Tvinga punkten att ligga innanför polygonen
#'   (`st_point_on_surface` när den geometriska centroiden hamnar utanför).
#' @param of_largest_polygon Använd den största delpolygonen för multipolygoner.
#'
#' @return Ett `sf`-objekt med punkter.
#' @seealso <https://github.com/r-spatial/sf/issues/1302>
#' @export
st_centroid_within_geo <- function(x, ensure_within = TRUE, of_largest_polygon = TRUE) {
  intern_krav("sf")
  cx <- sf::st_centroid(sf::st_set_agr(x, "identity"), of_largest_polygon = of_largest_polygon)

  if (ensure_within) {
    inom <- as.data.frame(sf::st_within(cx, x, sparse = TRUE))
    i_inom <- inom$row.id[inom$row.id == inom$col.id]
    i_utanfor <- setdiff(seq_len(nrow(x)), i_inom)
    if (length(i_utanfor) > 0) {
      bas <- if (of_largest_polygon) st_largest_ring(x[i_utanfor, ]) else x[i_utanfor, ]
      sf::st_geometry(cx[i_utanfor, ]) <- sf::st_point_on_surface(sf::st_geometry(bas))
    }
  }
  cx
}

#' Dra en linje längs en serie punkter i angiven ordning
#'
#' @param skickad_sf Ett `sf`-objekt med punkter.
#' @param kol_ord Namn på en numerisk kolumn som anger ordningen.
#' @param namn_kol Namn på kolumn med punktnamn (för etiketter).
#' @param namn_bara_startpunkt `TRUE` = etikett från startpunkten;
#'   `FALSE` = `"start - slut"`.
#'
#' @return Ett `sf`-objekt med linjesegment (`start`, `end`, `label`).
#' @export
skapa_linje_langs_med_punkter <- function(skickad_sf, kol_ord, namn_kol,
                                          namn_bara_startpunkt = TRUE) {
  intern_krav("sf")
  crs <- sf::st_crs(skickad_sf)
  sf_ord <- dplyr::arrange(skickad_sf, .data[[kol_ord]])

  segment <- lapply(seq_len(nrow(sf_ord) - 1), function(i) {
    par <- sf_ord[c(i, i + 1), ]
    if (sum(sf::st_coordinates(par[1, ])) < sum(sf::st_coordinates(par[2, ]))) {
      par <- par[c(2, 1), ]
    }
    linje <- sf::st_sfc(sf::st_linestring(sf::st_coordinates(par)), crs = crs)
    namn <- sf_ord[[namn_kol]]
    etikett <- if (namn_bara_startpunkt) namn[i] else paste(namn[i], "-", namn[i + 1])
    sf::st_sf(start = sf_ord[[kol_ord]][i], end = sf_ord[[kol_ord]][i + 1],
             label = etikett, geometry = linje)
  })
  do.call(rbind, segment)
}

#' Spatial join där punkter utanför områdeslagret får ett "övrigt"-värde
#'
#' @param gislager_grunddata `sf`-objekt att koda.
#' @param gislager_omr `sf`-objekt med områdespolygoner.
#' @param omrade_kol Namn på områdeskolumnen i `gislager_omr`.
#' @param ovrig_varde Värde för objekt som hamnar utanför alla områden.
#'
#' @return Ett `sf`-objekt.
#' @export
spatial_join_med_ovrkat <- function(gislager_grunddata, gislager_omr,
                                    omrade_kol = "omrade",
                                    ovrig_varde = "Övriga områden") {
  intern_krav("sf")
  ut <- sf::st_join(gislager_grunddata, gislager_omr)
  ut[[omrade_kol]] <- ifelse(is.na(ut[[omrade_kol]]), ovrig_varde, ut[[omrade_kol]])
  ut
}
