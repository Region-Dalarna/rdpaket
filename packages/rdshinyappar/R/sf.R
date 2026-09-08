#' Konvertera en data.frame med EWKB-geometri till ett sf-objekt
#'
#' När en PostGIS-tabell läses via `dbplyr` (`tbl() |> collect()`) kommer
#' geometrikolumnen tillbaka som EWKB-blob utan geografi. Den här funktionen
#' tolkar kolumnen och gör om ramen till ett `sf`-objekt, så att kedjan
#' `tbl() |> collect() |> df_till_sf()` fungerar smidigt.
#'
#' @param df En `data.frame` med en geometrikolumn i EWKB-format.
#' @param geom_col Namn på geometrikolumnen.
#' @param crs Koordinatsystem (EPSG-kod) att sätta på resultatet.
#'
#' @return Ett `sf`-objekt.
#' @export
df_till_sf <- function(df, geom_col = "geometry", crs = 3006) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Paketet 'sf' krävs för df_till_sf().", call. = FALSE)
  }

  df_sf <- df
  df_sf[[geom_col]] <- sf::st_as_sfc(df[[geom_col]], EWKB = TRUE)
  df_sf <- sf::st_as_sf(df_sf, sf_column_name = geom_col)
  sf::st_crs(df_sf) <- crs
  df_sf
}
