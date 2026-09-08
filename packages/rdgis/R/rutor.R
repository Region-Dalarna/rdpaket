# Skapa sf-objekt av rutdata (SCB:s statistikrutor).

#' Gissa rutstorlek från koordinatvärden
#'
#' SCB:s rutkoordinater ligger i nedre vänstra hörnet. Storleken framgår av hur
#' värdena slutar.
#'
#' @param x,y Numeriska vektorer med koordinater.
#' @return `100`, `500`, `1000` eller `NA`.
#' @export
rutstorlek_estimera <- function(x, y) {
  coords <- as.numeric(c(x, y))
  if (any(coords %% 1000 %in% c(100, 200, 300, 400))) return(100)
  if (any(coords %% 1000 == 500) && any(coords %% 1000 == 0)) return(500)
  if (all(coords %% 1000 == 0)) return(1000)
  NA
}

# Punktgeometri (mittpunkt) från x/y-kolumner. Hanterar att kolumnerna kan vara
# omkastade (x har 7 tecken, y har 6).
intern_sf_fran_xy <- function(skickad_df, x_kol, y_kol, rutstorlek, vald_crs) {
  intern_krav("sf")
  df <- skickad_df
  x <- as.character(df[[x_kol]])
  y <- as.character(df[[y_kol]])
  omkastad <- nchar(x) == 7 & nchar(y) == 6
  x_bas <- ifelse(omkastad, y, x)
  y_bas <- ifelse(omkastad, x, y)
  df$.x_ny <- as.numeric(x_bas) + rutstorlek / 2
  df$.y_ny <- as.numeric(y_bas) + rutstorlek / 2
  sf::st_cast(sf::st_as_sf(df, coords = c(".x_ny", ".y_ny"), crs = vald_crs), "POINT")
}

#' Skapa ett sf-objekt från en data.frame med x- och y-kolumner
#'
#' @param skickad_df En `data.frame`.
#' @param x_kol,y_kol Namn på koordinatkolumnerna (nedre vänstra hörnet).
#' @param rutstorlek Rutstorlek i meter (`NA` = gissa med
#'   [rutstorlek_estimera()]).
#' @param polygonlager `TRUE` = kvadratiska cellpolygoner, `FALSE` = punkter.
#' @param vald_crs Koordinatsystem (EPSG). Standard 3006 (SWEREF99 TM).
#'
#' @return Ett `sf`-objekt.
#' @export
sf_fran_df_med_x_y_kol <- function(skickad_df, x_kol, y_kol, rutstorlek = NA,
                                   polygonlager = TRUE, vald_crs = 3006) {
  if (is.na(rutstorlek)) rutstorlek <- rutstorlek_estimera(skickad_df[[x_kol]], skickad_df[[y_kol]])
  ut <- intern_sf_fran_xy(skickad_df, x_kol, y_kol, rutstorlek, vald_crs)
  if (polygonlager) ut <- sf::st_buffer(ut, rutstorlek / 2, endCapStyle = "SQUARE")
  ut
}

#' Beräkna mittpunktskoordinater för rutor
#'
#' @param df En `data.frame` med x- och y-kolumner (nedre vänstra hörnet).
#' @param xruta,yruta Namn på koordinatkolumnerna.
#' @param rutstorlek Rutstorlek i meter.
#' @param xkolnamn,ykolnamn Namn på de nya mittpunktskolumnerna.
#'
#' @return `df` med två nya kolumner, placerade efter `xruta`/`yruta`.
#' @export
berakna_mittpunkter <- function(df, xruta, yruta, rutstorlek,
                                xkolnamn = "mitt_x", ykolnamn = "mitt_y") {
  df[[xkolnamn]] <- df[[xruta]] + rutstorlek / 2
  df[[ykolnamn]] <- df[[yruta]] + rutstorlek / 2
  dplyr::relocate(df, dplyr::all_of(c(xkolnamn, ykolnamn)), .after = dplyr::all_of(yruta))
}

#' Skapa ett sf-objekt av rutceller från en fil med en rutid-kolumn
#'
#' Läser en csv- eller Excel-fil (via `rio`) där en kolumn innehåller SCB:s
#' rut-id (koordinat i nedre vänstra hörnet, format `XXXXXXYYYYYYY`). Härleder
#' mittpunkt och returnerar punkter eller kvadratiska cellpolygoner. Ersätter de
#' tidigare Supercross-specifika inläsningsfunktionerna.
#'
#' @param fil_med_sokvag Sökväg till csv/xlsx-filen.
#' @param rutid_kol Namn på rutid-kolumnen (`NULL` = gissa: enda kolumnen med
#'   "rut" i namnet, annars första kolumnen).
#' @param rutstorlek Rutstorlek i meter (`NULL` = gissa från tecken 11 i
#'   rut-id).
#' @param polygonlager `TRUE` = cellpolygoner, `FALSE` = punkter.
#' @param vald_crs Koordinatsystem (EPSG). Standard 3006.
#'
#' @return Ett `sf`-objekt.
#' @export
sf_fran_rutid_fil <- function(fil_med_sokvag, rutid_kol = NULL, rutstorlek = NULL,
                              polygonlager = TRUE, vald_crs = 3006) {
  intern_krav("sf")
  intern_krav("rio")
  df <- rio::import(fil_med_sokvag)

  if (is.null(rutid_kol)) {
    rut_traff <- which(grepl("rut", tolower(names(df))))
    rutid_kol <- if (length(rut_traff) == 1) names(df)[rut_traff] else names(df)[1]
  }
  rutid <- as.character(df[[rutid_kol]])

  if (is.null(rutstorlek)) {
    tecken11 <- substr(rutid, 11, 11)
    rutstorlek <- if (all(tecken11 == "0")) 1000
    else if (all(tecken11 %in% c("0", "5"))) 500
    else if (any(tecken11 %in% as.character(1:9))) 100
    else stop("Kunde inte gissa rutstorlek - ange 'rutstorlek'.", call. = FALSE)
  }

  df$rutid <- rutid
  df$.x_koord <- as.character(as.numeric(substr(rutid, 1, 6)) + rutstorlek / 2)
  df$.y_koord <- as.character(as.numeric(substr(rutid, 7, 13)) + rutstorlek / 2)

  punkter <- sf::st_as_sf(df, coords = c(".x_koord", ".y_koord"), crs = vald_crs)
  if (polygonlager) sf::st_buffer(punkter, rutstorlek / 2, endCapStyle = "SQUARE") else punkter
}
