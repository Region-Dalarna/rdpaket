#' Ungefärliga relativa lägen för Dalarnas kommuner
#'
#' Koordinater (0-100, y uppåt) för Dalarnas 15 kommuner, avlästa från
#' referensbilden dalarna_kommuner_aug2024. Används som standard-`layout_tabell`
#' i [skapa_packed_circles()] för `layout = "geo"`, `"repel"` och `"vinkel"`.
#'
#' @format En `data.frame` med 15 rader och 3 kolumner:
#' \describe{
#'   \item{grupp}{Kommunnamn (med svenska tecken).}
#'   \item{gx}{X-koordinat 0-100 (ökar åt höger).}
#'   \item{gy}{Y-koordinat 0-100 (ökar uppåt).}
#' }
"dalarna_layout"
