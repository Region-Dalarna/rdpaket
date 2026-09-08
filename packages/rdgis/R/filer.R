# Läsa GIS-filer ur zip-arkiv.

#' Läs en GIS-fil ur en zip-fil på en URL
#'
#' Laddar ner zip-filen, packar upp den och läser den med `sf::st_read()`.
#'
#' @param skickad_url URL till zip-filen.
#' @return Ett `sf`-objekt.
#' @export
las_gisfil_fran_zipfil_via_url <- function(skickad_url) {
  intern_krav("sf")
  zip_fil <- tempfile(fileext = ".zip")
  utils::download.file(url = skickad_url, destfile = zip_fil, mode = "wb", quiet = TRUE)
  las_gisfil_fran_zipfil_via_sokvag(zip_fil)
}

#' Läs en GIS-fil ur en lokal zip-fil
#'
#' @param skickad_sokvag Sökväg till zip-filen.
#' @return Ett `sf`-objekt.
#' @export
las_gisfil_fran_zipfil_via_sokvag <- function(skickad_sokvag) {
  intern_krav("sf")
  ut_mapp <- tempfile()
  utils::unzip(skickad_sokvag, exdir = ut_mapp)
  sf::st_read(ut_mapp, quiet = TRUE)
}

#' Packa upp en zip-fil som själv innehåller zip-filer
#'
#' Används t.ex. för FA-/LA-regioner och läns-/kommungränser med kustlinje, som
#' distribueras som zip-filer i en zip-fil.
#'
#' @param skickad_url URL till den yttre zip-filen.
#' @return En teckenvektor med sökvägar till de uppackade (inre) zip-filerna.
#' @export
unzip_zipfil_med_zipfiler <- function(skickad_url) {
  yttre <- tempfile(fileext = ".zip")
  if (requireNamespace("curl", quietly = TRUE)) {
    curl::curl_download(skickad_url, yttre)
  } else {
    utils::download.file(skickad_url, yttre, mode = "wb", quiet = TRUE)
  }
  ut_mapp <- tempfile()
  dir.create(ut_mapp)
  utils::unzip(yttre, exdir = ut_mapp)
  list.files(ut_mapp, full.names = TRUE, pattern = "\\.zip$")
}
