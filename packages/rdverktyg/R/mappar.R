# Konfigurerbara sökvägar - utbrutna ur func_API.R.
#
# Sätt egna värden i .Rprofile / .Renviron, t.ex.:
#   options(rdverktyg.utskriftsmapp = "D:/mina/utskrifter/")
# Standardvärdena är Region Dalarnas nätverkssökvägar och fungerar bara på
# datorer som når dem.

intern_mapp <- function(option_namn, standard) {
  getOption(option_namn, default = standard)
}

#' Sökväg till utskriftsmappen
#'
#' @return En sökväg (sträng). Sätt eget värde med
#'   `options(rdverktyg.utskriftsmapp = "...")`.
#' @export
utskriftsmapp <- function() {
  intern_mapp("rdverktyg.utskriftsmapp", "G:/Samhällsanalys/API/Fran_R/Utskrift/")
}

#' Sökväg till mappen för hämtad data
#'
#' @return En sökväg (sträng). Sätt eget värde med
#'   `options(rdverktyg.mapp_hamtadata = "...")`.
#' @export
mapp_hamtadata <- function() {
  intern_mapp("rdverktyg.mapp_hamtadata", "C:/gh/hamta_data/")
}

#' Sökväg till temp-mappen
#'
#' @return En sökväg (sträng). Sätt eget värde med
#'   `options(rdverktyg.mapp_temp = "...")`.
#' @export
mapp_temp <- function() {
  intern_mapp("rdverktyg.mapp_temp", "g:/skript/peter/temp/")
}

#' Sökväg till leveransmappen
#'
#' @return En sökväg (sträng). Sätt eget värde med
#'   `options(rdverktyg.mapp_leveranser = "...")`.
#' @export
mapp_leveranser <- function() {
  intern_mapp("rdverktyg.mapp_leveranser", "g:/Samhällsanalys/Leveranser/")
}

#' Sökväg till mappen med inläsningsdata från mikrodatabasen
#'
#' @return En sökväg (sträng). Sätt eget värde med
#'   `options(rdverktyg.mapp_inlasdata = "...")`.
#' @export
mapp_inlasdata <- function() {
  intern_mapp("rdverktyg.mapp_inlasdata", "g:/Samhällsanalys/Statistik/data_fran_mikrodb/")
}

#' Status för de konfigurerade sökvägarna
#'
#' Listar sökvägsfunktionerna, vilken sökväg de pekar på och om den finns.
#' Praktisk för att felsöka på en ny dator.
#'
#' @return En `tibble` med `funktion`, `sokvag` och `finns` (osynligt);
#'   skrivs också ut.
#' @export
rdverktyg_mappar_status <- function() {
  funktioner <- c("utskriftsmapp", "mapp_hamtadata", "mapp_temp",
                  "mapp_leveranser", "mapp_inlasdata")
  sokvagar <- vapply(funktioner, function(f) get(f)(), character(1))
  status <- tibble::tibble(
    funktion = funktioner,
    sokvag = unname(sokvagar),
    finns = dir.exists(unname(sokvagar))
  )
  print(status)
  invisible(status)
}
