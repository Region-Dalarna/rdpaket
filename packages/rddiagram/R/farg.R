# Färg- och skalhjälpare - utbrutna ur func_API.R.

#' Skapa runda skalvärden till en storleksskala
#'
#' Ger `antal_skalcirklar` värden jämnt fördelade på arean (kvadratrots-skala)
#' mellan `min_varde` och `max_varde`, avrundade till runda tal.
#'
#' @param min_varde,max_varde Minsta och största värde.
#' @param antal_skalcirklar Antal skalvärden.
#'
#' @return En numerisk vektor med unika skalvärden.
#' @export
skalcirklar_skapa <- function(min_varde, max_varde, antal_skalcirklar) {
  cirklar <- seq(sqrt(min_varde), sqrt(max_varde), length.out = antal_skalcirklar)^2
  unique(round(cirklar, -floor(log10(max_varde)) + 1))
}

#' Hitta en kontrasterande färg för text på färgad yta
#'
#' Ljusar upp mörka färger och mörkar ned ljusa, så att text på ytan syns.
#'
#' @param farg_vektor_hex En eller flera hex-färger.
#' @param cutoff Luminansgräns (0-1) för att räknas som mörk.
#' @param justering Hur mycket luminansen justeras (0-1).
#'
#' @return En teckenvektor med hex-färger.
#' @export
kontrastfarg_hitta <- function(farg_vektor_hex, cutoff = 0.6, justering = 0.3) {
  col_hcl <- farver::decode_colour(farg_vektor_hex, to = "hcl")
  lum <- col_hcl[, "l"] / 100
  col_hcl[, "l"] <- ifelse(
    lum < cutoff,
    pmin(col_hcl[, "l"] + justering * 100, 100),
    pmax(col_hcl[, "l"] - justering * 100, 0)
  )
  farver::encode_colour(col_hcl, from = "hcl")
}
