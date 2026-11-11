# Delad pendlingsdata för karta_lf_pendlingsfloden_scb() och karta_gg_nettopendling_kommun_scb():
# pendling mellan kommuner (inte inom kommunen) för de angivna kommunerna, ett år. Alla pendlare
# till och från kommunerna hämtas, alltså även relationer till kommuner utanför urvalet.
#' @noRd
intern_hamta_pendling <- function(kommunkoder, valt_ar = "9999") {
  rdverktyg::funktion_upprepa_forsok_om_fel(function()
    rdhamtadata::hamta_pendling_rams_bas_scb(region_vekt = kommunkoder, tid_vekt = valt_ar)
  ) |>
    dplyr::filter(nchar(regionkod_bo) == 4, nchar(regionkod_arb) == 4,
                  regionkod_bo != regionkod_arb,
                  pendlare > 0)
}
