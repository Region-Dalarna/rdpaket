#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rdverktyg list_komma_och
#' @importFrom rddiagram diagramfarger
#' @importFrom rdgis hamta_karttabell
#' @importFrom rdpostgres uppkoppling_db
#' @importFrom rdshinyappar shiny_get_password
## usethis namespace: end
NULL

.rd_paket <- c("rdverktyg", "rddiagram", "rdgis", "rdpostgres", "rdshinyappar")

.onAttach <- function(libname, pkgname) {
  laddade <- .rd_paket[vapply(.rd_paket, requireNamespace, logical(1), quietly = TRUE)]
  packageStartupMessage("rd: laddar ", paste(laddade, collapse = ", "))
}
