#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data :=
## usethis namespace: end
NULL

# Hjälpare: kräver ett Suggests-paket.
intern_krav <- function(paket) {
  if (!requireNamespace(paket, quietly = TRUE)) {
    stop("Paketet '", paket, "' krävs för den här funktionen.", call. = FALSE)
  }
}

utils::globalVariables(c("st_area", "row.id", "col.id", "varde", "area", "."))
