#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data
#' @importFrom utils globalVariables
## usethis namespace: end
NULL

# Kolumnnamn i data masking (dplyr/ggplot) + paketdata som ser ut som
# odefinierade globala variabler för R CMD check.
globalVariables(c(
  "dalarna_layout",
  ".antal", ".bransch", ".farg", ".gid", ".grupp", ".lbl", ".uid",
  "lbl", "x", "x0", "y", "y0"
))
