# Gamla namn från när analysrapporterna hette webbrapporter. Fungerar som förut men varnar.

#' Gamla namn för analysrapport-funktionerna
#'
#' Analysrapporterna hette tidigare webbrapporter. Funktionerna med de gamla
#' namnen fungerar som förut men varnar och tas bort senare.
#'
#' @param ... Skickas vidare till den nya funktionen.
#' @return Som den nya funktionen.
#' @seealso [analysrapport_publicera()], [analysrapport_avpublicera()],
#'   [skapa_analysrapport_github()]
#' @name webbrapport_gamla_namn
#' @keywords internal
NULL

#' @rdname webbrapport_gamla_namn
#' @export
webbrapport_publicera <- function(...) {
  .Deprecated("analysrapport_publicera", package = "rddeploy")
  analysrapport_publicera(...)
}

#' @rdname webbrapport_gamla_namn
#' @export
webbrapport_avpublicera <- function(...) {
  .Deprecated("analysrapport_avpublicera", package = "rddeploy")
  analysrapport_avpublicera(...)
}

#' @rdname webbrapport_gamla_namn
#' @export
skapa_webbrapport_github <- function(...) {
  .Deprecated("skapa_analysrapport_github", package = "rddeploy")
  skapa_analysrapport_github(...)
}
