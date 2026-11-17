#' Bygg om alla bakgrundskartor i geodatabasen
#'
#' Bygger om bakgrundskartorna ur de topografiska källorna i geodatabasen: `bakgrundskarta_dalarna` ur
#' `topografi_10`, `bakgrundskarta_sverige` ur `topografi_1m` och därefter grannländerna (lagret `grannlander`,
#' Natural Earth). Avsedd att köras i det schemalagda skriptet efter att `topografi_10` och `topografi_1m`
#' har uppdaterats. Varje del byggs om helt; ordningen är viktig eftersom grannländerna beskärs mot lagret
#' `utland` i Sverige-kartan.
#'
#' @param con En administratörsuppkoppling till `geodata`, eller `NULL` (standard) för en tillfällig.
#' @param dalarna,sverige,grannlander `FALSE` hoppar över respektive del. Alla `TRUE` som standard.
#'
#' @return Osynligt: en lista med lagerstatistik per schema.
#' @seealso [skapa_bakgrundskarta_topografi()], [skapa_grannlander()]
#' @export
uppdatera_bakgrundskartor <- function(con = NULL, dalarna = TRUE, sverige = TRUE, grannlander = TRUE) {
  if (is.null(con)) {
    con <- rdpostgres::uppkoppling_adm("geodata")
    if (is.null(con)) stop("Kunde inte ansluta till geodatabasen.", call. = FALSE)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }
  res <- list()
  if (dalarna) {
    message("== Dalarna ur topografi_10 ==")
    res$bakgrundskarta_dalarna <- skapa_bakgrundskarta_topografi(con, kalla_schema = "topografi_10")
  }
  if (sverige) {
    message("== Sverige ur topografi_1m ==")
    res$bakgrundskarta_sverige <- skapa_bakgrundskarta_topografi(con, kalla_schema = "topografi_1m")
  }
  if (grannlander) {
    message("== Grannländer (Natural Earth) ==")
    res$grannlander <- skapa_grannlander(con)
  }
  invisible(res)
}
