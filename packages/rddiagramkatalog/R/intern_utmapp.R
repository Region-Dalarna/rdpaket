# Validerar utmapp innan ett diagram skrivs till fil - se diag_*()-funktionernas
# gemensamma parametrar utmapp/skriv_diagramfil. rdverktyg::utskriftsmapp() kastar
# aldrig fel och returnerar aldrig NA om inget är konfigurerat via
# options(rdverktyg.utskriftsmapp = ...) - den faller tillbaka på en hårdkodad
# nätverkssökväg som bara finns på vissa datorer, så kontrollen måste vara
# dir.exists(), inte is.na().
#' @noRd
intern_sakerstall_utmapp <- function(utmapp, skriv_diagramfil) {
  if (!isTRUE(skriv_diagramfil)) {
    return(utmapp)
  }
  if (is.na(utmapp) || !dir.exists(utmapp)) {
    stop(
      "Mappen '", utmapp, "' finns inte, men skriv_diagramfil = TRUE.\n",
      "Ange en giltig mapp via parametern utmapp, eller kontrollera standardmappen:\n",
      "  rdverktyg::utskriftsmapp()\n",
      "Sätt en egen standardmapp för sessionen (eller permanent i .Rprofile) med:\n",
      "  options(rdverktyg.utskriftsmapp = \"sökväg/till/mapp/\")",
      call. = FALSE
    )
  }
  utmapp
}
