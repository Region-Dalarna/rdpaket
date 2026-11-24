# Flyttat hit från rddiagramkatalog/rdppt (där samma två funktioner fanns
# duplicerade, interna och oexporterade, i varje paket) - delat verktyg för
# att validera och flagga filer/mallar som kommer från en manuellt
# underhållen källa (G:, en delad fil) i stället för en live-hämtning eller
# automatisk generering.

#' Validera att en förväntad indatafil finns
#'
#' Kontrollerar att `sokvag` finns, med ett tydligt, handlingsbart
#' felmeddelande om den saknas eller har flyttats/bytt namn - i stället för
#' att låta ett kryptiskt "cannot open file"-fel dyka upp långt senare när
#' filen faktiskt läses. Skriver samtidigt ut en konsolvarning via
#' [meddela_manuell_datakalla()] om att filen är en manuellt underhållen
#' källa.
#'
#' @param sokvag Hel sökväg (inklusive filnamn) till filen som ska
#'   valideras.
#' @return `sokvag` (osynligt), för att kunna användas direkt i en pipe.
#' @export
sakerstall_indatafil <- function(sokvag) {
  if (!file.exists(sokvag)) {
    stop(
      "Hittar inte den förväntade filen:\n  ", sokvag,
      "\nKontrollera att filen finns på den sökvägen, eller att den inte ",
      "har flyttats eller bytt namn.",
      call. = FALSE
    )
  }
  meddela_manuell_datakalla(sokvag)
  invisible(sokvag)
}

#' Varna om att data/en mall kommer från en manuellt underhållen källa
#'
#' Skriver ett tydligt meddelande till konsolen om att `kalla` (en fil,
#' mapp eller url) är manuellt underhållen i stället för att hämtas live
#' eller genereras automatiskt - och att innehållet därför inte är
#' garanterat aktuellt. Används av funktioner som läser in en sådan källa,
#' så att den som kör funktionen alltid får samma tydliga varning.
#'
#' Meddelandet kan stängas av med `options(rdverktyg.visa_manuell_datakalla = FALSE)`
#' (eller [visa_manuell_datakalla_info()]), t.ex. överst i ett skript som
#' skapar en presentation och där du vet vilka källor som används.
#' Standard är att meddelandet visas.
#'
#' @param kalla Sökväg, mapp eller url till den manuellt underhållna
#'   källan - skrivs ut i meddelandet.
#' @param visa `TRUE` visar meddelandet, `FALSE` döljer det. Standard följer
#'   `options(rdverktyg.visa_manuell_datakalla)`, som är `TRUE` om den inte är satt.
#' @return `NULL` (osynligt). Funktionen anropas för sin konsol-utskrift.
#' @seealso [visa_manuell_datakalla_info()]
#' @export
meddela_manuell_datakalla <- function(kalla, visa = getOption("rdverktyg.visa_manuell_datakalla", TRUE)) {
  if (!isTRUE(visa)) return(invisible(NULL))
  message(
    "Data hämtas från en manuellt underhållen källa:\n  ", kalla,
    "\nDetta är INTE en live-hämtning mot ett API eller en automatisk ",
    "generering - innehållet är en ögonblicksbild som kan vara inaktuell ",
    "och måste uppdateras manuellt vid behov."
  )
  invisible(NULL)
}

#' Slå på eller av meddelandet om manuellt underhållna datakällor
#'
#' Sätter `options(rdverktyg.visa_manuell_datakalla)`, som styr om
#' [meddela_manuell_datakalla()] (och därmed alla `diag_*()`-funktioner och
#' datahämtare som läser en manuellt underhållen fil eller mapp) skriver ut
#' sitt meddelande. Meddelandet är påslaget som standard.
#'
#' @param visa `TRUE` (standard) visar meddelandet, `FALSE` döljer det.
#' @return Det tidigare värdet (osynligt), som kan skickas in igen för att återställa.
#' @examples
#' \dontrun{
#' visa_manuell_datakalla_info(FALSE)   # renare konsol i ett ppt-skript
#' }
#' @export
visa_manuell_datakalla_info <- function(visa = TRUE) {
  invisible(options(rdverktyg.visa_manuell_datakalla = isTRUE(visa))[[1]])
}
