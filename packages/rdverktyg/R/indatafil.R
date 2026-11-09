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
#' @param kalla Sökväg, mapp eller url till den manuellt underhållna
#'   källan - skrivs ut i meddelandet.
#' @return `NULL` (osynligt). Funktionen anropas för sin konsol-utskrift.
#' @export
meddela_manuell_datakalla <- function(kalla) {
  message(
    "Data hämtas från en manuellt underhållen källa:\n  ", kalla,
    "\nDetta är INTE en live-hämtning mot ett API eller en automatisk ",
    "generering - innehållet är en ögonblicksbild som kan vara inaktuell ",
    "och måste uppdateras manuellt vid behov."
  )
}
