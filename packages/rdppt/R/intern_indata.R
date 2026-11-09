# Skriver ett tydligt meddelande till konsolen om att en fil kommer från en manuellt underhållen
# plats (t.ex. en mallfil på G:) i stället för att hämtas/genereras automatiskt - och att innehållet
# därför inte är garanterat aktuellt. Samma konvention som används i rddiagramkatalog/rdhamtadata.
intern_meddela_manuell_datakalla <- function(kalla) {
  message(
    "Data/mall hämtas från en manuellt underhållen fil:\n  ", kalla,
    "\nDetta är INTE något som genereras automatiskt - innehållet kan ha ändrats ",
    "eller flyttats sedan senast."
  )
}

# Validerar att en specifik fil (hel sökväg inklusive filnamn, t.ex. en pptx-mallfil på G:) finns,
# med ett tydligt, handlingsbart felmeddelande om den saknas - i stället för att låta officer::
# read_pptx() ge ett kryptiskt "cannot open file"-fel längre ned.
intern_sakerstall_indatafil <- function(sokvag) {
  if (!file.exists(sokvag)) {
    stop(
      "Hittar inte den förväntade filen:\n  ", sokvag,
      "\nKontrollera att filen finns på den sökvägen, eller att den inte ",
      "har flyttats eller bytt namn.",
      call. = FALSE
    )
  }
  intern_meddela_manuell_datakalla(sokvag)
  invisible(sokvag)
}
