# Skriver ett tydligt meddelande till konsolen om att data kommer från en
# manuellt underhållen fil (lokal eller på GitHub) i stället för en
# live-hämtning mot ett API - och att innehållet därför inte är garanterat
# aktuellt. Används av alla diag_*()-funktioner som läser in en sådan fil,
# så att den som kör funktionen alltid får samma tydliga varning.
#' @noRd
intern_meddela_manuell_datakalla <- function(kalla) {
  message(
    "Data hämtas från en manuellt underhållen fil:\n  ", kalla,
    "\nDetta är INTE en live-hämtning mot ett API - innehållet är en ",
    "ögonblicksbild som kan vara inaktuell och måste uppdateras manuellt ",
    "vid behov."
  )
}

# Validerar att en specifik indatafil (hel sökväg inklusive filnamn) finns,
# med ett tydligt, handlingsbart felmeddelande om den saknas eller har
# flyttats/bytt namn - i stället för att låta ett kryptiskt
# "cannot open file"-fel dyka upp långt senare när filen faktiskt läses.
#' @noRd
intern_sakerstall_indatafil <- function(sokvag) {
  if (!file.exists(sokvag)) {
    stop(
      "Hittar inte den förväntade indatafilen:\n  ", sokvag,
      "\nKontrollera att filen finns på den sökvägen, eller att den inte ",
      "har flyttats eller bytt namn.",
      call. = FALSE
    )
  }
  intern_meddela_manuell_datakalla(sokvag)
  invisible(sokvag)
}

# Hittar den senast ändrade filen i en mapp som matchar ett filnamnsmönster,
# med ett tydligt felmeddelande om ingen fil matchar - i stället för det
# kryptiska "no non-missing arguments to max; returning -Inf"/"argument of
# length 0" som which.max() på en tom vektor annars ger.
#' @noRd
intern_hamta_senaste_fil <- function(mapp, monster) {
  if (!dir.exists(mapp)) {
    stop("Hittar inte mappen med indata:\n  ", mapp, call. = FALSE)
  }
  kandidater <- list.files(mapp, pattern = monster, full.names = TRUE)
  if (length(kandidater) == 0) {
    stop(
      "Hittade ingen fil som matchar mönstret '", monster, "' i mappen:\n  ", mapp,
      "\nKontrollera att filen finns där, eller att sökmönstret fortfarande stämmer.",
      call. = FALSE
    )
  }
  info <- file.info(kandidater)
  senaste <- kandidater[which.max(info$mtime)]
  intern_meddela_manuell_datakalla(senaste)
  senaste
}

# Hämtar Region Dalarnas delade bransch-/färgnyckel (Bransch_Gxx_farger) från
# GitHub-repot depot, i stället för den lokala G:-kopian (G:/skript/nycklar/
# Bransch_Gxx_farger.xlsx respektive .csv) som originalskripten använde - en
# centralt underhållen fil är tillgänglig oavsett vilken dator man kör
# ifrån, och behöver inte synkas manuellt till varje G:-enhet. Kolumnerna
# (BrKod, Br15kod, Bransch, HexCode m.fl.) är desamma som i de gamla lokala
# kopiorna - kontrollerat mot källfilen vid migreringen.
#' @noRd
intern_hamta_branschnyckel <- function() {
  url_xlsx <- "https://raw.githubusercontent.com/Region-Dalarna/depot/main/Bransch_Gxx_farger.xlsx"
  intern_meddela_manuell_datakalla(url_xlsx)
  temp_xlsx <- tempfile(fileext = ".xlsx")
  download.file(url_xlsx, temp_xlsx, mode = "wb", quiet = TRUE)
  branschtabell <- readxl::read_xlsx(temp_xlsx)
  unlink(temp_xlsx)
  branschtabell
}
