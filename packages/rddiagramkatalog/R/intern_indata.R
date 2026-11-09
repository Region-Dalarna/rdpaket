# intern_sakerstall_indatafil()/intern_meddela_manuell_datakalla() flyttade härifrån till
# rdverktyg::sakerstall_indatafil()/rdverktyg::meddela_manuell_datakalla() - samma två funktioner
# fanns duplicerade (oexporterade) i både rddiagramkatalog och rdppt.

# Hittar den senast ändrade filen i en mapp som matchar ett filnamnsmönster,
# med ett tydligt felmeddelande om ingen fil matchar - i stället för det
# kryptiska "no non-missing arguments to max; returning -Inf"/"argument of
# length 0" som which.max() på en tom vektor annars ger.
#' @noRd
intern_hamta_senaste_fil <- function(mapp, monster, recursive = FALSE) {
  if (!dir.exists(mapp)) {
    stop("Hittar inte mappen med indata:\n  ", mapp, call. = FALSE)
  }
  kandidater <- list.files(mapp, pattern = monster, full.names = TRUE, recursive = recursive)
  if (length(kandidater) == 0) {
    stop(
      "Hittade ingen fil som matchar mönstret '", monster, "' i mappen:\n  ", mapp,
      "\nKontrollera att filen finns där, eller att sökmönstret fortfarande stämmer.",
      call. = FALSE
    )
  }
  info <- file.info(kandidater)
  senaste <- kandidater[which.max(info$mtime)]
  rdverktyg::meddela_manuell_datakalla(senaste)
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
  rdverktyg::meddela_manuell_datakalla(url_xlsx)
  temp_xlsx <- tempfile(fileext = ".xlsx")
  download.file(url_xlsx, temp_xlsx, mode = "wb", quiet = TRUE)
  branschtabell <- readxl::read_xlsx(temp_xlsx)
  unlink(temp_xlsx)
  branschtabell
}
