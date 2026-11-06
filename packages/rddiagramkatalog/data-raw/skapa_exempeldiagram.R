# Genererar ett exempeldiagram per diag_*()-funktion, med rena
# standardinställningar (ingen anpassning av enskilda parametrar), till
# man/figures/<funktionsnamn>.png.
#
# Körs manuellt vid behov (t.ex. efter att nya diagram lagts till, eller om
# de befintliga exemplen känns för inaktuella) - INTE i CI, eftersom det
# kräver riktiga, levande anrop mot SCB:s och andra API:er. Bilderna blir
# därmed en ögonblicksbild från när skriptet senast kördes, inte en
# live-förhandsvisning av aktuell data.
#
# Körs från repo-roten: source("packages/rddiagramkatalog/data-raw/skapa_exempeldiagram.R")

library(rddiagramkatalog)

utmapp <- file.path("packages", "rddiagramkatalog", "man", "figures")
if (!dir.exists(utmapp)) dir.create(utmapp, recursive = TRUE)

funktioner <- sort(grep("^diag_", getNamespaceExports("rddiagramkatalog"), value = TRUE))

resultat <- lapply(funktioner, function(fn) {
  cat("Genererar exempel för", fn, "... ")
  res <- tryCatch({
    args <- formals(get(fn, envir = asNamespace("rddiagramkatalog")))
    # Kör med rena standardvärden för allt utom skriv_diagramfil, som alltid
    # tvingas till FALSE - vi vill ha full kontroll över filnamn/storlek här
    # i stället för funktionens egen (ofta annorlunda namngivna) sparlogik.
    anropsargument <- list()
    if ("skriv_diagramfil" %in% names(args)) anropsargument$skriv_diagramfil <- FALSE

    diagram_lista <- do.call(fn, anropsargument)

    ggplot2::ggsave(
      filename = file.path(utmapp, paste0(fn, ".png")),
      plot = diagram_lista[[1]],
      width = 7, height = 4.5, dpi = 110
    )
    "OK"
  }, error = function(e) paste("FEL:", conditionMessage(e)))
  cat(res, "\n")
  res
})

names(resultat) <- funktioner
cat("\n--- Sammanfattning ---\n")
print(unlist(resultat))
