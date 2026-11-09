# Genererar ett exempeldiagram per diag_*()- och karta_*()-funktion, med rena
# standardinställningar (ingen anpassning av enskilda parametrar), till
# man/figures/<funktionsnamn>.png. Kopierar därefter bilderna till det
# installerade paketet också (om det är installerat), så
# diagramkatalog_utforska() visar aktuella bilder utan omstart/ominstallation.
#
# Körs manuellt vid behov (t.ex. efter att nya diagram lagts till, eller om
# de befintliga exemplen känns för inaktuella) - INTE i CI, eftersom det
# kräver riktiga, levande anrop mot SCB:s och andra API:er. Bilderna blir
# därmed en ögonblicksbild från när skriptet senast kördes, inte en
# live-förhandsvisning av aktuell data.
#
# Körs från repo-roten: source("packages/rddiagramkatalog/data-raw/skapa_exempeldiagram.R")
#
# skriv_over_befintliga_bilder styr om diagram som redan har en sparad bild
# ska hoppas över (FALSE, standard - bara nya/saknade diagram skapas) eller
# köras om (TRUE, skriver över samtliga). Sätt variabeln innan source() för
# att ändra standardvärdet:
#   skriv_over_befintliga_bilder <- TRUE
#   source("packages/rddiagramkatalog/data-raw/skapa_exempeldiagram.R")
if (!exists("skriv_over_befintliga_bilder")) skriv_over_befintliga_bilder <- FALSE

if (skriv_over_befintliga_bilder) {
  cat("skriv_over_befintliga_bilder = TRUE - skriver över ALLA exempelbilder, även de som redan finns.\n\n")
} else {
  cat("skriv_over_befintliga_bilder = FALSE (standard) - hoppar över diagram som redan har en sparad bild.\n")
  cat("Kör med skriv_over_befintliga_bilder <- TRUE före source() för att skriva över alla bilder.\n\n")
}

library(rddiagramkatalog)

utmapp <- file.path("packages", "rddiagramkatalog", "man", "figures")
if (!dir.exists(utmapp)) dir.create(utmapp, recursive = TRUE)

funktioner <- sort(grep("^(diag_|karta_)", getNamespaceExports("rddiagramkatalog"), value = TRUE))

resultat <- lapply(funktioner, function(fn) {
  cat("Genererar exempel för", fn, "... ")

  bildfil <- file.path(utmapp, paste0(fn, ".png"))
  if (!skriv_over_befintliga_bilder && file.exists(bildfil)) {
    cat("Hoppar över, bild finns redan\n")
    return("Hoppade över")
  }

  res <- tryCatch({
    args <- formals(get(fn, envir = asNamespace("rddiagramkatalog")))
    # Kör med rena standardvärden för allt utom skriv_diagramfil/skriv_leaflet_fil, som alltid
    # tvingas till FALSE - vi vill ha full kontroll över filnamn/storlek här i stället för
    # funktionens egen (ofta annorlunda namngivna) sparlogik.
    anropsargument <- list()
    if ("skriv_diagramfil" %in% names(args)) anropsargument$skriv_diagramfil <- FALSE
    if ("skriv_leaflet_fil" %in% names(args)) anropsargument$skriv_leaflet_fil <- FALSE

    diagram_lista <- do.call(fn, anropsargument)

    # karta_*()-funktioner som producerar EN karta returnerar list(ggplot = ..., leaflet = ...) -
    # plockar ggplot-versionen direkt. karta_*()-funktioner som kan producera FLERA kartor i ett
    # anrop (t.ex. en per kommun/pendlingstyp) returnerar i stället en namngiven lista där varje
    # element SJÄLVT är list(ggplot = ..., leaflet = ...) - plockar ggplot ur det första elementet
    # då. Annars (diag_*()-funktionerna) är det första elementet redan ett ggplot-objekt.
    plot_obj <- if ("ggplot" %in% names(diagram_lista)) {
      diagram_lista$ggplot
    } else if ("ggplot" %in% names(diagram_lista[[1]])) {
      diagram_lista[[1]]$ggplot
    } else {
      diagram_lista[[1]]
    }

    ggplot2::ggsave(
      filename = bildfil,
      plot = plot_obj,
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

# ========================================== Synka till installerat paket ============================================
# diagramkatalog_utforska() (Shiny-gadgeten) läser exempelbilderna från det
# INSTALLERADE paketet (system.file("help", "figures", ...)), inte från
# källträdet man/figures/ som den här sidan skriver till - R kopierar bara
# dit vid R CMD INSTALL/devtools::install(). Kopierar därför över alla
# bilder direkt här också, så gadgeten visar aktuella bilder utan att man
# behöver installera om paketet.
installerad_figurmapp <- system.file("help", "figures", package = "rddiagramkatalog")
if (nzchar(installerad_figurmapp)) {
  kopierade <- file.copy(
    list.files(utmapp, full.names = TRUE),
    installerad_figurmapp,
    overwrite = TRUE
  )
  cat("\nKopierade", sum(kopierade), "bild(er) till installerat paket (", installerad_figurmapp, ") - diagramkatalog_utforska() visar nu aktuella bilder.\n")
} else {
  cat("\nPaketet rddiagramkatalog verkar inte vara installerat - hoppar över synk till installerat paket.\n")
}
