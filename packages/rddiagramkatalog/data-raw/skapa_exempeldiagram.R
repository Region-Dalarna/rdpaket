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

if (any(grepl("^karta_lf_", getNamespaceExports("rddiagramkatalog"))) && !requireNamespace("webshot2", quietly = TRUE)) {
  cat("OBS: paketet webshot2 (och Chrome/Chromium) saknas - exempelbilderna (png) för karta_lf_*() kan inte skapas.\n",
      "Installera med install.packages(\"webshot2\").\n\n")
}

funktioner <- sort(grep("^(diag_|karta_gg_|karta_lf_)", getNamespaceExports("rddiagramkatalog"), value = TRUE))

# Vilka funktioner som redan har en exempelbild. Bara de som saknar bild (eller alla, om
# skriv_over_befintliga_bilder = TRUE) namnges och körs; de som redan har bild räknas bara.
bildfil_for <- function(fn) file.path(utmapp, paste0(fn, ".png"))
har_bild <- file.exists(vapply(funktioner, bildfil_for, character(1)))
att_skapa <- if (skriv_over_befintliga_bilder) funktioner else funktioner[!har_bild]

cat(sum(har_bild), "av", length(funktioner), "funktioner har redan en exempelbild",
    if (skriv_over_befintliga_bilder) "(skrivs över).\n" else "(hoppas över).\n")
if (length(att_skapa) == 0) {
  cat("Inga bilder att skapa.\n")
} else {
  cat("Skapar bild för", length(att_skapa), if (skriv_over_befintliga_bilder) "funktioner:\n" else "funktioner som saknar bild:\n")
  if (!skriv_over_befintliga_bilder) cat(paste0("  ", att_skapa, collapse = "\n"), "\n", sep = "")
  cat("\n")
}

resultat <- lapply(seq_along(att_skapa), function(i) {
  fn <- att_skapa[[i]]
  cat(sprintf("[%d/%d] %s ... ", i, length(att_skapa), fn))
  bildfil <- bildfil_for(fn)

  res <- tryCatch({
    args <- formals(get(fn, envir = asNamespace("rddiagramkatalog")))
    # Kör med rena standardvärden för allt utom skriv_diagramfil/skriv_leaflet_fil, som alltid
    # tvingas till FALSE - vi vill ha full kontroll över filnamn/storlek här i stället för
    # funktionens egen (ofta annorlunda namngivna) sparlogik.
    anropsargument <- list()
    if ("skriv_diagramfil" %in% names(args)) anropsargument$skriv_diagramfil <- FALSE
    if ("skriv_leaflet_fil" %in% names(args)) anropsargument$skriv_leaflet_fil <- FALSE

    diagram_lista <- do.call(fn, anropsargument)

    # Alla diag_*()- och karta_*()-funktioner returnerar en namngiven lista; exemplet görs av det
    # första elementet. karta_lf_*() ger leaflet-kartor: de sparas som en png (ögonblicksbild via
    # webshot2) och som en html-fil. html-filerna delar ett gemensamt bibliotek (mappen lib) i
    # stället för att bädda in JavaScript i varje fil, så att de blir små.
    if (grepl("^karta_lf_", fn)) {
      rddiagram::skriv_leaflet_till_fil(diagram_lista[[1]], utmapp, fn, karta_filformat = "html", delat_bibliotek = TRUE)
      rddiagram::skriv_leaflet_till_fil(diagram_lista[[1]], utmapp, fn, karta_filformat = "png",
                                         karta_bredd = 770, karta_hojd = 495)
    } else {
      # kartor är oftast högre än breda och får därför en högre bild än diagrammen
      ggplot2::ggsave(
        filename = bildfil,
        plot = diagram_lista[[1]],
        width = 7, height = if (grepl("^karta_gg_", fn)) 7 else 4.5, dpi = 110
      )
    }
    "OK"
  }, error = function(e) paste("FEL:", conditionMessage(e)))
  cat(res, "\n")
  res
})

names(resultat) <- att_skapa
cat("\n--- Sammanfattning ---\n")
if (!skriv_over_befintliga_bilder) cat(sum(har_bild), "funktioner hade redan bild och hoppades över.\n")
if (length(resultat) > 0) {
  utfall <- unlist(resultat)
  cat(sum(utfall == "OK"), "bild(er) skapades.\n")
  if (any(utfall != "OK")) {
    cat(sum(utfall != "OK"), "misslyckades:\n")
    print(utfall[utfall != "OK"])
  }
}

# ========================================== Synka till installerat paket ============================================
# diagramkatalog_utforska() (Shiny-gadgeten) läser exempelbilderna från det
# INSTALLERADE paketet (system.file("help", "figures", ...)), inte från
# källträdet man/figures/ som den här sidan skriver till - R kopierar bara
# dit vid R CMD INSTALL/devtools::install(). Kopierar därför över alla
# bilder direkt här också, så gadgeten visar aktuella bilder utan att man
# behöver installera om paketet.
installerad_figurmapp <- system.file("help", "figures", package = "rddiagramkatalog")
if (nzchar(installerad_figurmapp)) {
  # recursive = TRUE tar med mappen lib, som de interaktiva exempelkartorna (html) delar
  kopierade <- file.copy(
    list.files(utmapp, full.names = TRUE),
    installerad_figurmapp,
    overwrite = TRUE,
    recursive = TRUE
  )
  cat("\nKopierade", sum(kopierade), "bild(er) till installerat paket (", installerad_figurmapp, ") - diagramkatalog_utforska() visar nu aktuella bilder.\n")
} else {
  cat("\nPaketet rddiagramkatalog verkar inte vara installerat - hoppar över synk till installerat paket.\n")
}
