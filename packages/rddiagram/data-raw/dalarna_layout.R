# Kör detta för att bygga om data/dalarna_layout.rda:
#   source('data-raw/dalarna_layout.R')

# Ungefärliga relativa lägen (0-100) för Dalarnas kommuner enligt
# referensbilden dalarna_kommuner_aug2024. x ökar åt höger, y uppåt.

dalarna_layout <- data.frame(
  grupp = c("Älvdalen", "Orsa", "Malung-Sälen", "Mora", "Vansbro",
            "Rättvik", "Leksand", "Gagnef", "Borlänge", "Falun",
            "Ludvika", "Smedjebacken", "Säter", "Hedemora", "Avesta"),
  # Koordinater avlasta direkt fran referensbilden (dalarna_kommuner_aug2024).
  # gx/gy i 0-100, y UPPAT. Justera vid behov.
  gx = c(42.4, 67.6, 34.0, 58.3, 32.6,  84.0, 63.2, 41.7, 55.6, 86.1,
         34.0, 56.2, 72.2, 83.3, 97.2),
  gy = c(88.1, 88.4, 76.8, 77.5, 63.7,  63.7, 61.8, 52.4, 39.9, 42.4,
         18.6, 14.9, 24.9, 14.9, 12.4),
  stringsAsFactors = FALSE
)

# save(dalarna_layout, file = "data/dalarna_layout.rda", version = 2)
