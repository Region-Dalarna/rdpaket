# Installera alla paket i monorepot (i beroendeordning).
# Kör från repo-roten: source("install_all.R")

if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")

paket <- c(
  "rdverktyg",
  "rdhamtadata",
  "rddiagram",
  "rddiagramkatalog",
  "rdpostgres",
  "rdgis",
  "rdgtfs",
  "rdgeorouting",
  "rdshinyappar",
  "rddeploy",
  "rdppt",
  "rdadminportal",
  "rd"
)

pak::pkg_install(file.path("local::packages", paket))
