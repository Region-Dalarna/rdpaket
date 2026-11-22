## Globala inställningar för Shinyappen: <<github_repo>>

# Ladda nödvändiga paket
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(ggiraph)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
<<telemetri_block>>
# Felmeddelanden: på servern visas inga R-fel för användaren (sanitize = TRUE); felet skrivs ändå till appens logg
# (se Adminportalen, Driftstatus - Loggar). Lokalt i RStudio visas felen i klartext för att underlätta felsökning.
options(shiny.sanitize.errors = !interactive())
