# rdpaket

Monorepo för Region Dalarnas interna R-paket. Varje paket ligger under
`packages/` och är ett eget, installerbart R-paket.

Utbrutet ur funktionsskripten i
[`Region-Dalarna/funktioner`](https://github.com/Region-Dalarna/funktioner).
Planen för uppdelningen finns i `funktioner/PLAN-paketuppdelning.md`.

## Paket

| Paket | Innehåll | Status |
|---|---|---|
| `rdverktyg` | text, filer, SCB-/regionhämtning, allmänna verktyg | text, filer, region, verktyg klara; grupp C/D/F kvar |
| `rddiagram` | diagram (ggplot), färg/skala, bubbeldiagram | planerat |
| `rdgis` | geometri, kartdata, rutor, GIS-filinläsning | planerat |
| `rdpostgres` | databasanslutningar och postgres-hjälpare | planerat |
| `rdgeorouting` | postgis, pgRouting, pendlingsnätverk | planerat |
| `rddeploy` | github/git, webbrapport, shiny-app-deploy, landningssida | planerat |
| `rdshinyappar` | runtime-hjälpare för Shiny-appar (DB, lösenord) | planerat |
| `rd` | paraplypaket – `library(rd)` drar in allt | planerat |

`pxweb2r` (PxWeb API v2) ligger i eget repo: <https://github.com/FaluPeppe/pxweb2r>.

## Installera

Ett paket i taget:

```r
# install.packages("remotes")
remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
```

Beroende `rd*`-paket installeras automatiskt. Allt på en gång: se `install_all.R`.

## R CMD check

`rd*`-paketen har **1 förväntad WARNING** ("non-ASCII characters in code") —
svenska i kod och utdatasträngar är avsiktligt, och paketen ska aldrig till
CRAN. Allt annat (ERROR, NOTE, en andra WARNING) är nytt och ska åtgärdas.

## Utveckling

```r
# från repo-roten, med ett paket som working directory:
devtools::load_all("packages/rdverktyg")
devtools::document("packages/rdverktyg")
devtools::test("packages/rdverktyg")
devtools::check("packages/rdverktyg")
```
