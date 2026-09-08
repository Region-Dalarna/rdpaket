# rdpaket

Monorepo för Region Dalarnas interna R-paket. Varje paket ligger under
`packages/` och är ett eget, installerbart R-paket.

Utbrutet ur funktionsskripten i
[`Region-Dalarna/funktioner`](https://github.com/Region-Dalarna/funktioner).
Planen för uppdelningen finns i `funktioner/PLAN-paketuppdelning.md`.

## Paket

| Paket | Innehåll | Status |
|---|---|---|
| `rdverktyg` | text, filer, SCB-/regionhämtning, allmänna verktyg, datakällor | **klart** (utom `oppnadata_hamta` → rdpostgres) |
| `rddiagram` | diagram (ggplot), färg/skala, bubbeldiagram | **klart** |
| `rdgis` | geometri, kartdata, rutor, GIS-filinläsning | planerat |
| `rdpostgres` | databasanslutningar, roller/rättigheter, metadata, grants | **klart** |
| `rdgeorouting` | postgis, pgRouting, pendlingsnätverk | planerat |
| `rddeploy` | github/git, webbrapport/portal, shiny-app-scaffolding & -publicering | **klart** |
| `rdshinyappar` | runtime-hjälpare för Shiny-appar (DB, lösenord, telemetri) | **klart** |
| `rdadminportal` | adminportal + serverdrift: landningssidor, ikoner, nedladdningar, cron | **klart** |
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
