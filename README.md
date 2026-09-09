# rdpaket

Monorepo för Region Dalarnas interna R-paket. Varje paket ligger under
`packages/` och är ett eget, installerbart R-paket.

Utbrutet ur funktionsskripten i
[`Region-Dalarna/funktioner`](https://github.com/Region-Dalarna/funktioner).
Planen för uppdelningen finns i `funktioner/PLAN-paketuppdelning.md`.

## Paket

| Paket | Innehåll | Status |
|---|---|---|
| `rdverktyg` | text, filer, SCB-/regionhämtning, allmänna verktyg, datakällor | **klart** |
| `rddiagram` | diagram (ggplot), färg/skala, bubbeldiagram | **klart** |
| `rdgis` | geometri, kartdata, rutor, GIS-filinläsning | **klart** |
| `rdpostgres` | databasanslutningar, roller/rättigheter, metadata, grants | **klart** |
| `rdgeorouting` | postgis, pgRouting, pendlingsnätverk (ur `func_GIS.R`) | **klart** (SQL-pipelines behöver verifieras mot riktig pgRouting-databas) |
| `rddeploy` | github/git, webbrapport/portal, shiny-app-scaffolding & -publicering | **klart** |
| `rdshinyappar` | runtime-hjälpare för Shiny-appar (DB, lösenord, telemetri) | **klart** |
| `rdadminportal` | adminportal + serverdrift: landningssidor, ikoner, nedladdningar, cron | **klart** |
| `rd` | paraplypaket – `library(rd)` drar in analyspaketen | **klart** |

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
Undantag: paraplypaketet `rd` har ingen kod med svenska strängar och går
igenom rent (Status: OK).

## Utveckling

```r
# från repo-roten, med ett paket som working directory:
devtools::load_all("packages/rdverktyg")
devtools::document("packages/rdverktyg")
devtools::test("packages/rdverktyg")
devtools::check("packages/rdverktyg")
```
