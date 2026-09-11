# rdadminportal 0.0.0.9001

Versionsbump utan innehållsändring (för att `remotes`/`pak`
tillförlitligt ska upptäcka uppdateringar i det här monorepot).

---

# rdadminportal 0.0.0.9000

Första versionen. Utbrutet ur `func_landningssida_adminportal.R` och
`func_kor_cron_jobb.R` i `Region-Dalarna/funktioner`.

Två lägen genomgående: `"publik"` skriver till `adminshiny`-tabellerna i
sekretess-databasen (adminportal-lyssnaren synkar till servern),
`"intern"` gör direkta lokala anrop.

## Exkludering

* `landningssida_lista_exkluderade()`, `landningssida_exkludera()`,
  `landningssida_inkludera()`.

## Ikoner

* `landningssida_ikoner_lista()`, `landningssida_ikoner_koppla()`,
  `landningssida_ikoner_ta_bort_koppling()`,
  `landningssida_ikoner_lista_tillgangliga()`, `landningssida_ikoner_bladdra()`.

## Appar och rapporter

* `landningssida_synka_app_lista()`, `landningssida_app_oversikt()`,
  `landningssida_synka_app_lista_nu()`.

## Nedladdningsfiler

* `landningssida_synka_nedladdning()`, `landningssida_nedladdning_lista()`,
  `landningssida_nedladdning_lista_filer()`,
  `landningssida_nedladdning_har_aktivt_cronjobb()`,
  `landningssida_nedladdning_ta_bort_fil()`,
  `landningssida_nedladdning_ta_bort_mapp()`.

## Cron

* `kor_cron_jobb()` – kör ett schemalagt datauttag (`csv`/`xlsx`/`csv_zip`/
  `gpkg`) och uppdaterar status i `adminshiny.cron_jobb`.

## Ändringar mot originalfilerna

* DB-anslutningarna går via `rdshinyappar::shiny_uppkoppling_las/skriv`.
* Gemensam validering och "regenerera efter ändring"-logik bruten ut till
  `intern_validera_target/namn/filnamn`, `intern_efter_andring()`.
* `sf` och `writexl` i `Suggests` med `requireNamespace()`-vakt i
  `kor_cron_jobb()`.
* De gamla SSH-baserade `landningssida_*` i `func_API.R` (som förutsatte
  `~/.ssh/config`) ersätts – den DB-baserade varianten är den enda kvar.
