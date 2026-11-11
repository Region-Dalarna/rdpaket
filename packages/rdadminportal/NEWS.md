# rdadminportal 0.0.0.9006

## Ordningen på landningssidan

* Nya funktioner `landningssida_ordning_satt()`, `landningssida_ordning_lista()`
  och `landningssida_ordning_aterstall()` för en handpåförd ordning på appar och
  webbrapporter på landningssidan. Ordningen sparas i den nya tabellen
  `adminshiny.landningssida_ordning`. Poster utan ordning kommer efter de
  ordnade, alfabetiskt. PDF-rapporter sorteras på publiceringsdatum.
* `landningssida_app_oversikt()` har en ny kolumn `ordning` och sorteras som
  landningssidan: appar, webbrapporter, pdf-rapporter, och inom en grupp
  handpåförd ordning först och därefter alfabetiskt. Fungerar även om tabellen
  inte skapats ännu (då är `ordning` `NA`).
* En ändring på den publika servern notifierar lyssnaren (`pg_notify` på kanalen
  `adminportal_publik`), så att landningssidan regenereras inom några sekunder.

# rdadminportal 0.0.0.9005

* Funktionerna som ändrar landningssidans poster (`landningssida_exkludera()`,
  `landningssida_inkludera()`, ta bort ikon- och beskrivningskoppling)
  godkänner nu även pdf-rapporter, med namn som
  `pdf-rapporter/Årsrapport 2025.pdf`. Tidigare stoppades de av kravet att
  namn bara fick innehålla bokstäver, siffror, `_` och `-`, så en pdf-rapport
  hade inte gått att dölja eller ta bort ikon/beskrivning för. Filnamnet får inte
  innehålla `/`, `..` eller kontrolltecken. Nedladdningsfunktionerna är
  oförändrat strikta.

# rdadminportal 0.0.0.9004

* `landningssida_synka_app_lista()` tar nu med pdf-filerna i
  `/srv/rapporter/pdf-rapporter/` (ny parameter `pdf_mapp`) som egna poster med
  typ `"pdf"` och namnet `"pdf-rapporter/<filnamn>"`, så att de syns i
  adminportalen (synlighet, ikon och beskrivning) och på landningssidan under
  "Publicerade rapporter". Förutsätter att kolumnen `typ` i
  `adminshiny.landningssida_app` tillåter värdet `"pdf"`.

# rdadminportal 0.0.0.9003

* Listningen av nedladdningsfiler hämtar inte längre filägare
  (`file.info(extra_cols = FALSE)`). På Windows gav uppslagningen varningar
  ("cannot resolve owner of file ...") för filer vars ägarkonto inte går att
  slå upp.

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
