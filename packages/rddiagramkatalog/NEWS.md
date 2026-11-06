# rddiagramkatalog 0.0.0.9009

- Fix: ordet "pull-right" syntes som text i den övre listen i
  `diagramkatalog_utforska()`. Orsak: `miniUI::gadgetTitleBar()`s `right`
  måste vara EN tagg, inte en `tagList()` med flera knappar - annars
  misslyckas den interna `tagAppendAttributes(right, class = "pull-right")`.
  Knapparna ligger nu i en gemensam `div`.
- "Visa hjälp" har fått en egen färg (`btn-info`, varken grå som "Cancel"
  eller blå som de andra två), och ligger separat från "Visa diagram" och
  "Infoga kod", som nu ligger intill varandra.
- Alla tre knapparna är något mindre (mindre padding/typsnittsstorlek).

# rddiagramkatalog 0.0.0.9008

- Knapparna "Visa diagram", "Visa hjälp" och "Infoga kod" i
  `diagramkatalog_utforska()` sitter nu i den övre listen bredvid "Cancel",
  som blåa knappar (samma stil som gadgetens "Done"-knapp normalt har) för
  att synas tydligt mot den grå Cancel-knappen - i stället för att ligga i
  en rad under parameterformuläret.

# rddiagramkatalog 0.0.0.9007

- `diagramkatalog_bladdra()` har döpts om till `diagramkatalog_utforska()`
  - "bläddra" fångade bara en del av vad funktionen faktiskt gör (söka,
  läsa hjälp, fylla i parametrar, inte bara skumma en lista). Addinet i
  RStudios Addins-meny heter nu "Utforska diagramkatalog".

# rddiagramkatalog 0.0.0.9006

- `diagramkatalog_bladdra()` har fått en knapp "Visa hjälp" som öppnar den
  valda funktionens vanliga hjälpsida (samma som `?funktionsnamn`) i
  RStudios Hjälp-ruta.

# rddiagramkatalog 0.0.0.9005

- Fix: `sok_diagram()` och `diagramkatalog_bladdra()` listade tidigare sig
  själva (och varandra) bland diagrammen, eftersom sökindexet byggdes av
  alla exporterade funktioner i paketet, inte bara `diag_*()`-funktionerna.
  Filtrerar nu till funktionsnamn som börjar med `diag_`.
- `diagramkatalog_bladdra()` öppnas nu i ett eget, större dialogfönster
  ovanpå RStudio (`shiny::dialogViewer()`, 1100x750) i stället för det
  mindre, fasta Viewer-fönstret.

# rddiagramkatalog 0.0.0.9004

- `sok_diagram()` lägger nu, som standard, de matchande funktionsnamnen i
  urklippshanteraren (en per rad), redo att klistras in. Styrs av den nya
  parametern `funktionsnamn_lagg_i_urklippshanteraren` (standard `TRUE`),
  vars eget standardvärde läses från
  `options(rddiagramkatalog.kopiera_funktionsnamn = ...)` - så det går att
  stänga av permanent (t.ex. i `.Rprofile`) utan att ange parametern vid
  varje anrop. Kräver paketet `clipr`; hoppar annars bara tyst över
  kopieringen.
- `sok_diagram()` ger nu ett begripligt, handlingsbart felmeddelande om
  paketets hjälpdatabas inte kan läsas (t.ex. "lazy-load database is
  corrupt"/"internal error in R_decompress1"), i stället för R:s kryptiska
  originalfel. Det här är ofta en tillfällig låsning av hjälpfilerna (t.ex.
  OneDrive-synk eller ett antivirusprogram) som löser sig vid en omstart av
  R/RStudio.

# rddiagramkatalog 0.0.0.9003

Sök- och bläddringsfunktionalitet för katalogen:

- `sok_diagram(sokord)` söker igenom titel, beskrivning och parameter-
  dokumentation (samma text som `?funktionsnamn` visar) för alla
  diagramfunktioner och returnerar de som matchar. Bygger sökindexet från
  paketets installerade `.Rd`-filer (`tools::Rd_db()`), inte källkoden, så
  det fungerar oavsett hur paketet installerats och kräver inget separat
  register att hålla uppdaterat när fler diagram läggs till.
- `diagramkatalog_bladdra()` öppnar ett grafiskt gränssnitt (RStudio-addin,
  "Bläddra diagramkatalog" i Addins-menyn) där man söker, väljer en
  funktion, fyller i parametrar i ett formulär som byggs generiskt från
  funktionens `formals()` (fungerar automatiskt för alla diagram, även
  framtida tillskott), och antingen förhandsvisar diagrammet eller infogar
  den färdiga koden i det aktiva skriptet. Kräver `shiny` och `miniUI`
  (valfria beroenden, bara nödvändiga för denna funktion); "Infoga kod"
  kräver dessutom `rstudioapi` och att man kör RStudio.

# rddiagramkatalog 0.0.0.9002

Konsekvent namngivning över alla 15 pilotfunktioner:

- Alla funktionsnamn börjar med `diag_`, är enbart gemener, och slutar med
  källan - utom de som jämför bestämda, utvalda perioder (inte en
  tidsserie), där det inte är lika meningsfullt:
  `diag_chefer` -> `diag_chefer_scb`, `diag_gini_SCB` -> `diag_gini_scb`,
  `diag_matchning_lan` -> `diag_matchning_lan_scb`,
  `diag_narstaendepenning` -> `diag_narstaendepenning_forsakringskassan`,
  `diag_fodelsetal_summerad_fruktsamhet_jmfr_riket_lan_kommuner` -> `..._scb`,
  `diag_trangboddhet_inrikes_utrikes` -> `..._scb`,
  `diagram_langtidsarbetslohet_tidsserie` -> `diag_langtidsarbetslohet_tidsserie_kolada`,
  `diagram_storsta_fodelseland` -> `diag_storsta_fodelseland_scb`.
- Outputmappen heter nu `utmapp` i alla funktioner (var tidigare `output_mapp`,
  `output_mapp_figur` eller `output_fold`), med `rdverktyg::utskriftsmapp()`
  som standardvärde.
- Skriv-till-fil-parametern heter nu `skriv_diagramfil` i alla funktioner
  (var tidigare bl.a. `spara_figur`, `skriv_bildfil`, `spara_diagrambildfil`).
- Regionparametern heter nu `region_vekt` (var `region`/`vald_region` i
  några funktioner), färgvektorn `diag_fargvekt` (var `valda_farger`/
  `diag_fargvektor`), och data-till-global-environment-parametern
  `returnera_data` (var bl.a. `returnera_data_rmarkdown`).
- Loggan i diagrammet styrs nu alltid av två separata parametrar,
  `visa_logga_i_diagram` (TRUE/FALSE) och `logga_sokvag` (sökväg eller NA),
  i stället för en enda kombinerad parameter i vissa funktioner.
- Om `utmapp` inte finns (t.ex. om `rdverktyg::utskriftsmapp()` inte är
  konfigurerad för datorn) och `skriv_diagramfil = TRUE`: funktionen
  stoppar nu med ett tydligt felmeddelande, i stället för att misslyckas
  längre in i anropet till rddiagram.
- Flera funktioner som hämtar en tidsserie har fått en `tid_koder`-parameter
  (om de inte redan hade en): `"*"` ger allt, `"9999"` ger senaste perioden,
  och ett nytt mönster, `"2015:9999"`, ger "från 2015 till och med senaste
  tillgängliga period" (år eller månad beroende på tabell).

# rddiagramkatalog 0.0.0.9001

Första versionen - pilotomgång med 15 diagramfunktioner flyttade från
Region-Dalarna/diagram:

- `diag_arbloshet_procent_utbildning_facet_ar_scb`
- `diag_aterstaende_medellivslangd_utbniva_lan_scb`
- `diag_bas_arbloshet_manad_jmfr_1ar_tillbaka_scb`
- `diag_bas_syss_arbloshet_aldersgrupp_kon_scb`
- `diag_befolkningsforandring_manad_scb`
- `diag_chefer`
- `diag_fodelsetal_globalt_varldsbanken`
- `diag_fodelsetal_summerad_fruktsamhet_jmfr_riket_lan_kommuner`
- `diag_gini_SCB`
- `diag_inkomst_bakgrund_scb`
- `diag_matchning_lan`
- `diag_narstaendepenning`
- `diagram_langtidsarbetslohet_tidsserie`
- `diagram_storsta_fodelseland`
- `diag_trangboddhet_inrikes_utrikes`

De fem ursprungligt tänkta funktionerna `diag_antal_asylsokande_1984_IntRap`,
`diagram_50proc_lonesumma`, `diagram_examen_hogskolan_NMS`,
`diagram_kompetensbrist` och `diagram_utb_niva_bakgr_alder` visade sig ha
hårdkodade sökvägar till indata på G:-enheten och är bytta mot de fem
verifierat rena alternativ som listas ovan.

Källfilernas inline `requireNamespace()`/`remotes::install_github()`-kontroller
är ersatta med vanliga `Imports:`/`Remotes:` i `DESCRIPTION`. Övrig kod är
oförändrad. Resten av diagrammen i Region-Dalarna/diagram flyttas hit i
senare omgångar.
