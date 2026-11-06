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
