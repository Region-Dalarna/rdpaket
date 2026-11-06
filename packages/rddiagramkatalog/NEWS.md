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
