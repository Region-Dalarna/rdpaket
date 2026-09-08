# rdverktyg 0.0.0.9000

Första versionen. Funktioner utbrutna ur funktionsskripten i
`Region-Dalarna/funktioner`.

## Region- och kommunkoder (ur `func_API.R`)

* `hamtaregtab()`, `hamtakommuner()`, `hamtaAllaLan()`,
  `hamtaregion_kod_namn()`, `hamta_kommunkoder()`, `skapa_kortnamn_lan()`,
  `region_kolumn_splitta_kod_klartext()`, `ar_alla_kommuner_i_ett_lan()`,
  `ar_alla_lan_i_sverige()`.
* Region-/kommunlistan hämtas nu från SCB:s codelist-endpoints
  (`vs_RegionRiket99`, `vs_RegionLän07`, `vs_RegionKommun07`) via
  `pxweb2r::pxweb2_get_codelist()`, i stället för att extraheras ur en
  befolkningstabell över pxweb v1. `hamtaregtab()` cachar per session.
* Ännu ej flyttade (kräver per-tabell-beslut): `regsokoder_bearbeta()`,
  `desokoder_bearbeta()`, `tatortskoder_bearbeta()`,
  `hamta_regionkod_med_knas_regionkod()`.
* `svenska_tecken_byt_ut()` togs bort - dubblett av `byt_ut_svenska_tecken()`.

## Allmänna verktyg (ur `func_API.R`)

* Upprepa försök: `funktion_upprepa_forsok_tills_retur_TRUE()`,
  `funktion_upprepa_forsok_om_fel()`, `skriptrader_upprepa_om_fel()`.
* `ladda_funk_parametrar()`, `lista_funktioner_i_skript()`,
  `hitta_funktioner_i_fil_ej_inuti_andra_funktioner()`, `stop_tyst()`,
  `suppress_specific_warning()`, `period_jmfr_filter()`, `avrundning_dynamisk()`,
  `skapa_intervaller()`, `varden_jamnt_spridda_valj_ut()`, `vektor_till_text()`,
  `nummer_till_text()`, `slash_lagg_till()`, `sokvag_for_skript_hitta()`,
  `urklipp()`, `skapa_aldersgrupper()`.
* `sokvag_for_skript_hitta()` provar nu `this.path` först.
* `urklipp()` / `vektor_till_text()` använder `clipr` (Suggests) med
  plattforms-fallback.
* Fixad bugg i `lista_funktioner_i_skript()` (testade fel variabel för
  URL-detektering); läser nu URL:er direkt via `readr::read_lines()`.
* `korrigera_kolnamn_supercross()` togs INTE med (Supercross fasas ut).
  Färg-/skalhjälparna `skalcirklar_skapa()`, `kontrastfarg_hitta()`,
  `hamta_logga_path()` och `demo_diagrambild_skapa()` går till `rddiagram`
  (den sista stryks).

## Filer (ur `func_filer.R`)

* `sparafil_unik()`, `skapa_mapp_om_den_inte_finns()`,
  `sparafil_en_backup_nvdb()`, `sparafil_backup_omfinns()`, `ladda_ned_fil()`,
  `nextweekday()`.
* Sökvägshantering går via `fs`.
* `%notin%` togs INTE med - finns i base R sedan 4.5.0. Paketet kräver
  därför R >= 4.5.0.
* `get_shapefile()` togs INTE med - den var trasig (refererade odefinierade
  globala variabler) och överlappade `ladda_ned_fil()`.

## Text (ur `func_text.R`)

* `list_komma_och()`, `list_komma_eller()`, `list_komma_samt()`,
  `dela_upp_strang_radbryt()`, `byt_ut_svenska_tecken()`,
  `procent_till_text()`, `forandring_till_text()`.
* `dela_upp_strang_radbryt()` använder nu `stringr::str_wrap()` för det vanliga
  fallet (radbrytning vid mellanslag) - kan ge något annan brytning än förr.
* `byt_ut_svenska_tecken()` använder nu `stringi::stri_trans_general()` och
  hanterar även andra diakriter (é, ü, ...), inte bara å/ä/ö.
* Känd bugg som INTE ändrats: `procent_till_text(100)` ger `"nästan alla"`
  (branchen `procent == 100` nås aldrig).
