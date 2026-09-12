# rdverktyg 0.0.0.9002

## Dokumentation

* `utskriftsmapp()`, `mapp_hamtadata()`, `mapp_temp()`, `mapp_leveranser()`
  och `mapp_inlasdata()` förklarar nu även hur man sätter ett värde
  permanent (mellan R-sessioner, inte bara i den pågående) genom att lägga
  `options(...)`-raden i `.Rprofile` - tidigare stod bara att man kunde
  sätta det med `options()`, utan att säga var.

---

# rdverktyg 0.0.0.9001

## Ändrat beteende (bakåtinkompatibelt)

* `csv_fran_zipfiler_inlasning()` returnerar nu som standard en namngiven
  lista med en `tibble` per csv-fil (namn `"<zipfil>/<csvfil>"`) i stället
  för att alltid binda ihop allt till en enda `tibble`. Det gamla
  beteendet fås med `bind_ihop_dataseten = TRUE`. Motivet är att csv-filer
  i en zip inte nödvändigtvis har samma kolumner, och att man annars bara
  fick ut en enda ihoprörd `tibble` utan att kunna se filerna var för sig.
* `kalla_som_kolumn` styr nu bara `zip_fil`/`csv_fil`-kolumnerna när
  `bind_ihop_dataseten = TRUE`; listans element är redan namngivna efter
  zip- och csv-fil.

---

# rdverktyg 0.0.0.9000

Första versionen. Funktioner utbrutna ur funktionsskripten i
`Region-Dalarna/funktioner`.

## Datakällor och filformat (ur `func_API.R`, grupp C/D/F)

* **Kolada** (`Suggests: rKolada`): `hamta_kolada_giltiga_ar()`,
  `hamta_kolada_df()`.
* **Skolverket**: `skolverket_generera_kolumnnamn()`,
  `skolverket_hitta_startrad()`, `gymnprg_inr_koder_hamta_api_skolverket()`.
* **Försäkringskassan-JSON**: `hamta_fk_json_dataset_med_url()` +
  `json_extrahera_*()`/`json_ersatt_nycklar_med_etiketter()` (interna).
  Sköra parsers - verifiera mot riktiga FK-dataset.
* **Excel/filformat**: `excel_xml_las_fil()` (`Suggests: xml2`),
  `hamta_excel_dataset_med_url()` (`readxl`), `konvertera_dataset_filformat()`
  (`rio`), `excelfil_spara_formaterad()` (`openxlsx`), `spara_som_csv_i_zip()`
  (`zip`), `csv_fran_zipfiler_inlasning()`, `las_b64()`.
* **Webb**: `filhamtning_med_url_och_sokord()`,
  `webbsida_extrahera_url_med_sokord()` (`rvest`; namnet var
  `webbsida_af_extrahera_url_med_sokord` - "af" borttaget, bugg fixad där
  sista raden skrev över det filtrerade resultatet), `url_finns_webbsida()`.
* **source-hjälpare** (övergångsinfra): `source_utan_cache()`,
  `source_funktioner()`, `copilot_konvertera()`.
* `separator_gissa()` blev intern (`intern_separator_gissa`).
* `oppnadata_hamta()` togs INTE med - beror på postgres-funktioner, hör till
  `rdpostgres`.
* Tunga/nischade beroenden ligger i `Suggests` med `requireNamespace()`-vakt.

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
