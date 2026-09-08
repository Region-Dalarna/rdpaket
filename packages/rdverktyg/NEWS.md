# rdverktyg 0.0.0.9000

Första versionen. Funktioner utbrutna ur funktionsskripten i
`Region-Dalarna/funktioner`.

## Filer (ur `func_filer.R`)

* `sparafil_unik()`, `skapa_mapp_om_den_inte_finns()`,
  `sparafil_en_backup_nvdb()`, `sparafil_backup_omfinns()`, `ladda_ned_fil()`,
  `nextweekday()`, `%notin%`.
* Sökvägshantering går via `fs`.
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
