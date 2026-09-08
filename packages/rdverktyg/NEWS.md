# rdverktyg 0.0.0.9000

* Första versionen. Textfunktioner utbrutna ur `func_text.R` i
  `Region-Dalarna/funktioner`:
  `list_komma_och()`, `list_komma_eller()`, `list_komma_samt()`,
  `dela_upp_strang_radbryt()`, `byt_ut_svenska_tecken()`,
  `procent_till_text()`, `forandring_till_text()`.
* `dela_upp_strang_radbryt()` använder nu `stringr::str_wrap()` för det vanliga
  fallet (radbrytning vid mellanslag) - kan ge något annan brytning än förr.
* `byt_ut_svenska_tecken()` använder nu `stringi::stri_trans_general()` och
  hanterar även andra diakriter (é, ü, ...), inte bara å/ä/ö.
