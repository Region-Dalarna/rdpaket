# rddiagram 0.0.0.9000

Första versionen. Utbrutet ur `func_SkapaDiagram.R`,
`func_diagramfunktioner.R`, `func_logga_i_diagram.R` och
`func_bubbeldiagram.R` i `Region-Dalarna/funktioner`.

## Hjälpfunktioner (ur `func_diagramfunktioner.R`)

* `hamta_logga_path()`, `nDigits()`, `avrunda_till_multipel()`,
  `hitta_div_for_jamn_intervall()`, `nice_breaks()`,
  `Berakna_varden_stodlinjer()`, `SkapaProcForandrTvaAr()`, `diagramfarger()`.
* `diagramfarger()` bygger nu på en namngiven lista i stället för `get()`.
* `Berakna_varden_stodlinjer()`: `plyr::round_any()` inlinad (inget
  plyr-beroende).
* `SkapaProcForandrTvaAr()` moderniserad (`|>`, `.data[[...]]`, `across()`).

## Färg/skala (ur `func_API.R`)

* `skalcirklar_skapa()`, `kontrastfarg_hitta()` (`farver`).

## Logga (ur `func_logga_i_diagram.R`)

* `add_logo()` (`Suggests: magick`) - buggfix: `image_write` var okvalificerad.

## Kvar (kommande commits)

* `SkapaStapelDiagram()`, `SkapaLinjeDiagram()`, `every_nth()`
* `skapa_koropletkarta_ggplot()`
* `skriv_till_diagramfil()`, `ggsave_retry()`, `save_eps_retry()`
* Packed circles (`func_bubbeldiagram.R`)
