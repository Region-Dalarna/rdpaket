# rddiagram 0.0.0.9000

Första versionen. Utbrutet ur `func_SkapaDiagram.R`,
`func_diagramfunktioner.R`, `func_logga_i_diagram.R` och
`func_bubbeldiagram.R` i `Region-Dalarna/funktioner`.

## Diagram

* `SkapaStapelDiagram()`, `SkapaLinjeDiagram()` - städade enligt
  `funktioner/REVIEW-SkapaDiagram.md`:
  * borttaget: filter-mekanismen (`skickad_filter_OR_*`), `berakna_index`
    (Stapel - var dead code), `AF_special`, `utan_diagramtitel`,
    `skriv_till_excelfil` (trasig), `min_och_max_negativa`, `%||%`,
    duplicerad stödlinjeberäkning (Linje), ~30 block bortkommenterad kod.
  * `diagram_facet` + `facet_grp` -> **`facet_grp = NULL`** (satt = på).
  * `lagg_pa_logga` + `logga_path` -> **`logga`** (`TRUE`/`FALSE`/`"sökväg"`).
  * `manual_color` + `brew_palett` -> **`farger`** (`NULL`/hex-vektor/palettnamn).
  * `x_axis_sort_value` + `x_axis_sort_grp` -> **`sortera_x`**
    (`NULL`/`TRUE`/heltal).
  * alla `= NA`-sentineller -> `= NULL`.
  * `etikett_format` använder nu `decimal.mark = ","` i båda (var `"."` i Linje).
  * NSE moderniserad: `.data[[...]]` i stället för `as.name()` + `!!`.
  * ~40 % delad kod utbruten: `intern_forbered_plotdata`, `intern_valj_farger`,
    `intern_rd_diagramtema`, `intern_etikett_format`, `intern_yaxel_grans`,
    `intern_stack_extremvarde`, `intern_sortera_stapel_x`.
  * ~90 parametrar -> ~55; ~580/390 rader -> ~250/210.
* `every_nth()`, `ggsave_retry()`, `save_eps_retry()`, `skriv_till_diagramfil()`.

## Hjälpfunktioner (ur `func_diagramfunktioner.R`)

* `hamta_logga_path()`, `nDigits()`, `avrunda_till_multipel()`,
  `hitta_div_for_jamn_intervall()`, `nice_breaks()`,
  `Berakna_varden_stodlinjer()`, `SkapaProcForandrTvaAr()`, `diagramfarger()`.

## Färg/skala (ur `func_API.R`) och logga

* `skalcirklar_skapa()`, `kontrastfarg_hitta()`, `add_logo()`.

## Karta

* `skapa_koropletkarta_ggplot()` - städad enligt granskningen: döda
  `exists()`-wrappers borttagna (`titel_legend` fanns aldrig), `antal_unika`
  beräknas en gång, `karta_farg_hogst = NA`/`karta_bredd = "auto"` -> `NULL`,
  `returnera_ggobj` borttagen, `glue` borttaget. `sf` m.fl. i `Suggests` med
  `requireNamespace()`-vakt.

## Packed circles (ur `func_bubbeldiagram.R`)

* `skapa_packed_circles()`, `aktivera_font()`, `forhandsvisa()`.
* `dalarna_layout` blev paketdata (`data/dalarna_layout.rda`), dokumenterad.
* `library()`-raderna borttagna; ggplot2/packcircles/ggforce kvalificerade.
* `bestam_omfattning()` (intern) använder nu `rdverktyg::hamtakommuner()` m.fl.
  via `requireNamespace()`-vakt i stället för `exists()`/`get()`.
* Hjälpfunktionerna är interna. Filen var redan i bra skick - porten är i
  huvudsak mekanisk; rendering får verifieras mot riktig branschdata.
