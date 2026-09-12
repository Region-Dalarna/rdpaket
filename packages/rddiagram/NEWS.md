# rddiagram 0.0.0.9002

## Buggfix: `skickad_x_grupp = NA` kraschade

`manual_color`, `x_axis_sort_grp`, `logga_path` m.fl. använder genomgående
`NA` som "inget värde" - men `skickad_x_grupp` följde inte samma konvention:
`har_grupp <- !is.null(skickad_x_grupp)` blev sant även när `NA` skickades
(t.ex. `skickad_x_grupp = ifelse(length(kon) == 1, NA, "kön")`, ett vanligt
mönster i diagramskripten), vilket kraschade på `plot_df[[NA]]`.
`SkapaStapelDiagram()`/`SkapaLinjeDiagram()` normaliserar nu `NA` till
`NULL` innan grupperingen avgörs.

---

# rddiagram 0.0.0.9001

## Parameternamn tillbaka till func_SkapaDiagram.R

Efter önskemål: `SkapaStapelDiagram()` och `SkapaLinjeDiagram()` använder
igen de gamla parameternamnen, så befintliga skript fungerar utan
ändring. De ihopslagna namnen finns kvar som **alias**:

| Primärt namn (som förr) | Alias |
|---|---|
| `manual_color` + `brew_palett` | `farger` |
| `lagg_pa_logga` + `logga_path` | `logga` |
| `logga_scaling` | `logga_storlek` |
| `x_axis_sort_value` + `x_axis_sort_grp` (stapel) | `sortera_x` |

`facet_grp` styr facet direkt (ingen `diagram_facet` – den återinförs inte).
Fortsatt borttaget: filter-mekanismen, `AF_special`, `utan_diagramtitel`
(använd `diagram_titel = NULL`), `skriv_till_excelfil`, `berakna_index` i
stapeldiagram (finns kvar i linje).

---

# rddiagram 0.0.0.9000

## Rättat

* `SkapaStapelDiagram()`: när det inte fanns någon x-grupp (bara en
  stapelgrupp) men `farger` var en vektor med fler än en färg kraschade
  bygget med *"Aesthetics must be either length 1 or the same as the data"*.
  Nu används första färgen i det läget, som i originalet och som
  `SkapaLinjeDiagram()` redan gjorde.
* `stodlinjer_avrunda_fem = TRUE` kraschade (*"missing value where TRUE/FALSE
  needed"* i `nice_breaks()`) när datat bara hade en punkt eller alla värden
  var lika (spann = 0). `nice_breaks()` är omskriven:
  * **1-2-5-10-serien** i stället för 1-2.5-5-10 (inget `2.5`-steg).
  * spann som är 0, `NA`, `Inf` eller negativt ger nu steget `1` i stället
    för att krascha.
  * `Berakna_varden_stodlinjer()` räknar spannet från 0 när axeln tvingas
    börja där, så stödlinjerna blir vettiga även när värdena ligger tätt
    men långt från noll.
  * de tunna stödlinjerna delas nu `/2` när det tjocka steget börjar på 2
    (så `2 → 1`, inte `2 → 0.4`), annars `/5` — alltid ett steg på
    1/2/5 × 10^k.

---

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
