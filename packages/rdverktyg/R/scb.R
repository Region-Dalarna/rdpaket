# SCB-specifika databearbetningsfunktioner - utbrutna ur func_API.R.

intern_svenska_manader <- c(
  "januari", "februari", "mars", "april", "maj", "juni",
  "juli", "augusti", "september", "oktober", "november", "december"
)

#' Bearbeta en SCB-månadskolumn till år- och månadskolumner
#'
#' SCB-tabeller anger månad som `"2023M11"` (år, `"M"`, tvåsiffrigt
#' månadsnummer). Funktionen döper om kolumnen till `tid` och lägger till
#' `år`, `månad`, `år_månad` och `månad_år` som sorterade faktorer.
#'
#' @param skickad_df En data.frame med en SCB-månadskolumn.
#' @param kolumn_manad Namnet på månadskolumnen. Standard `"månad"`.
#' @param kortmanad Om `TRUE` läggs även `mån_år` till (formatet `"jan 2026"`).
#'
#' @return `skickad_df` med tidskolumnerna tillagda och omsorterade.
#' @export
manader_bearbeta_scbtabeller <- function(skickad_df, kolumn_manad = "månad", kortmanad = FALSE) {

  retur_df <- skickad_df |>
    dplyr::rename(tid = dplyr::all_of(kolumn_manad)) |>
    dplyr::mutate(
      år = as.integer(stringr::str_sub(.data$tid, 1, 4)),
      månad_nr = as.integer(stringr::str_sub(.data$tid, 6, 7)),
      månad = intern_svenska_manader[.data$månad_nr],
      år_månad = paste0(.data$år, " - ", .data$månad),
      månad_år = paste0(.data$månad, " ", .data$år),
      mån_år = paste0(stringr::str_sub(.data$månad, 1, 3), " ", .data$år)
    )

  sorterade_rader <- order(retur_df$år, retur_df$månad_nr)

  retur_df <- retur_df |>
    dplyr::mutate(
      månad_år = factor(.data$månad_år, levels = unique(.data$månad_år[sorterade_rader])),
      mån_år   = factor(.data$mån_år,   levels = unique(.data$mån_år[sorterade_rader])),
      år_månad = factor(.data$år_månad, levels = unique(.data$år_månad[sorterade_rader])),
      år       = factor(.data$år),
      månad    = factor(.data$månad, levels = intern_svenska_manader)
    ) |>
    dplyr::select(-"månad_nr") |>
    dplyr::relocate("år", .after = "tid") |>
    dplyr::relocate("månad", .after = "år") |>
    dplyr::relocate("år_månad", .after = "månad") |>
    dplyr::relocate("månad_år", .after = "år_månad")

  if (!kortmanad) retur_df <- dplyr::select(retur_df, -"mån_år")

  retur_df
}
