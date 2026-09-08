# Försäkringskassan-JSON - utbrutet ur func_API.R.
# Sköra parsers - verifiera mot riktiga FK-dataset vid ändringar.

#' Hämta ett dataset i JSON-format från Försäkringskassan via URL
#'
#' URL:er hittas på <https://www.dataportal.se> (filtrera på Försäkringskassan).
#' För Excel-filer, använd [hamta_excel_dataset_med_url()] i stället.
#'
#' @param url_fk URL till JSON-datasetet.
#'
#' @return En `tibble` med klartextkolumner och tabellnamn.
#' @export
hamta_fk_json_dataset_med_url <- function(url_fk) {

  kolumnordning <- c("Period", "period", "tid", "Ar", "ar", "År", "år", "Manad",
                     "Månad", "manad", "månad", "år_månad", "månad_år", "regionkod",
                     "Regionkod", "region", "Region", "Lankod", "Länkod", "lankod",
                     "länkod", "Län", "Lan", "län", "lan", "Kommunkod", "kommunkod",
                     "Kommun", "kommun")

  meta_url <- stringr::str_replace(url_fk, "/[^/]*$", "/meta.json")

  data_df <- jsonlite::fromJSON(
    rawToChar(curl::curl_fetch_memory(url_fk)$content), flatten = TRUE
  ) |>
    dplyr::select(-dplyr::contains("rojd")) |>
    dplyr::rename_with(~ stringr::str_remove_all(., "observations\\.|\\.value|dimensions\\.")) |>
    dplyr::select(-dplyr::any_of("row_nr"))

  meta_df <- jsonlite::fromJSON(
    rawToChar(curl::curl_fetch_memory(meta_url)$content), flatten = TRUE
  )

  tabellnamn <- meta_df$key

  meta_kol <- meta_df$table$columns |>
    dplyr::mutate(element_namn = "meta_dim", under_element = NA) |>
    dplyr::select(-dplyr::any_of("format"))

  meta_dim <- meta_df$filter$dimension
  names(meta_dim$values) <- meta_dim$key

  nyckel_etikett_tabell <- json_extrahera_nyckel_etikett(meta_dim)

  kolumnnamn <- nyckel_etikett_tabell |>
    dplyr::filter(.data$element_namn == "meta_dim") |>
    dplyr::bind_rows(meta_kol) |>
    dplyr::distinct() |>
    dplyr::filter(.data$key %in% colnames(data_df))

  nyckel_etikett_tabell <- dplyr::filter(
    nyckel_etikett_tabell, .data$under_element %in% kolumnnamn$key
  )

  data_df2 <- json_ersatt_nycklar_med_etiketter(data_df, nyckel_etikett_tabell) |>
    dplyr::rename_with(~ kolumnnamn$label[match(., kolumnnamn$key)], .cols = kolumnnamn$key)

  fk_json <- data_df2 |>
    dplyr::select(dplyr::any_of(kolumnordning), where(~ !is.numeric(.x)), where(is.numeric))

  if (sum(c("Kommun", "Län") %in% names(fk_json)) > 1) {
    fk_json <- dplyr::select(fk_json, -dplyr::any_of(c("län", "Län", "lan", "Lan")))
  }
  if ("Kommun" %in% names(fk_json)) {
    fk_json <- tidyr::separate_wider_delim(
      fk_json, "Kommun", delim = " ", names = c("Regionkod", "Region"),
      too_few = "align_end", too_many = "merge"
    )
  }
  if ("Län" %in% names(fk_json)) {
    fk_json <- tidyr::separate_wider_delim(
      fk_json, "Län", delim = " ", names = c("Länskod", "Län"),
      too_few = "align_end", too_many = "merge"
    )
  }

  fk_json |>
    dplyr::mutate(Tabellnamn = tabellnamn) |>
    dplyr::relocate("Tabellnamn", .before = 1)
}

#' @keywords internal
json_extrahera_subdimensions <- function(meta_dim) {
  tom <- tibble::tibble(element_namn = character(), under_element = character(),
                        key = character(), label = character())
  purrr::map_dfr(seq_along(meta_dim$values), function(i) {
    value <- meta_dim$values[[i]]
    if (is.null(value$subdimension.values) || !is.list(value$subdimension.values) ||
        !"subdimension.key" %in% colnames(value)) {
      return(tom)
    }
    purrr::map2_dfr(value$subdimension.values, value$subdimension.key, function(sv, sk) {
      if (!is.data.frame(sv)) return(tom)
      sv |>
        dplyr::mutate(element_namn = "subdimensions.values", under_element = sk) |>
        dplyr::select("element_namn", "under_element", "key", "label")
    })
  })
}

#' @keywords internal
json_extrahera_values <- function(meta_dim) {
  tom <- tibble::tibble(element_namn = character(), under_element = character(),
                        key = character(), label = character())
  purrr::map_dfr(seq_along(meta_dim$values), function(i) {
    value <- meta_dim$values[[i]]
    if (!is.data.frame(value)) return(tom)
    value |>
      dplyr::mutate(element_namn = "values", under_element = meta_dim$key[i]) |>
      dplyr::select("element_namn", "under_element", "key", "label")
  })
}

#' @keywords internal
json_extrahera_subdimension_keys <- function(meta_dim) {
  tom <- tibble::tibble(element_namn = character(), under_element = character(),
                        key = character(), label = character())
  purrr::map_dfr(seq_along(meta_dim$values), function(i) {
    value <- meta_dim$values[[i]]
    if (!is.data.frame(value) ||
        !all(c("subdimension.key", "subdimension.label") %in% colnames(value))) {
      return(tom)
    }
    value |>
      dplyr::select(key = "subdimension.key", label = "subdimension.label") |>
      dplyr::mutate(element_namn = "meta_dim", under_element = NA) |>
      dplyr::distinct()
  })
}

#' @keywords internal
json_extrahera_nyckel_etikett <- function(meta_dim) {
  huvuddata <- meta_dim |>
    dplyr::mutate(element_namn = "meta_dim", under_element = NA) |>
    dplyr::select("element_namn", "under_element", "key", "label")

  dplyr::bind_rows(
    huvuddata,
    json_extrahera_values(meta_dim),
    json_extrahera_subdimensions(meta_dim),
    json_extrahera_subdimension_keys(meta_dim)
  ) |>
    dplyr::distinct() |>
    dplyr::mutate(element_namn = factor(
      .data$element_namn,
      levels = c("meta_dim", "values", "subdimensions.values")
    )) |>
    dplyr::arrange(.data$element_namn, .data$under_element, .data$key, .data$label)
}

#' @keywords internal
json_ersatt_nycklar_med_etiketter <- function(data_df, nyckel_etikett) {
  under <- unique(nyckel_etikett$under_element[!is.na(nyckel_etikett$under_element)])
  data_df |>
    dplyr::mutate(dplyr::across(dplyr::all_of(under), function(kol) {
      karta <- nyckel_etikett |>
        dplyr::filter(.data$under_element == dplyr::cur_column()) |>
        dplyr::distinct(.data$key, .data$label) |>
        tibble::deframe()
      dplyr::coalesce(unname(karta[kol]), kol)
    })) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ ifelse(. == "Riket", "00 Riket", .))) |>
    dplyr::mutate(dplyr::across(where(is.character), ~ as.character(unname(.))))
}
