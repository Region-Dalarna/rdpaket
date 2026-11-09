#' Hämta Region Dalarnas webb-/pdf-rapporter för en "Rapporter"-slide
#'
#' Skrapar Samhällsanalys webbplats (regiondalarna.se) efter länkar till
#' publicerade webbrapporter och pdf-rapporter, för
#' `"Rapporter_Samhallsanalys"`-mallen i [skapa_ppt_fran_lista()].
#'
#' @return En `tibble` med kolumnerna `text`, `url` och `typ` (`"webb"`
#'   eller `"pdf"`).
#' @export
rapporter_rd_webb_hamta <- function() {
  # URL till webbsidan
  url <- "https://www.regiondalarna.se/verksamhet/regional-utveckling/statistik-och-rapporter/"

  webbsida <- rvest::read_html(url)

  alla_lankar <- rvest::html_nodes(webbsida, "a")
  lankar_df <- dplyr::tibble(
    text = rvest::html_text(alla_lankar, trim = TRUE),
    url = rvest::html_attr(alla_lankar, "href")
  )

  github_lankar <- lankar_df |>
    dplyr::filter(stringr::str_detect(url, "region-dalarna.github.io|https://samhallsanalys.regiondalarna.se")) |>
    dplyr::mutate(typ = "webb")

  pdf_lankar <- lankar_df |>
    dplyr::filter(stringr::str_detect(url, "\\.pdf$")) |>
    dplyr::mutate(typ = "pdf",
           text = stringr::str_remove(text, "\\(.*"),
           artal = stringr::str_extract(text, "\\d{4}"),
           text = stringr::str_remove(text, ".*:\\s*"),
           text = stringr::str_trim(paste0(text, " (", artal, ")")),
           text = dplyr::if_else(
             stringr::str_detect(text, "Fakta om Dalarna"),
             stringr::str_replace(
               text,
               "Fakta om Dalarna.*version (\\d{6}).*",
               function(x) {
                 datum_str <- stringr::str_match(x, "version (\\d{6})")[, 2]
                 datum <- suppressWarnings(lubridate::ymd(paste0("20", datum_str)))
                 manad_ar <- format(datum, "%b %Y")
                 paste0("Fakta om Dalarna (", tolower(manad_ar), ")")
               }
             ),
             text
           )) |>
    dplyr::arrange(dplyr::desc(artal)) |>
    dplyr::select(-artal)

  artal_fakta_om_dalarna <- pdf_lankar |>
    dplyr::filter(stringr::str_detect(tolower(text), "fakta|dalarna")) |>
    dplyr::pull(text) |>
    stringr::str_extract("(?<=\\().*?(?=\\))")

  hamta_uppdatering <- function(url) {
    tryCatch({
      text <- rvest::html_text(rvest::html_nodes(rvest::read_html(url), "p"))
      rad_med_datum <- text[stringr::str_detect(text, "uppdaterad:")]
      datum <- stringr::str_extract(rad_med_datum, "(?<=uppdaterad: )\\S+")
      return(datum)
    }, error = function(e) {
      return(NA)
    })
  }

  datum_fakta_om_dalarna <- if (identical(artal_fakta_om_dalarna, "NA") || is.na(artal_fakta_om_dalarna[1])) {
    pdf_lankar |>
      dplyr::filter(stringr::str_detect(text, "Fakta om Dalarna")) |>
      dplyr::pull(url) |>
      stringr::str_extract("[^/]+$") |>
      stringr::str_extract("\\d{6,8}") |>
      lubridate::ymd()
  } else {
    pdf_lankar |>
      dplyr::filter(stringr::str_detect(text, "Fakta om Dalarna")) |>
      dplyr::pull(text) |>
      stringr::str_extract("\\d{6,8}") |>
      lubridate::ymd()
  }

  datum_fakta_om_dalarna_txt <- format(datum_fakta_om_dalarna, "%b %Y")

  retur_df <- github_lankar |>
    dplyr::mutate(datum_sort = purrr::map_chr(url, hamta_uppdatering),
      uppdaterad_datum = as.Date(datum_sort),
           text = stringr::str_trim(paste0(text, " (", format(uppdaterad_datum, "%b %Y"), ")"))) |>
    dplyr::bind_rows(pdf_lankar) |>
    dplyr::mutate(
      text = dplyr::if_else(stringr::str_detect(text, "Fakta om Dalarna"), paste0("Fakta om Dalarna (", datum_fakta_om_dalarna_txt, ")"), text),
      typ = dplyr::if_else(stringr::str_detect(text, "Fakta om Dalarna"), "webb", typ),
      datum_sort = dplyr::if_else(stringr::str_detect(text, "Fakta om Dalarna"), as.character(datum_fakta_om_dalarna), datum_sort)
    ) |>
    dplyr::arrange(dplyr::desc(typ), dplyr::desc(datum_sort)) |>
    dplyr::select(-datum_sort)

  return(retur_df)
}
