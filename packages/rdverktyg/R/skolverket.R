# Skolverket-funktioner - utbrutna ur func_API.R.

#' Bygg kolumnnamn ur flerradiga rubriker
#'
#' Slår ihop värden från flera rubrikrader (uppifrån och ned, och åt vänster
#' vid tomma celler) till ett kolumnnamn. Praktiskt för Skolverkets
#' Excel-exporter med sammanslagna rubrikceller.
#'
#' @param df Data.frame där de första `namnrad` raderna är rubriker.
#' @param namnrad Radnummer för den nedersta (mest specifika) rubrikraden.
#' @param separator Tecken mellan rubriknivåerna i det sammansatta namnet.
#' @param konv_kolnamn_gemener Om `TRUE` görs den nedersta nivån till gemener.
#'
#' @return En teckenvektor med ett namn per kolumn.
#' @export
skolverket_generera_kolumnnamn <- function(df, namnrad, separator = " ",
                                           konv_kolnamn_gemener = TRUE) {
  namnmatris <- as.matrix(
    data.frame(lapply(df[seq_len(namnrad), , drop = FALSE], as.character),
               stringsAsFactors = FALSE)
  )
  colnames(namnmatris) <- paste0("X", seq_len(ncol(namnmatris)))

  basnamn <- namnmatris[namnrad, ]
  dubbletter <- duplicated(basnamn) | duplicated(basnamn, fromLast = TRUE)

  forsta_icke_tomma_vanster <- function(rad, kolindex) {
    while (kolindex > 0) {
      v <- namnmatris[rad, kolindex]
      if (!is.na(v) && v != "") return(v)
      kolindex <- kolindex - 1
    }
    ""
  }

  skapa_namn <- function(kolindex) {
    namn <- character(0)
    for (rad in seq(namnrad, 1)) {
      v <- namnmatris[rad, kolindex]
      if (is.na(v) || v == "") v <- forsta_icke_tomma_vanster(rad, kolindex - 1)
      if (is.na(v) || v == "") break
      niva <- if (rad == namnrad && konv_kolnamn_gemener) tolower(v) else v
      namn <- c(niva, namn)
    }
    paste(namn, collapse = separator)
  }

  vapply(seq_along(basnamn), function(i) {
    if (dubbletter[i]) skapa_namn(i) else basnamn[i]
  }, character(1))
}

#' Hitta första datarad i en Skolverks-tabell
#'
#' Returnerar radnumret för första positionen där minst `min_langd` rader i
#' följd har ett värde i `kolumn`.
#'
#' @param df Data.frame.
#' @param kolumn Kolumn att kontrollera (namn eller position).
#' @param min_langd Antal rader i följd med värde som krävs.
#'
#' @return Radnummer, eller `NA` om ingen sådan rad finns.
#' @export
skolverket_hitta_startrad <- function(df, kolumn = 1, min_langd = 5) {
  vektor <- as.character(df[[kolumn]])
  har_varde <- !is.na(vektor) & vektor != ""
  for (i in seq_len(length(har_varde) - min_langd + 1)) {
    if (all(har_varde[i:(i + min_langd - 1)])) return(i)
  }
  NA_integer_
}

#' Hämta gymnasieprogrammens inriktningskoder från Skolverkets API
#'
#' @param url API-endpoint för program.
#'
#' @return En `tibble` med `Kod`, `Namn` och `skolform`, eller `NULL` vid fel.
#' @export
gymnprg_inr_koder_hamta_api_skolverket <- function(
    url = "https://api.skolverket.se/planned-educations/v3/support/programs") {

  tryCatch({
    resp <- httr::GET(url)
    httr::stop_for_status(resp)
    kropp <- jsonlite::fromJSON(
      httr::content(resp, as = "text", encoding = "UTF-8"), flatten = TRUE
    )$body

    gy_alla <- kropp[stringr::str_detect(names(kropp), "gy")] |>
      purrr::imap(~ dplyr::mutate(.x, skolform = .y)) |>
      purrr::list_rbind() |>
      dplyr::mutate(skolform = dplyr::case_when(
        .data$skolform == "gy" ~ "Gymnasieskola",
        .data$skolform == "gyan" ~ "Anpassad gymnasieskola",
        TRUE ~ .data$skolform
      ))

    gy_df <- dplyr::bind_rows(
      dplyr::select(gy_alla, "code", "name", "skolform"),
      tidyr::unnest(dplyr::select(gy_alla, "studyPaths", "skolform"),
                    "studyPaths", keep_empty = TRUE)
    ) |>
      dplyr::distinct() |>
      dplyr::filter(!is.na(.data$code)) |>
      dplyr::rename(Kod = "code", Namn = "name")

    gy25 <- gy_df |>
      dplyr::filter(stringr::str_detect(.data$Kod, "25")) |>
      dplyr::mutate(Kod = stringr::str_remove_all(.data$Kod, "25"))

    dplyr::bind_rows(gy_df, gy25)
  }, error = function(e) NULL)
}
