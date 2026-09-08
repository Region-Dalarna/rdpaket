# Kolada-funktioner - utbrutna ur func_API.R. Kräver paketet rKolada (Suggests).

intern_kraver <- function(paket) {
  if (!requireNamespace(paket, quietly = TRUE)) {
    stop("Paketet '", paket, "' krävs för den här funktionen. ",
         "Installera det med install.packages(\"", paket, "\").", call. = FALSE)
  }
}

#' Giltiga år för ett Kolada-KPI
#'
#' @param kpi_id KPI-id, t.ex. `"N00945"`.
#' @param vald_region Kommunkod (nollutfylls till 4 tecken). Standard Falun.
#'
#' @return En teckenvektor med år.
#' @export
hamta_kolada_giltiga_ar <- function(kpi_id, vald_region = "2080") {
  intern_kraver("rKolada")
  vald_region <- stringr::str_pad(vald_region, 4, pad = "0")

  hamtade <- rKolada::get_values(
    kpi = kpi_id,
    municipality = vald_region,
    period = 1900:2060
  )
  unique(as.character(hamtade$year))
}

#' Hämta Kolada-data som en tidy tibble
#'
#' @param kpi_id Ett eller flera KPI-id.
#' @param valda_kommuner Kommunkoder (nollutfylls till 4 tecken).
#' @param valda_ar År att hämta. `NA` = alla giltiga år, `"9999"` = senaste år.
#' @param konsuppdelat Om `TRUE` behålls köns­uppdelade rader (K/M) när de finns.
#' @param konsuppdelat_total_ta_bort Om `TRUE` och `konsuppdelat = FALSE`,
#'   behåll bara totalrader.
#' @param dop_om_kolumner Om `TRUE` döps kolumnerna om till svenska
#'   (`ar`, `regionkod`, `region`, `kon`, `variabelkod`, `variabel`, `varde`).
#'
#' @return En `tibble`.
#' @export
hamta_kolada_df <- function(kpi_id, valda_kommuner, valda_ar = NA,
                            konsuppdelat = TRUE,
                            konsuppdelat_total_ta_bort = FALSE,
                            dop_om_kolumner = TRUE) {
  intern_kraver("rKolada")

  kolnamn_vektor <- c(ar = "year", regionkod = "municipality_id", region = "municipality",
                      kon = "gender", variabelkod = "kpi", variabel = "fraga", varde = "value")

  valda_kommuner <- stringr::str_pad(valda_kommuner, 4, pad = "0")

  alla_ar <- hamta_kolada_giltiga_ar(kpi_id, valda_kommuner[1])
  senaste_ar <- max(alla_ar)

  hamta_ar <- if (all(is.na(valda_ar))) {
    alla_ar
  } else if (all(valda_ar == "9999", na.rm = TRUE)) {
    senaste_ar
  } else {
    valda_ar[valda_ar %in% alla_ar]
  }

  hamtade <- rKolada::get_values(kpi = kpi_id, municipality = valda_kommuner, period = hamta_ar)
  kpi_df <- dplyr::select(rKolada::get_kpi(kpi_id), "id", "title")

  retur_df <- hamtade |>
    dplyr::left_join(kpi_df, by = c("kpi" = "id")) |>
    dplyr::rename(fraga = "title")

  if ("gender" %in% names(retur_df)) {
    if (konsuppdelat && nrow(retur_df[retur_df$gender %in% c("K", "M"), ]) > 0) {
      retur_df <- dplyr::filter(retur_df, .data$gender != "T")
    } else if (konsuppdelat_total_ta_bort) {
      retur_df <- dplyr::filter(retur_df, .data$gender == "T")
    }
    retur_df <- dplyr::mutate(retur_df, gender = dplyr::case_when(
      .data$gender == "T" ~ "Båda könen",
      .data$gender == "K" ~ "Kvinnor",
      .data$gender == "M" ~ "Män"
    ))
    if (!konsuppdelat && nrow(retur_df[retur_df$gender == "Båda könen", ]) > 0) {
      retur_df <- dplyr::filter(retur_df, .data$gender == "Båda könen")
    }
  }

  retur_df <- dplyr::mutate(retur_df, year = as.character(.data$year))

  if (dop_om_kolumner) {
    retur_df <- retur_df |>
      dplyr::select(dplyr::any_of(kolnamn_vektor)) |>
      dplyr::mutate(
        regionkod = stringr::str_replace(as.character(as.numeric(.data$regionkod)), "^0$", "00"),
        region = stringr::str_remove(.data$region, "Region ")
      )
  }
  retur_df
}
