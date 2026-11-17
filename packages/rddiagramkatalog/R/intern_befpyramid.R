# Hjälpfunktioner för diag_befpyramid_scb(): hämtar befolkning (utfall och prognos) per region, kön och ålder, och
# eventuellt uppdelat på inrikes och utrikes födda, ur SCB:s tabeller med pxweb2r.
#
# Tabeller:
#   utfall, totalt:           TAB638 (till och med 2024) och TAB5557 (2025, CKM)
#   utfall, inrikes/utrikes:  TAB4823 (till och med 2024) och TAB6645 (2025, CKM)
#   prognos, regioner:        TAB698 (befolkningsframskrivning 2024, län och kommuner, till 2070)
#   prognos, riket:           TAB5974 (inrikes/utrikes, från 2026 till 2120)
#   prognos, regioner, inrikes/utrikes: TAB6302 (framskrivning 2022, föråldrad, till 2070)

# Ålderskoder för ettårsklasser i en tabell: 0-99 år, och för 100 år och äldre antingen den sammanslagna gruppen
# "100+ år" eller, om tabellen har ettårsklasser över 99 (t.ex. "100 år" ... "105+ år"), dessa (de slås ihop till "100+ år"
# senare). Aggregerade åldersgrupper hoppas över.
#' @noRd
intern_befpyramid_aldrar <- function(tabell) {
  v <- pxweb2r::pxweb2_get_values(tabell, "Alder", quiet = TRUE)
  v <- v[!duplicated(v$label), ]
  ar_etikett <- grepl("^[0-9]+\\+? år$", v$label)
  tal <- suppressWarnings(as.numeric(sub("^([0-9]+)\\+? år$", "\\1", v$label)))
  har_ettarsklasser_over_99 <- any(ar_etikett & tal >= 100 & v$label != "100+ år")
  ok <- ar_etikett & (tal <= 99 | (har_ettarsklasser_over_99 & v$label != "100+ år") |
                       (!har_ettarsklasser_over_99 & v$label == "100+ år"))
  v$code[ok]
}

# Tidskoder en tabell har (som textvektor).
#' @noRd
intern_befpyramid_tid <- function(tabell) pxweb2r::pxweb2_get_values(tabell, "Tid", quiet = TRUE)$code

# Gör om ett uttag till gemensamma kolumner: regionkod, region, kon, alder, ar, fodd (inrikes/utrikes, annars NA), bef.
#' @noRd
intern_befpyramid_standard <- function(df, fodd_kol = NULL) {
  df <- dplyr::rename(df, regionkod = region_kod, kon = `kön`, alder = `ålder`, ar = `år`, bef = value)
  df$fodd <- if (is.null(fodd_kol)) NA_character_ else df[[fodd_kol]]
  df$fodd <- dplyr::case_when(
    grepl("^född i Sverige$|^inrikes född", df$fodd) ~ "inrikes födda",
    grepl("^utrikes född", df$fodd) ~ "utrikes födda",
    TRUE ~ df$fodd)
  dplyr::select(df, regionkod, region, kon, alder, ar, fodd, bef)
}

# Hämtar utfall eller prognos för de angivna åren. `med_fodd = TRUE` ger inrikes och utrikes födda (varje födelseregion
# som egen rad), annars totalt. Returnerar en data.frame med kolumnerna ovan plus `typ` ("utfall" eller "prognos")
# och `kalla` (text om var värdena kommer ifrån).
#' @noRd
intern_befpyramid_hamta <- function(region_vekt, ar_vekt, med_fodd) {
  ar_vekt <- as.character(ar_vekt)
  tid_638 <- intern_befpyramid_tid("TAB638"); tid_5557 <- intern_befpyramid_tid("TAB5557")
  senaste_utfall <- max(c(tid_638, tid_5557))
  utfall_ar <- ar_vekt[ar_vekt <= senaste_utfall]
  prognos_ar <- ar_vekt[ar_vekt > senaste_utfall]

  riket <- region_vekt[region_vekt == "00"]
  ej_riket <- region_vekt[region_vekt != "00"]
  delar <- list()

  hamta <- function(tabell, query) pxweb2r::pxweb2_get_data(table = tabell, query = query, quiet = TRUE)

  # ------------------------------------------------------------------ utfall
  if (!med_fodd) {
    ar_638 <- intersect(utfall_ar, tid_638); ar_5557 <- intersect(utfall_ar, tid_5557)
    if (length(ar_638) > 0) {
      delar$u638 <- hamta("TAB638", list(Region = region_vekt, Civilstand = c("OG", "G", "SK", "ÄNKL"),
                                         Alder = intern_befpyramid_aldrar("TAB638"), Kon = c("1", "2"),
                                         ContentsCode = "BE0101N1", Tid = ar_638)) |>
        dplyr::group_by(region_kod, region, `kön`, `ålder`, `år`) |>
        dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
        intern_befpyramid_standard() |> dplyr::mutate(typ = "utfall", kalla = "utfall")
    }
    if (length(ar_5557) > 0) {
      delar$u5557 <- hamta("TAB5557", list(Region = region_vekt, Civilstand = "SC",
                                           Alder = intern_befpyramid_aldrar("TAB5557"), Kon = c("1", "2"),
                                           ContentsCode = "000007ME", Tid = ar_5557)) |>
        intern_befpyramid_standard() |> dplyr::mutate(typ = "utfall", kalla = "utfall")
    }
  } else {
    tid_4823 <- intern_befpyramid_tid("TAB4823"); tid_6645 <- intern_befpyramid_tid("TAB6645")
    ar_4823 <- intersect(utfall_ar, tid_4823); ar_6645 <- intersect(utfall_ar, tid_6645)
    if (length(ar_4823) > 0) {
      delar$u4823 <- hamta("TAB4823", list(Region = region_vekt, Alder = intern_befpyramid_aldrar("TAB4823"),
                                           Kon = c("1", "2"), Fodelseregion = c("09", "11"),
                                           ContentsCode = "000001NS", Tid = ar_4823)) |>
        intern_befpyramid_standard("födelseregion") |> dplyr::mutate(typ = "utfall", kalla = "utfall")
    }
    if (length(ar_6645) > 0) {
      delar$u6645 <- hamta("TAB6645", list(Region = region_vekt, Alder = intern_befpyramid_aldrar("TAB6645"),
                                           Kon = c("1", "2"), Fodelseregion = c("09", "11"),
                                           ContentsCode = "0000086R", Tid = ar_6645)) |>
        intern_befpyramid_standard("födelseregion") |> dplyr::mutate(typ = "utfall", kalla = "utfall")
    }
  }

  # ------------------------------------------------------------------ prognos
  if (length(prognos_ar) > 0) {
    # riket: SCB:s nationella framskrivning (inrikes/utrikes, totalt är koden 83)
    if (length(riket) > 0) {
      ar_riket <- intersect(prognos_ar, intern_befpyramid_tid("TAB5974"))
      if (length(ar_riket) > 0) {
        delar$p_riket <- hamta("TAB5974", list(InrikesUtrikes = if (med_fodd) c("13", "23") else "83",
                                               Alder = intern_befpyramid_aldrar("TAB5974"), Kon = c("1", "2"),
                                               ContentsCode = "000005NO", Tid = ar_riket)) |>
          dplyr::rename(fodd_kol = `inrikes/utrikes född`) |>
          dplyr::mutate(region_kod = "00", region = "Riket") |>
          intern_befpyramid_standard("fodd_kol") |>
          dplyr::mutate(typ = "prognos", kalla = "SCB:s befolkningsprognos för riket")
        if (!med_fodd) delar$p_riket$fodd <- NA_character_
      }
    }
    # regioner
    if (length(ej_riket) > 0) {
      if (!med_fodd) {
        ar_reg <- intersect(prognos_ar, intern_befpyramid_tid("TAB698"))
        if (length(ar_reg) > 0) {
          delar$p_reg <- hamta("TAB698", list(Region = ej_riket, Alder = intern_befpyramid_aldrar("TAB698"),
                                              Kon = c("1", "2"), ContentsCode = "000004LG", Tid = ar_reg)) |>
            intern_befpyramid_standard() |>
            dplyr::mutate(typ = "prognos", kalla = "SCB:s regionala befolkningsprognos 2024")
        }
      } else {
        ar_reg <- intersect(prognos_ar, intern_befpyramid_tid("TAB6302"))
        if (length(ar_reg) > 0) {
          delar$p_reg_fodd <- hamta("TAB6302", list(Region = ej_riket, InrikesUtrikes = c("13", "23"),
                                                    Alder = intern_befpyramid_aldrar("TAB6302"), Kon = c("1", "2"),
                                                    ContentsCode = "000006N8", Tid = ar_reg)) |>
            dplyr::rename(fodd_kol = `inrikes/utrikes född`) |>
            intern_befpyramid_standard("fodd_kol") |>
            dplyr::mutate(typ = "prognos",
                          kalla = "SCB:s regionala befolkningsprognos 2022 (föråldrad, den senaste med inrikes och utrikes födda per region)")
        }
      }
    }
  }

  resultat <- dplyr::bind_rows(delar)
  if (nrow(resultat) == 0) stop("Inga data hittades för de valda regionerna och åren.", call. = FALSE)
  resultat
}

# Åldersgrupper (eller ettårsklasser) som ordnad faktor. Ettårsklasserna 100 år och äldre slås ihop till "100+ år".
#' @noRd
intern_befpyramid_aldergrupper <- function(alder, aldersgrupper) {
  tal <- suppressWarnings(as.numeric(gsub("\\D", "", alder)))
  if (aldersgrupper) {
    grans <- c(0, 5, 15, 25, 35, 45, 55, 65, 75, 85, 95, Inf)
    etiketter <- c("0–4 år", "5–14 år", "15–24 år", "25–34 år", "35–44 år", "45–54 år", "55–64 år", "65–74 år",
                   "75–84 år", "85–94 år", "95+ år")
    factor(etiketter[findInterval(tal, grans)], levels = etiketter)
  } else {
    faktor <- ifelse(tal >= 100, "100+ år", paste0(tal, " år"))
    factor(faktor, levels = c(paste0(0:99, " år"), "100+ år"))
  }
}
