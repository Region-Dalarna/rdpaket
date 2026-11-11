# Delad datahämtning för karta_gg_befolkningsprognos_kommun_scb() och
# karta_lf_befolkningsprognos_kommun_scb(): procentuell befolkningsförändring per kommun mellan
# start_ar och till_ar enligt prognosen. Returnerar kartobjektet (sf) och det som behövs till
# titlar, filnamn och färgskala.
#' @noRd
intern_befprognos_kartdata <- function(region_vekt, start_ar, till_ar, filnamns_prefix,
                                       kommuner_egetnamn) {

  kommunnyckel <- rdverktyg::hamtaregtab() |>                     # hämta alla kommunkoder
    dplyr::filter(nchar(regionkod) == 4) |>
    dplyr::rename(Kommun = region,
           Kommunkod = regionkod)

  region_kommuner <- region_vekt[nchar(region_vekt) == 4]            # lägg alla kommunkoder i en vektor
  region_lan <- region_vekt[nchar(region_vekt) == 2]                 # lägg alla länskoder i en vektor
  kommunkoder <- rdverktyg::hamtakommuner(region_lan, FALSE, FALSE) |>       # hämta alla kommuner som finns i län med länskod
    c(region_kommuner)                                               # och lägg ihop med skickade kommunkoder

  kommunkoder <- kommunkoder[kommunkoder %in% kommunnyckel$Kommunkod]      # behåll bara kommunkoder som är giltiga

  filnamn_kommuner <- rdverktyg::ar_alla_kommuner_i_ett_lan(kommunkoder, returnera_text = TRUE, returtext = paste0(kommunkoder, collapse = "_")) |> stringr::str_replace_all(" ", "_")

  if (is.na(kommuner_egetnamn)) {
    retur_kommuner <- rdverktyg::hamtaregion_kod_namn(kommunkoder)$region |> rdverktyg::list_komma_och()
    kommuner_egetnamn <- rdverktyg::ar_alla_kommuner_i_ett_lan(kommunkoder, returnera_text = TRUE, returtext = retur_kommuner)
  }

  # "+N" ska betyda "N år efter startåret" - hamta_befprognos_data() tolkar "+N" som prognosår + N, så
  # vi ber om "+/-(N-1)" för att landa på samma målår.
  till_ar_hamta <- if (stringr::str_detect(till_ar, "^[+-][0-9]+$")) {
    tecken <- stringr::str_sub(till_ar, 1, 1)
    paste0(tecken, as.numeric(stringr::str_sub(till_ar, 2)) - 1)
  } else till_ar

  if (any(stringr::str_sub(kommunkoder, 1, 2) != "20")) {
    befprogn <- rdhamtadata::hamta_befprognos_data(region_vekt = kommunkoder,
                                      url_prognos_vektor = "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefProgOsiktRegN",
                                      tid_vekt = till_ar_hamta)
  } else {
    befprogn <- rdhamtadata::hamta_befprognos_data(region_vekt = kommunkoder,
                                      tid_vekt = till_ar_hamta)
  }

  # Hjälpfunktion: kod för en given ålder i en given tabell - TAB638 och TAB5557 har olika koder för
  # samma ålder (t.ex. är öppna åldersklassen "100+" i TAB638 men "100+1" i TAB5557, och "totalt"-koden
  # heter "tot" respektive "TotSA"). aldrar = NA ger bara totalkoden, "*" ger alla individuella åldrar,
  # en karaktärsvektor med specifika åldrar ("0".."99"/"100+") ger just dem.
  hamta_alderskoder <- function(table_id, aldrar) {
    totalkod <- if (table_id == "TAB638") "tot" else "TotSA"
    if (all(is.na(aldrar))) return(totalkod)
    v <- pxweb2r::pxweb2_get_values(table_id, "Alder", quiet = TRUE)
    v <- v[grepl("^[0-9]+\\+? år$", v$label), ]
    v <- v[!duplicated(v$label), ]
    if (identical(aldrar, "*")) return(v$code)
    label_sokt <- ifelse(aldrar == "100+", "100+ år", paste0(aldrar, " år"))
    v$code[match(label_sokt, v$label)]
  }

  # Civilstånd saknar en riktig "totalt"-kod i TAB638 - alla fyra hämtas explicit och summeras ihop.
  # "9999" (senaste tillgängliga år) hanteras här själva, eftersom det bara var en konvention i den
  # gamla v1-funktionen och inte förstås av pxweb2r.
  civilstand_hamta <- c("ogifta", "gifta", "skilda", "änkor/änklingar")
  hamta_bef_folkmangd_v2 <- function(region_vekt, tid, aldrar) {
    if (identical(tid, "9999")) {
      tid <- as.character(max(as.numeric(c(
        pxweb2r::pxweb2_get_values("TAB638", "Tid", quiet = TRUE)$code,
        pxweb2r::pxweb2_get_values("TAB5557", "Tid", quiet = TRUE)$code
      ))))
    }
    historik <- pxweb2r::pxweb2_get_data(
      table = "TAB638",
      query = list(Region = region_vekt, Civilstand = civilstand_hamta,
                   Alder = hamta_alderskoder("TAB638", aldrar), Kon = c("män", "kvinnor"),
                   ContentsCode = "Folkmängd", Tid = tid),
      on_all_values_invalid = "null", quiet = TRUE)
    ckm <- pxweb2r::pxweb2_get_data(
      table = "TAB5557",
      query = list(Region = region_vekt, Civilstand = civilstand_hamta,
                   Alder = hamta_alderskoder("TAB5557", aldrar), Kon = c("män", "kvinnor"),
                   ContentsCode = "Folkmängd", Tid = tid),
      on_all_values_invalid = "null", quiet = TRUE)

    dplyr::bind_rows(historik, ckm) |>
      dplyr::rename(regionkod = region_kod, Folkmängd = value) |>
      dplyr::select(-tabellinnehåll) |>
      dplyr::mutate(ålder = ifelse(ålder %in% c("totalt, samtliga åldrar", "tot", "TotSA"), "totalt ålder", ålder)) |>
      dplyr::group_by(dplyr::across(-c(civilstånd, Folkmängd))) |>
      dplyr::summarise(Folkmängd = sum(Folkmängd, na.rm = TRUE), .groups = "drop")
  }

  befutfall <- hamta_bef_folkmangd_v2(region_vekt = kommunkoder, tid = start_ar, aldrar = "*") |>
    dplyr::mutate(prognos_ar = "utfall")

  bef_diff <- befutfall |>
    dplyr::bind_rows(befprogn) |>
    dplyr::group_by(år, regionkod, region, ålder, prognos_ar) |>
    dplyr::summarise(Folkmängd = sum(Folkmängd, na.rm = TRUE), .groups = "drop")

  min_ar <- min(bef_diff$år)
  max_ar <- max(bef_diff$år)
  prognos_ar <- unique(befprogn$prognos_ar)

  if (is.na(filnamns_prefix)) {
    filnamn_pre <- glue::glue("befprogn_karta_{filnamn_kommuner}_ar{min_ar}-{max_ar}")
  } else {
    filnamn_pre <- filnamns_prefix
  }

  bef_diff_tot <- bef_diff |>
    dplyr::filter(ålder != "totalt ålder") |>
    dplyr::group_by(år, regionkod, region) |>
    dplyr::summarise(Folkmängd = sum(Folkmängd, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(aldergrp = "totalt")

  bef_df <- bef_diff_tot |>
    tidyr::pivot_wider(names_from = "år", values_from = "Folkmängd") |>
    dplyr::mutate(diff = .data[[max_ar]] - .data[[min_ar]],
           diff_proc = (.data[[max_ar]] - .data[[min_ar]]) / .data[[min_ar]] * 100)

  kommuner_sf <- rdgis::hamta_karta("kommuner", regionkoder = kommunkoder)

  bef_karta <- kommuner_sf |>
    dplyr::left_join(bef_df, by = c("knkod" = "regionkod")) |>
    dplyr::filter(aldergrp == "totalt")

  max_abs <- max(abs(bef_karta$diff_proc), na.rm = TRUE)

  list(bef_karta = bef_karta, min_ar = min_ar, max_ar = max_ar, prognos_ar = prognos_ar,
       kommuner_egetnamn = kommuner_egetnamn, filnamn_pre = filnamn_pre, max_abs = max_abs)
}
