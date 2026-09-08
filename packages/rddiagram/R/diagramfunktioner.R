# Hjälpfunktioner för diagram - utbrutna ur func_diagramfunktioner.R.

#' Sökväg (URL) till Region Dalarnas logga
#'
#' @return En URL till logga-PNG:en i `Region-Dalarna/depot`.
#' @export
hamta_logga_path <- function() {
  "https://raw.githubusercontent.com/Region-Dalarna/depot/main/rd_logo_liggande_fri_svart.png"
}

#' Antal siffror i heltalsdelen av ett tal
#'
#' @param x Ett tal.
#' @return Antal siffror i `trunc(abs(x))`.
#' @export
nDigits <- function(x) nchar(trunc(abs(x)))

#' Hitta ett jämnt antal tjocka stödlinjer
#'
#' @param max_varde Preliminärt maxvärde för axeln.
#' @param multipel Grundsteg (oftast 5).
#' @param div_varden Tillåtna antal tjocka stödlinjer.
#'
#' @return En lista med `slut_div` (antal steg) och `resultat` (ev. justerat
#'   maxvärde).
#' @export
hitta_div_for_jamn_intervall <- function(max_varde, multipel, div_varden = 4:12) {
  multipel_vekt <- c(multipel, multipel * 2, multipel * multipel)

  har_godkant <- FALSE
  nytt_max <- max_varde
  godkant_heltal <- NA

  while (!har_godkant) {
    antal_maj <- nytt_max / multipel_vekt
    kandidater <- div_varden[div_varden %in% antal_maj]
    if (length(kandidater) > 0) {
      godkant_heltal <- min(kandidater)
      har_godkant <- TRUE
    } else {
      nytt_max <- nytt_max + multipel
    }
  }
  list(slut_div = godkant_heltal, resultat = nytt_max)
}

#' Avrunda uppåt till närmaste "fina" tal
#'
#' `avrunda_till_multipel(73)` ger `75`, `avrunda_till_multipel(88743)` ger
#' `90000`. För snyggare y-axlar.
#'
#' @param n Talet som avrundas.
#' @param multipel_in Multipel att avrunda till (standard 5).
#'
#' @return En lista med `max_varde`, `maj_by_var` och `min_by_var`.
#' @export
avrunda_till_multipel <- function(n, multipel_in = 5) {
  multiple_diff <- nDigits(n) - nDigits(multipel_in) - 1
  multipel <- (10^multiple_diff) * multipel_in

  resultat <- floor((n + multipel / 2) / multipel) * multipel
  resultat <- as.numeric(format(resultat, scientific = FALSE))

  repeat {
    if (n > 0) {
      if (resultat > n) break else resultat <- resultat + multipel
    } else {
      if (resultat < n) break else resultat <- resultat - multipel
    }
  }

  retur_div <- hitta_div_for_jamn_intervall(max_varde = resultat, multipel = multipel)
  resultat <- retur_div$resultat
  slut_div <- retur_div$slut_div
  maj_by_var <- resultat / slut_div

  if (n > 0) {
    ny_resultat <- if (maj_by_var * (slut_div - 1) > n) resultat - maj_by_var else resultat
  } else {
    ny_resultat <- if (maj_by_var * (slut_div - 1) < n) resultat + maj_by_var else resultat
  }

  list(max_varde = ny_resultat, maj_by_var = maj_by_var, min_by_var = maj_by_var / multipel_in)
}

#' "Snyggt" intervallsteg enligt 1-2.5-5-10-serien
#'
#' @param spann Datats totala spann.
#' @param antal_optimala_intervall Ungefärligt önskat antal intervall.
#'
#' @return Ett intervallsteg.
#' @export
nice_breaks <- function(spann, antal_optimala_intervall = 5) {
  exp10 <- 10^floor(log10(spann / antal_optimala_intervall))
  steg <- spann / (antal_optimala_intervall * exp10)
  multipel <- if (steg < 1.5) 1 else if (steg < 7.5) 2.5 else if (steg < 15) 5 else 10
  exp10 * multipel
}

#' Beräkna min/max och stödlinjeintervall för en diagramaxel
#'
#' @param min_varde,max_varde Datats min- och maxvärde.
#' @param y_borjar_pa_noll Tvinga axeln att börja vid 0 om datat är positivt.
#' @param procent_0_100_10intervaller Fast skala 0-100 med 10-steg.
#' @param max_antal_stodlinjer Max antal tjocka stödlinjer.
#' @param avrunda_fem Använd 1-2.5-5-metoden i stället för den äldre logiken.
#' @param minus_plus_samma Gör minus- och plusskalan lika stora.
#'
#' @return En lista med `min_yvar`, `max_yvar`, `min_by_yvar`, `maj_by_yvar`.
#' @export
Berakna_varden_stodlinjer <- function(min_varde, max_varde, y_borjar_pa_noll = TRUE,
                                      procent_0_100_10intervaller = FALSE,
                                      max_antal_stodlinjer = 8, avrunda_fem = FALSE,
                                      minus_plus_samma = FALSE) {

  min_och_max_mindre_an_noll <- min_varde < 0 && max_varde < 0
  if (min_och_max_mindre_an_noll) {
    min_varde <- abs(min_varde)
    max_varde <- abs(max_varde)
    if (min_varde > max_varde) {
      temp <- min_varde; min_varde <- max_varde; max_varde <- temp
    }
  }

  if (procent_0_100_10intervaller) {
    return(list(min_yvar = 0, max_yvar = 100, min_by_yvar = 2, maj_by_yvar = 10))
  }

  modifierat_varde <- FALSE

  if (avrunda_fem) {
    spann <- abs(max_varde - min_varde)
    maj_by_yvar <- nice_breaks(spann)
    if (spann < 10 && maj_by_yvar < 1) maj_by_yvar <- 1
    if (spann < 5 && maj_by_yvar < 0.5) maj_by_yvar <- 0.5

    if (ceiling(spann / maj_by_yvar) > max_antal_stodlinjer) maj_by_yvar <- maj_by_yvar * 2
    min_by_yvar <- maj_by_yvar / 5

    max_yvar <- ceiling(max_varde / maj_by_yvar) * maj_by_yvar
    min_yvar <- floor(min_varde / maj_by_yvar) * maj_by_yvar
    if (max_yvar < max_varde) max_yvar <- max_yvar + maj_by_yvar
    if (min_yvar > min_varde) min_yvar <- min_yvar - maj_by_yvar
    if (y_borjar_pa_noll && min_yvar > 0) min_yvar <- 0
    if (min_varde < 0 && max_varde < 0) max_yvar <- 0

  } else {
    if (max_varde < 1) {
      max_varde <- max_varde * 100
      min_varde <- min_varde * 100
      modifierat_varde <- TRUE
    }

    min_yvar <- if (min_varde > 0 && y_borjar_pa_noll) 0 else round(min_varde, (nchar(trunc(min_varde)) - 2) * -1)
    if (min_yvar > min_varde) min_yvar <- floor(min_varde)
    max_yvar <- round(max_varde, (nchar(trunc(max_varde)) - 2) * -1)

    antal_siff_avrundn <- if (nchar(trunc(max_yvar)) < 2) -1 else -2
    if (max_yvar < max_varde) {
      steg <- 10^(nchar(trunc(max_yvar)) + antal_siff_avrundn)
      max_yvar <- ceiling(max_yvar / steg) * steg
    }

    maj_by_yvar <- round((max_yvar - min_yvar) / 6, (nchar(trunc((max_yvar - min_yvar) / 6)) - 1) * -1)
    maj_by_yvar <- 2 * (if (floor(maj_by_yvar / 2) == 0) 1 else ceiling(maj_by_yvar / 2))

    max_yvar <- round(max_yvar / maj_by_yvar) * maj_by_yvar
    if (max_yvar < max_varde) max_yvar <- (round(max_yvar / maj_by_yvar) + 1) * maj_by_yvar

    min_by_yvar <- NA
    for (d in c(5, 6, 4, 7)) {
      if (maj_by_yvar %% d == 0) { min_by_yvar <- maj_by_yvar / d; break }
    }
    if (is.na(min_by_yvar)) min_by_yvar <- maj_by_yvar / 5

    if (min_varde < 0 && max_varde > 0) {
      tv <- 0; while (tv < max_varde) tv <- tv + maj_by_yvar; max_yvar <- tv
      tv <- 0; while (tv > min_varde) tv <- tv - maj_by_yvar; min_yvar <- tv
    }
    if (min_varde < 0 && max_varde < 0) max_yvar <- 0
  }

  if (min_och_max_mindre_an_noll) {
    min_yvar <- min_yvar * -1
    max_yvar <- max_yvar * -1
    if (min_yvar > max_yvar) {
      temp <- min_yvar; min_yvar <- max_yvar; max_yvar <- temp
    }
  }

  if ((min_yvar < 0 && max_yvar > 0) && minus_plus_samma) {
    if (abs(min_yvar) > max_yvar) max_yvar <- abs(min_yvar) else min_yvar <- max_yvar * -1
  }

  stodlinjer <- list(min_yvar = min_yvar, max_yvar = max_yvar,
                     min_by_yvar = min_by_yvar, maj_by_yvar = maj_by_yvar)
  if (modifierat_varde) stodlinjer <- lapply(stodlinjer, function(v) v / 100)
  stodlinjer
}

#' Procentuell förändring mellan två år per grupp
#'
#' @param df En data.frame.
#' @param ar_kol Namn på årskolumnen.
#' @param gruppering_vect Kolumner att gruppera på.
#' @param summ_var Kolumn att summera.
#' @param startar,slutar Start- och slutår. `NA` = min/max i `ar_kol`.
#'
#' @return En `tibble` med ett värde per år och en förändringskolumn i procent.
#' @export
SkapaProcForandrTvaAr <- function(df, ar_kol, gruppering_vect, summ_var,
                                  startar = NA, slutar = NA) {
  if (is.na(startar)) startar <- min(df[[ar_kol]])
  if (is.na(slutar)) slutar <- max(df[[ar_kol]])

  retur_df <- df |>
    dplyr::filter(.data[[ar_kol]] %in% c(startar, slutar)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(ar_kol, gruppering_vect)))) |>
    dplyr::summarise(syss = sum(.data[[summ_var]]), .groups = "drop") |>
    tidyr::pivot_wider(names_from = dplyr::all_of(ar_kol), names_prefix = "ar_",
                       values_from = "syss")

  sista <- names(retur_df)[ncol(retur_df)]
  nast_sista <- names(retur_df)[ncol(retur_df) - 1]

  retur_df <- dplyr::mutate(
    retur_df,
    proc = ((.data[[sista]] - .data[[nast_sista]]) / .data[[nast_sista]]) * 100
  )
  retur_df$proc[is.infinite(retur_df$proc)] <- NA
  names(retur_df)[ncol(retur_df)] <-
    paste0("Förändring ", tolower(summ_var), " ", startar, "-", slutar, " (procent)")
  retur_df
}

#' Region Dalarnas färgskalor för diagram
#'
#' @param farg Namn på färgskalan, t.ex. `"rus_sex"`, `"kon"`, `"rd_alla_primar"`.
#'
#' @return En teckenvektor med hex-färger.
#' @export
diagramfarger <- function(farg = "rus_sex") {
  farger <- list(
    rus_gradient = c("#93CEC1", "#87C7B9", "#7CC0B2", "#71BAAB", "#65B3A3", "#5AAC9C",
                     "#4FA695", "#449F8E", "#389886", "#2D927F", "#228B78", "#178571"),
    rus_tva_fokus = c("#93cec1", "#178571"),
    rus_tva_gra = c("#178571", "#e6e6e6"),
    rus_tre_fokus = c("#93cec1", "#178571", "#000000"),
    rus_sex = c("#178571", "#93cec1", "#0e5a4c", "#8edded", "#158daf", "#00577b"),
    orange_en = c("#ED7D31"),
    orange_tva = c("#833C0C", "#ED7D31"),
    orange_tre_fokus = c("#ED7D31", "#833C0C", "#000000"),
    orange_fyra = c("#F5C2B1", "#DE752D", "#BA6124", "#833C0C"),
    orange_sex = c("#F5BEAF", "#EB9A93", "#DE752D", "#BA6124", "#975020", "#673513"),
    gron_tva_fokus_morkgron = c("#70AD47", "#375623"),
    gron_tva_fokus = c("#70AD47", "#000000"),
    gron_tre_fokus = c("#70AD47", "#375623", "#000000"),
    gron_fyra = c("#A9D08E", "#70AD47", "#548235", "#375623"),
    gron_sex = c("#E2EFDA", "#C6E0B4", "#A9D08E", "#70AD47", "#548235", "#375623"),
    gron_export = c("#A1D99B", "#74C476", "#41AB5D", "#238B45", "#006D2C"),
    gron_export_en = c("#41AB5D"),
    bla_sex = c("#F7FBFF", "#C6DBEF", "#6BAED6", "#2171B5", "#08306B"),
    kon = c("#e2a855", "#459079"),
    kon_och_total = c("#e2a855", "#459079", "#969696"),
    kon_fokus = c("#e2a855", "#459079", "#ED7D31", "#0e5a4c"),
    grp_und_kon = c("#FFEC9F", "#FFD378", "#E2A855", "#93CEC1", "#54B798", "#459079"),
    "Kön" = c("#e2a855", "#459079"),
    Fodelseland_kat = c("#E2EFDA", "#C6E0B4", "#A9D08E", "#70AD47", "#548235", "#375623"),
    Vistelsetid_kat = c("#E2EFDA", "#C6E0B4", "#A9D08E", "#70AD47", "#548235", "#375623"),
    socek_kat = c("#F5BEAF", "#EB9A93", "#DE752D", "#BA6124", "#975020", "#673513"),
    funk_kat = c("#FDE0DD", "#FA9FB5", "#DD3497", "#AE017E", "#7A0177", "#49006A"),
    "Kön_år" = c("#9ECAE1", "#4292C6", "#08519C"),
    "Kön_alla" = c("#FFEC9F", "#93CEC1", "#FFD378", "#54B798", "#E2A855", "#459079"),
    Fodelseland_kat_alla = c("#70AD47", "#E2EFDA", "#548235", "#C6E0B4", "#375623", "#A9D08E"),
    Vistelsetid_kat_alla = c("#A9D08E", "#70AD47", "#548235", "#375623", "#A9D08E", "#70AD47",
                             "#548235", "#375623", "#A9D08E", "#70AD47", "#548235", "#375623"),
    socek_kat_alla = c("#BA6124", "#F5BEAF", "#975020", "#EB9A93", "#673513", "#DE752D"),
    kommun_jmfr = c("#8C6BB1", "#88419D", "#810F7C"),
    utan_grp_alla = c("#EC7014", "#CC4C02", "#993404", "#662506"),
    rd_alla_primar = c("#F15060", "#FFD378", "#00B4E4", "#54B798", "#969696"),
    rd_bla = c("#00B4E4", "#8EDDED", "#B6F0FD", "#0074A2"),
    rd_gron = c("#54B798", "#93CEC1", "#D5EAE6", "#459079"),
    rd_gul = c("#FFD378", "#FFEC9F", "#FFF5CC", "#E2A855"),
    rd_rod = c("#F15060", "#F8AAB6", "#FFDDE2", "#AE2D3A"),
    rd_gra = c("#969696", "#e6e6e6", "#f1f1f1", "#424242"),
    rd_karta_gron = c("#459079", "#54B798", "#93CEC1", "#D5EAE6"),
    rd_karta_gron_sex = c("#3C7C69", "#459079", "#54B798", "#7FCAB5", "#BFE2DC", "#E3F3F1"),
    rd_karta_gron_sju = c("#356F5F", "#459079", "#54B798", "#7FCAB5", "#A9D9CE", "#D5EAE6", "#ECF7F6"),
    rd_primar_atta = c("#F15060", "#FFD378", "#00B4E4", "#54B798", "#969696", "#F8AAB6",
                       "#0074A2", "#459079"),
    rd_primar_nio = c("#F15060", "#00B4E4", "#54B798", "#AE2D3A", "#0074A2", "#459079",
                      "#E2A855", "#F8AAB6", "#969696"),
    rd_handelsbalans = c("#54B798", "#969696", "#459079"),
    rd_export_gron = c("#93CEC1", "#459079", "#54B798", "#93CEC1", "#459079", "#54B798",
                       "#93CEC1", "#459079", "#54B798", "#93CEC1", "#459079"),
    rd_gron_tva_fokus = c("#54B798", "#000000"),
    rd_gron_tre_fokus = c("#54B798", "#459079", "#000000"),
    bla_gra_tre = c("#5B9BD5", "#BFBFBF", "#1F4E78"),
    bla_gra_fyra = c("#5B9BD5", "#BFBFBF", "#1F4E78", "#969696"),
    gron_gul_tvagrp_fyra = c("#375623", "#548235", "#70AD47", "#C6E0B4", "#806000",
                             "#BF8F00", "#FFC000", "#FFE699"),
    gron_yrke_4_fokus = c("#000000", "#548235", "#70AD47", "#375623"),
    utb_floden = c("#70AD47", "#548235", "#375623", "#F4A460", "#D2691E", "#8B4513"),
    etabl_fokus = c("#4472C4", "#203864"),
    konsbalans_fem = c("#E2A855", "#FFD378", "#969696", "#459079", "#54B798"),
    kon_bakgrund = c("#e2a855", "#459079", "#00B4E4", "#0074A2"),
    rsp_enkat = c("#178571", "#0e5a4c", "#158daf", "#00577b", "#BFBFBF")
  )

  if (!farg %in% names(farger)) {
    stop("Okänd färgskala: '", farg, "'. Tillgängliga: ",
         paste(names(farger), collapse = ", "), call. = FALSE)
  }
  farger[[farg]]
}
