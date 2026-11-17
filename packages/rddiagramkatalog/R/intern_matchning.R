# Översätter klartext till SCB:s koder för en variabel i en tabell (t.ex. utbildningsgrupper eller
# kön/ålder/födelseland i de regionala matchningsindikatorerna). Hittas klartexten flera gånger (t.ex. "totalt"
# finns som två koder) används den första. Stannar med tydligt fel om någon klartext saknas.
#' @noRd
intern_matchning_kod <- function(tabell, variabel, klartext) {
  varden <- pxweb2r::pxweb2_get_values(tabell, variabel, quiet = TRUE)
  koder <- varden$code[match(klartext, varden$label)]
  if (anyNA(koder)) {
    stop("Hittade inte ", paste0("\"", klartext[is.na(koder)], "\"", collapse = ", "), " i variabeln ", variabel,
         " (", tabell, "). Giltiga värden hämtas med pxweb2r::pxweb2_get_values(\"", tabell, "\", \"", variabel,
         "\").", call. = FALSE)
  }
  koder
}

# Kontrollerar att regionkoderna finns i tabellen  och stannar annars med tydligt fel.
#' @noRd
intern_matchning_kontrollera_regioner <- function(tabell, region_vekt) {
  giltiga <- pxweb2r::pxweb2_get_values(tabell, "Region", quiet = TRUE)$code
  okanda <- setdiff(region_vekt, giltiga)
  if (length(okanda) > 0) {
    stop("Regionkoderna finns inte i ", tabell, ": ", paste(okanda, collapse = ", "),
         ". Giltiga värden hämtas med pxweb2r::pxweb2_get_values(\"", tabell, "\", \"Region\").", call. = FALSE)
  }
  region_vekt
}

# Regionnamn till rubriker: kortnamn för län, "FA-region" framför FA-regionernas namn.
#' @noRd
intern_matchning_regiontext <- function(regionkod, region) {
  ifelse(grepl("^FA", regionkod) & regionkod != "FA00", paste0("FA-region ", region),
         rdverktyg::skapa_kortnamn_lan(region))
}

# Filnamnsdel av en text (små bokstäver, utan å/ä/ö och specialtecken).
#' @noRd
intern_matchning_filtext <- function(x) gsub("^_|_$", "", gsub("[^a-z0-9]+", "_", chartr("åäö", "aao", tolower(x))))

# Gemensam ritning för matchningsdiagrammen med ett värde per region, utbildningsgrupp och år (data i `df` med
# kolumnerna regionkod, region_txt, utbildning_kod, utbildning, ar, varde och, om kön används, kon).
#' @noRd
intern_matchning_stapel <- function(df, diagramtyp, titel_bas, filprefix, y_titel, proc_0_100, region_vekt, utb_klartext,
                                    ar, fargvekt_kon, visa_logga_i_diagram, logga_sokvag, diagram_capt, utmapp,
                                    skriv_diagramfil) {
  har_kon <- "kon" %in% names(df)
  region_txt <- unique(df$region_txt[df$regionkod == region_vekt[1]])
  region_fil <- intern_matchning_filtext(region_txt)
  y_extra <- if (proc_0_100) list(procent_0_100_10intervaller = TRUE) else list(stodlinjer_avrunda_fem = TRUE)
  gemensamt <- c(list(diagram_capt = diagram_capt, manual_y_axis_title = y_titel, lagg_pa_logga = visa_logga_i_diagram,
                      logga_path = logga_sokvag, output_mapp = utmapp, skriv_till_diagramfil = skriv_diagramfil), y_extra)

  if (diagramtyp == "utbildningsgrupper") {
    # inte nivåer (00N), inriktningar (00I) eller nivåkoder med två tecken eller färre
    akt_ar <- if (is.na(ar)) max(df$ar) else as.character(ar)
    chart_df <- df |>
      dplyr::filter(regionkod == region_vekt[1], ar == akt_ar, !utbildning_kod %in% c("00N", "00I"),
                    nchar(utbildning_kod) > 2, utbildning != "okänd utbildningsnivå") |>
      dplyr::mutate(fokus = as.integer(utbildning %in% utb_klartext[!is.na(utb_klartext)]))
    if (nrow(chart_df) == 0) stop("Inga värden för år ", akt_ar, ".", call. = FALSE)
    diagramfil <- paste0(filprefix, "_", region_fil, "_", akt_ar, ".png")
    argument <- c(list(
      skickad_df = chart_df, skickad_x_var = "utbildning", skickad_y_var = "varde",
      x_var_fokus = if (!har_kon) "fokus" else NULL,
      skickad_x_grupp = if (har_kon) "kon" else NULL,
      manual_color = if (har_kon) fargvekt_kon else rddiagram::diagramfarger("rd_gron_tva_fokus"),
      diagram_titel = paste0(titel_bas, " per utbildningsgrupp i ", region_txt, " år ", akt_ar),
      x_axis_sort_value = TRUE, x_axis_lutning = 0, y_axis_storlek = 6, diagram_liggande = TRUE,
      filnamn_diagram = diagramfil, diagramfil_hojd = 10), gemensamt)
  } else if (diagramtyp == "tidsserie") {
    valda <- if (all(is.na(utb_klartext))) "samtliga utbildningsgrupper" else utb_klartext
    chart_df <- df |> dplyr::filter(regionkod == region_vekt[1], utbildning %in% valda)
    saknas <- setdiff(valda, chart_df$utbildning)
    if (length(saknas) > 0) stop("Hittade inte utbildningsgrupperna: ", paste(saknas, collapse = ", "), call. = FALSE)
    flera <- length(unique(chart_df$utbildning)) > 1
    if (flera) chart_df$utbildning <- stringr::str_wrap(chart_df$utbildning, 40)
    utb_titel <- if (flera) "per utbildningsgrupp" else paste0("med ", unique(chart_df$utbildning))
    diagramfil <- paste0(filprefix, "_tid_", region_fil, "_",
                         if (flera) paste0(length(unique(chart_df$utbildning)), "_utbgrp") else unique(chart_df$utbildning_kod), ".png")
    argument <- c(list(
      skickad_df = chart_df, skickad_x_var = "ar", skickad_y_var = "varde",
      skickad_x_grupp = if (har_kon) "kon" else NULL,
      manual_color = if (har_kon) fargvekt_kon else rddiagram::diagramfarger("rd_gron")[1],
      diagram_titel = stringr::str_wrap(paste0(titel_bas, " ", utb_titel, " i ", region_txt), 80),
      x_axis_lutning = 0, facet_grp = if (flera) "utbildning" else NULL, facet_scale = "fixed",
      filnamn_diagram = diagramfil), gemensamt)
  } else {   # jamforelse: regionerna jämförda för en utbildningsgrupp ett år
    valda <- if (all(is.na(utb_klartext))) "samtliga utbildningsgrupper" else utb_klartext[1]
    akt_ar <- if (is.na(ar)) max(df$ar) else as.character(ar)
    chart_df <- df |>
      dplyr::filter(utbildning == valda, ar == akt_ar) |>
      dplyr::mutate(fokus = as.integer(regionkod == region_vekt[1]))
    if (nrow(chart_df) == 0) stop("Inga värden för \"", valda, "\" år ", akt_ar, ".", call. = FALSE)
    manga <- length(unique(chart_df$regionkod)) > 12
    diagramfil <- paste0(filprefix, "_jmfr_", intern_matchning_filtext(valda), "_", akt_ar, ".png")
    argument <- c(list(
      skickad_df = chart_df, skickad_x_var = "region_txt", skickad_y_var = "varde",
      x_var_fokus = if (!har_kon) "fokus" else NULL,
      skickad_x_grupp = if (har_kon) "kon" else NULL,
      manual_color = if (har_kon) fargvekt_kon else rddiagram::diagramfarger("rd_gron_tva_fokus"),
      diagram_titel = stringr::str_wrap(paste0(titel_bas, ": ", valda, " år ", akt_ar), 80),
      x_axis_sort_value = TRUE, x_axis_lutning = if (manga) 0 else 45,
      manual_x_axis_text_vjust = if (manga) NULL else 1, manual_x_axis_text_hjust = if (manga) NULL else 1,
      diagram_liggande = manga, filnamn_diagram = diagramfil), gemensamt)
  }
  argument <- argument[!vapply(argument, is.null, logical(1))]
  gg_obj <- do.call(rddiagram::SkapaStapelDiagram, argument)
  stats::setNames(list(gg_obj), stringr::str_remove(diagramfil, "\\.[^.]+$"))
}
