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
    manga_ar <- length(unique(chart_df$ar)) > 8   # många år: lodräta etiketter så att de inte krockar
    if (flera) chart_df$utbildning <- stringr::str_wrap(chart_df$utbildning, 40)
    utb_titel <- if (flera) "per utbildningsgrupp" else paste0("med ", unique(chart_df$utbildning))
    diagramfil <- paste0(filprefix, "_tid_", region_fil, "_",
                         if (flera) paste0(length(unique(chart_df$utbildning)), "_utbgrp") else unique(chart_df$utbildning_kod), ".png")
    argument <- c(list(
      skickad_df = chart_df, skickad_x_var = "ar", skickad_y_var = "varde",
      skickad_x_grupp = if (har_kon) "kon" else NULL,
      manual_color = if (har_kon) fargvekt_kon else rddiagram::diagramfarger("rd_gron")[1],
      diagram_titel = stringr::str_wrap(paste0(titel_bas, " ", utb_titel, " i ", region_txt), 80),
      x_axis_lutning = if (manga_ar) 90 else 0,
      manual_x_axis_text_vjust = if (manga_ar) 0.5 else NULL, manual_x_axis_text_hjust = if (manga_ar) 1 else NULL,
      facet_grp = if (flera) "utbildning" else NULL, facet_scale = "fixed",
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

# Hämtar samma urval ur en ny och en äldre tabell (t.ex. U1a: TAB6367 från 2019 och TAB5400 till 2018) och sätter ihop
# dem till en tidsserie. Tabellerna har samma regioner och nästan samma utbildningsgrupper (samma koder och namn).
#
# tabeller      namngiven vektor c(ny = "TAB...", gammal = "TAB...")
# koder         lista med en namngiven kodvektor per tabell (list(ny = c(A = "..."), gammal = c(A = "...")))
# region_vekt   regionkoder
# utb_klartext  utbildningsgrupper som klartext, eller NULL för alla
# extra_klartext namngiven lista med fler variabler som klartext, t.ex. list(Kon = "totalt")
# ar            ett enskilt år, eller NULL för alla år. Ett år till och med 2018 hämtas bara ur den äldre tabellen.
# med_gammal    FALSE hämtar bara ur den nya tabellen
#
# Returnerar en data.frame i långt format med kolumnerna regionkod, region, utbildning_kod, utbildning, ar, post (namnen
# i `koder`), value och eventuella extra variabler.
#' @noRd
intern_matchning_hamta_tabellpar <- function(tabeller, koder, region_vekt, utb_klartext = NULL, extra_klartext = list(),
                                             ar = NULL, med_gammal = TRUE) {
  tid_ny <- pxweb2r::pxweb2_get_values(tabeller[["ny"]], "Tid", quiet = TRUE)$code
  tid_gammal <- pxweb2r::pxweb2_get_values(tabeller[["gammal"]], "Tid", quiet = TRUE)$code
  if (!is.null(ar) && !as.character(ar) %in% c(tid_ny, if (med_gammal) tid_gammal)) {
    stop("Året ", ar, " finns inte", if (!med_gammal) " i den nya tabellen (använd inkludera_aldre_tabell = TRUE för äldre år)",
         ". Tillgängliga år: ", min(c(tid_ny, if (med_gammal) tid_gammal)), "–", max(tid_ny), ".", call. = FALSE)
  }

  hamta_en <- function(rolle) {
    tabell <- tabeller[[rolle]]
    tid <- if (rolle == "ny") tid_ny else tid_gammal
    if (!is.null(ar)) tid <- intersect(tid, as.character(ar)) else tid <- "*"
    if (length(tid) == 0) return(NULL)

    utb_var <- if ("Utbildngrupp" %in% pxweb2r::pxweb2_get_variables(tabell)$code) "Utbildngrupp" else "Utbildning"
    if (is.null(utb_klartext)) {
      utb <- "*"
    } else {
      varden <- pxweb2r::pxweb2_get_values(tabell, utb_var, quiet = TRUE)
      utb <- varden$code[match(utb_klartext, varden$label)]
      utb <- utb[!is.na(utb)]
      if (length(utb) == 0) return(NULL)   # ingen av utbildningsgrupperna finns i den här tabellen
    }

    query <- c(list(Region = region_vekt, ContentsCode = unname(koder[[rolle]]), Tid = tid),
               stats::setNames(list(utb), utb_var),
               purrr::imap(extra_klartext, \(klartext, var) intern_matchning_kod(tabell, var, klartext)))
    df <- pxweb2r::pxweb2_get_data(table = tabell, query = query, quiet = TRUE)
    innehall <- pxweb2r::pxweb2_get_values(tabell, "ContentsCode", quiet = TRUE)
    df$post <- names(koder[[rolle]])[match(df$tabellinnehåll, innehall$label[match(koder[[rolle]], innehall$code)])]
    if ("kön" %in% names(df)) names(df)[names(df) == "kön"] <- "kon"
    df |>
      dplyr::rename(regionkod = region_kod, ar = år) |>
      dplyr::select(regionkod, region, utbildning_kod, utbildning, dplyr::any_of("kon"), ar, post, value) |>
      dplyr::mutate(tabell = tabell)
  }

  resultat <- dplyr::bind_rows(hamta_en("ny"), if (med_gammal) hamta_en("gammal"))
  if (nrow(resultat) == 0) stop("SCB gav inga värden för det valda urvalet.", call. = FALSE)

  # utbildningsgrupper som inte finns i någon av tabellerna
  if (!is.null(utb_klartext)) {
    saknas <- setdiff(utb_klartext, resultat$utbildning)
    if (length(saknas) > 0) {
      stop("Hittade inte utbildningsgrupperna: ", paste(saknas, collapse = ", "), call. = FALSE)
    }
  }
  resultat
}
