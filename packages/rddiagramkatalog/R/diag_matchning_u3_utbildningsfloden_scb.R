#' Diagram: utbildningsflöden (U3)
#'
#' Stapeldiagram ur SCB:s regionala matchningsindikatorer, tabellen Utbildningsflöden (U3, TAB6928): hur
#' befolkningen 18–69 år i en utbildningsgrupp förändras mellan två år, uppdelat på inflöden (inflyttare till
#' regionen, examinerade, åldersinträden) och utflöden (utflyttare från regionen, vidareutbildade, åldersutträden).
#' Utflödena är negativa i tabellen och ritas under nollstrecket. Staplarna visar de sex posterna staplade per årsintervall. Med flera utbildningsgrupper blir det ett
#' facetdiagram (en panel per grupp) eller ett diagram per grupp.
#'
#' Äldre versionen (U3 utan CKM, TAB6370) uppdateras inte längre och har samma tidsperiod (2019–2024), så den
#' används inte. Tabellen finns för län och kommuner, men inte för FA-regioner.
#'
#' @param region_vekt Länskod eller kommunkod, bara en åt gången.
#' @param utb_klartext Utbildningsgrupper som klartext. Standard `"samtliga utbildningsgrupper"`.
#' @param kon_bakgr_klartext Urval av befolkningen: `"totalt"` (standard), `"män"`, `"kvinnor"`,
#'   `"inrikes född"`, `"utrikes född"` eller en åldersgrupp (`"18–24 år"`, `"25–29 år"` ... `"65–69 år"`).
#'   Flera urval går bra, t.ex. `c("kvinnor", "män")`: med `diag_facet = TRUE` och en utbildningsgrupp blir
#'   urvalen paneler i ett facetdiagram.
#' @param visa_totalvarden `TRUE` (standard) markerar nettoförändringen (inflöden minus utflöden) per årsintervall med
#'   en svart markering. Ritas bara i diagram med en utbildningsgrupp, inte i facetdiagram.
#' @param diag_facet `TRUE` (standard) ger ett facetdiagram när flera utbildningsgrupper (eller, med en
#'   utbildningsgrupp, flera urval i `kon_bakgr_klartext`) valts, `FALSE` ger ett diagram per grupp/urval.
#'   Är både flera utbildningsgrupper och flera urval valda blir utbildningsgrupperna paneler, med ett diagram per urval.
#' @param visa_logga_i_diagram `TRUE` om logga ska visas i diagrammet.
#' @param logga_sokvag Sökväg till logga. `NA` använder standardloggan.
#' @param diagram_capt Diagrammets källtext.
#' @param utmapp Mapp där diagrammen sparas. Måste finnas om `skriv_diagramfil = TRUE`.
#' @param skriv_diagramfil `TRUE` om diagrammen ska skrivas till fil.
#' @param returnera_data `TRUE` lägger data i `.GlobalEnv` som `matchning_u3`.
#' @param returnera_figur `TRUE` (standard) returnerar diagrammen.
#'
#' @return En namngiven lista med `ggplot`-objekt, ett eller ett per utbildningsgrupp.
#' @export
diag_matchning_u3_utbildningsfloden_scb <- function(
    region_vekt = "20",
    utb_klartext = "samtliga utbildningsgrupper",
    kon_bakgr_klartext = "totalt",
    visa_totalvarden = TRUE,
    diag_facet = TRUE,
    visa_logga_i_diagram = TRUE,
    logga_sokvag = NA,
    diagram_capt = "Källa: SCB:s öppna statistikdatabas, Regionala matchningsindikatorer\nBearbetning: Samhällsanalys, Region Dalarna",
    utmapp = rdverktyg::utskriftsmapp(),
    skriv_diagramfil = TRUE,
    returnera_data = FALSE,
    returnera_figur = TRUE
) {

  # ========================================== Inställningar ============================================

  if (length(region_vekt) != 1) stop("region_vekt ska vara en läns- eller kommunkod (ett värde).", call. = FALSE)
  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  # ========================================== Hämta data ===============================================
  # Länk till tabell: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM9906__AM9906B/RegionInd19U3N/
  # (motsvarande nya tabell TAB6928)

  floden_df <- pxweb2r::pxweb2_get_data(
    table = "TAB6928",
    query = list(Region = region_vekt,
                 Utbildngrupp = intern_matchning_kod("TAB6928", "Utbildngrupp", utb_klartext),
                 KonAlderFodelseland = intern_matchning_kod("TAB6928", "KonAlderFodelseland", kon_bakgr_klartext),
                 ContentsCode = "*", Tid = "*"),
    quiet = TRUE) |>
    dplyr::rename(regionkod = region_kod, kon_bakgr = `kön/ålder/födelseland`)

  poster <- c("inflyttare till regionen", "examinerade", "åldersinträden",
              "utflyttare från regionen", "vidareutbildade", "åldersutträden")
  floden_df <- floden_df |>
    dplyr::mutate(tabellinnehåll = sub("^- varav ", "", tabellinnehåll)) |>
    dplyr::filter(tabellinnehåll %in% poster) |>
    dplyr::mutate(tabellinnehåll = factor(tabellinnehåll, levels = poster),
                  value = dplyr::coalesce(value, 0),
                  region = rdverktyg::skapa_kortnamn_lan(region))

  if (returnera_data) assign("matchning_u3", floden_df, envir = .GlobalEnv)

  # ========================================== Diagram ==================================================

  region_txt <- unique(floden_df$region)
  region_fil <- gsub("[^a-z0-9]+", "_", chartr("åäö", "aao", tolower(region_txt)))

  # svart markering vid nettoförändringen (summan av de sex posterna) för varje årsintervall
  nettomarkeringar <- function(df) {
    netto <- df |> dplyr::group_by(årsintervall) |> dplyr::summarise(netto = sum(value), .groups = "drop") |>
      dplyr::arrange(årsintervall)
    halv_hojd <- (max(df$value) - min(df$value)) * 0.004
    purrr::map(seq_len(nrow(netto)), \(i) list(geom = "rect", ymin = netto$netto[i] - halv_hojd,
                                               ymax = netto$netto[i] + halv_hojd, xmin = i - 0.3, xmax = i + 0.3,
                                               alpha = 1, fill = "black"))
  }

  skapa <- function(df, titel_tillagg, fil_tillagg, facet_var) {
    diagramfil <- paste0("utbildningsfloden_", region_fil, "_", fil_tillagg, ".png")
    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = df,
      skickad_x_var = "årsintervall",
      skickad_y_var = "value",
      skickad_x_grupp = "tabellinnehåll",
      geom_position_stack = TRUE,
      manual_color = rddiagram::diagramfarger("utb_floden"),
      fokusera_varden = if (visa_totalvarden && is.null(facet_var)) nettomarkeringar(df) else NULL,
      diagram_titel = paste0("Utbildningsflöden i ", region_txt, titel_tillagg),
      diagram_capt = diagram_capt,
      manual_y_axis_title = "antal personer",
      x_axis_lutning = 45,
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      facet_grp = facet_var,
      facet_legend_bottom = TRUE,
      legend_byrow = TRUE,
      lagg_pa_logga = visa_logga_i_diagram,
      logga_path = logga_sokvag,
      output_mapp = utmapp,
      filnamn_diagram = diagramfil,
      diagramfil_hojd = 10,
      skriv_till_diagramfil = skriv_diagramfil)
    stats::setNames(list(gg_obj), stringr::str_remove(diagramfil, "\\.[^.]+$"))
  }

  # Facetdiagram: en panel per utbildningsgrupp, eller (med en grupp) en panel per urval i kon_bakgr_klartext.
  # Allt som inte är facet blir egna diagram.
  floden_df$kon_bakgr <- factor(floden_df$kon_bakgr, levels = unique(floden_df$kon_bakgr))
  n_utb <- length(unique(floden_df$utbildning))
  n_kon <- length(unique(floden_df$kon_bakgr))
  kon_fil <- function(x) gsub("[^a-z0-9]+", "_", chartr("åäö", "aao", tolower(as.character(x))))

  facet_var <- if (!diag_facet) NULL else if (n_utb > 1) "utbildning" else if (n_kon > 1) "kon_bakgr" else NULL
  floden_df$utbildning <- stringr::str_wrap(floden_df$utbildning, 35)

  # en rad per diagram: det som inte ligger som paneler delas upp i egna diagram
  delar <- unique(floden_df[, c("utbildning_kod", "utbildning", "kon_bakgr")])
  if (identical(facet_var, "utbildning")) delar <- delar[!duplicated(delar$kon_bakgr), ]
  if (identical(facet_var, "kon_bakgr")) delar <- delar[!duplicated(delar$utbildning_kod), ]

  gg_list <- purrr::flatten(purrr::map(seq_len(nrow(delar)), \(i) {
    df <- floden_df
    titel <- ""
    fil <- character()
    if (!identical(facet_var, "utbildning")) {
      df <- df[df$utbildning_kod == delar$utbildning_kod[i], ]
      if (n_utb > 1) {
        titel <- paste0(titel, "\n", gsub("\n", " ", delar$utbildning[i]))
        fil <- c(fil, delar$utbildning_kod[i])
      }
    } else {
      fil <- c(fil, paste0(n_utb, "_utbgrp"))
    }
    if (!identical(facet_var, "kon_bakgr")) {
      df <- df[df$kon_bakgr == delar$kon_bakgr[i], ]
      if (n_kon > 1) {
        titel <- paste0(titel, if (nzchar(titel)) ", " else "\n", delar$kon_bakgr[i])
        fil <- c(fil, kon_fil(delar$kon_bakgr[i]))
      }
    } else {
      fil <- c(fil, paste0(n_kon, "_urval"))
    }
    if (length(fil) == 0) fil <- unique(df$utbildning_kod)
    skapa(df, titel, paste(fil, collapse = "_"), facet_var)
  }))

  if (returnera_figur) return(gg_list)
  invisible(NULL)
}
