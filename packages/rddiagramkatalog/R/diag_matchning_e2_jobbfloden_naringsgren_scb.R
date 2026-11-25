#' Diagram: jobbflöden till och från näringsgrenar (E2)
#'
#' Stapeldiagram ur SCB:s regionala matchningsindikatorer, tabellen Jobbflöden (E2, TAB6927): hur antalet
#' förvärvsarbetande (20–69 år, dagbefolkning) i en näringsgren förändras mellan två år, uppdelat på inflöden (nya
#' förvärvsarbetande och jobbytare från annan näringsgren) och utflöden (personer som lämnat sysselsättning och
#' jobbytare till annan näringsgren). Utflödena ritas under nollstrecket. Jobbytare inom samma näringsgren ingår inte,
#' eftersom de inte ändrar näringsgrenens storlek. Med flera näringsgrenar blir det ett facetdiagram (en panel per
#' näringsgren) eller ett diagram per näringsgren.
#'
#' För `"samtliga näringsgrenar"` tar jobbytarna från och till annan näringsgren i stort sett ut varandra, så där syns
#' mest nya förvärvsarbetande mot dem som lämnat sysselsättning. Tabellen finns för län och kommuner, men inte för
#' FA-regioner. Jämför [diag_matchning_e2_jobbfloden_utbgrp_scb()] som visar jobbflöden per utbildningsgrupp.
#'
#' @param region_vekt Länskod eller kommunkod, bara en åt gången.
#' @param naringsgren_klartext Näringsgrenar som klartext. Standard `"samtliga näringsgrenar"`. Hämta giltiga värden med
#'   `pxweb2r::pxweb2_get_values("TAB6927", "SNI2007")`.
#' @param utb_klartext Utbildningsgrupp, bara en åt gången. Standard `"samtliga utbildningsgrupper"`.
#' @param kon_bakgr_klartext Urval av befolkningen: `"totalt"` (standard), `"män"`, `"kvinnor"`, `"inrikes född"`,
#'   `"utrikes född"` eller en åldersgrupp (`"20–24 år"`, `"25–29 år"` ... `"65–69 år"`). Flera urval går bra, t.ex.
#'   `c("kvinnor", "män")`: med `diag_facet = TRUE` och en näringsgren blir urvalen paneler i ett facetdiagram.
#' @param visa_totalvarden `TRUE` (standard) markerar nettoförändringen (inflöden minus utflöden) per årsintervall med
#'   en svart markering. Ritas bara i diagram med ett enda urval och en näringsgren, inte i facetdiagram.
#' @param diag_facet `TRUE` (standard) ger ett facetdiagram när flera näringsgrenar (eller, med en näringsgren, flera
#'   urval i `kon_bakgr_klartext`) valts, `FALSE` ger ett diagram per näringsgren/urval. Är både flera näringsgrenar och
#'   flera urval valda blir näringsgrenarna paneler, med ett diagram per urval.
#' @param facet_skala Y-axelns skala i facetdiagram: `"fixed"` (standard) ger samma axel i alla paneler, `"free"` ger
#'   var sin axel.
#' @param visa_logga_i_diagram `TRUE` om logga ska visas i diagrammet.
#' @param logga_sokvag Sökväg till logga. `NA` använder standardloggan.
#' @param diagram_capt Diagrammets källtext.
#' @param utmapp Mapp där diagrammen sparas. Måste finnas om `skriv_diagramfil = TRUE`.
#' @param skriv_diagramfil `TRUE` om diagrammen ska skrivas till fil.
#' @param returnera_data `TRUE` lägger data i `.GlobalEnv` som `matchning_e2_naringsgren`.
#' @param returnera_figur `TRUE` (standard) returnerar diagrammen.
#'
#' @return En namngiven lista med `ggplot`-objekt.
#' @export
diag_matchning_e2_jobbfloden_naringsgren_scb <- function(
    region_vekt = "20",
    naringsgren_klartext = "samtliga näringsgrenar",
    utb_klartext = "samtliga utbildningsgrupper",
    kon_bakgr_klartext = "totalt",
    visa_totalvarden = TRUE,
    diag_facet = TRUE,
    facet_skala = "fixed",
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
  if (length(utb_klartext) != 1) stop("utb_klartext ska vara ett enda värde.", call. = FALSE)
  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  # ========================================== Hämta data ===============================================
  # Länk till tabell: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM9906__AM9906A/RegionInd19E2AN/
  # (motsvarande nya tabell TAB6927)

  poster <- c("jobbytare från annan näringsgren", "nya förvärvsarbetande",
              "jobbytare till annan näringsgren", "lämnat sysselsättning")
  innehall <- paste0("- varav ", poster[c(2, 4, 1, 3)])
  flode_df <- pxweb2r::pxweb2_get_data(
    table = "TAB6927",
    query = list(Region = region_vekt,
                 SNI2007 = intern_matchning_kod("TAB6927", "SNI2007", naringsgren_klartext),
                 Utbildning = intern_matchning_kod("TAB6927", "Utbildning", utb_klartext),
                 KonAlderFodelseland = intern_matchning_kod("TAB6927", "KonAlderFodelseland", kon_bakgr_klartext),
                 ContentsCode = intern_matchning_kod("TAB6927", "ContentsCode", innehall),
                 Tid = "*"),
    quiet = TRUE) |>
    dplyr::rename(regionkod = region_kod, kon_bakgr = `kön/ålder/födelseland`,
                  naringsgren_kod = `näringsgren sni 2007_kod`, naringsgren = `näringsgren SNI 2007`) |>
    dplyr::mutate(tabellinnehåll = factor(sub("^- varav ", "", tabellinnehåll), levels = poster),
                  value = dplyr::coalesce(value, 0),
                  region = rdverktyg::skapa_kortnamn_lan(region)) |>
    dplyr::filter(!is.na(tabellinnehåll))

  if (returnera_data) assign("matchning_e2_naringsgren", flode_df, envir = .GlobalEnv)

  # ========================================== Diagram ==================================================

  region_txt <- unique(flode_df$region)
  filtext <- function(x) gsub("^_|_$", "", gsub("[^a-z0-9]+", "_", chartr("åäö", "aao", tolower(as.character(x)))))
  region_fil <- filtext(region_txt)

  # svart markering vid nettoförändringen (summan av posterna) för varje årsintervall
  nettomarkeringar <- function(df) {
    netto <- df |> dplyr::group_by(årsintervall) |> dplyr::summarise(netto = sum(value), .groups = "drop") |>
      dplyr::arrange(årsintervall)
    halv_hojd <- max((max(df$value) - min(df$value)) * 0.004, 1)
    purrr::map(seq_len(nrow(netto)), \(i) list(geom = "rect", ymin = netto$netto[i] - halv_hojd,
                                               ymax = netto$netto[i] + halv_hojd, xmin = i - 0.3, xmax = i + 0.3,
                                               alpha = 1, fill = "black"))
  }

  skapa <- function(df, titel_tillagg, fil_tillagg, facet_var) {
    diagramfil <- paste0("jobbfloden_naringsgren_", region_fil, "_", fil_tillagg, ".png")
    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = df,
      skickad_x_var = "årsintervall",
      skickad_y_var = "value",
      skickad_x_grupp = "tabellinnehåll",
      geom_position_stack = TRUE,
      manual_color = rddiagram::diagramfarger("utb_floden")[c(1, 3, 4, 6)],
      fokusera_varden = if (visa_totalvarden && is.null(facet_var)) nettomarkeringar(df) else NULL,
      diagram_titel = paste0("Jobbflöden i ", region_txt, titel_tillagg),
      diagram_capt = diagram_capt,
      manual_y_axis_title = "antal personer",
      x_axis_lutning = 45,
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      facet_grp = facet_var,
      facet_scale = facet_skala,
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

  # Facetdiagram: en panel per näringsgren, eller (med en näringsgren) en panel per urval i kon_bakgr_klartext.
  # Allt som inte är facet blir egna diagram.
  flode_df$kon_bakgr <- factor(flode_df$kon_bakgr, levels = unique(flode_df$kon_bakgr))
  n_ng <- length(unique(flode_df$naringsgren_kod))
  n_kon <- length(unique(flode_df$kon_bakgr))

  facet_var <- if (!diag_facet) NULL else if (n_ng > 1) "naringsgren" else if (n_kon > 1) "kon_bakgr" else NULL
  # panelerna i den ordning näringsgrenarna angavs
  flode_df$naringsgren <- factor(stringr::str_wrap(flode_df$naringsgren, 35),
                                 levels = unique(stringr::str_wrap(naringsgren_klartext, 35)))

  delar <- unique(flode_df[, c("naringsgren_kod", "naringsgren", "kon_bakgr")])
  if (identical(facet_var, "naringsgren")) delar <- delar[!duplicated(delar$kon_bakgr), ]
  if (identical(facet_var, "kon_bakgr")) delar <- delar[!duplicated(delar$naringsgren_kod), ]

  gg_list <- purrr::flatten(purrr::map(seq_len(nrow(delar)), \(i) {
    df <- flode_df
    titel <- ""
    fil <- character()
    if (!identical(facet_var, "naringsgren")) {
      df <- df[df$naringsgren_kod == delar$naringsgren_kod[i], ]
      titel <- paste0("\n", gsub("\n", " ", as.character(delar$naringsgren[i])))
      fil <- c(fil, filtext(delar$naringsgren_kod[i]))
    } else {
      fil <- c(fil, paste0(n_ng, "_naringsgrenar"))
    }
    if (!identical(facet_var, "kon_bakgr")) {
      df <- df[df$kon_bakgr == delar$kon_bakgr[i], ]
      if (n_kon > 1) {
        titel <- paste0(titel, if (nzchar(titel)) ", " else "\n", delar$kon_bakgr[i])
        fil <- c(fil, filtext(delar$kon_bakgr[i]))
      }
    } else {
      fil <- c(fil, paste0(n_kon, "_urval"))
    }
    skapa(df, titel, paste(fil, collapse = "_"), facet_var)
  }))

  if (returnera_figur) return(gg_list)
  invisible(NULL)
}
