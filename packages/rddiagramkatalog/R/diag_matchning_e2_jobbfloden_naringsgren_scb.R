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
#' @param visa_flode Vad som ska visas: `"uppdelat"` (standard) ritar de fyra posterna staplade (nya förvärvsarbetande,
#'   lämnat sysselsättning och jobbytare från/till annan näringsgren), `"totalt"` ritar bara inflöden totalt över och
#'   utflöden totalt under nollstrecket, och `"bada"` ger ett facetdiagram med båda vyerna (totalen till vänster).
#' @param visa_totalvarden `TRUE` (standard) markerar nettoförändringen (inflöden minus utflöden) per årsintervall med
#'   ett svart streck, även i facetdiagram.
#' @param diag_facet `TRUE` (standard) ger ett facetdiagram när flera näringsgrenar (eller, med en näringsgren, flera
#'   urval i `kon_bakgr_klartext`) valts, `FALSE` ger ett diagram per näringsgren/urval. Är både flera näringsgrenar och
#'   flera urval valda blir näringsgrenarna paneler, med ett diagram per urval. Med `visa_flode = "bada"` är de två vyerna
#'   paneler, och urvalen i `kon_bakgr_klartext` läggs som egna rader av paneler (en rad per urval); flera näringsgrenar
#'   ger då ett diagram per näringsgren.
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
    visa_flode = c("uppdelat", "totalt", "bada"),
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

  visa_flode <- match.arg(visa_flode)
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

  # totalt in- och utflöde per årsintervall, och de två vyerna som paneler när visa_flode = "bada"
  poster_tot <- c("inflöden totalt", "utflöden totalt")
  vyer <- c(totalt = "Totalt in- och utflöden", uppdelat = "Uppdelat på orsak")
  tot_df <- flode_df |>
    dplyr::mutate(tabellinnehåll = factor(ifelse(tabellinnehåll %in% poster[1:2], poster_tot[1], poster_tot[2]),
                                          levels = poster_tot)) |>
    dplyr::group_by(dplyr::across(-value)) |>
    dplyr::summarise(value = sum(value), .groups = "drop")
  flode_df$vy <- vyer[["uppdelat"]]
  tot_df$vy <- vyer[["totalt"]]
  niva <- switch(visa_flode, uppdelat = poster, totalt = poster_tot, bada = c(poster, poster_tot))
  flode_df <- switch(visa_flode,
                     uppdelat = flode_df,
                     totalt = tot_df,
                     bada = dplyr::bind_rows(dplyr::mutate(flode_df, tabellinnehåll = as.character(tabellinnehåll)),
                                             dplyr::mutate(tot_df, tabellinnehåll = as.character(tabellinnehåll))))
  flode_df$tabellinnehåll <- factor(as.character(flode_df$tabellinnehåll), levels = niva)
  farger <- rddiagram::diagramfarger("utb_floden")
  fargvekt <- switch(visa_flode, uppdelat = farger[c(1, 3, 4, 6)], totalt = farger[c(2, 5)],
                     bada = farger[c(1, 3, 4, 6, 2, 5)])

  flode_df$kon_bakgr <- factor(flode_df$kon_bakgr, levels = unique(flode_df$kon_bakgr))
  flode_df$naringsgren <- factor(stringr::str_wrap(flode_df$naringsgren, 35),
                                 levels = unique(stringr::str_wrap(naringsgren_klartext, 35)))
  n_ng <- length(unique(flode_df$naringsgren_kod))
  n_kon <- length(unique(flode_df$kon_bakgr))

  # Vad som blir paneler: de två vyerna (visa_flode = "bada"), näringsgrenarna eller urvalen i
  # kon_bakgr_klartext. Det som inte är paneler delas upp i egna diagram.
  vy_panel <- visa_flode == "bada"
  ng_panel <- diag_facet && n_ng > 1 && !vy_panel
  kon_panel <- diag_facet && n_kon > 1 && !ng_panel
  har_panel <- vy_panel || ng_panel || kon_panel

  if (har_panel) {
    flode_df$vy <- factor(flode_df$vy, levels = unname(vyer))
    delar_av_panel <- list(if (vy_panel) as.character(flode_df$vy),
                           if (ng_panel) as.character(flode_df$naringsgren),
                           if (kon_panel) as.character(flode_df$kon_bakgr))
    flode_df$panel <- do.call(paste, c(Filter(Negate(is.null), delar_av_panel), sep = " – "))
    nollor <- integer(nrow(flode_df))
    ordning <- order(if (ng_panel) as.integer(flode_df$naringsgren) else nollor,
                     if (kon_panel) as.integer(flode_df$kon_bakgr) else nollor,
                     if (vy_panel) as.integer(flode_df$vy) else nollor)
    flode_df$panel <- factor(flode_df$panel, levels = unique(flode_df$panel[ordning]))
  }

  # svart streck vid nettoförändringen (summan av posterna) för varje årsintervall, i varje panel
  netto_lager <- function(df) {
    ar <- sort(unique(df$årsintervall))
    netto <- df |>
      dplyr::group_by(dplyr::across(dplyr::any_of(c("panel", "årsintervall")))) |>
      dplyr::summarise(netto = sum(value), .groups = "drop") |>
      dplyr::mutate(x = match(årsintervall, ar))
    list(ggplot2::geom_segment(data = netto, ggplot2::aes(x = x - 0.3, xend = x + 0.3, y = netto, yend = netto),
                               inherit.aes = FALSE, colour = "black", linewidth = 1.3),
         # osynligt linjelager som ger ett streck i teckenförklaringen (override.aes gör det synligt)
         ggplot2::geom_line(ggplot2::aes(group = 1, color = "line"), alpha = 0),
         ggplot2::scale_color_manual(name = "", values = c("line" = "black"), labels = "nettoförändring"),
         ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(alpha = 1, linewidth = 1.5))),
         ggplot2::theme(legend.key = ggplot2::element_rect(fill = "white"), legend.box.just = "bottom"))
  }

  skapa <- function(df, titel_tillagg, fil_tillagg) {
    diagramfil <- paste0("jobbfloden_naringsgren_", region_fil, "_", fil_tillagg, ".png")
    logga <- if (visa_logga_i_diagram) (if (is.na(logga_sokvag)) TRUE else logga_sokvag) else FALSE
    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = df,
      skickad_x_var = "årsintervall",
      skickad_y_var = "value",
      skickad_x_grupp = "tabellinnehåll",
      geom_position_stack = TRUE,
      manual_color = fargvekt,
      diagram_titel = paste0("Jobbflöden i ", region_txt, titel_tillagg),
      diagram_capt = diagram_capt,
      manual_y_axis_title = "antal personer",
      x_axis_lutning = 45,
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      facet_grp = if (har_panel) "panel" else NULL,
      facet_scale = facet_skala,
      facet_kolumner = if (vy_panel) 2 else NULL,
      facet_legend_bottom = TRUE,
      legend_byrow = TRUE,
      legend_rader = 2,
      lagg_pa_logga = visa_logga_i_diagram,
      logga_path = logga_sokvag,
      output_mapp = utmapp,
      filnamn_diagram = diagramfil,
      diagramfil_hojd = 10,
      skriv_till_diagramfil = skriv_diagramfil && !visa_totalvarden)
    if (visa_totalvarden) {
      gg_obj <- gg_obj + netto_lager(df)
      if (skriv_diagramfil) {
        rddiagram::skriv_till_diagramfil(gg_obj, output_mapp = utmapp, filnamn_diagram = diagramfil,
                                         diagramfil_hojd = 10, logga = logga)
      }
    }
    stats::setNames(list(gg_obj), stringr::str_remove(diagramfil, "\\.[^.]+$"))
  }

  delar <- unique(flode_df[, c("naringsgren_kod", "naringsgren", "kon_bakgr")])
  if (ng_panel) delar <- delar[!duplicated(delar$kon_bakgr), ]
  if (kon_panel) delar <- delar[!duplicated(delar$naringsgren_kod), ]

  gg_list <- purrr::flatten(purrr::map(seq_len(nrow(delar)), \(i) {
    df <- flode_df
    titel <- ""
    fil <- character()
    if (!ng_panel) {
      df <- df[df$naringsgren_kod == delar$naringsgren_kod[i], ]
      titel <- paste0("\n", gsub("\n", " ", as.character(delar$naringsgren[i])))
      fil <- c(fil, filtext(delar$naringsgren_kod[i]))
    } else {
      fil <- c(fil, paste0(n_ng, "_naringsgrenar"))
    }
    if (!kon_panel) {
      df <- df[df$kon_bakgr == delar$kon_bakgr[i], ]
      if (n_kon > 1) {
        titel <- paste0(titel, if (nzchar(titel)) ", " else "\n", delar$kon_bakgr[i])
        fil <- c(fil, filtext(delar$kon_bakgr[i]))
      }
    } else {
      fil <- c(fil, paste0(n_kon, "_urval"))
    }
    if (visa_flode != "uppdelat") fil <- c(fil, visa_flode)
    skapa(df, titel, paste(fil, collapse = "_"))
  }))

  if (returnera_figur) return(gg_list)
  invisible(NULL)
}
