#' Diagram: var finns gymnasiets avgångselever tio år senare?
#'
#' Stapeldiagram över var avgångseleverna från gymnasiet bor tio år efter avgången: kvarboende, återflyttade,
#' utflyttade, utvandrade eller avlidna, per avgångsår och utan uppdelning på kön eller bakgrund. Flyttningarna kan
#' redovisas över kommungräns, över länsgräns eller båda i samma diagram (två paneler). Data är en manuellt
#' underhållen ögonblicksbild ur Regionala utvecklingsdatabasen (SCB), vyn Gymnasiet – avgångna och RTB, en Excel-fil på G:, och hämtas alltså inte live från SCB.
#'
#' Excel-filen ska ha en flik vars namn innehåller `kommun_kon` och en vars namn innehåller `lan_kon`, med kolumnerna
#' `avgangs_ar`, `regionkod`, `region`, `antal` och `flytt_kat` (`flytt_kat_lan` i länsfliken).
#'
#' @param region_vekt Länskod (ett län) för det län vars avgångselever följs. Standard `"20"` (Dalarna).
#' @param datafil Sökväg till Excel-filen.
#' @param diagram_capt Diagrammets källtext.
#' @param visa_dataetiketter `TRUE` visar dataetiketter.
#' @param diag_kommungrans `TRUE` ger ett diagram över flyttningar över kommungräns.
#' @param diag_lansgrans `TRUE` ger ett diagram över flyttningar över länsgräns.
#' @param diag_lans_och_kommungrans `TRUE` (standard) ger ett diagram med kommun- och länsgräns som två paneler.
#' @param utmapp Mapp där diagrammen sparas. Måste finnas om `skriv_diagramfil = TRUE`.
#' @param skriv_diagramfil `TRUE` om diagrammen ska skrivas till fil.
#' @param returnera_data `TRUE` = lägger data i `.GlobalEnv` som `gymnavg_flytt`.
#' @param returnera_figur `TRUE` (standard) returnerar diagrammen.
#'
#' @return En namngiven lista med `ggplot`-objekt, ett per valt diagram.
#' @export
diag_uppfoljning_gymnavg_kvarboende_flytt_mikrodata_scb <- function(
    region_vekt = "20",
    datafil = "G:/Samhällsanalys/Statistik/Flyttningar/gymnavg_flytt kon_bakgr.xlsx",
    diagram_capt = "Källa: Regionala utvecklingsdatabasen (SCB), Gymnasiet – avgångna och RTB\nBearbetning: Samhällsanalys, Region Dalarna",
    visa_dataetiketter = FALSE,
    diag_kommungrans = FALSE,
    diag_lansgrans = FALSE,
    diag_lans_och_kommungrans = TRUE,
    utmapp = rdverktyg::utskriftsmapp(),
    skriv_diagramfil = TRUE,
    returnera_data = FALSE,
    returnera_figur = TRUE
) {

  # ========================================== Inställningar ============================================

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)
  region_txt <- rdverktyg::skapa_kortnamn_lan(unique(rdverktyg::hamtaregion_kod_namn(region_vekt)$region))

  # ========================================== Läser in data ============================================
  # Filen på G: är en manuellt underhållen ögonblicksbild.

  rdverktyg::sakerstall_indatafil(datafil)
  flikar <- readxl::excel_sheets(datafil)
  flik <- function(monster) {
    f <- flikar[stringr::str_detect(tolower(flikar), monster)]
    if (length(f) != 1) {
      stop("Förväntade exakt en flik med '", monster, "' i namnet i ", datafil, ", hittade: ",
           if (length(f) == 0) "ingen" else paste(f, collapse = ", "), ".", call. = FALSE)
    }
    f
  }

  # andelar per avgångsår, region och geografi (summerat över kön och bakgrund)
  andelar <- function(df, geografi) {
    df |>
      dplyr::mutate(geografi = geografi) |>
      dplyr::group_by(avgangs_ar, geografi, regionkod, region, flytt_kat) |>
      dplyr::summarise(antal = sum(antal, na.rm = TRUE), .groups = "drop_last") |>
      dplyr::mutate(andel = antal / sum(antal) * 100) |>
      dplyr::ungroup()
  }
  kommun_df <- andelar(readxl::read_excel(datafil, sheet = flik("kommun_kon")), "flytt över kommungräns")
  lan_df <- andelar(readxl::read_excel(datafil, sheet = flik("lan_kon")) |>
                      dplyr::rename(flytt_kat = flytt_kat_lan), "flytt över länsgräns")

  kategorier <- c("Kvarboende", "Återflyttad", "Utflyttad", "Utvandrad", "Avliden")
  flytt_df <- dplyr::bind_rows(lan_df, kommun_df) |>
    dplyr::mutate(flytt_kat = factor(flytt_kat, levels = rev(kategorier))) |>
    dplyr::filter(!is.na(flytt_kat))

  if (returnera_data) assign("gymnavg_flytt", flytt_df, envir = .GlobalEnv)

  farg_vektor <- rddiagram::diagramfarger("rus_sex")
  names(farg_vektor) <- levels(flytt_df$flytt_kat)

  # ========================================== Diagram ==================================================

  skapa <- function(df, undertitel, filnamn_bas, facet = FALSE) {
    df <- df[df$regionkod == region_vekt, ]
    if (nrow(df) == 0) stop("Inga data för region_vekt = ", region_vekt, " i ", datafil, ".", call. = FALSE)
    diagramfil <- paste0(filnamn_bas, "_", region_txt, "_facet.png")
    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = df,
      skickad_x_var = "avgangs_ar",
      skickad_y_var = "andel",
      skickad_x_grupp = "flytt_kat",
      x_axis_lutning = 0,
      geom_position_stack = TRUE,
      legend_vand_ordning = TRUE,
      procent_0_100_10intervaller = TRUE,
      diagram_titel = paste0("Var finns ", region_txt, "s avgångselever från gymnasiet tio år senare?"),
      diagram_undertitel = undertitel,
      diagram_capt = diagram_capt,
      stodlinjer_avrunda_fem = TRUE,
      filnamn_diagram = diagramfil,
      dataetiketter = visa_dataetiketter,
      manual_y_axis_title = "procent",
      manual_x_axis_title = if (facet) "avgångsår" else NA,
      manual_color = farg_vektor,
      output_mapp = utmapp,
      skriv_till_diagramfil = skriv_diagramfil,
      facet_grp = if (facet) "geografi" else NULL,
      facet_scale = "fixed",
      facet_legend_bottom = TRUE)
    stats::setNames(list(gg_obj), stringr::str_remove(diagramfil, "\\.png$"))
  }

  gg_list <- list()
  if (diag_kommungrans) {
    gg_list <- c(gg_list, skapa(flytt_df[flytt_df$geografi == "flytt över kommungräns", ],
                                "- analys av flyttningar över kommungräns", "uppfoljning_gymnavg_kommungrans"))
  }
  if (diag_lansgrans) {
    gg_list <- c(gg_list, skapa(flytt_df[flytt_df$geografi == "flytt över länsgräns", ],
                                "- analys av flyttningar över länsgräns", "uppfoljning_gymnavg_lansgrans"))
  }
  if (diag_lans_och_kommungrans) {
    gg_list <- c(gg_list, skapa(flytt_df, "- analys av kvarboende och flyttningar över kommun- respektive länsgräns",
                                "uppfoljning_gymnavg_lan_kommun", facet = TRUE))
  }

  if (returnera_figur) return(gg_list)
  invisible(NULL)
}
