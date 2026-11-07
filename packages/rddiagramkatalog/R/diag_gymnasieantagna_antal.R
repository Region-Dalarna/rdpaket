#' Diagram: antagna gymnasieelever per program
#'
#' Antal antagna gymnasieelever i Dalarna per program - antingen för
#' senaste år (uppdelat på kön eller inte) eller jämfört mellan valda år.
#' Data hämtas från en lokal, manuellt underhållen källa (Dalarnas
#' kommunförbunds egen Excel-export).
#'
#' @param utmapp Mapp där figuren sparas. Måste finnas om
#'   `skriv_diagramfil = TRUE`.
#' @param output_mapp_data Mapp där data sparas, om `filnamn_data` också
#'   anges. `NA` (standard) = data sparas inte.
#' @param filnamn_data Filnamn för sparad Excel-data.
#' @param diag_fargvekt Vilka färger som ska användas i diagram.
#' @param skriv_diagramfil Om TRUE sparas figuren till `utmapp`.
#' @param diagram_capt Diagrammets källtext (diagram för senaste år).
#' @param diagram_capt_fleraar Diagrammets källtext (diagram för flera år).
#' @param diag_antal Skapar ett diagram för senaste år.
#' @param diag_antal_fleraar Skapar ett diagram för flera år.
#' @param valda_ar Vilka år som ska väljas. `"99"` ger första året, `"9999"`
#'   ger senaste året.
#' @param konsuppdelat Skall diagrammet (för senaste år) vara uppdelat på
#'   kön.
#' @param returnera_figur Skall figur returneras (i en lista).
#' @param returnera_data Skall data returneras (till R-studios globala
#'   miljö).
#'
#' @return En namngiven lista med `ggplot`-objekt.
#' @export
diag_gymnasieantagna_antal <- function(utmapp = rdverktyg::utskriftsmapp(),
                                       output_mapp_data = NA,
                                       filnamn_data = "gymnasieantagning.xlsx",
                                       diag_fargvekt = rddiagram::diagramfarger("rus_sex"),
                                       skriv_diagramfil = TRUE,
                                       diagram_capt = "Källa: Gymnasieantagningen, Dalarnas kommunförbund\nBearbetning:Samhällsanalys, Region Dalarna\nDiagramförklaring: Antagna i början av september",
                                       diagram_capt_fleraar = "Källa: Gymnasieantagningen, Dalarnas kommunförbund\nBearbetning:Samhällsanalys, Region Dalarna\nDiagramförklaring: Antagna i början av september\nEnbart program som existerat samtliga år",
                                       diag_antal = TRUE,
                                       diag_antal_fleraar = TRUE,
                                       valda_ar = c("99", 2017, "9999"),
                                       konsuppdelat = FALSE,
                                       returnera_figur = TRUE,
                                       returnera_data = FALSE) {

  # ========================================== Allmän info ============================================
  # Skapar diagram för antagna på gymnasiet, antingen för senaste år (uppdelat på kön eller inte)
  # eller för valda år. Ingen SCB-hämtning - datan kommer från Dalarnas kommunförbunds egen lokala
  # Excel-export, via ett skript på G: (inte en del av funktioner-/hamta_data-reporna på GitHub).
  # ========================================== Inställningar ============================================

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  objektnamn <- c()

  # =============================================== Läser in data ===============================================
  # Själva hämtlogiken (gymnasieantagningen_las_in_data()) ligger på G: och är inte tillgänglig för
  # portning hit - behandlas som en svart låda. intern_sakerstall_indatafil() validerar att
  # skriptfilen finns innan den körs, i stället för ett kryptiskt source()-fel, och
  # intern_meddela_manuell_datakalla() skriver ut en konsolvarning om att datan är en manuell
  # ögonblicksbild.
  gymnasieantagning_skript <- "G:/skript/hamta_data/func_gymnasieantagningen.R"
  intern_sakerstall_indatafil(gymnasieantagning_skript)
  intern_meddela_manuell_datakalla("Dalarnas kommunförbunds gymnasieantagningsstatistik (G:/skript/hamta_data/func_gymnasieantagningen.R)")
  source(gymnasieantagning_skript, encoding = "utf-8", echo = FALSE)

  df <- gymnasieantagningen_las_in_data() |>
    dplyr::group_by(ar, program) |>
    dplyr::summarize(Män = sum(Ant_Män),
              Kvinnor = sum(Ant_Kv), .groups = "drop") |>
    tidyr::pivot_longer(3:4, names_to = "Kon", values_to = "Antagna")

  # Slår ihop flera mindre program till en större kategori
  df[df == "Flygteknikutbildningen, Marinteknikutbildningen, Sjöfartsutbildningen, Tågteknikutbildningen, Utbildningen samiska näringar eller Yrkesdansarutbildningen"] <- "Flygteknikutbildningen mfl."

  if (returnera_data == TRUE) {
    assign("gymnasieantagning_df", df, envir = .GlobalEnv)
  }

  # =============================================== Diagram ===============================================

  if (diag_antal == TRUE) {

    diagram_titel <- paste0("Antagna gymnasieelever i Dalarna ", max(df$ar), " per program")
    diagramfil <- "gymnasieantagning_tot.png"
    objektnamn <- if (konsuppdelat == FALSE) c(objektnamn, "gymnasieantagning_tot") else c(objektnamn, "gymnasieantagning_kon_tot")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = dplyr::filter(df, ar == max(ar)),
      skickad_x_var = "program",
      skickad_y_var = "Antagna",
      skickad_x_grupp = ifelse(konsuppdelat == FALSE, NA, "Kon"),
      manual_x_axis_text_vjust = 0,
      manual_x_axis_text_hjust = 0.6,
      manual_color = if (konsuppdelat == FALSE) diag_fargvekt[1] else rddiagram::diagramfarger("kon"),
      diagram_titel = diagram_titel,
      x_axis_sort_value = TRUE,
      diagram_capt = diagram_capt,
      x_axis_lutning = 0,
      diagram_liggande = TRUE,
      manual_y_axis_title = "Antagna elever",
      stodlinjer_avrunda_fem = TRUE,
      output_mapp = utmapp,
      filnamn_diagram = diagramfil,
      skriv_till_diagramfil = skriv_diagramfil
    )

    gg_list <- c(gg_list, list(gg_obj))

  }

  if (diag_antal_fleraar == TRUE) {

    if ("99" %in% valda_ar) valda_ar <- replace(valda_ar, valda_ar == "99", min(df$ar))
    if ("9999" %in% valda_ar) valda_ar <- replace(valda_ar, valda_ar == "9999", max(df$ar))

    diagram_titel <- paste0("Antagna gymnasieelever i Dalarna per program")
    diagramfil <- "gymnasieantagning_fleraar.png"
    objektnamn <- c(objektnamn, "gymnasieantagning_fleraar")

    program_forsta_ar <- unique(dplyr::pull(dplyr::filter(df, ar == dplyr::first(valda_ar)), program))
    program_sista_ar  <- unique(dplyr::pull(dplyr::filter(df, ar == dplyr::last(valda_ar)), program))

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = dplyr::mutate(
        dplyr::filter(df, program %in% program_forsta_ar, program %in% program_sista_ar, ar %in% valda_ar),
        ar = factor(ar, levels = valda_ar)
      ),
      skickad_x_var = "program",
      skickad_y_var = "Antagna",
      skickad_x_grupp = "ar",
      manual_x_axis_text_vjust = 0,
      manual_x_axis_text_hjust = 0.6,
      manual_color = rddiagram::diagramfarger("rus_sex"),
      diagram_titel = diagram_titel,
      x_axis_sort_value = TRUE,
      diagram_capt = diagram_capt_fleraar,
      x_axis_lutning = 0,
      legend_vand_ordning = FALSE,
      diagram_liggande = TRUE,
      manual_y_axis_title = "Antagna elever",
      output_mapp = utmapp,
      filnamn_diagram = diagramfil,
      skriv_till_diagramfil = skriv_diagramfil
    )

    gg_list <- c(gg_list, list(gg_obj))
  }

  names(gg_list) <- c(objektnamn)

  # OBS: originalet returnerade (vid returnera_figur = TRUE, standardvärdet) INNAN Excel-exporten
  # nedan, vilket gjorde att output_mapp_data/filnamn_data aldrig skrevs i praktiken. Exporten görs nu
  # alltid innan returneringen.
  if (!is.na(output_mapp_data) & !is.na(filnamn_data)) {
    openxlsx::write.xlsx(df, paste0(output_mapp_data, filnamn_data))
  }

  if (returnera_figur == TRUE) return(gg_list)
}
