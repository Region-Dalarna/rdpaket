#' Diagram: andel som bor kvar fem år efter inflyttning till kommunen
#'
#' Ett stapeldiagram per kommun i Dalarna med andelen av de som flyttade in till kommunen ett visst år och som
#' fortfarande bor kvar i kommunen fem år senare, per inflyttningsår. Data är en manuellt underhållen
#' ögonblicksbild ur Regionala utvecklingsdatabasen (SCB), en Excel-fil på G:, och hämtas alltså inte live från SCB.
#' Filen ska ha kolumnerna `Ar` (inflyttningsår), `andel` (procent) och `Kommun_namn`.
#'
#' @param indatafil Sökväg till Excel-filen med kvarboendeandelar.
#' @param diagram_capt Diagrammets källtext.
#' @param utmapp Mapp där diagrammet sparas. Måste finnas om `skriv_diagramfil = TRUE`.
#' @param skriv_diagramfil `TRUE` om diagrammet ska skrivas till fil.
#' @param returnera_data `TRUE` = lägger data i `.GlobalEnv` som `inflyttare_kvarboende`, för den som vill jobba vidare med den.
#' @param returnera_figur `TRUE` (standard) returnerar diagrammet.
#'
#' @return En namngiven lista med ett `ggplot`-objekt.
#' @export
diag_inflyttare_kvarboende_5_ar_kommun_mikrodata_scb <- function(
    indatafil = "G:/Samhällsanalys/Leveranser/inr_flytt_kvarboende_1975-2017.xlsx",
    diagram_capt = "Källa: Regionala utvecklingsdatabasen (SCB)\nBearbetning: Samhällsanalys, Region Dalarna",
    utmapp = rdverktyg::utskriftsmapp(),
    skriv_diagramfil = TRUE,
    returnera_data = FALSE,
    returnera_figur = TRUE
) {

  # ========================================== Inställningar ============================================

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  # ========================================== Läser in data ============================================
  # Filen på G: är en manuellt underhållen ögonblicksbild (flyttare 1975-2017 och deras kvarboende efter 5 år).

  rdverktyg::sakerstall_indatafil(indatafil)
  flytt_kvar_df <- readxl::read_xlsx(indatafil)

  saknas <- setdiff(c("Ar", "andel", "Kommun_namn"), names(flytt_kvar_df))
  if (length(saknas) > 0) {
    stop("Filen saknar kolumnerna: ", paste(saknas, collapse = ", "), ".\n  ", indatafil,
         "\nFörväntade kolumner: Ar, andel, Kommun_namn.", call. = FALSE)
  }

  if (returnera_data) assign("inflyttare_kvarboende", flytt_kvar_df, envir = .GlobalEnv)

  # ========================================== Diagram ==================================================

  ar_intervall <- paste0(min(flytt_kvar_df$Ar), "-", max(flytt_kvar_df$Ar))
  diagram_titel <- stringr::str_wrap(
    paste0("Andel kvarboende 5 år efter att man flyttat in till kommunen ", ar_intervall))
  diagramfil <- paste0("andel_kvarboende_inflytt_", ar_intervall, ".png")

  gg_obj <- rddiagram::SkapaStapelDiagram(
    skickad_df = flytt_kvar_df,
    skickad_x_var = "Ar",
    skickad_y_var = "andel",
    diagram_titel = diagram_titel,
    diagram_capt = diagram_capt,
    stodlinjer_avrunda_fem = TRUE,
    manual_y_axis_title = "procent",
    manual_x_axis_title = "inflyttningsår",
    manual_x_axis_text_vjust = 1,
    manual_x_axis_text_hjust = 1,
    facet_x_axis_storlek = 6,
    x_axis_visa_var_xe_etikett = 2,
    manual_color = rddiagram::diagramfarger("rd_gron")[1],
    facet_grp = "Kommun_namn",
    facet_scale = "fixed",
    skriv_till_diagramfil = skriv_diagramfil,
    output_mapp = utmapp,
    filnamn_diagram = diagramfil)

  gg_list <- stats::setNames(list(gg_obj), stringr::str_remove(diagramfil, "\\.png$"))
  if (returnera_figur) return(gg_list)
  invisible(NULL)
}
