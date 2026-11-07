#' Diagram: antal asylsökande i Sverige över tid
#'
#' Tidsserie över antal asylsökande i Sverige från ett valt startår och
#' framåt. Data för de tidigaste åren (innan SCB:s API-täckning) hämtas
#' från en manuellt underhållen Excel-fil på G:, resten hämtas live från
#' SCB:s öppna statistikdatabas.
#'
#' @param utmapp Mapp där figuren sparas. Måste finnas om
#'   `skriv_diagramfil = TRUE`.
#' @param skriv_diagramfil TRUE om figuren ska sparas till `utmapp`.
#' @param visa_logga_i_diagram TRUE om logga ska visas i diagrammet.
#' @param logga_sokvag Sökväg till logga.
#' @param start_ar Första år som ska tas med i diagrammet.
#' @param returnera_figur Skall figur returneras (i en lista).
#' @param diag_fargvekt Vilka färger som ska användas i diagrammet.
#' @param diagram_capt Diagrammets källtext.
#' @param returnera_data Skall data returneras till global environment.
#'
#' @return En namngiven lista med ett `ggplot`-objekt.
#' @export
diag_asylsokande_tidsserie_scb <- function(utmapp = rdverktyg::utskriftsmapp(),
                                         skriv_diagramfil = TRUE,
                                         visa_logga_i_diagram = FALSE,
                                         logga_sokvag = NA,
                                         start_ar = 1984,
                                         returnera_figur = TRUE,
                                         diag_fargvekt = rddiagram::diagramfarger("rus_sex"),
                                         diagram_capt = "Källa: Migrationsverket, SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
                                         returnera_data = FALSE) {

  # ========================================== Allmän info ============================================
  # Tidsserie för antal asylsökande i Sverige från 1984 och framåt. Data för de första åren finns inte
  # via SCB:s API utan hämtas från en manuellt underhållen Excel-fil på G: (senaste filen som matchar
  # "asyl" i mappen). Används i integrationsrapporten.
  # ========================================== Inställningar ============================================

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)

  # ========================================== Läser in data ============================================
  # Senaste filen som matchar mönstret i mappen väljs automatiskt utifrån ändringsdatum.
  # intern_hamta_senaste_fil() stoppar med ett tydligt felmeddelande om mappen saknas eller ingen fil
  # matchar, och skriver ut en konsolvarning om att datan är en manuell ögonblicksbild.
  input_mapp <- "G:/skript/projekt/data/integration"
  latest_file <- intern_hamta_senaste_fil(input_mapp, "asyl")

  asyl_1984 <- openxlsx::read.xlsx(latest_file, startRow = 2) |>
    dplyr::rename(år = "År",
           Antal = "Asylsökande")

  # Länk till tabell: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__BE__BE0101__BE0101P/AsylsokandeN/
  asylsokande <- pxweb2r::pxweb2_get_data(
    table = "TAB5183",
    query = list(
      Medborgarskapsland = "SAMTL",
      Kon = "1+2",
      ContentsCode = "000003WL",
      Period = "hel",
      Tid = "*"
    ), quiet = TRUE) |>
    dplyr::rename(Antal = value) |>
    dplyr::select(år, Antal)

  asylsokande_df <- rbind(dplyr::filter(asyl_1984, !(år %in% unique(asylsokande$år))), asylsokande) |>
    dplyr::filter(is.na(Antal) == FALSE) |>
    dplyr::filter(år >= start_ar)

  if (returnera_data == TRUE) {
    assign("asylsokande_df", asylsokande_df, envir = .GlobalEnv)
  }

  diagramfilnamn <- "asylsokande_antal.png"

  gg_obj <- rddiagram::SkapaStapelDiagram(
    skickad_df = asylsokande_df,
    skickad_x_var = "år",
    skickad_y_var = "Antal",
    manual_color = diag_fargvekt,
    diagram_titel = "Antal asylsökande i Sverige",
    manual_x_axis_text_vjust = 1,
    manual_x_axis_text_hjust = 1,
    diagram_capt = diagram_capt,
    output_mapp = utmapp,
    stodlinjer_avrunda_fem = TRUE,
    x_axis_visa_var_xe_etikett = 2,
    lagg_pa_logga = visa_logga_i_diagram,
    logga_path = logga_sokvag,
    manual_y_axis_title = "",
    filnamn_diagram = diagramfilnamn,
    skriv_till_diagramfil = skriv_diagramfil)

  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, ".png")

  if (returnera_figur == TRUE) {
    return(gg_list)
  }
}
