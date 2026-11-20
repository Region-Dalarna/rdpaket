#' Diagram: konjunkturbarometern
#'
#' Två linjediagram för Konjunkturinstitutets konjunkturbarometer: ett
#' generellt över barometerindikatorn för Sverige, och ett uppdelat på
#' bredare branschgrupper. I båda diagrammen ritas en referenslinje vid 100
#' ("normalt läge i ekonomin"). Data laddas automatiskt ned från
#' Konjunkturinstitutet.
#'
#' @param utmapp Mapp där figurerna sparas. Måste finnas om
#'   `skriv_diagramfil = TRUE`.
#' @param skriv_diagramfil Skall figurerna sparas.
#' @param returnera_data Skall data läggas i R-studios globala miljö.
#' @param returnera_figur Skall en lista med figurer returneras.
#' @param diag_fargvekt Vilka färger som ska användas i diagrammen.
#' @param diag_barometern Skapa diagram: barometerindikatorn (generellt).
#' @param diag_bransch Skapa diagram: barometerindikatorn per branschgrupp.
#' @param legend_byrow TRUE om legendens objekt ska ordnas radvis.
#' @param diagram_capt Diagramtext (källa, bearbetning och förklaring).
#' @param antal_etiketter_barometern Intervall mellan visade x-etiketter (i
#'   månader) i det generella diagrammet.
#' @param antal_etiketter_bransch Intervall mellan visade x-etiketter (i
#'   månader) i branschdiagrammet.
#' @param startvarde_barometern Startvärde (år-månad) för det generella
#'   diagrammet. Finns från 1996-07.
#' @param startvarde_bransch Startvärde (år-månad) för branschdiagrammet.
#' @param data_namm Namn på dataobjektet som läggs i globala miljön om
#'   `returnera_data = TRUE`.
#'
#' @return En namngiven lista med `ggplot`-objekt.
#' @export
diag_konjunkturbarometern_konj <- function(utmapp = rdverktyg::utskriftsmapp(),
                                           skriv_diagramfil = TRUE,
                                           returnera_data = FALSE,
                                           returnera_figur = TRUE,
                                           diag_fargvekt = rddiagram::diagramfarger("rus_sex"),
                                           diag_barometern = TRUE,
                                           diag_bransch = TRUE,
                                           legend_byrow = FALSE,
                                           diagram_capt = "Källa: Konjunkturinstitutet.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: En indikator över 100 motsvarar en starkare ekonomi än normalt och värden över 110\nen mycket starkare ekonomi än normalt. En indikator under 100 respektive under 90\nvisar en svagare respektive mycket svagare ekonomi än normalt.",
                                           antal_etiketter_barometern = 12,
                                           antal_etiketter_bransch = 12,
                                           startvarde_barometern = "1996-07",
                                           startvarde_bransch = "2000-01",
                                           data_namm = "barometern_df") {
  
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_konjunkturbarometern_indikator_period_konj.R")
  
  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)
  gg_list <- list()
  objektnamn <- c()
  
  barometern_df <- hamta_konjunkturbarometern(
    indikator = c("Barometerindikatorn", "Tillverkningsindustri (SNI 10-33)",
                  "Byggverksamhet (SNI 41-43)", "Handel (SNI 45-47)",
                  "Tjänstesektorn (SNI 49-82 + 95-96)", "Konfidensindikator hushåll"),
    period = "*")
  
  barometern_df <- barometern_df |>
    dplyr::filter(!(is.na(`Barometerindikatorn och andra indikatorer`))) |>
    dplyr::mutate(ar = substr(Period, 1, 4),
                  manad_long = format(as.Date(paste(ar, stringr::str_sub(Period, 6, 7), "1", sep = "-")), "%B"),
                  Period = paste(ar, stringr::str_sub(Period, 6, 7), sep = "-"),
                  manad_ar = paste0(manad_long, " ", ar),
                  manad_kort_ar = paste0(substr(manad_long, 1, 3), " ", ar)) |>
    dplyr::filter(Period >= min(startvarde_barometern, startvarde_bransch)) |>
    dplyr::rename("varde" = `Barometerindikatorn och andra indikatorer`)
  
  if (returnera_data == TRUE) {
    assign(data_namm, barometern_df, envir = .GlobalEnv)
  }
  
  if (diag_barometern == TRUE) {
    
    barometern_jmf_df <- barometern_df |>
      dplyr::filter(Period >= startvarde_barometern, Indikator == "Barometerindikatorn") |>
      dplyr::mutate(varde = 100,
                    "Indikator" = "Normalt läge i ekonomin")
    
    barometern_gen_utskrift <- rbind(
      barometern_df |> dplyr::filter(Period >= startvarde_barometern, Indikator == "Barometerindikatorn"),
      barometern_jmf_df)
    
    diagramtitel <- paste0("Barometerindikatorns utveckling i Sverige mellan ",
                           dplyr::first(barometern_gen_utskrift$manad_long), " ",
                           dplyr::first(barometern_gen_utskrift$ar), " och ",
                           dplyr::last(barometern_gen_utskrift$manad_long), " ",
                           dplyr::last(barometern_gen_utskrift$ar))
    diagramtitel <- stringr::str_wrap(diagramtitel, 45)
    diagramfilnamn <- "barometern.png"
    objektnamn <- c(objektnamn, diagramfilnamn |> stringr::str_remove(".png"))
    
    gg_obj <- rddiagram::SkapaLinjeDiagram(
      skickad_df = barometern_gen_utskrift |>
        dplyr::mutate(manad_kort_ar = factor(manad_kort_ar, levels = unique(manad_kort_ar), ordered = TRUE)),
      skickad_x_var = "manad_kort_ar",
      skickad_y_var = "varde",
      skickad_x_grupp = "Indikator",
      berakna_index = FALSE,
      manual_color = c(diag_fargvekt[1], "#000000"),
      stodlinjer_avrunda_fem = TRUE,
      manual_y_axis_title = "Indikatorvärde",
      inkludera_sista_vardet_var_xe_etikett = TRUE,
      x_axis_var_xe_etikett_ta_bort_nast_sista_vardet = FALSE,
      diagram_titel = diagramtitel,
      diagram_capt = diagram_capt,
      output_mapp = utmapp,
      x_axis_visa_var_xe_etikett = antal_etiketter_barometern,
      filnamn_diagram = diagramfilnamn,
      skriv_till_diagramfil = skriv_diagramfil)
    
    gg_list <- c(gg_list, list(gg_obj))
  }
  
  if (diag_bransch == TRUE) {
    
    barometern_jmf_df <- barometern_df |>
      dplyr::filter(Period >= startvarde_bransch, Indikator == "Tillverkningsindustri (SNI 10-33)") |>
      dplyr::mutate(varde = 100,
                    "Indikator" = "Normalt läge i ekonomin")
    
    barometern_bransch_utskrift <- rbind(
      barometern_df |> dplyr::filter(Indikator != "Barometerindikatorn", Period >= startvarde_bransch),
      barometern_jmf_df)
    
    barometern_bransch_utskrift <- barometern_bransch_utskrift |>
      dplyr::mutate(Indikator = dplyr::case_when(
        Indikator == "Byggverksamhet (SNI 41-43)"        ~ "Byggindustri",
        Indikator == "Handel (SNI 45-47)"                ~ "Handel",
        Indikator == "Tillverkningsindustri (SNI 10-33)" ~ "Tillverkningsindustri",
        Indikator == "Tjänstesektorn (SNI 49-82 + 95-96)" ~ "Tjänstesektorn",
        Indikator == "Konfidensindikator hushåll"        ~ "Hushåll",
        .default = Indikator))
    
    barometern_bransch_utskrift <- barometern_bransch_utskrift |>
      dplyr::mutate(Indikator = factor(Indikator, levels = c("Byggindustri", "Handel", "Tillverkningsindustri",
                                                             "Tjänstesektorn", "Hushåll",
                                                             "Normalt läge i ekonomin")))
    
    diagramtitel <- paste0("Barometerindikatorns utveckling i Sverige mellan ",
                           dplyr::first(barometern_bransch_utskrift$manad_long), " ",
                           dplyr::first(barometern_bransch_utskrift$ar), " och ",
                           dplyr::last(barometern_bransch_utskrift$manad_long), " ",
                           dplyr::last(barometern_bransch_utskrift$ar))
    diagramtitel <- stringr::str_wrap(diagramtitel, 45)
    diagramfilnamn <- "barometern_bransch.png"
    objektnamn <- c(objektnamn, diagramfilnamn |> stringr::str_remove(".png"))
    
    gg_obj <- rddiagram::SkapaLinjeDiagram(
      skickad_df = barometern_bransch_utskrift |>
        dplyr::mutate(manad_kort_ar = factor(manad_kort_ar, levels = unique(manad_kort_ar), ordered = TRUE)),
      skickad_x_var = "manad_kort_ar",
      skickad_y_var = "varde",
      skickad_x_grupp = "Indikator",
      berakna_index = FALSE,
      manual_color = c(diag_fargvekt[1:5], "#000000"),
      manual_y_axis_title = "Indikatorvärde",
      stodlinjer_avrunda_fem = TRUE,
      legend_byrow = legend_byrow,
      inkludera_sista_vardet_var_xe_etikett = TRUE,
      x_axis_var_xe_etikett_ta_bort_nast_sista_vardet = TRUE,
      diagram_titel = diagramtitel,
      diagram_capt = diagram_capt,
      output_mapp = utmapp,
      x_axis_visa_var_xe_etikett = antal_etiketter_bransch,
      filnamn_diagram = diagramfilnamn,
      skriv_till_diagramfil = skriv_diagramfil)
    
    gg_list <- c(gg_list, list(gg_obj))
  }
  
  names(gg_list) <- objektnamn
  
  if (returnera_figur == TRUE) return(gg_list)
}