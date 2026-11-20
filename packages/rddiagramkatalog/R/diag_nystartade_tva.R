#' Diagram: nystartade företag (Tillväxtanalys)
#'
#' Två diagram över nystartade företag för vald region: ett linjediagram per
#' kvartal och ett stapeldiagram per år. Data laddas automatiskt ned från
#' Tillväxtanalys statistikdatabas.
#'
#' @param utmapp Mapp där figurerna sparas. Måste finnas om
#'   `skriv_diagramfil = TRUE`.
#' @param region_vekt Vilken region som ska väljas.
#' @param skriv_diagramfil Skall figurerna sparas.
#' @param diag_kvartal Skapa diagram: nystartade företag per kvartal.
#' @param diag_år Skapa diagram: nystartade företag per år.
#' @param returnera_data Skall data läggas i R-studios globala miljö.
#' @param returnera_figur Skall en lista med figurer returneras.
#' @param data_namm_kvartal Namn på kvartalsdataobjektet som läggs i globala
#'   miljön om `returnera_data = TRUE`.
#' @param data_namm_ar Namn på årsdataobjektet som läggs i globala miljön om
#'   `returnera_data = TRUE`.
#'
#' @return En namngiven lista med `ggplot`-objekt.
#' @export
diag_nystartade_tva <- function(utmapp = rdverktyg::utskriftsmapp(),
                                region_vekt = "20",
                                skriv_diagramfil = TRUE,
                                diag_kvartal = TRUE,
                                diag_år = TRUE,
                                returnera_data = FALSE,
                                returnera_figur = TRUE,
                                data_namm_kvartal = "nystartade_df",
                                data_namm_ar = "nystartade_df_ar") {
  
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_nystartade_ftg_kvartal_lan_variabel_nyaf_2011_tva.R")
  
  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)
  gg_list <- list()
  
  # Källa: https://statistik.tillvaxtanalys.se/PxWeb/pxweb/sv/Tillvaxtanalys statistikdatabas/
  nystartade_df <- hamta_nystartade_kvartal_lan_variabel_tva(
    region_vekt = region_vekt,
    variabel_klartext = "*",
    kvartal_klartext = "*")
  
  diagram_capt <- "Källa: Tillväxtanalys.\nBearbetning: Samhällsanalys, Region Dalarna."
  
  # Diverse justeringar
  nystartade_df <- nystartade_df |>
    dplyr::rename(tid = kvartal,
                  antal = matches("nys", ignore.case = TRUE)) |>
    dplyr::mutate(ar = substr(tid, 1, 4),
                  kvartal = substr(tid, 5, 6)) |>
    dplyr::mutate(kvartal_namn = dplyr::case_when(
      kvartal == "Q1" ~ "kvartal ett",
      kvartal == "Q2" ~ "kvartal två",
      kvartal == "Q3" ~ "kvartal tre",
      kvartal == "Q4" ~ "kvartal fyra")) |>
    dplyr::mutate(kvartal = stringr::str_replace_all(kvartal, "\\bQ(?=\\d)", "K"),
                  kvartal_ar = paste0(kvartal, " ", ar))
  
  if (diag_kvartal == TRUE) {
    
    if (returnera_data == TRUE) {
      assign(data_namm_kvartal, nystartade_df, envir = .GlobalEnv)
    }
    
    diagram_titel <- paste0("Antal nystartade företag per kvartal i ", rdverktyg::skapa_kortnamn_lan(unique(nystartade_df$region)))
    diagramfilnamn <- paste0("nystartade_ftg_", rdverktyg::skapa_kortnamn_lan(unique(nystartade_df$region)), ".png")
    
    gg_obj <- rddiagram::SkapaLinjeDiagram(
      skickad_df = nystartade_df |>
        dplyr::mutate(kvartal_ar = factor(kvartal_ar, levels = unique(kvartal_ar), ordered = TRUE)),
      skickad_x_var = "kvartal_ar",
      skickad_y_var = "antal",
      skickad_x_grupp = "variabel",
      stodlinjer_avrunda_fem = FALSE,
      manual_color = c(rddiagram::diagramfarger("rus_sex")[1], "#000000"),
      diagram_titel = diagram_titel,
      diagram_capt = diagram_capt,
      inkludera_sista_vardet_var_xe_etikett = TRUE,
      x_axis_var_xe_etikett_ta_bort_nast_sista_vardet = TRUE,
      output_mapp = utmapp,
      manual_y_axis_title = "Antal",
      legend_tabort = TRUE,
      x_axis_visa_var_xe_etikett = 4,
      filnamn_diagram = diagramfilnamn,
      skriv_till_diagramfil = skriv_diagramfil)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn |> stringr::str_remove(".png")
  }
  
  if (diag_år == TRUE) {
    
    nystartade_df_ar <- nystartade_df |>
      dplyr::mutate(
        ar = ifelse(is.na(ar), as.integer(substr(tid, 1, 4)), as.integer(ar))
      ) |>
      dplyr::group_by(regionkod, region, variabel, ar) |>
      dplyr::summarise(
        antal = sum(antal, na.rm = TRUE),
        n_kvartal = dplyr::n_distinct(kvartal),
        .groups = "drop") |>
      dplyr::arrange(regionkod, variabel, ar)
    
    if (returnera_data == TRUE) {
      assign(data_namm_ar, nystartade_df_ar, envir = .GlobalEnv)
    }
    
    diagram_capt <- paste0("Källa: Tillväxtanalys.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Data för ",
                           max(nystartade_df_ar$ar), " till och med kvartal ",
                           nystartade_df_ar |> dplyr::filter(ar == max(ar)) |> dplyr::pull(n_kvartal), ".")
    diagram_titel <- paste0("Antal nystartade företag per år i ", rdverktyg::skapa_kortnamn_lan(unique(nystartade_df_ar$region)))
    diagramfilnamn <- paste0("nystartade_ftg_ar_", rdverktyg::skapa_kortnamn_lan(unique(nystartade_df_ar$region)), ".png")
    
    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = nystartade_df_ar |> dplyr::mutate(ar = as.character(ar)),
      skickad_x_var = "ar",
      skickad_y_var = "antal",
      skickad_x_grupp = "variabel",
      stodlinjer_avrunda_fem = TRUE,
      manual_color = c(rddiagram::diagramfarger("rus_sex")[1], "#000000"),
      diagram_titel = diagram_titel,
      diagram_capt = diagram_capt,
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      inkludera_sista_vardet_var_xe_etikett = TRUE,
      x_axis_var_xe_etikett_ta_bort_nast_sista_vardet = TRUE,
      output_mapp = utmapp,
      manual_y_axis_title = "Antal",
      legend_tabort = TRUE,
      filnamn_diagram = diagramfilnamn,
      skriv_till_diagramfil = skriv_diagramfil)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn |> stringr::str_remove(".png")
  }
  
  if (returnera_figur == TRUE) {
    return(gg_list)
  }
}