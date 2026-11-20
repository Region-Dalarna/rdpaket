#' Diagram: konkurser per år (Tillväxtanalys)
#'
#' Stapeldiagram per år över antal konkurser (och/eller antal anställda
#' berörda av konkurser) för vald region. Ett diagram skapas per vald
#' variabel. Data laddas automatiskt ned från Tillväxtanalys statistik-
#' databas, eftersom SCB inte längre publicerar konkursdata.
#'
#' @param region_vekt Vilken region som ska väljas.
#' @param variabel_klartext Vald variabel. `"*"` = alla. Finns: "Antal
#'   konkurser", "Antal anställda berörda av konkurser".
#' @param utmapp Mapp där figurerna sparas. Måste finnas om
#'   `skriv_diagramfil = TRUE`.
#' @param skriv_diagramfil Skall figurerna sparas.
#' @param returnera_data Skall data läggas i R-studios globala miljö.
#' @param returnera_figur Skall en lista med figurer returneras.
#' @param data_namm Namn på dataobjektet som läggs i globala miljön om
#'   `returnera_data = TRUE`.
#'
#' @return En namngiven lista med `ggplot`-objekt.
#' @export
diag_konkurser_ar_tva <- function(region_vekt = "20",
                                  variabel_klartext = "*",
                                  utmapp = rdverktyg::utskriftsmapp(),
                                  skriv_diagramfil = TRUE,
                                  returnera_data = FALSE,
                                  returnera_figur = TRUE,
                                  data_namm = "konkurser_df_ar") {
  
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_konkurser_manad_lan_variabel_konk_1996_tva.R")
  
  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)
  gg_list <- list()
  
  # Källa: https://statistik.tillvaxtanalys.se/PxWeb/pxweb/sv/Tillvaxtanalys statistikdatabas/
  # SCB har inte längre data för konkurser, så vi använder Tillväxtanalys data istället.
  konkurser_df <- hamta_konkurser_manad_lan_variabel_tva(
    region_vekt = region_vekt,
    variabel_klartext = variabel_klartext,
    manad_klartext = "*")
  
  # Diverse justeringar
  konkurser_df <- konkurser_df |>
    dplyr::rename(tid = månad,
                  antal = matches("konkurser", ignore.case = TRUE)) |>
    dplyr::mutate(år = stringr::str_sub(tid, 1, 4),
                  månad = format(as.Date(paste(år, stringr::str_sub(tid, 6, 7), "1", sep = "-")), "%b"),
                  manad_long = format(as.Date(paste(år, stringr::str_sub(tid, 6, 7), "1", sep = "-")), "%B"),
                  år_månad = paste0(år, " - ", månad),
                  månad_år = paste0(månad, " ", år)) |>
    dplyr::select(-tid)
  
  senaste_ar <- max(konkurser_df$år)
  senaste_manad <- dplyr::last(konkurser_df$manad_long)
  
  # gruppera per år
  konkurser_df_ar <- konkurser_df |>
    dplyr::mutate(år = as.integer(år)) |>
    dplyr::group_by(regionkod, region, variabel, år) |>
    dplyr::summarise(
      antal = sum(antal, na.rm = TRUE),
      n_månader = dplyr::n_distinct(månad),
      .groups = "drop") |>
    dplyr::arrange(regionkod, variabel, år)
  
  if (returnera_data == TRUE) {
    assign(data_namm, konkurser_df_ar, envir = .GlobalEnv)
  }
  
  skapa_diagram <- function(df, vald_variabel) {
    df <- df |>
      dplyr::filter(variabel == vald_variabel)
    
    diagram_titel <- paste0(unique(df$variabel), " i ", rdverktyg::skapa_kortnamn_lan(unique(df$region)))
    if (unique(df$variabel) == "Antal anställda berörda av konkurser") {
      diagramfilnamn <- paste0("anstalla_berorda_konkurser_ar", rdverktyg::skapa_kortnamn_lan(unique(df$region)), ".png")
    } else {
      diagramfilnamn <- paste0("anstal_konkurser_ar", rdverktyg::skapa_kortnamn_lan(unique(df$region)), ".png")
    }
    
    diagram_capt <- paste0("Källa: Tillväxtanalys\nBearbetning: Samhällsanalys, Region Dalarna\nData för ",
                           senaste_ar, " till och med ", senaste_manad)
    
    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = df |> dplyr::mutate(år = as.character(år)),
      skickad_x_var = "år",
      skickad_y_var = "antal",
      manual_color = rddiagram::diagramfarger("rus_sex"),
      diagram_titel = diagram_titel,
      diagram_capt = diagram_capt,
      output_mapp = utmapp,
      stodlinjer_avrunda_fem = TRUE,
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      manual_y_axis_title = "Antal",
      filnamn_diagram = diagramfilnamn,
      skriv_till_diagramfil = skriv_diagramfil)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list) <- stringr::str_remove(diagramfilnamn, ".png")
    return(gg_list)
  }
  
  diag <- purrr::map(unique(konkurser_df_ar$variabel), ~ skapa_diagram(konkurser_df_ar, .x)) |>
    purrr::flatten()
  
  if (returnera_figur == TRUE) {
    return(diag)
  }
}