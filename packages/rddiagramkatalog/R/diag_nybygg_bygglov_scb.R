#' Diagram: nybyggnation och bygglov
#'
#' Två linjediagram per kvartal: antal påbörjade bostäder (nybyggnation) och
#' antal bygglov för nybyggnad, för vald region och hustyp. Data laddas
#' automatiskt ned från SCB:s öppna statistikdatabas.
#'
#' @param region_vekt Vilken region som ska väljas.
#' @param utmapp Mapp där figurerna sparas. Måste finnas om
#'   `skriv_diagramfil = TRUE`.
#' @param skriv_diagramfil Skall figurerna sparas.
#' @param returnera_data Skall data läggas i R-studios globala miljö.
#' @param returnera_figur Skall en lista med figurer returneras.
#' @param hustyp_klartext Vald hustyp. `"*"` = alla hustyper. Finns:
#'   "flerbostadshus", "småhus".
#' @param diag_fargvekt Vilka färger som ska användas i diagrammen.
#' @param diag_nybyggnation Skapa diagram: påbörjade bostäder.
#' @param diag_bygglov Skapa diagram: bygglov.
#' @param startar_nybygg Startår för nybyggnadsdiagrammet. Finns från 1975.
#' @param startar_bygglov Startår för bygglovsdiagrammet. Finns från 1996.
#' @param data_namm_nybygg Namn på nybyggnadsdataobjektet som läggs i globala
#'   miljön om `returnera_data = TRUE`.
#' @param data_namm_bygglov Namn på bygglovsdataobjektet som läggs i globala
#'   miljön om `returnera_data = TRUE`.
#'
#' @return En namngiven lista med `ggplot`-objekt.
#' @export
diag_nybyggnation_bygglov_scb <- function(region_vekt = "20",
                                          utmapp = rdverktyg::utskriftsmapp(),
                                          skriv_diagramfil = TRUE,
                                          returnera_data = FALSE,
                                          returnera_figur = TRUE,
                                          hustyp_klartext = "småhus",
                                          diag_fargvekt = rddiagram::diagramfarger("rus_sex"),
                                          diag_nybyggnation = TRUE,
                                          diag_bygglov = TRUE,
                                          startar_nybygg = "2020",
                                          startar_bygglov = "2020",
                                          data_namm_nybygg = "Husbyggande_df",
                                          data_namm_bygglov = "Bygglov_df") {
  
  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)
  
  diagram_capt_nybygg <- "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Gäller för nybyggnad. Data är preliminär och det finns en viss eftersläpning.\nUnderskattningen det senaste kvartalet är mellan 20 och 60 procent beroende på kvartal.\nUnderskattningen är som störst för andra kvartalet. Efter ett år är underskattningen cirka 2 procent."
  diagram_capt_bygglov <- "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Gäller för nybyggnad. Notera att det finns en eftersläpning i statistiken.\nFör bygglov gör eftersläpningen att de senaste fyra kvartalens siffror är underskattade.\nStörst är underskattningen det senaste kvartalet."
  gg_list <- list()
  objektnamn <- c()
  
  if (diag_nybyggnation == TRUE) {
    
    # Länk till tabell: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__BO__BO0101__BO0101C/LagenhetNyKv16/
    nybyggnation_df <- pxweb2r::pxweb2_get_data(
      table = "TAB4572",
      query = list(
        Region       = region_vekt,
        Hustyp       = hustyp_klartext,
        ContentsCode = "Påbörjade lägenheter i nybyggda hus",
        Tid          = "*"
      ), quiet = TRUE) |>
      dplyr::rename(`Påbörjade lägenheter i nybyggda hus` = value,
                    regionkod = region_kod) |>
      dplyr::select(-tabellinnehåll)
    
    nybyggnation_df <- nybyggnation_df |>
      dplyr::mutate(variabel = dplyr::last(names(nybyggnation_df)),
                    ar = substr(kvartal, 1, 4),
                    ar_kvartal = kvartal,
                    kvartal = substr(kvartal, 5, 6),
                    kvartal_ar = paste0(kvartal, " ", ar)) |>
      dplyr::filter(ar >= startar_nybygg) |>
      dplyr::rename(varde = `Påbörjade lägenheter i nybyggda hus`) |>
      dplyr::select(regionkod, region, ar, ar_kvartal, kvartal, kvartal_ar, hustyp, variabel, varde)
    
    if (returnera_data == TRUE) {
      assign(data_namm_nybygg, nybyggnation_df, envir = .GlobalEnv)
    }
    
    if (hustyp_klartext == "*") {
      diagramtitel <- glue::glue("Påbörjade bostäder i {unique(nybyggnation_df$region) |> rdverktyg::skapa_kortnamn_lan() |> rdverktyg::list_komma_och()}")
    } else {
      diagramtitel <- glue::glue("Påbörjade {unique(nybyggnation_df$hustyp)} i {unique(nybyggnation_df$region) |> rdverktyg::skapa_kortnamn_lan() |> rdverktyg::list_komma_och()}")
    }
    
    diagramfil <- glue::glue("nybyggnation_{unique(nybyggnation_df$regionkod) |> paste0(collapse = '_')}_ar{min(nybyggnation_df$kvartal)}_{max(nybyggnation_df$kvartal)}.png")
    objektnamn <- c(objektnamn, diagramfil |> stringr::str_remove(".png"))
    
    gg_obj <- rddiagram::SkapaLinjeDiagram(
      skickad_df = nybyggnation_df |>
        dplyr::mutate(kvartal_ar = factor(kvartal_ar, levels = unique(kvartal_ar), ordered = TRUE)),
      skickad_x_var = "kvartal_ar",
      skickad_y_var = "varde",
      skickad_x_grupp = ifelse(hustyp_klartext != "*", NA, "hustyp"),
      stodlinjer_avrunda_fem = TRUE,
      manual_color = diag_fargvekt,
      diagram_titel = diagramtitel,
      diagram_capt = diagram_capt_nybygg,
      output_mapp = utmapp,
      manual_y_axis_title = "Antal",
      x_axis_visa_var_xe_etikett = 1,
      filnamn_diagram = diagramfil,
      skriv_till_diagramfil = skriv_diagramfil)
    
    gg_list <- c(gg_list, list(gg_obj))
  }
  
  if (diag_bygglov == TRUE) {
    
    # Länk till tabell: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__BO__BO0101__G/LghHustypKv/
    bygglov_df <- pxweb2r::pxweb2_get_data(
      table = "TAB2534",
      query = list(
        Region       = region_vekt,
        Hustyp       = hustyp_klartext,
        ContentsCode = "*",
        Tid          = "*"
      ), quiet = TRUE) |>
      dplyr::rename(Antal = value,
                    regionkod = region_kod) |>
      dplyr::select(-tabellinnehåll) |>
      dplyr::mutate(ar = substr(kvartal, 1, 4),
                    ar_kvartal = kvartal,
                    kvartal = substr(kvartal, 5, 6),
                    kvartal_ar = paste0(kvartal, " ", ar)) |>
      dplyr::filter(ar >= startar_bygglov)
    
    if (returnera_data == TRUE) {
      assign(data_namm_bygglov, bygglov_df, envir = .GlobalEnv)
    }
    
    if (hustyp_klartext == "*") {
      diagramtitel <- glue::glue("Bygglov för bostäder i {unique(bygglov_df$region) |> rdverktyg::skapa_kortnamn_lan() |> rdverktyg::list_komma_och()}")
    } else {
      diagramtitel <- glue::glue("Bygglov för {unique(bygglov_df$hustyp)} i {unique(bygglov_df$region) |> rdverktyg::skapa_kortnamn_lan() |> rdverktyg::list_komma_och()}")
    }
    
    diagramfil <- glue::glue("bygglov_{unique(bygglov_df$regionkod) |> paste0(collapse = '_')}_ar{min(bygglov_df$kvartal)}_{max(bygglov_df$kvartal)}.png")
    objektnamn <- c(objektnamn, diagramfil |> stringr::str_remove(".png"))
    
    gg_obj <- rddiagram::SkapaLinjeDiagram(
      skickad_df = bygglov_df |>
        dplyr::mutate(kvartal_ar = factor(kvartal_ar, levels = unique(kvartal_ar), ordered = TRUE)) |>
        dplyr::filter(ar >= startar_bygglov) |>
        dplyr::filter(hustyp %in% c("småhus", "flerbostadshus exkl. specialbostäder")),
      skickad_x_var = "kvartal_ar",
      skickad_y_var = "Antal",
      skickad_x_grupp = ifelse(hustyp_klartext != "*", NA, "hustyp"),
      berakna_index = FALSE,
      manual_color = diag_fargvekt,
      stodlinjer_avrunda_fem = TRUE,
      diagram_titel = diagramtitel,
      diagram_capt = diagram_capt_bygglov,
      output_mapp = utmapp,
      manual_y_axis_title = "Antal",
      x_axis_visa_var_xe_etikett = 1,
      filnamn_diagram = diagramfil,
      skriv_till_diagramfil = skriv_diagramfil)
    
    gg_list <- c(gg_list, list(gg_obj))
  }
  
  names(gg_list) <- objektnamn
  
  if (returnera_figur == TRUE) return(gg_list)
}