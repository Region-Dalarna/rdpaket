#' Diagram: ekonomiskt stöd efter bakgrund, kön och totalt
#'
#' Diagram som skapar upp till tre figurer för ekonomiskt stöd, används både
#' i det samhällsekonomiska läget i Dalarna och integrationsrapporten. Om
#' man vill veta vad ekonomiskt stöd innefattar:
#' <https://www.scb.se/contentassets/592dcafe2a3b4e65b8e5434796bab0af/huvudsaklig-inkomstkalla-och-arbetsrelaterad-inkomstniva_x.pdf>
#'
#' @param region_vekt Region.
#' @param visa_logga_i_diagram TRUE om logga ska visas i diagrammet.
#' @param logga_sokvag Sökväg till logga.
#' @param diag_bakgrund Skapa diagram uppdelat på födelseregion.
#' @param diag_totalt Skapa diagram totalt (ej uppdelat).
#' @param diag_kon Skapa diagram uppdelat på födelseregion och kön.
#' @param stodlinjer_avrunda_fem Avrunda stödlinjerna till närmaste femtal. Blir för plottrigt ibland - välj FALSE i så fall.
#' @param tid_koder Vilka tidsperioder som ska hämtas. `"*"` = alla, `"9999"` = senaste,
#'   eller `"2021M01:9999"` för "från januari 2021 till och med senaste tillgängliga månad".
#' @param utmapp Mapp där diagram ska sparas. Måste finnas om `skriv_diagramfil = TRUE`.
#' @param alder_klartext Finns: `"15–19 år"`, `"20–24 år"`, `"25–54 år"`, `"55–64 år"`, `"65–74 år"`, `"15–74 år"`, `"16–64 år"`, `"16–65 år"`, `"16–66 år"`, `"20–64 år"`, `"20–65 år"`, `"20–66 år"`.
#' @param ta_bort_nast_sista_varde Ta bort näst sista värdet på x-axeln.
#' @param skriv_diagramfil TRUE om diagram ska skrivas till fil.
#' @param returnera_data Skall data returneras till global environment.
#'
#' @return En namngiven lista med `ggplot`-objekt.
#' @export
diag_ek_stod_bakgrund_scb <- function(region_vekt = "20",
                                         visa_logga_i_diagram = TRUE,
                                         logga_sokvag = NA,
                                         diag_bakgrund = TRUE,
                                         diag_totalt = TRUE,
                                         diag_kon = TRUE,
                                         stodlinjer_avrunda_fem = TRUE,
                                         tid_koder = "*",
                                         utmapp = rdverktyg::utskriftsmapp(),
                                         alder_klartext = "15–74 år",
                                         ta_bort_nast_sista_varde = TRUE,
                                         skriv_diagramfil = FALSE,
                                         returnera_data = FALSE
){

  # ========================================== Allmän info =============================================================================================================================
  # Diagram som skapar två figurer för ekonomiskt stöd, används både i det samhällsekonomiska läget i Dalarna och integrationsrapporten. API från SCB
  # Om man vill veta vad ekonomiskt stöd innefattar: https://www.scb.se/contentassets/592dcafe2a3b4e65b8e5434796bab0af/huvudsaklig-inkomstkalla-och-arbetsrelaterad-inkomstniva_x.pdf
  #
  # Uppdaterat skript med ny version av PXweb Jon 2026-07-01
  # Rättat märklig felaktighet där SCB har ändrat till längre linje mellan åldrar i åldersgrupper. Jon 2026-07-17
  # ====================================================================================================================================================================================

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  ekonomiskt_bistand_grund <- pxweb2r::pxweb2_get_data(
    table = "TAB1784",
    query = list(
      Region = region_vekt,
      HuvudFoT1m = "ekonomiskt stöd",
      Kon = "*",
      Alder = alder_klartext,
      Fodelseregion = "*",
      ContentsCode = "antal totalt",
      Tid = intern_tolka_tidskoder("TAB1784", tid_koder)
    ), quiet = TRUE) |>
    dplyr::rename(`antal totalt` = value,
           regionkod = region_kod) |>
    dplyr::mutate(`antal totalt` = as.numeric(`antal totalt`)) |>
    dplyr::select(-tabellinnehåll)

  # Fixar lite med data
  ekonomiskt_bistand_df <- ekonomiskt_bistand_grund |>
    dplyr::filter(kön == "totalt") |>
    dplyr::rename(antal = `antal totalt`) |>
    rdverktyg::manader_bearbeta_scbtabeller()

  gg_list <- list()

  if(returnera_data == TRUE){
    assign("ekonomiskt_stod_df", ekonomiskt_bistand_df, envir = .GlobalEnv)
  }

  diagram_capt <- "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Antal individer som har ekonomiskt stöd som huvudsaklig inkomstkälla."

  if(diag_totalt){

    diagram_titel <- paste0("Antal individer ",unique(ekonomiskt_bistand_df$ålder), " med ekonomiskt stöd i ",unique(ekonomiskt_bistand_df$region))
    diagramfilnamn <- paste0("ekonomiskt_bistand_alla_",unique(ekonomiskt_bistand_df$region),".png")

    gg_obj <- rddiagram::SkapaLinjeDiagram(
      skickad_df = dplyr::filter(ekonomiskt_bistand_df, födelseregion == "totalt"),
      skickad_x_var = "månad_år",
      skickad_y_var = "antal",
      berakna_index = FALSE,
      diagram_titel = diagram_titel,
      manual_color = rddiagram::diagramfarger("rus_sex")[1],
      x_axis_var_xe_etikett_ta_bort_nast_sista_vardet = ta_bort_nast_sista_varde,
      diagram_capt =  diagram_capt,
      stodlinjer_avrunda_fem = stodlinjer_avrunda_fem,
      manual_y_axis_title = "",
      x_axis_visa_var_xe_etikett = 6,
      output_mapp = utmapp,
      filnamn_diagram = diagramfilnamn,
      lagg_pa_logga = visa_logga_i_diagram,
      skriv_till_diagramfil = skriv_diagramfil)

    gg_list <- c(gg_list, list(gg_obj))

    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, "\\.png")

  }

  if(diag_bakgrund){

    diagram_titel <- paste0("Antal individer ",unique(ekonomiskt_bistand_df$ålder), " med ekonomiskt stöd i ",unique(ekonomiskt_bistand_df$region))
    diagramfilnamn <- paste0("ekonomiskt_bistand_fodelseland_",unique(ekonomiskt_bistand_df$region),".png")

    gg_obj <- rddiagram::SkapaLinjeDiagram(
      skickad_df = dplyr::filter(ekonomiskt_bistand_df, födelseregion != "totalt"),
      skickad_x_var = "månad_år",
      skickad_y_var = "antal",
      skickad_x_grupp = "födelseregion",
      berakna_index = FALSE,
      diagram_titel = diagram_titel,
      manual_color = rev(rddiagram::diagramfarger("rus_sex")[1:2]),
      x_axis_var_xe_etikett_ta_bort_nast_sista_vardet = ta_bort_nast_sista_varde,
      diagram_capt =  diagram_capt,
      stodlinjer_avrunda_fem = stodlinjer_avrunda_fem,
      manual_y_axis_title = "",
      x_axis_visa_var_xe_etikett = 6,
      output_mapp = utmapp,
      filnamn_diagram = diagramfilnamn,
      lagg_pa_logga = visa_logga_i_diagram,
      skriv_till_diagramfil = skriv_diagramfil)

    gg_list <- c(gg_list, list(gg_obj))

    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, "\\.png")

  }

  if(diag_kon == TRUE){
    # Fixar lite med data
    ekonomiskt_bistand_df_kon <- ekonomiskt_bistand_grund |>
      dplyr::filter(kön != "totalt",
             födelseregion != "totalt") |>
        dplyr::mutate(födelseregion = paste0(födelseregion,"a")) |>
        dplyr::rename(antal = `antal totalt`) |>
          rdverktyg::manader_bearbeta_scbtabeller() |>
            dplyr::mutate(kon_bakgrund = paste0(födelseregion, " ", kön))

    if(returnera_data == TRUE){
      assign("ekonomiskt_stod_kon_df", ekonomiskt_bistand_df_kon, envir = .GlobalEnv)
    }

    diagram_titel <- paste0("Antal individer ",unique(ekonomiskt_bistand_df_kon$ålder), " med ekonomiskt stöd i ",unique(ekonomiskt_bistand_df_kon$region))
    diagramfilnamn <- paste0("ekonomiskt_bistand_fodelseland_kon_",unique(ekonomiskt_bistand_df_kon$region),".png")

    gg_obj <- rddiagram::SkapaLinjeDiagram(
      skickad_df = dplyr::filter(ekonomiskt_bistand_df_kon, födelseregion != "totalt"),
      skickad_x_var = "månad_år",
      skickad_y_var = "antal",
      skickad_x_grupp = "kon_bakgrund",
      berakna_index = FALSE,
      diagram_titel = diagram_titel,
      manual_color = rddiagram::diagramfarger("rus_sex"),
      x_axis_var_xe_etikett_ta_bort_nast_sista_vardet = ta_bort_nast_sista_varde,
      diagram_capt =  diagram_capt,
      stodlinjer_avrunda_fem = stodlinjer_avrunda_fem,
      manual_y_axis_title = "",
      x_axis_visa_var_xe_etikett = 6,
      output_mapp = utmapp,
      filnamn_diagram = diagramfilnamn,
      lagg_pa_logga = visa_logga_i_diagram,
      skriv_till_diagramfil = skriv_diagramfil)

    gg_list <- c(gg_list, list(gg_obj))

    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, "\\.png")

  }

    return(gg_list)


}
