#' Diagram: vistelsetid för godkända i SFI efter bakgrund
#'
#' Två diagram över vistelsetid för godkända i SFI (svenska för
#' invandrare) i Sverige, uppdelat på utbildningsbakgrund.
#'
#' @param region_vekt Enbart ett i taget.
#' @param diag_vistelsetid_senaste_ar Diagram för vistelsetid, senaste år.
#' @param diag_vistelsetid_tidsserie Diagram för vistelsetid, tidsserie.
#' @param visa_logga_i_diagram TRUE om logga ska visas i diagrammet.
#' @param logga_sokvag Sökväg till logga.
#' @param diag_fargvekt Valfri färgvektor.
#' @param utmapp Mapp där diagram ska sparas. Måste finnas om `skriv_diagramfil = TRUE`.
#' @param skriv_diagramfil TRUE om diagram ska skrivas till fil.
#' @param returnera_data Skall data returneras till global environment.
#' @param demo Sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren.
#'
#' @return En namngiven lista med `ggplot`-objekt.
#' @export
diag_sfi_bakgrund_scb <- function(region_vekt = "20",
                              diag_vistelsetid_senaste_ar = TRUE,
                              diag_vistelsetid_tidsserie = TRUE,
                              visa_logga_i_diagram = FALSE,
                              logga_sokvag = NA,
                              diag_fargvekt = NA,
                              utmapp = rdverktyg::utskriftsmapp(),
                              skriv_diagramfil = FALSE,
                              returnera_data = FALSE,
                              demo = FALSE
) {


  # =======================================================================================================================
  #
  # Två diagram godkända i SFI efter vistelsetid i Sverige
  # =======================================================================================================================

  # om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
  # demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
  if (demo){
    demo_url <-
      c("https://region-dalarna.github.io/utskrivna_diagram/medellivslangd_aterstaende_vid_30 år_alder_Dalarna_ar2012-2016_2019-2023.png")
    purrr::walk(demo_url, ~browseURL(.x))
    if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
    rdverktyg::stop_tyst()
  }

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  if (all(is.na(diag_fargvekt))) diag_fargvekt <- rddiagram::diagramfarger("rus_sex")

  gg_list <- list()

  valt_lan <- rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(region_vekt)$region)

  # Nya PXweb
  SFI_df <- pxweb2r::pxweb2_get_data(
    table = "TAB1808",
    query = list(
      Region = region_vekt,
      Kon = "*",
      Bakgrund = c("utbildningsnivå: förgymnasial utbildning", "utbildningsnivå: gymnasial utbildning", "utbildningsnivå: eftergymnasial utbildning"),
      ContentsCode = "Vistelsetid för godkända i sfi, median i antal dagar",
      Tid = "*"
    ), quiet = TRUE) |>
    dplyr::rename(variabel = bakgrundsvariabel) |>
    dplyr::mutate(variabel = sub("utbildningsnivå: ", "", variabel),
           variabel = stringr::str_to_sentence(variabel)) |>
    dplyr::select(-tabellinnehåll)


  if(returnera_data == TRUE){
    assign("SFI_df", SFI_df, envir = .GlobalEnv)
  }

  diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."

  #Skapar en faktorvariabel för att få tid sedan etablering i "rätt" ordning i figuren
  SFI_df$variabel <- factor(SFI_df$variabel, levels = unique(SFI_df$variabel))

  if(diag_vistelsetid_senaste_ar){

    diagramtitel <- glue::glue("Vistelsetid för godkända i SFI i {valt_lan} år {max(SFI_df$år)}")
    diagramfilnamn <- paste0("sfi_vistelsetid_senastear_",valt_lan,".png")

    gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = dplyr::filter(SFI_df, kön != "män och kvinnor", år == max(år)),
                                 skickad_x_var = "variabel",
                                 skickad_y_var = dplyr::last(names(SFI_df)),
                                 skickad_x_grupp = "kön",
                                 manual_color = rddiagram::diagramfarger("kon"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 facet_scale = "fixed",
                                 x_axis_lutning = 0,
                                 output_mapp = utmapp,
                                 filnamn_diagram = diagramfilnamn,
                                 lagg_pa_logga = visa_logga_i_diagram,
                                 logga_path = logga_sokvag,
                                 skriv_till_diagramfil = skriv_diagramfil)


    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, ".png")

  }

  if(diag_vistelsetid_tidsserie){

    diagramtitel <- glue::glue("Vistelsetid för godkända i SFI i {valt_lan}")
    diagramfilnamn <- paste0("sfi_vistelsetid_tidsserie_",valt_lan,".png")

    gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = dplyr::filter(SFI_df, kön == "män och kvinnor"),
                                 skickad_x_var = "år",
                                 skickad_y_var = dplyr::last(names(SFI_df)),
                                 skickad_x_grupp = "variabel",
                                 manual_color = diag_fargvekt,
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 x_axis_lutning = 45,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 output_mapp = utmapp,
                                 filnamn_diagram = diagramfilnamn,
                                 lagg_pa_logga = visa_logga_i_diagram,
                                 logga_path = logga_sokvag,
                                 skriv_till_diagramfil = skriv_diagramfil)


    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, ".png")
  }

  return(gg_list)

}
