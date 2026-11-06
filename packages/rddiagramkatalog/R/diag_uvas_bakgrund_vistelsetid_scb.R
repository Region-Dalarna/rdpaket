#' Diagram: unga som varken arbetar eller studerar (UVAS), vistelsetid
#'
#' Två diagram för unga som varken arbetar eller studerar kopplat till
#' vistelsetid i Sverige.
#'
#' @param region_vekt Enbart ett i taget.
#' @param diag_vistelsetid OBS: oanvänd i originalet, preserverad oförändrad.
#' @param diag_utbniva OBS: oanvänd i originalet, preserverad oförändrad.
#' @param visa_logga_i_diagram TRUE om logga ska visas i diagrammet.
#' @param logga_sokvag Sökväg till logga.
#' @param diag_senaste_ar Diagram för senaste år, uppdelat på kön.
#' @param diag_tidsserie Diagram för tidsserie, uppdelat på vistelsetid.
#' @param diag_fargvekt Valfri färgvektor.
#' @param utmapp Mapp där diagram ska sparas. Måste finnas om `skriv_diagramfil = TRUE`.
#' @param skriv_diagramfil TRUE om diagram ska skrivas till fil.
#' @param returnera_data Skall data returneras till global environment.
#' @param demo Sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren.
#'
#' @return En namngiven lista med `ggplot`-objekt.
#' @export
diag_uvas_bakgrund_vistelsetid_scb <- function(region_vekt = "20",
                                           diag_vistelsetid = TRUE,
                                           diag_utbniva = TRUE,
                                           visa_logga_i_diagram = FALSE,
                                           logga_sokvag = NA,
                                           diag_senaste_ar = TRUE,
                                           diag_tidsserie = TRUE,
                                           diag_fargvekt = NA,
                                           utmapp = rdverktyg::utskriftsmapp(),
                                           skriv_diagramfil = FALSE,
                                           returnera_data = FALSE,
                                           demo = FALSE
) {


  # =======================================================================================================================
  #
  # Två diagram för unga som varken arbetar eller studerar kopplat till vistelsetid i Sverige
  #
  # Migrerad till pxweb2r/rddiagram/rdverktyg. hamta_data-repots hamta_UVAS_mm_region_kon_bakgrund_tid_scb()
  # hämtade mot AA/AA0003/AA0003H/IntGr8LanKON1N, som fortfarande finns i v1 men nu motsvaras av
  # v2-tabellen TAB4931 ("Andel studerande alternativt varken förvärvsarbetande eller studerande
  # personer efter län och kön. 1997-2023"). Används bara av det här skriptet - logiken läggs därför in
  # direkt här i stället för i rdverktyg.
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
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."

  valt_lan <- rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(region_vekt)$region)

  # =============================================== API-uttag ===============================================

  # Bakgrund-koderna för vistelsetid/födelseregion (i stället för klartext - v2:s etiketter för vistelsetid
  # använder en-dash i st.f. vanligt bindestreck, t.ex. "vistelsetid 0–1 år").
  UVAS_df <- pxweb2r::pxweb2_get_data(
    table = "TAB4931",
    query = list(
      Region = region_vekt,
      Kon = "*",
      Bakgrund = c("SE", "INT010", "INT020", "INT030", "INT040"),
      ContentsCode = c("Andel personer 16-19 år som varken förvärvsarbetar eller studerar, procent",
                        "Andel personer 20-25 år som varken förvärvsarbetar eller studerar, procent"),
      Tid = "*"
    ),
    on_all_values_invalid = "null", quiet = TRUE) |>
    dplyr::rename(regionkod = region_kod, variabel = bakgrundsvariabel, sysselsattning = tabellinnehåll, varde = value) |>
    dplyr::mutate(
      # normalisera en-dash till vanligt bindestreck innan vi matchar mot klartexterna nedan
      variabel = stringr::str_replace_all(variabel, "–", "-"),
      variabel = dplyr::case_when(
        variabel == "vistelsetid 0-1 år" ~ "0-1 år",
        variabel == "vistelsetid 2-3 år" ~ "2-3 år",
        variabel == "vistelsetid 4-9 år" ~ "4-9 år",
        variabel == "vistelsetid 10- år" ~ "10- år",
        variabel == "födelseregion: Sverige" ~ "Inrikes född",
        TRUE ~ variabel
      ))


  if(returnera_data == TRUE){
    assign("UVAS_df", UVAS_df, envir = .GlobalEnv)
  }

  # Skapar en faktorvariabel för att få tid sedan etablering i "rätt" ordning i figuren
  UVAS_df$variabel <- factor(UVAS_df$variabel, levels = c("0-1 år","2-3 år",
                                                          "4-9 år","10- år",
                                                          "Inrikes född"))

  if(diag_senaste_ar){

    diagramtitel <- glue::glue("Unga som varken arbetar eller studerar i {valt_lan} år {max(UVAS_df$år)} efter vistelsetid")
    diagramfilnamn <- paste0("UVAS_vistelsetid_inrikes_senastear_",valt_lan,".png")

    # Skapar diagram där etableringstiden jämförs mellan män och kvinnor, oavsett utbildning
    gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = dplyr::mutate(
                                   dplyr::filter(UVAS_df, kön != "män och kvinnor", år == max(år)),
                                   sysselsattning = stringr::str_extract(sysselsattning, "\\d{2}-\\d{2} år")),
                                 skickad_x_var = "variabel",
                                 skickad_y_var = "varde",
                                 skickad_x_grupp = "kön",
                                 manual_color = rddiagram::diagramfarger("kon"),
                                 facet_grp = "sysselsattning",
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 manual_y_axis_title = "procent",
                                 manual_x_axis_title = "Vistelsetid i Sverige",
                                 facet_scale = "fixed",
                                 facet_legend_bottom = TRUE,
                                 y_axis_100proc = TRUE,
                                 x_axis_lutning = 0,
                                 output_mapp = utmapp,
                                 filnamn_diagram = diagramfilnamn,
                                 lagg_pa_logga = visa_logga_i_diagram,
                                 logga_path = logga_sokvag,
                                 skriv_till_diagramfil = skriv_diagramfil)


    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, ".png")

  }

  if(diag_tidsserie){

    diagramtitel <- glue::glue("Unga som varken arbetar eller studerar i {valt_lan} efter vistelsetid")
    diagramfilnamn <- paste0("UVAS_vistelsetid_inrikes_tid_",valt_lan,".png")

    # Skapar diagram där etableringstiden jämförs mellan män och kvinnor, oavsett utbildning
    gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = dplyr::mutate(
                                   dplyr::filter(UVAS_df, kön == "män och kvinnor", variabel %in% c("0-1 år","2-3 år","4-9 år")),
                                   sysselsattning = stringr::str_extract(sysselsattning, "\\d{2}-\\d{2} år")),
                                 skickad_x_var = "år",
                                 skickad_y_var = "varde",
                                 skickad_x_grupp = "variabel",
                                 manual_color = diag_fargvekt,
                                 facet_grp = "sysselsattning",
                                 facet_legend_bottom = TRUE,
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 manual_y_axis_title = "procent",
                                 facet_scale = "fixed",
                                 facet_rader = 2,
                                 y_axis_100proc = TRUE,
                                 x_axis_lutning = 45,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 output_mapp = utmapp,
                                 filnamn_diagram = diagramfilnamn,
                                 # OBS: originalet skickade inte med logga_path här (till skillnad
                                 # från diag_senaste_ar-blocket ovan) - bevarat oförändrat.
                                 lagg_pa_logga = visa_logga_i_diagram,
                                 skriv_till_diagramfil = skriv_diagramfil)


    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, ".png")
  }

  return(gg_list)

}
