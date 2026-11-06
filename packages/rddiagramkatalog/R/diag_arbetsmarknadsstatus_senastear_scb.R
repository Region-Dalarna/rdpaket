#' Diagram: arbetsmarknadsstatus för senaste observation
#'
#' Diagram för arbetslöshet, sysselsättningsgrad och arbetskraftsdeltagande
#' för senaste observation. Går att dela upp på kön och ändra åldersgrupp
#' (max 1 åt gången). Det går även att dela upp på utrikes/inrikes födda,
#' vilket ger ett facet-diagram. Funkar både för län och kommuner, men bara
#' senaste år.
#'
#' @param region_vekt Region(er). Använd förslagsvis `rdverktyg::hamtakommuner()` och `rdverktyg::hamtaAllaLan()`.
#' @param fokus_lan Måste väljas. Det län som, vid sidan om riket, fokuseras i figuren. Gäller inte vid könsuppdelat.
#' @param output_mapp_data Mapp där data sparas.
#' @param ta_bort_diagramtitel Tar bort diagramtitel.
#' @param ta_bort_caption Tar bort caption.
#' @param filnamn_data Filnamn för datafil.
#' @param utmapp Mapp där figuren sparas. Måste finnas om `skriv_diagramfil = TRUE`.
#' @param skriv_diagramfil Sparar figuren till `utmapp`.
#' @param returnera_figur Returnerar en figur.
#' @param kon_klartext Finns även `c("kvinnor", "män")` om man vill ha könsuppdelat.
#' @param fodelseregion_klartext_vekt Finns även `c("inrikes född", "utrikes född")`. Då skapas ett facet-diagram.
#' @param alder_klartext Finns: `"15–19 år"` `"16–19 år"` `"20–24 år"` `"25–29 år"` `"30–34 år"` `"35–39 år"` `"40–44 år"` `"45–49 år"` `"50–54 år"` `"55–59 år"` `"60–64 år"` `"65–69 år"` `"70–74 år"` `"15–74 år"` `"16–64 år"` `"16–65 år"` `"16–66 år"` `"20–64 år"` `"20–65 år"` `"20–66 år"`.
#' @param diag_fargvekt Ändra till `rddiagram::diagramfarger("kon")` om man vill ha de färgerna.
#' @param diag_arbetslosthet TRUE för figur för arbetslöshet.
#' @param diag_arbetskraftsdeltagande TRUE för figur för arbetskraftsdeltagande.
#' @param diag_sysselsattningsgrad TRUE för figur för sysselsättningsgrad.
#' @param returnera_data Skall data returneras.
#' @param data_namm Vad skall returnerat dataset heta. Viktigt om data returneras två gånger i samma projekt (annars skrivs de över).
#'
#' @return En namngiven lista med `ggplot`-objekt.
#' @export
diag_arbetsmarknadsstatus_senastear_scb <- function(region_vekt = rdverktyg::hamtakommuner("20"),
                                        fokus_lan = "20",
                                        output_mapp_data = NA,
                                        ta_bort_diagramtitel = FALSE,
                                        ta_bort_caption = FALSE,
                                        filnamn_data = "arbetsmarknadsstatus.xlsx",
                                        utmapp = rdverktyg::utskriftsmapp(),
                                        skriv_diagramfil = TRUE,
                                        returnera_figur = TRUE,
                                        kon_klartext = "totalt",
                                        fodelseregion_klartext_vekt = "totalt",
                                        alder_klartext = "20–64 år",
                                        diag_fargvekt = rddiagram::diagramfarger("rus_tre_fokus"),
                                        diag_arbetslosthet = TRUE,
                                        diag_arbetskraftsdeltagande = TRUE,
                                        diag_sysselsattningsgrad = TRUE,
                                        returnera_data = FALSE,
                                        data_namm = "arbetsmarknadsstatus"){

  ## =================================================================================================================
  # Diagram för arbetslöshet, sysselsättningsgrad och arbetskraftsdeltagande för senaste observation.
  # Går att dela upp på kön och ändra åldersgrupp (max 1 åt gången). Det går även att dela upp på utrikes/inrikes födda, vilket ger ett facet-diagram
  # Funkar både för län och kommuner, men bara senaste år
  # diag_arbetsloshet, diag_arbetskraftsdeltagande och diag_sysselsattningsgrad sätts till TRUE baserat på vilka variabler man vill ha
  # Potentiell förbättring: Ändra så att man kan köra för utrikes/inrikes separat. Funkar nu, men diagramtitel hänger inte med.
  # =================================================================================================================

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  objektnamn <- c()

  dia_titel = NULL
  # Tar bara ut data för de variabler som skal vara med
  variabel <- c()
  if(diag_sysselsattningsgrad==TRUE) variabel <- c(variabel,"sysselsättningsgrad")
  if(diag_arbetslosthet==TRUE) variabel <- c(variabel,"arbetslöshet")
  if(diag_arbetskraftsdeltagande==TRUE) variabel <- c(variabel,"arbetskraftsdeltagande")

  # Facetgruppering (utrikes/inrikes födda) - facet_grp = NULL slår av facetering
  # med den nuvarande rddiagram-mekaniken (diagram_facet-argumentet finns inte längre).
  facet_grp_vald <- if (all(c("inrikes född","utrikes född") == fodelseregion_klartext_vekt)) "födelseregion" else NULL

  # Länk till tabell: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0210__AM0210A/ArbStatusM/
  arbetsmarknadsstatus_df <- pxweb2r::pxweb2_get_data(
    table = "TAB6260",
    query = list(
      Region = region_vekt,
      Kon = kon_klartext,
      Alder = alder_klartext,
      Fodelseregion = fodelseregion_klartext_vekt,
      ContentsCode = variabel,
      Tid = "9999"
    ), quiet = TRUE) |>
    dplyr::rename(varde = value,
           regionkod = region_kod,
           variabel = tabellinnehåll) |>
      dplyr::mutate(ar=substr(månad,1,4),
             manad_long=format(as.Date(paste(ar, stringr::str_sub(månad, 6,7),"1", sep = "-")), "%B"),
             Period=paste(ar, stringr::str_sub(månad, 6,7),sep = "-")) |>
        dplyr::select(-månad)


  # Län att fokusera på
  valt_lan = rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(fokus_lan)$region)
  # Tar bort län i länsnamn och gör om riket till Sverige
  arbetsmarknadsstatus_df$region = rdverktyg::skapa_kortnamn_lan(arbetsmarknadsstatus_df$region,byt_ut_riket_mot_sverige = TRUE)

  # Sparar data
  if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
    flik_lista=list("Arbetsmarknadsstatus" = arbetsmarknadsstatus_df)
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp_data,filnamn_data))
  }

  # Returnerar data
  if(returnera_data == TRUE){
    assign(data_namm, arbetsmarknadsstatus_df, envir = .GlobalEnv)
  }

  if(diag_sysselsattningsgrad==TRUE){

    diagram_capt = "Källa: SCB:s öppna statistikdatabas, befolkningens arbetsmarknadsstatus (BAS).\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Andelen av befolkningen som är sysselsatt (sysselsättningsgrad)."
    diagramtitel <- paste0("Sysselsättningsgrad i åldersgruppen ",unique(arbetsmarknadsstatus_df$ålder), " i ",unique(arbetsmarknadsstatus_df$manad_long)," ",unique(arbetsmarknadsstatus_df$ar))
    diagramtitel <- stringr::str_wrap(diagramtitel,50)
    objektnamn <-c(objektnamn,("sysselsattningsgrad_senastear"))

    if(ta_bort_diagramtitel == TRUE){
      diagramtitel <- dia_titel
    }

    if(ta_bort_caption == TRUE){
      diagram_capt <- NULL
    }

    # Skapar diagram
    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = arbetsmarknadsstatus_df |>
        dplyr::filter(variabel == "sysselsättningsgrad") |>
        dplyr::mutate(fokus = ifelse(region == valt_lan,1,
                                      ifelse(region == "Sverige",2,0))),
      skickad_x_var = "region",
      skickad_y_var = "varde",
      skickad_x_grupp =ifelse("totalt" %in% kon_klartext,NA,"kön"),
      manual_y_axis_title = "procent",
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      facet_grp = facet_grp_vald,
      facet_scale = "fixed",
      facet_legend_bottom = TRUE,
      y_axis_100proc = TRUE,
      manual_color = diag_fargvekt,
      diagram_titel = diagramtitel,
      diagram_capt =  diagram_capt,
      x_var_fokus = ifelse("totalt" %in% kon_klartext,"fokus",NA),
      x_axis_sort_value = TRUE,
      x_axis_lutning = 45,
      stodlinjer_avrunda_fem = FALSE,
      output_mapp = utmapp,
      filnamn_diagram = "sysselsattningsgrad_senastear.png",
      skriv_till_diagramfil = skriv_diagramfil)

    gg_list <- c(gg_list, list(gg_obj))
  }

  if(diag_arbetslosthet==TRUE){

    diagram_capt = "Källa: SCB:s öppna statistikdatabas, befolkningens arbetsmarknadsstatus (BAS).\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Andelen av personer i arbetskraften som är arbetslösa."
    diagramtitel <- paste0("Arbetslöshet i åldersgruppen ",unique(arbetsmarknadsstatus_df$ålder), " i ",unique(arbetsmarknadsstatus_df$manad_long)," ",unique(arbetsmarknadsstatus_df$ar))
    diagramtitel <- stringr::str_wrap(diagramtitel,50)
    objektnamn <-c(objektnamn,("arbetslosthet_senastear"))

    if(ta_bort_diagramtitel == TRUE){
      diagramtitel <- dia_titel
    }

    if(ta_bort_caption == TRUE){
      diagram_capt <- NULL
    }

    # Skapar diagram
    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = arbetsmarknadsstatus_df |>
        dplyr::filter(variabel == "arbetslöshet") |>
        dplyr::mutate(fokus = ifelse(region == valt_lan,1,
                                      ifelse(region == "Sverige",2,0))),
      skickad_x_var = "region",
      skickad_y_var = "varde",
      skickad_x_grupp = ifelse("totalt" %in% kon_klartext,NA,"kön"),
      manual_y_axis_title = "procent",
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      manual_color = diag_fargvekt,
      facet_grp = facet_grp_vald,
      facet_scale = "fixed",
      facet_legend_bottom = TRUE,
      diagram_titel = diagramtitel,
      diagram_capt =  diagram_capt,
      x_var_fokus = ifelse("totalt" %in% kon_klartext,"fokus",NA),
      x_axis_sort_value = TRUE,
      x_axis_lutning = 45,
      stodlinjer_avrunda_fem = TRUE,
      output_mapp = utmapp,
      filnamn_diagram = "arbetslöshet_senastear.png",
      skriv_till_diagramfil = skriv_diagramfil)

    gg_list <- c(gg_list, list(gg_obj))
  }

  if(diag_arbetskraftsdeltagande == TRUE){

    diagram_capt = "Källa: SCB:s öppna statistikdatabas, befolkningens arbetsmarknadsstatus (BAS).\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Andelen av personer i arbetskraften som är arbetslösa."
    diagramtitel <- paste0("Arbetskraftsdeltagande i åldersgruppen ",unique(arbetsmarknadsstatus_df$ålder), " i ",unique(arbetsmarknadsstatus_df$manad_long)," ",unique(arbetsmarknadsstatus_df$ar))
    diagramtitel <- stringr::str_wrap(diagramtitel,50)
    objektnamn <-c(objektnamn,("arbetskraftsdeltagande_senastear"))

    if(ta_bort_diagramtitel == TRUE){
      diagramtitel <- dia_titel
    }

    if(ta_bort_caption == TRUE){
      diagram_capt <- NULL
    }

    # Skapar diagram
    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = arbetsmarknadsstatus_df |>
        dplyr::filter(variabel == "arbetskraftsdeltagande") |>
        dplyr::mutate(fokus = ifelse(region == valt_lan,1,
                                      ifelse(region == "Sverige",2,0))),
      skickad_x_var = "region",
      skickad_y_var = "varde",
      skickad_x_grupp = ifelse("totalt" %in% kon_klartext,NA,"kön"),
      manual_y_axis_title = "procent",
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      facet_grp = facet_grp_vald,
      facet_scale = "fixed",
      facet_legend_bottom = TRUE,
      y_axis_100proc = TRUE,
      manual_color = diag_fargvekt,
      diagram_titel = diagramtitel,
      diagram_capt =  diagram_capt,
      x_var_fokus = ifelse("totalt" %in% kon_klartext,"fokus",NA),
      x_axis_sort_value = TRUE,
      x_axis_lutning = 45,
      stodlinjer_avrunda_fem = FALSE,
      output_mapp = utmapp,
      filnamn_diagram = "arbetskraftsdeltagande_senastear.png",
      skriv_till_diagramfil = skriv_diagramfil)

    gg_list <- c(gg_list, list(gg_obj))
  }

  if(returnera_figur == TRUE){
    names(gg_list) <- objektnamn
    return(gg_list)
  }

}
