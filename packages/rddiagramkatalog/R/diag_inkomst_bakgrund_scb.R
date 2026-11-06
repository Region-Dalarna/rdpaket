#' Diagram: förvärvsinkomst efter bakgrund (vistelsetid i Sverige)
#'
#' Stapeldiagram över förvärvsinkomst uppdelat på vistelsetid i Sverige,
#' för utrikes födda jämfört med inrikes födda. Från integrationsrapporten.
#'
#' @param region_vekt Region (max 1 åt gången).
#' @param visa_logga_i_diagram TRUE om logga ska visas i diagrammet.
#' @param logga_sokvag Sökväg till logga.
#' @param utmapp Mapp där diagram ska sparas. Måste finnas om `skriv_diagramfil = TRUE`.
#' @param inkomst_typ Finns `"Medianinkomst, tkr"`, `"Medelinkomst, tkr"`. Max 1 åt gången.
#' @param skriv_diagramfil TRUE om diagram ska skrivas till fil.
#' @param alder_klartext Finns: `"20+ år"`, `"20–64 år"`, `"20–65 år"`, `"65+ år"`, `"66+ år"`. Max 1 åt gången.
#' @param returnera_data Skall data returneras till global environment.
#' @param demo Sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren.
#'
#' @return En namngiven lista med ett `ggplot`-objekt.
#' @export
diag_inkomst_bakgrund_scb <- function(region_vekt = "20",
                                      visa_logga_i_diagram = FALSE,
                                      logga_sokvag = NA,
                                      utmapp = rdverktyg::utskriftsmapp(),
                                      inkomst_typ = "Medianinkomst, tkr",
                                      skriv_diagramfil = FALSE,
                                      alder_klartext = "20–64 år",
                                      returnera_data = FALSE,
                                      demo = FALSE
) {

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

  # Tabellen TAB5278 (v2) har redan använts direkt (Tid = "9999" förstås
  # av pxweb2r::pxweb2_get_data(, quiet = TRUE) som "senaste period" via dess egen
  # latest_period_code-inställning), så inget bakomliggande hamta_data-
  # skript att ersätta här.
  forvarvsinkomst_df <- pxweb2r::pxweb2_get_data(
    table = "TAB5278",
    query = list(
      Region = region_vekt,
      Kon = "*",
      Fodelseregion = "*",
      VistelsetidUF = "*",
      Alder = alder_klartext,
      ContentsCode = inkomst_typ,
      Tid ="9999"
    ),
  quiet = TRUE) |>
    dplyr::filter(!is.na(value)) |>
    dplyr::rename(!!inkomst_typ := value,
                     regionkod = region_kod,
                     vistelsetid = `vistelsetid år`) |>
    dplyr::mutate(vistelsetid = ifelse(födelseregion == "födda i Sverige","Inrikes född",vistelsetid)) |>
    dplyr::filter(födelseregion %in% c("födda i Sverige","utrikes födda"),
                 vistelsetid != "samtliga") |>
    dplyr::mutate(vistelsetid = dplyr::case_when(
            vistelsetid == "1–2 år i Sverige" ~ "1-2 år",
            vistelsetid == "3–4 år i Sverige" ~ "3-4 år",
            vistelsetid == "5–9 år i Sverige" ~ "5-9 år",
            vistelsetid == "10–19 år i Sverige" ~ "10-19 år",
            vistelsetid == "20– år i Sverige" ~ "20- år",
            TRUE ~ vistelsetid
          ))


  if(returnera_data == TRUE){
    assign("forvarvsinkomst_df", forvarvsinkomst_df, envir = .GlobalEnv)
  }

  gg_list <- list()


  diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna.\nSammanräknad förvärvsinkomst, dvs. alla skattepliktiga inkomster före skatt (dock ej kapitalinkomster)."

  # Skapar en faktorvariabel för att få tid sedan etablering i "rätt" ordning i figuren
  forvarvsinkomst_df$vistelsetid <- factor(forvarvsinkomst_df$vistelsetid, levels = c("1-2 år","3-4 år",
                                                                                      "5-9 år","10-19 år",
                                                                                      "20- år","Inrikes född"))
  variabel <- sub(",.*", "", dplyr::last(names(forvarvsinkomst_df)))
  diagramtitel <- paste0(variabel," (", unique(forvarvsinkomst_df$ålder),") i Dalarna ",max(forvarvsinkomst_df$år)," efter vistelsetid")
  diagramfilnamn <- paste0(variabel,"_bakgrund.png")

  # Skapar diagram där etableringstiden jämförs mellan män och kvinnor, oavsett utbildning
  gg_obj <- rddiagram::SkapaStapelDiagram(
    skickad_df = dplyr::filter(forvarvsinkomst_df, kön != "totalt"),
    skickad_x_var = "vistelsetid",
    skickad_y_var = dplyr::last(names(forvarvsinkomst_df)),
    skickad_x_grupp = "kön",
    manual_color = rddiagram::diagramfarger("kon"),
    diagram_titel = diagramtitel,
    diagram_capt =  diagram_capt,
    manual_x_axis_title = "Vistelsetid i Sverige",
    y_axis_100proc = FALSE,
    x_axis_lutning = 0,
    output_mapp = utmapp,
    filnamn_diagram = diagramfilnamn,
    lagg_pa_logga = visa_logga_i_diagram,
    logga_path = logga_sokvag,
    skriv_till_diagramfil = skriv_diagramfil)


  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, "\\.png")

  return(gg_list)

}
