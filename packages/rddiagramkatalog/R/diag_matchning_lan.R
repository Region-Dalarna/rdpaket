#' Diagram: matchningsgrad på arbetsmarknaden
#'
#' Skapar diagram för matchningsgraden på arbetsmarknaden, dels på länsnivå,
#' dels i ett enskilt län uppdelat på bakgrund (födelselandsgrupp). Enbart
#' för senaste år.
#'
#' @param region_vekt Region vi är intresserade av. Gäller diagrammet med bakgrundsuppdelning.
#' @param output_mapp_figur Mapp där figur sparas.
#' @param output_mapp_data Mapp där data sparas.
#' @param filnamn_data Filnamn för sparad data.
#' @param spara_figur Sparar figuren till `output_mapp_figur`.
#' @param kon_klartext Alternativet är `"samtliga anställda"` för att undvika könsuppdelning. Vill man ha alla tre i datafilen som returneras sätts denna till `"*"`. Det blir enbart kvinnor och män i diagrammet.
#' @param valda_farger Vilka färger som ska användas i diagram.
#' @param diag_lan Skapar ett diagram där alla län jämförs.
#' @param diag_bakgrund Skapar ett diagram där bakgrund i samma län jämförs.
#' @param returnera_figur Skall figur returneras (i en lista).
#' @param returnera_data Skall data returneras (till R-studios globala miljö).
#' @param demo Sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren.
#'
#' @return En namngiven lista med `ggplot`-objekt.
#' @export
diag_matchning_lan <- function(region_vekt = "20",
                               output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                               output_mapp_data = NA,
                               filnamn_data = "matchning.xlsx",
                               spara_figur = TRUE,
                               kon_klartext = c("män", "kvinnor"),
                               valda_farger = rddiagram::diagramfarger("kon"),
                               diag_lan = TRUE,
                               diag_bakgrund = TRUE,
                               returnera_figur = TRUE,
                               returnera_data = FALSE,
                               demo = FALSE
                               ) {

  # ========================================== Allmän info ============================================
  # 1: Skapar diagram för matchningsgraden på arbetsmarknaden, dels på länsnivå, dels i ett enskilt län uppdelat på bakgrund.
  # Enbart för senaste år
  # ========================================== Inställningar ============================================

  # om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
  # demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
  if (demo){
    demo_url <-
      c("https://region-dalarna.github.io/utskrivna_diagram/matchning_bakgrund.png",
        "https://region-dalarna.github.io/utskrivna_diagram/matchning_jmf.png")
    purrr::walk(demo_url, ~browseURL(.x))
    if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
    rdverktyg::stop_tyst()
  }

  # Det som står under diagrammet
  diagram_capt <- "Källa: BAS i SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Branschens andel av totalt antal förvärvsarbetande"

  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  # Skapar en tom vektor som skall innehålla objektnamn
  objektnamn <- c()

  vald_region = rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(region_vekt)$region)

  # =============================================== API-uttag ===============================================

  # Matchning – olika grupper (M2). Andel anställda med matchning mellan utbildning och yrke, efter region, kön, ålder respektive födelselandsgrupp (nattbefolkning). År 2019-2024
  df <- pxweb2r::pxweb2_get_data(
    table = "TAB6366",
    query = list(
      Region = rdverktyg::hamtaAllaLan(tamedriket = TRUE),
      Kon = kon_klartext,
      AlderFodelselandgr = c("Sverige","Norden/EU","Afrika","Asien","Övriga_världen","totalt"),
      ContentsCode = "000007I3",
      Tid = "9999"
    ), quiet = TRUE) |>
    dplyr::select(-region_kod,-tabellinnehåll) |>
    dplyr::rename(matchningsgrad = value) |>
    dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region,byt_ut_riket_mot_sverige = TRUE))


  df <- df |>
    dplyr::rename("fodelseland" = `ålder/födelselandgrupp`) |>
    dplyr::mutate("fodelseland" = ifelse(fodelseland %in% c("födda i Europa utom Norden och EU samt Sydamerika, Nordamerika och Oceanien"),"övriga", fodelseland))

  if(returnera_data == TRUE){
    assign("matchning_df", df, envir = .GlobalEnv)
  }

  # Om det finns fler än två kön i datafilen så filtreras de bort.
  if(length(unique(df$kön))>2) df <- df |> dplyr::filter(kön %in%c("män","kvinnor"))

  if(diag_lan==TRUE){

    diagram_titel <- paste0("Matchningsgrad på arbetsmarknaden år ",max(df$år))
    diagramfil <- "matchning_jmf.png"
    objektnamn <- c(objektnamn,"matchning_jmf")

    diagram_capt <- "Källa: SCB:s öppna statistikdatabas, regionala matchningsindikatorer.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Med matchning avses matchad förvärvsgrad\ndvs. andelen anställda som har ett yrke som helt matchar deras utbildning (enligt SCB)."

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = df |>
        dplyr::filter(fodelseland == "totalt",
                      år == max(år)),
      skickad_x_var = "region",
      skickad_y_var = "matchningsgrad",
      skickad_x_grupp = ifelse("samtliga anställda" %in% kon_klartext,NA,"kön"),
      manual_x_axis_text_vjust=1,
      manual_x_axis_text_hjust=1,
      manual_color = valda_farger,
      diagram_titel = diagram_titel,
      diagram_capt =  diagram_capt,
      y_axis_100proc = TRUE,
      x_axis_sort_value = TRUE,
      x_axis_lutning = 45,
      manual_y_axis_title = "procent",
      output_mapp = output_mapp_figur,
      filnamn_diagram = diagramfil,
      skriv_till_diagramfil = spara_figur)

    gg_list <- c(gg_list, list(gg_obj))

  }


  if(diag_bakgrund==TRUE){

    diagram_capt <- "Källa: SCB:s öppna statistikdatabas, regionala matchningsindikatorer.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Med matchning avses matchad förvärvsgrad\ndvs. andelen anställda som har ett yrke som helt matchar deras utbildning (enligt SCB)."
    diagram_titel <- paste0("Matchning på arbetsmarknaden i " ,vald_region, " år ",max(df$år))
    diagramfil <- paste0("matchning_bakgrund.png")
    objektnamn <- c(objektnamn,"matchning_bakgrund")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = df |>
        dplyr::filter(år == max(år),
                      region == vald_region,
                      fodelseland != "totalt"),
      skickad_x_var = "fodelseland",
      skickad_y_var = "matchningsgrad",
      skickad_x_grupp = ifelse("samtliga anställda" %in% kon_klartext,NA,"kön"),
      manual_color = valda_farger,
      diagram_titel = diagram_titel,
      diagram_capt =  diagram_capt,
      x_axis_sort_value = TRUE,
      x_axis_lutning =0,
      y_axis_100proc = TRUE,
      diagram_liggande = TRUE,
      manual_y_axis_title="procent",
      output_mapp = output_mapp_figur,
      filnamn_diagram = diagramfil,
      skriv_till_diagramfil = spara_figur)

    gg_list <- c(gg_list, list(gg_obj))
  }

  names(gg_list) <- c(objektnamn)
  if(returnera_figur == TRUE) return(gg_list)

  if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
    openxlsx::write.xlsx(df,paste0(output_mapp_data,filnamn_data))
  }

}
