#' Diagram: demografisk försörjningskvot och medelålder
#'
#' Skript som skriver ut diagram för demografisk försörjningskvot och
#' medelålder. Går att välja med eller utan uppdelning på kön. Data för
#' län som helhet finns enbart från 2007.
#'
#' @param region_vekt Val av region.
#' @param utmapp Mapp där figuren sparas. Måste finnas om `skriv_diagramfil = TRUE`.
#' @param output_mapp_data Mapp där data sparas, om man vill spara data.
#' @param filnamn_data Filnamn på sparad data.
#' @param diag_forsorjningskvot Diagram för demografisk försörjningskvot.
#' @param diag_medelalder Diagram för medelålder.
#' @param konsuppdelat Om TRUE jämförs kön för senaste år, om FALSE jämförs första och sista år i urvalet.
#' @param diag_fargvekt Val av diagramfärger.
#' @param stodlinjer_avrunda_fem Avrunda stödlinjerna till närmaste femtal.
#' @param tid_koder Vilka år som ska hämtas från Kolada, t.ex. `"2007:9999"` för "från 2007 till och med senaste tillgängliga år".
#' @param skriv_diagramfil Skall figuren sparas.
#' @param returnera_figur Skall figur returneras från funktionen.
#' @param returnera_data Om TRUE returneras data till R-Studios globala miljö.
#' @param demo Sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren.
#'
#' @return En namngiven lista med `ggplot`-objekt.
#' @export
diag_demografi_kolada <-function(region_vekt = rdverktyg::hamtakommuner("20",tamedlan = TRUE,tamedriket = TRUE),
                          utmapp = rdverktyg::utskriftsmapp(),
                          output_mapp_data = NA,
                          filnamn_data = "medelalder_demo.xlsx",
                          diag_forsorjningskvot = TRUE,
                          diag_medelalder = TRUE,
                          konsuppdelat = FALSE,
                          diag_fargvekt = rddiagram::diagramfarger("rus_sex"),
                          stodlinjer_avrunda_fem = TRUE,
                          tid_koder = "2007:9999",
                          skriv_diagramfil = TRUE,
                          returnera_figur = TRUE,
                          returnera_data = FALSE,
                          demo = FALSE
) {

  # ===========================================================================================================  #
  # Skript som skriver ut diagram för demografisk försörjningskvot och medelålder. Går att välja med eller utan uppdelning på kön
  # Data för län som helhet finns enbart från 2007
  # ===========================================================================================================

  # om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
  # demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
  if (demo){
    demo_url <-
      c("https://region-dalarna.github.io/utskrivna_diagram/demo_fors.png",
        "https://region-dalarna.github.io/utskrivna_diagram/medelalder.png")
    purrr::walk(demo_url, ~browseURL(.x))
    if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
    rdverktyg::stop_tyst()
  }

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  gg_list <- list() # Skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  objektnamn <- c() # Används för att namnge
  list_data <- list() # Skapa tom lista som används för att spara till Excel.

  # =============================================== API-uttag ===============================================

  if(diag_forsorjningskvot == TRUE){

    demo_df <- rdverktyg::hamta_kolada_df(
      kpi_id = "N00927",
      valda_kommuner = region_vekt,
      valda_ar = intern_tolka_ar_intervall(tid_koder),
      konsuppdelat = konsuppdelat
    ) |>
      dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region, byt_ut_riket_mot_sverige = TRUE),
                    varde = varde * 100)

    if(returnera_data == TRUE){
      assign("demografisk_forsorjningskvot", demo_df, envir = .GlobalEnv)
    }

    if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
      list_data <- c(list_data,list("Demografisk försörjningskvot" = demo_df))
    }

    if(konsuppdelat == FALSE){
      demo_df <- demo_df |>
        dplyr::filter(ar %in% c(min(ar),max(ar)))
      diag_farger = diag_fargvekt
      diagram_titel <- "Demografisk försörjningskvot"


    } else{
      demo_df <- demo_df |> dplyr::filter(ar == max(ar))
      diag_farger = rddiagram::diagramfarger("kon")
      diagram_titel <- paste0("Demografisk försörjningskvot år ",unique(demo_df$ar))
    }


    diagram_typ <- "demo_fors"
    diagramfil <- "demo_fors.png"
    objektnamn <- c(objektnamn,diagram_typ)

    diagram_capt <- "Källa: SCB (via RKA/Kolada)\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Den demografiska försörjningskvoten beräknas som summan av antal personer 0-19 år och\nantal personer 65 år och äldre dividerat med antal personer 20-64 år. Ett värde över 100 innebär att gruppen\näldre och yngre är större än den i arbetsför ålder."

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = demo_df,
      skickad_x_var = "region",
      skickad_y_var = "varde",
      skickad_x_grupp = ifelse(konsuppdelat == FALSE,"ar","kon"),
      manual_x_axis_text_vjust=1,
      manual_x_axis_text_hjust=1,
      manual_color = diag_farger,
      x_axis_sort_value = TRUE,
      x_axis_sort_grp = 2,
      vand_sortering = TRUE,
      stodlinjer_avrunda_fem = stodlinjer_avrunda_fem,
      manual_y_axis_title= "",
      diagram_titel = diagram_titel,
      diagram_capt = diagram_capt,
      output_mapp = utmapp,
      filnamn_diagram = diagramfil,
      skriv_till_diagramfil = skriv_diagramfil)

    gg_list <- c(gg_list, list(gg_obj))
  }


  if(diag_medelalder == TRUE){

    medelalder_df <- rdverktyg::hamta_kolada_df(
      kpi_id = "N00959",
      valda_kommuner = region_vekt,
      valda_ar = intern_tolka_ar_intervall(tid_koder),
      konsuppdelat = konsuppdelat
    ) |>
      dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region, byt_ut_riket_mot_sverige = TRUE))

    # Då data inte finns för alla år i alla län/riket, så måste jag hitta det senaste år där alla regioner har data, och sedan bara visa från det året och framåt.
    regions_2024 <- medelalder_df |>
      dplyr::filter(ar == max(ar)) |>
      dplyr::distinct(region) |>
      dplyr::pull(region)

    common_start <- medelalder_df |>
      dplyr::filter(region %in% regions_2024) |>
      dplyr::group_by(ar) |>
      dplyr::summarise(n_present = dplyr::n_distinct(region), .groups = "drop") |>
      dplyr::filter(n_present == length(regions_2024)) |>
      dplyr::summarise(common_start = min(ar)) |>
      dplyr::pull(common_start)

    medelalder_df <- medelalder_df |>
      dplyr::filter(region %in% regions_2024,
                    ar >= common_start)

    if(returnera_data == TRUE){
      assign("medelalder", medelalder_df, envir = .GlobalEnv)
    }

    if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
      list_data <- c(list_data,list("Medelålder" = medelalder_df))
    }

    if(konsuppdelat == FALSE){
      medelalder_df <- medelalder_df |>
        dplyr::filter(ar%in%c(min(ar),max(ar)))
      diag_farger = diag_fargvekt
      diagram_titel <- "Medelålder"


    } else{
      medelalder_df <- medelalder_df |> dplyr::filter(ar == max(ar))
      diag_farger = rddiagram::diagramfarger("kon")
      diagram_titel <- paste0("Medelålder år ",unique(medelalder_df$ar))
    }

    diagram_typ <- "medelalder"
    diagramfil <- "medelalder.png"
    objektnamn <-  c(objektnamn,diagram_typ)
    diagram_capt <- "Källa: SCB (via RKA/Kolada)\nBearbetning: Samhällsanalys, Region Dalarna\n"

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = medelalder_df,
      skickad_x_var = "region",
      skickad_y_var = "varde",
      skickad_x_grupp = ifelse(konsuppdelat == FALSE,"ar","kon"),
      manual_x_axis_text_vjust=1,
      manual_x_axis_text_hjust=1,
      manual_color = diag_farger,
      x_axis_sort_value = TRUE,
      dataetiketter = FALSE,
      stodlinjer_avrunda_fem = stodlinjer_avrunda_fem,
      dataetiketter_antal_dec = 1,
      manual_y_axis_title="Medelålder",
      diagram_titel = diagram_titel,
      diagram_capt = diagram_capt,
      output_mapp = utmapp,
      filnamn_diagram = diagramfil,
      skriv_till_diagramfil = skriv_diagramfil)

    gg_list <- c(gg_list, list(gg_obj))
  }

  names(gg_list) <- c(objektnamn)

  # OBS: originalet returnerade (vid returnera_figur = TRUE, standardvärdet)
  # INNAN Excel-exporten nedan, vilket gjorde att output_mapp_data/
  # filnamn_data aldrig skrevs i praktiken. Exporten görs nu alltid innan
  # returneringen.
  if (!is.na(output_mapp_data) & !is.na(filnamn_data)){
    openxlsx::write.xlsx(list_data,paste0(output_mapp_data,filnamn_data))
  }

  if(returnera_figur == TRUE) return(gg_list)

}
