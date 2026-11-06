#' Diagram: inrikes flyttnetto per åldersgrupp
#'
#' Diagram för inrikes flyttar uppdelat på åldersgrupper. Går även att
#' fokusera på en specifik åldersgrupp. Finns både med och utan facet. Det
#' är även möjligt att skapa grupper av regioner - gruppen namnges med
#' `gruppera_namn`.
#'
#' @param region_vekt Val av kommuner/regioner. `"00"` (Riket) stöds inte -
#'   saknar inrikes flyttnetto.
#' @param utmapp Mapp där figuren sparas. Måste finnas om `skriv_diagramfil = TRUE`.
#' @param gruppera_namn Välj namn på gruppen.
#' @param diag_fargvekt Vilken färgvektor vill man ha. Blir alltid `"kon"` när man delar upp på kön.
#' @param skriv_diagramfil Sparar figuren till `utmapp`.
#' @param tid_koder Vilken tid som skall användas.
#' @param diag_flyttnetto_alder Skapa diagram för flyttnetto per åldersgrupp.
#' @param diag_alder_fokus Skapa diagram för flyttnetto, fokuserat på en åldersgrupp.
#' @param diag_facet Skall diagrammet göras med facet eller ej.
#' @param visa_dataetiketter Visa dataetiketter i diagrammet (funkar bara om `diag_facet = FALSE`).
#' @param alder_grp Vilka åldersgrupper skall användas. Välj enligt principen upp till första, sedan intervall mellan och sedan från sista.
#' @param alder_grp_fokus Vilken åldersgrupp skall fokuseras i `diag_alder_fokus`. Måste finnas bland grupperna i `alder_grp`.
#' @param demo Sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren.
#' @param stodlinjer_avrunda_fem Avrunda till närmaste fem på y-axeln.
#' @param valda_ar Vilka år skall användas i `diag_flyttnetto_alder`. `NULL` = de tre senaste åren som faktiskt finns i den hämtade datan.
#' @param returnera_figur Om man vill att figuren skall returneras från funktionen.
#' @param returnera_data TRUE om användaren vill returnera data från funktionen.
#'
#' @return En lista med `ggplot`-objekt.
#' @export
diag_flytt_inrikes_aldersgrupper_scb <- function(region_vekt = "20",
                                        utmapp = rdverktyg::utskriftsmapp(),
                                        gruppera_namn = NA,
                                        diag_fargvekt = rddiagram::diagramfarger("rus_sex"),
                                        skriv_diagramfil = FALSE,
                                        tid_koder = "*",
                                        diag_flyttnetto_alder = TRUE,
                                        diag_alder_fokus = TRUE,
                                        diag_facet = FALSE,
                                        visa_dataetiketter = TRUE,
                                        alder_grp = c(20, 30, 40, 50, 60),
                                        alder_grp_fokus = "20-29 år",
                                        demo = FALSE,
                                        stodlinjer_avrunda_fem = TRUE,
                                        valda_ar = NULL,
                                        returnera_figur = TRUE,
                                        returnera_data = TRUE
){

  # ===========================================================================================================
  # Diagram för inrikes flyttar uppdelat på åldersgrupper. Går även att fokusera på en specifik åldersgrupp.
  # Finns både med och utan facet. Det är även möjligt att skapa grupper av regioner. Gruppen namnges med gruppera_namn
  # Ändrat så att det blir NA på kön snarare än uppdelat och sedan summering. Detta var inget problem tidigare, men CKM gör att summan av delarna inte alltid överensstämmer med totalen
  # ===========================================================================================================

  # om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
  # demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
  if (demo){
    demo_url <-
      c("https://region-dalarna.github.io/utskrivna_diagram/Inrikes_flyttnetto_alder_Dalarna.png",
        "https://region-dalarna.github.io/utskrivna_diagram/Inrikes_flyttnetto_alder_20-29_%C3%A5r_Dalarna.png")
    purrr::walk(demo_url, ~browseURL(.x))
    if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
    rdverktyg::stop_tyst()
  }

  if("00" %in% region_vekt){
    stop("Region 00 (Riket) saknar inrikes flyttnetto och kan inte användas.\nÄndra region_vekt")
  }

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  diagram_capt_bas <- "Källa: SCB:s öppna statistikdatabas, bearbetning av Samhällsanalys, Region Dalarna."

  gg_list <- list()
  objektnamn <- c()

  # Hämtar samma data som tidigare kom via
  # hamta_data/hamta_bef_flyttningar_region_alder_kon_scb.R, men direkt mot
  # SCB:s PxWeb-API v2 med pxweb2r, från två tabeller (SCB bytte metod - CKM,
  # röjandekontroll - för nya årgångar, med en ny tabell från och med 2025):
  # - TAB1212 = gamla "Flyttningar97", 1997-2024.
  # - TAB6640 = nya CKM-tabellen "Flyttningar97CKM", från 2025.
  # CKM-tabellen har varje "riktig" ålderskategori under FLERA olika koder
  # (t.ex. tre olika koder för "100+ år", fyra för "totalt ålder" - en per
  # åldersklassificering tabellen stödjer). Alder = "*" skulle då räkna varje
  # sådan kategori flera gånger vid summering. hamta_giltiga_aldrar() plockar
  # i stället ut exakt en kod per riktig ålder (0-99 år samt en "100+ år"),
  # och exkluderar femårs-/tioårsklasser och totalrader helt.
  hamta_giltiga_aldrar <- function(table_id) {
    meta <- pxweb2r::pxweb2_get_metadata(table_id)
    vals <- pxweb2r::pxweb2_get_values(meta, quiet = TRUE)
    vals$Alder |>
      dplyr::filter(grepl("^[0-9]+(\\+)? år$", label)) |>
      dplyr::distinct(label, .keep_all = TRUE) |>
      dplyr::pull(code)
  }

  hamta_flytt <- function(table_id) {
    pxweb2r::pxweb2_get_data(
      table_id,
      query = list(Region = region_vekt, Kon = NA, Alder = hamta_giltiga_aldrar(table_id),
                   ContentsCode = c("Inrikes flyttningsöverskott", "Invandringsöverskott"), Tid = tid_koder),
      on_all_values_invalid = "null",
    quiet = TRUE)
  }

  flytt_hist <- hamta_flytt("TAB1212")
  flytt_ckm  <- hamta_flytt("TAB6640")

  har_ckm_data <- !is.null(flytt_ckm) && nrow(flytt_ckm) > 0
  ckm_fran_ar <- if (har_ckm_data) min(as.integer(flytt_ckm$år)) else NULL
  diagram_capt <- rddiagram::lagg_till_ckm_notering(diagram_capt_bas, har_ckm_data, fran_ar = ckm_fran_ar)

  flytt_df <- dplyr::bind_rows(flytt_hist, flytt_ckm) |>
    dplyr::rename(regionkod = region_kod, variabel = tabellinnehåll, varde = value) |>
    dplyr::mutate(alder_grupper = rdverktyg::skapa_aldersgrupper(ålder, alder_grp))

  # Standard: de tre senaste åren som faktiskt finns i den hämtade datan
  # (i stället för hårdkodade årtal som blir gamla i takt med att nya år
  # publiceras).
  if (is.null(valda_ar)) {
    valda_ar <- utils::tail(sort(unique(as.integer(flytt_df$år))), 3)
  }

  if(!is.na(gruppera_namn)){
    # Tar bort regionkod och region i gruppering vilket ger en summering på grupp-nivå
    flytt_df <- flytt_df |>
      dplyr::group_by(år, variabel, alder_grupper) |>
      dplyr::summarize(varde = sum(varde), .groups = "drop") |>
      dplyr::mutate(region = gruppera_namn)
    # Regionvekt måste sättas till ett värde för att map-funktionen bara skall köra en gång
    # Används inte vid gruppera namn
    region_vekt = "00"

  } else {
    flytt_df <- flytt_df |>
      dplyr::group_by(år, regionkod, region, variabel, alder_grupper) |>
      dplyr::summarize(varde = sum(varde), .groups = "drop")
  }

  # Returnerar data till R globala miljö
  if(returnera_data == TRUE){
    assign("flytt_aldersgrupper_df", flytt_df, envir = .GlobalEnv)
  }

  skapa_diagram <- function(df, vald_region){ # Start map-funktion
    if(diag_flyttnetto_alder == TRUE){

      if(is.na(gruppera_namn)) df <- dplyr::filter(df, regionkod %in% vald_region)

      reg_txt <- (rdverktyg::skapa_kortnamn_lan(unique(df$region), TRUE))[1]

      if(length(unique(df$region)) > 1){
        reg_txt <- paste0(reg_txt,"_facet")
        diagram_titel <- paste0("Inrikes flyttnetto")
      }else{
        diagram_titel <- paste0("Inrikes flyttnetto i ", reg_txt)
      }

      diagramfil <- paste0("Inrikes_flyttnetto_alder_", reg_txt,".png")
      objektnamn <- c(objektnamn, stringr::str_remove(diagramfil, ".png"))

      gg_obj <- rddiagram::SkapaStapelDiagram(
        skickad_df = dplyr::filter(df, år %in% valda_ar, variabel == "Inrikes flyttningsöverskott"),
        skickad_x_var = "alder_grupper",
        skickad_y_var = "varde",
        skickad_x_grupp = "år",
        diagram_titel = diagram_titel,
        diagram_capt = diagram_capt,
        stodlinjer_avrunda_fem = stodlinjer_avrunda_fem,
        manual_x_axis_text_vjust = 1,
        manual_x_axis_text_hjust = 1,
        manual_y_axis_title = "",
        geom_position_stack = FALSE,
        facet_grp = if (length(unique(df$region)) > 1) "region" else NULL,
        facet_scale = "free",
        facet_legend_bottom = TRUE,
        facet_x_axis_storlek = 6,
        legend_vand_ordning = FALSE,
        dataetiketter = ifelse(diag_facet==TRUE,FALSE,visa_dataetiketter),
        manual_color = diag_fargvekt,
        output_mapp = utmapp,
        skriv_till_diagramfil = skriv_diagramfil,
        filnamn_diagram = diagramfil
      )

      gg_list <- c(gg_list, list(gg_obj))
    }# diag_flyttnetto_alder

    if(diag_alder_fokus == TRUE){

      if(is.na(gruppera_namn)) df <- dplyr::filter(df, regionkod %in% vald_region)

      reg_txt <- (rdverktyg::skapa_kortnamn_lan(unique(df$region), TRUE))[1]

      if(length(unique(df$region)) > 1){
        reg_txt <- paste0(reg_txt,"_facet")
        diagram_titel <- paste0("Inrikes flyttnetto (",alder_grp_fokus ,")")
      }else{
        diagram_titel <- paste0("Inrikes flyttnetto (",alder_grp_fokus ,") i ", reg_txt)
      }

      diagramfil <- paste0("Inrikes flyttnetto_alder_",alder_grp_fokus,"_", reg_txt,".png")
      objektnamn <- c(objektnamn, stringr::str_remove(diagramfil, ".png"))

      gg_obj <- rddiagram::SkapaStapelDiagram(
        skickad_df = dplyr::filter(df, alder_grupper == alder_grp_fokus, variabel == "Inrikes flyttningsöverskott"),
        skickad_x_var = "år",
        skickad_y_var = "varde",
        diagram_titel = diagram_titel,
        diagram_capt = diagram_capt,
        stodlinjer_avrunda_fem = stodlinjer_avrunda_fem,
        manual_x_axis_text_vjust = 1,
        manual_x_axis_text_hjust = 1,
        manual_y_axis_title = "",
        geom_position_stack = FALSE,
        x_axis_visa_var_xe_etikett = ifelse(diag_facet==TRUE,2,NA),
        facet_grp = if (length(unique(df$region)) > 1) "region" else NULL,
        facet_scale = "free",
        facet_legend_bottom = TRUE,
        facet_x_axis_storlek = 6,
        legend_vand_ordning = FALSE,
        dataetiketter = FALSE,
        manual_color = diag_fargvekt[1],
        output_mapp = utmapp,
        skriv_till_diagramfil = skriv_diagramfil,
        filnamn_diagram = diagramfil
      )

      gg_list <- c(gg_list, list(gg_obj))

    } # Slut diag_alder_fokus
    names(gg_list) <- objektnamn
    return(gg_list)
  } # Slut map-funktion
  # Om facet väljs körs ingen map-funktion (vi har bara 1 diagram)
  if (diag_facet) {
    diag <- skapa_diagram(flytt_df,region_vekt)

  } else {
    diag <- purrr::flatten(purrr::map(region_vekt, ~ skapa_diagram(flytt_df, .x)))

  }
  # Går inte att använda facet och gruppera namn samtidigt
  if(!is.na(gruppera_namn)&&diag_facet==TRUE){
    print("OBS! --- Gruppera namn och facet kan ej användas samtidigt. Sätt gruppera namn till NA om du vill ha ett facet-diagram --- OBS!")
  }

  if(returnera_figur==TRUE) return(diag)

}
