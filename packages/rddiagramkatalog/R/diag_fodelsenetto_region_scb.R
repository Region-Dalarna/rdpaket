#' Diagram: födelsenetto (födda minus döda) per region
#'
#' Diagram för födelsenettot (födda - döda). Finns som facet eller
#' enskilda diagram. Ej uppdelat på kön.
#'
#' @param region_vekt Val av kommuner/regioner.
#' @param utmapp Mapp där figuren sparas. Måste finnas om `skriv_diagramfil = TRUE`.
#' @param diag_fargvekt Vilken färgvektor vill man ha. Blir alltid `"kon"` när man väljer det diagrammet.
#' @param skriv_diagramfil Sparar figuren till `utmapp`.
#' @param diag_facet Skall ett facetdiagram skapas.
#' @param stodlinjer_avrunda_fem Funkar för tillfället inte som TRUE. Blir något konstigt med svarta streck för netto.
#' @param svarta_streck_tjocklek Tjocklek på svarta streck som visar födelsenetto. Är en andel av skillnaden mellan högsta och lägsta värdet i datasetet, så att det anpassar sig efter olika skalor.
#' @param visa_totalvarden Visa totalvärden i diagrammet. Funkar om `diag_facet = FALSE`.
#' @param etiketter_xaxel Intervall för etiketter på x-axeln.
#' @param tid_koder Välj tid, finns från 1968 till senaste år (som skrivs `"9999"`).
#' @param returnera_figur Om man vill att figuren skall returneras från funktionen.
#' @param returnera_data TRUE om användaren vill returnera data från funktionen.
#' @param diagram_capt Diagrammets källtext.
#' @param demo Sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren.
#'
#' @return En lista med `ggplot`-objekt.
#' @export
diag_fodelsenetto_region_scb <- function(region_vekt = "20",
                               utmapp = rdverktyg::utskriftsmapp(),
                               diag_fargvekt = NA,
                               skriv_diagramfil = TRUE,
                               diag_facet = FALSE,
                               stodlinjer_avrunda_fem = TRUE,
                               svarta_streck_tjocklek = 0.002,
                               visa_totalvarden = TRUE,
                               etiketter_xaxel = 4,
                               tid_koder = "*",
                               returnera_figur = TRUE,
                               returnera_data = FALSE,
                               diagram_capt = "Källa: SCB:s öppna statistikdatabas, bearbetning av Samhällsanalys, Region Dalarna.",
                               demo = FALSE
                               ) {

  # ===========================================================================================================
  # Diagram för födelsenettot (födda - döda). Finns som facet eller enskilda diagram. Ej uppdelat på kön
  # Förbättringsmöjligheter: Svart linje för födelsenetto funkar inte med facet.
  # ===========================================================================================================

  # om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
  # demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
  if (demo){
    demo_url <- c("https://region-dalarna.github.io/utskrivna_diagram/fodelsenetto_Dalarna.png")
    purrr::walk(demo_url, ~browseURL(.x))
    if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
    rdverktyg::stop_tyst()
  }

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  if (all(is.na(diag_fargvekt))) diag_fargvekt <- rddiagram::diagramfarger("rus_sex")

  gg_list <- list()
  objektnamn <- c()

  # Hämtar samma data som tidigare kom via
  # hamta_data/hamta_fodda_moderns_alder_region_scb.R och
  # hamta_doda_alder_kon_region_scb.R (båda hade redan förberedda men
  # utkommenterade CKM-varianter - aktiverade här), men direkt mot SCB:s
  # PxWeb-API v2 med pxweb2r, från fyra tabeller (två per ämne: historik +
  # CKM-tabell från och med 2025):
  # - TAB1264/TAB959  = "FoddaK"/"DodaFodelsearK", 1968-2024. Moderns
  #   ålder/ålder kan utelämnas (elimineringsbara).
  # - TAB6401/TAB6757 = CKM-tabellerna, från 2025. Inte elimineringsbara där
  #   längre - måste anges explicit som totalkoden "TotSA", vilket också gör
  #   dem till egna kolumner i svaret; select() plockar bort dem igen.
  # Kön hålls odelat (NA) i alla fyra, som i originalet.
  hamta_fodda <- function(table_id, alder_moder) {
    pxweb2r::pxweb2_get_data(
      table_id,
      query = list(Region = region_vekt, Kon = NA, AlderModer = alder_moder, Tid = tid_koder),
      on_all_values_invalid = "null",
    quiet = TRUE)
  }
  hamta_doda <- function(table_id, alder) {
    pxweb2r::pxweb2_get_data(
      table_id,
      query = list(Region = region_vekt, Kon = NA, Alder = alder, Tid = tid_koder),
      on_all_values_invalid = "null",
    quiet = TRUE)
  }

  fodda_hist <- hamta_fodda("TAB1264", NA)
  fodda_ckm  <- hamta_fodda("TAB6401", "TotSA")
  if (!is.null(fodda_ckm)) fodda_ckm <- dplyr::select(fodda_ckm, -dplyr::any_of("moderns ålder"))

  doda_hist <- hamta_doda("TAB959", NA)
  doda_ckm  <- hamta_doda("TAB6757", "TotSA")
  if (!is.null(doda_ckm)) doda_ckm <- dplyr::select(doda_ckm, -dplyr::any_of("ålder"))

  har_ckm_data <- (!is.null(fodda_ckm) && nrow(fodda_ckm) > 0) || (!is.null(doda_ckm) && nrow(doda_ckm) > 0)
  ckm_fran_ar <- if (har_ckm_data) min(as.integer(c(fodda_ckm$år, doda_ckm$år))) else NULL
  diagram_capt <- rddiagram::lagg_till_ckm_notering(diagram_capt, har_ckm_data, fran_ar = ckm_fran_ar)

  fodda_df <- dplyr::bind_rows(fodda_hist, fodda_ckm) |>
    dplyr::rename(regionkod = region_kod, födda = value) |>
    dplyr::select(-dplyr::any_of("tabellinnehåll"))

  doda_df <- dplyr::bind_rows(doda_hist, doda_ckm) |>
    dplyr::rename(regionkod = region_kod, Döda = value) |>
    dplyr::select(-dplyr::any_of("tabellinnehåll"))

  df <- fodda_df |>
    dplyr::left_join(doda_df, by = c("regionkod","region", "år")) |>
    dplyr::mutate(netto = födda-Döda) |>
    tidyr::pivot_longer(cols = c("födda", "Döda", "netto"),
                 names_to = "variabel",
                 values_to = "varde")

  if(returnera_data == TRUE){
    assign("fodda_doda_df", df, envir = .GlobalEnv)
  }

  skapa_diagram <- function(df, vald_region){

    df <- dplyr::filter(df, regionkod %in% vald_region)

    if (visa_totalvarden == TRUE && diag_facet == FALSE){

      diff <- max(df$varde) - min(df$varde) # ta reda på skillnaden mellan det högsta och lägsta värdet i datasetet
      totalvarden_linjebredd <- svarta_streck_tjocklek*diff      # gör en linjetjocklek på totallinjerna som är 0,2 % av diff (på raden ovan)
      total_list <- list()
      unika_ar <- unique(df$år)
      vald_regionkod = vald_region
      unika_reg <- unique(vald_regionkod)
      unika_reg_txt <- rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(unika_reg)$region)

      for (reg in 1:length(unika_reg)) {
        for (ar in 1:length(unika_ar)){
          arsvarde <- df |>
            dplyr::filter(år == unika_ar[ar], regionkod == unika_reg[reg], variabel == "netto") |>
            dplyr::pull(varde)
          total_list <- c(total_list, list(list(geom = "rect", ymin=arsvarde-totalvarden_linjebredd, ymax=arsvarde+totalvarden_linjebredd, xmin=ar-0.45, xmax=ar+0.45, alpha=1, fill="black")))

        } # slut for-loop unika_ar
      } # slut for_loop unika_reg
    } else total_list <- NA # slut if-sats visa_totalvärden

    reg_txt <- (rdverktyg::skapa_kortnamn_lan(unique(df$region), TRUE))[1]

    if(length(unique(df$region)) > 1){
      reg_txt <- paste0(reg_txt,"_facet")
      diagram_titel <- paste0("Födelsenetto")

    }else{
      diagram_titel <- paste0("Födelsenetto i ", reg_txt)
    }

    diagramfil <- paste0("fodelsenetto_", reg_txt,".png")
    objektnamn <- c(objektnamn, stringr::str_remove(diagramfil, ".png"))

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = dplyr::mutate(
        dplyr::filter(df, variabel != "netto"),
        varde = ifelse(variabel=="Döda",varde*-1,varde),
        variabel = stringr::str_to_title(variabel)
      ),
      skickad_x_var = "år",
      skickad_y_var = "varde",
      skickad_x_grupp = "variabel",
      diagram_titel = diagram_titel,
      diagram_capt = diagram_capt,
      x_axis_visa_var_xe_etikett = ifelse(length(unique(df$region)) > 1,12,etiketter_xaxel),
      stodlinjer_avrunda_fem = stodlinjer_avrunda_fem,
      x_axis_var_xe_etikett_ta_bort_nast_sista_vardet = TRUE,
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      manual_y_axis_title = "",
      geom_position_stack = TRUE,
      facet_grp = if (length(unique(df$region)) > 1) "region" else NULL,
      facet_scale = "free",
      facet_legend_bottom = TRUE,
      fokusera_varden = total_list,
      legend_vand_ordning = TRUE,
      manual_color = diag_fargvekt,
      output_mapp = utmapp,
      skriv_till_diagramfil = FALSE,
      filnamn_diagram = diagramfil
    )

    dia_med_legend <- gg_obj +
      ggplot2::geom_line(ggplot2::aes(color="line"))+
      ggplot2::scale_color_manual(name = "", values = c("line" = "black"), labels = "Födelsenetto")+
      ggplot2::theme(legend.key = ggplot2::element_rect(fill = "white"),
            legend.box.just = "bottom")

    if(skriv_diagramfil == TRUE){
      ggplot2::ggsave(paste0(utmapp, diagramfil), dia_med_legend, width = 12, height = 7, dpi = 300)
    }

    gg_list <- c(gg_list, list(dia_med_legend))
    names(gg_list) <- objektnamn

    return(gg_list)
  }

  if (diag_facet) {
    diag <- skapa_diagram(df,region_vekt)

  } else {
    diag <- purrr::flatten(purrr::map(region_vekt, ~ skapa_diagram(df, .x)))

  }

  if(returnera_figur == TRUE){
    return(diag)
  }
}
