#' Diagram: inrikes flyttnetto uppdelat på bakgrund
#'
#' Inrikes flyttnetto (flyttningsnetto till/från andra kommuner/regioner),
#' uppdelat på född i Sverige/utrikes född. Ett diagram per region, eller
#' ett gemensamt facet-diagram.
#'
#' Ingen åldersuppdelning eller relativt flyttnetto - källrepot
#' (diagram_flytt_inrikes_bakgrund_SCB.R) har ett syskonskript
#' (diag_inr_flyttnetto_inr_utr_fodda_scb.R) med samma funktionsnamn i
#' källan men mer funktionalitet (åldersgrupper, relativt flyttnetto) -
#' inte migrerat än. Döpt om här för att undvika namnkrock den dagen det
#' migreras.
#'
#' @param region_vekt Region(er). `"00"` (Riket) stöds inte - riket saknar
#'   inrikes flyttnetto.
#' @param gruppera_namn Namn att gruppera ihop de medskickade regionerna
#'   till en gemensam rad under, eller `NA` (standard) för ingen gruppering.
#' @param facet_diagram Om `TRUE` skapas ett gemensamt facet-diagram för
#'   alla regioner, annars (standard) ett diagram per region.
#' @param diag_fargvekt Färgvektor för diagrammet.
#' @param diagram_capt Diagrammets källtext.
#' @param utmapp Mapp där diagram (och ev. excelfil) sparas.
#' @param skriv_diagramfil TRUE om diagram ska skrivas till fil.
#' @param skriv_till_excelfil TRUE om data ska skrivas till en excelfil i
#'   `utmapp`.
#' @param returnera_data Skall data returneras till global environment.
#' @param filformat Bildformat på diagrammet, t.ex. `"png"` (standard).
#' @param visa_totalvarden Visa ett streck för totalt netto (inrikes +
#'   utrikes födda tillsammans).
#' @param fixa_y_axel_varden_jamna_tal TRUE för jämnare värden på y-axeln.
#'   Fungerar inte alltid - stäng av vid behov.
#' @param demo TRUE om man bara vill se ett exempel på diagrammet i
#'   webbläsaren och inget annat.
#' @param visa_totalvarden_dataetiketter Visa dataetiketter för
#'   totalvärdena.
#' @param totalvarden_dataetiketter_farg Färg på totalstrecken.
#' @param totalvarden_dataetiketter_hjust Justerar dataetiketter för
#'   totalvärden i höjdled.
#' @param totalvarden_dataetiketter_textstorlek Textstorlek för
#'   dataetiketter för totalvärden.
#' @param totalvarden_linjetjocklek Tjocklek på totalstrecken, i tiondels
#'   procent av hela differensen i datasetet.
#'
#' @return En namngiven lista med `ggplot`-objekt.
#' @export
diag_inr_flyttnetto_bakgrund_scb <- function(
    region_vekt = "20",
    gruppera_namn = NA,
    facet_diagram = FALSE,
    diag_fargvekt = rddiagram::diagramfarger("rd_gron")[c(1, 4)],
    diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nInrikes flyttnetto är skillnaden mellan de som flyttat in till och de som flyttat ut från en kommun/region, från och till andra kommuner/regioner",
    utmapp = rdverktyg::utskriftsmapp(),
    skriv_diagramfil = TRUE,
    skriv_till_excelfil = FALSE,
    returnera_data = FALSE,
    filformat = "png",
    visa_totalvarden = TRUE,
    fixa_y_axel_varden_jamna_tal = TRUE,
    demo = FALSE,
    visa_totalvarden_dataetiketter = FALSE,
    totalvarden_dataetiketter_farg = "black",
    totalvarden_dataetiketter_hjust = 20,
    totalvarden_dataetiketter_textstorlek = 2,
    totalvarden_linjetjocklek = 4
) {

  # ========================================== Allmän info ============================================
  # Diagram för inrikes flyttnetto uppdelat på bakgrund (inrikes och utrikes födda). Går att skapa
  # diagram för flera regioner samtidigt eller som ett facet-diagram.
  # Förbättringsmöjligheter: Går för tillfället inte att summera flera regioner.
  # ========================================== Inställningar ============================================

  # om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
  if (demo) {
    demo_url <- "https://region-dalarna.github.io/utskrivna_diagram/Flyttnetto_bakgrund_Dalarna.png"
    browseURL(demo_url)
    rdverktyg::stop_tyst()
  }

  if ("00" %in% region_vekt) {
    stop("Region 00 (Riket) saknar inrikes flyttnetto och kan inte användas.\nÄndra region_vekt")
  }

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil || skriv_till_excelfil)

  gg_list <- list()

  if (is.na(diag_fargvekt[1])) diag_fargvekt <- rddiagram::diagramfarger("rd_gron")

  # =====================================================================================================
  # Hämtas via SCB:s PxWeb-API v2 med pxweb2r, från två tabeller (SCB bytte metod - CKM,
  # röjandekontroll - för nya årgångar, med en ny tabell från och med 2025):
  # - TAB4693 = gamla "FlyttFodReg", 2002-2024. Ålder kan utelämnas (elimineringsbar), ger då totalen
  #   automatiskt.
  # - TAB6657 = nya CKM-tabellen, från 2025. Ålder är INTE elimineringsbar där längre - måste anges
  #   explicit som totalkoden "TotSA", vilket också gör den till en egen kolumn i svaret; select()
  #   plockar bort den igen så tabellernas kolumner stämmer överens innan de binds ihop.
  # Födelseregion (född i Sverige/utrikes född) och kön (utelämnas - CKM gör att en egen summering av
  # delarna kan avvika från den riktiga totalen) hanteras likadant i båda tabellerna.
  hamta_flytt_bakgrund <- function(table_id, alder) {
    pxweb2r::pxweb2_get_data(
      table_id,
      query = list(Region = region_vekt, Kon = NA, Alder = alder,
                   Fodelseregion = c("född i Sverige", "utrikes född"),
                   ContentsCode = c("Flyttningsnetto, eget län", "Flyttningsnetto, övriga län"),
                   Tid = "*"),
      on_all_values_invalid = "null",
    quiet = TRUE)
  }

  flytt_hist <- hamta_flytt_bakgrund("TAB4693", NA)
  flytt_ckm  <- hamta_flytt_bakgrund("TAB6657", "TotSA")
  if (!is.null(flytt_ckm)) flytt_ckm <- dplyr::select(flytt_ckm, -dplyr::any_of("ålder"))

  har_ckm_data <- !is.null(flytt_ckm) && nrow(flytt_ckm) > 0
  ckm_fran_ar <- if (har_ckm_data) min(as.integer(flytt_ckm$år)) else NULL
  diagram_capt <- rddiagram::lagg_till_ckm_notering(diagram_capt, har_ckm_data, fran_ar = ckm_fran_ar)

  # Lägg ihop till px_df: döp om till regionkod/varde, gör om län till kortnamn (riket -> Sverige).
  # Flyttningsnetto totalt (eget län + övriga län) fås genom att summera "varde" utan att gruppera på
  # tabellinnehåll - samma resultat som att summera två breda kolumner, men enklare i long-format.
  px_df <- dplyr::bind_rows(flytt_hist, flytt_ckm) |>
    dplyr::rename(regionkod = region_kod, varde = value) |>
    dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region, byt_ut_riket_mot_sverige = TRUE))

  if (!is.na(gruppera_namn)) {
    px_df <- px_df |>
      dplyr::group_by(år, födelseregion) |>
      dplyr::summarise(Inrikes_flyttnetto = sum(varde, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(region = gruppera_namn)

  } else {
    px_df <- px_df |>
      dplyr::group_by(år, regionkod, region, födelseregion) |>
      dplyr::summarise(Inrikes_flyttnetto = sum(varde, na.rm = TRUE), .groups = "drop")
  }

  if (skriv_till_excelfil) {
    reg_namn <- ifelse(!is.na(gruppera_namn), gruppera_namn, paste0(region_vekt, collapse = "_"))
    openxlsx::write.xlsx(px_df, paste0(utmapp, "Flyttnetto_", reg_namn, "_ar", min(px_df$år), "_", max(px_df$år), ".xlsx"), overwrite = TRUE)
  }

  # Returnerar data till R globala miljö
  if (returnera_data == TRUE) {
    assign("flytt_bakgrund_df", px_df, envir = .GlobalEnv)
  }

  # ============================================= Skapa diagram ==============================================

  skapa_diagram <- function(df, vald_regionkod) {  # skapa en funktion som skapar diagram för varje region

    df <- dplyr::filter(df, regionkod %in% vald_regionkod)

    reg_txt <- (rdverktyg::skapa_kortnamn_lan(unique(df$region), byt_ut_riket_mot_sverige = TRUE))[1]

    if (length(unique(df$region)) > 1) {
      reg_txt <- paste0(reg_txt, "_facet")
      diagram_titel <- paste0("Inrikes flyttnetto")
    } else {
      diagram_titel <- paste0("Inrikes flyttnetto i ", reg_txt)
    }
    # =================================== visa totalvärden ========================================
    if (visa_totalvarden == TRUE && facet_diagram == FALSE) {

      diff <- max(df$Inrikes_flyttnetto) - min(df$Inrikes_flyttnetto) # ta reda på skillnaden mellan det högsta och lägsta värdet i datasetet
      totalvarden_linjebredd <- diff * (totalvarden_linjetjocklek / 1000)      # gör en linjetjocklek på totallinjerna som är 0,2 % av diff (på raden ovan)
      total_list <- list()
      unika_ar <- unique(df$år)
      unika_reg <- unique(vald_regionkod)

      for (reg in 1:length(unika_reg)) {
        for (ar in 1:length(unika_ar)) {
          arsvarde <- dplyr::pull(
            dplyr::select(
              dplyr::filter(df, år == unika_ar[ar], regionkod == unika_reg[reg]),
              Inrikes_flyttnetto
            )
          )
          arsvarde <- sum(arsvarde, na.rm = TRUE)
          total_list <- c(total_list, list(list(geom = "rect", ymin = arsvarde - totalvarden_linjebredd, ymax = arsvarde + totalvarden_linjebredd, xmin = ar - 0.45, xmax = ar + 0.45, alpha = 1, fill = "black")))
          if (visa_totalvarden_dataetiketter) {
            total_list <- c(total_list, list(list(geom = "text", y = arsvarde + totalvarden_dataetiketter_hjust, x = ar, size = totalvarden_dataetiketter_textstorlek, angle = 0, fontface = "plain", label = arsvarde, color = totalvarden_dataetiketter_farg)))
          } # slut if-sats om man vill vissa dataetiketter
        } # slut for-loop unika_ar
      } # slut for_loop unika_reg
    } else total_list <- NA # slut if-sats visa_totalvärden

    # ======================= skapa ggplot-objekt =================

    diagramfil <- paste0("Flyttnetto_bakgrund_", paste0(reg_txt, collapse = "_"), ".png")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = df,
      skickad_x_var = "år",
      skickad_y_var = "Inrikes_flyttnetto",
      skickad_x_grupp = "födelseregion",
      diagram_titel = diagram_titel,
      diagram_capt = diagram_capt,
      stodlinjer_avrunda_fem = fixa_y_axel_varden_jamna_tal,
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      manual_y_axis_title = "",
      geom_position_stack = TRUE,
      fokusera_varden = total_list,
      facet_grp = if (facet_diagram) "region" else NULL,
      skriv_till_diagramfil = skriv_diagramfil && !visa_totalvarden,
      manual_color = diag_fargvekt,
      diagram_bildformat = filformat,
      output_mapp = utmapp,
      filnamn_diagram = diagramfil
    )

    if (visa_totalvarden) {
      gg_obj <- gg_obj +
        ggplot2::geom_line(ggplot2::aes(color = "line")) +
        ggplot2::scale_color_manual(name = "", values = c("line" = "black"), labels = "inrikes flyttnetto totalt") +
        ggplot2::theme(legend.key = ggplot2::element_rect(fill = "white"),
              legend.box.just = "bottom")
    } # slut if-sats visa_totalvarden

    gg_list <- c(gg_list, list(gg_obj))

    # Diagrammet ovan skriver aldrig till fil på egen hand när visa_totalvarden = TRUE (eftersom
    # totalstrecket läggs på EFTER SkapaStapelDiagram(), så det måste sparas separat här för att
    # komma med på bilden).
    if (skriv_diagramfil && visa_totalvarden) {
      rddiagram::skriv_till_diagramfil(gg_obj,
                            output_mapp = utmapp,
                            filnamn_diagram = diagramfil)
    } # slut if-sats
    names(gg_list) <- stringr::str_remove(diagramfil, ".png")
    return(gg_list)
  } # slut skapa diagram-funktion för varje region

  if (facet_diagram) {
    diag <- skapa_diagram(px_df, region_vekt)

  } else {
    diag <- purrr::flatten(purrr::map(unique(region_vekt), ~ skapa_diagram(px_df, .x)))

  }

  return(diag)
}
