#' Diagram: förädlingsvärde över tid, tidsserie och jämförelse med riket
#'
#' Två diagram: en tidsserie med första och sista tillgängliga år för en
#' valfri FEK-variabel per bransch, och en indexerad linje som jämför
#' utvecklingen i valt län mot riket.
#'
#' @param region_vekt Val av region.
#' @param diagram_capt Diagrammets källtext.
#' @param utmapp Mapp där diagram sparas. Måste finnas om
#'   `skriv_diagramfil = TRUE`.
#' @param visa_dataetiketter TRUE om dataetiketter ska visas i diagrammet.
#' @param cont_klartext Innehållsvariabel, se
#'   [diag_fek_foradlingsvarde_bransch_lan_scb()] för giltiga värden.
#' @param diag_tidsserie Skapa diagram med tidsserie per bransch (första
#'   och sista tillgängliga år).
#' @param diag_jmfr_riket Skapa diagram med indexerad jämförelse mot riket.
#' @param visa_logga_i_diagram TRUE om logga ska visas i diagrammet.
#' @param logga_sokvag Sökväg till logga.
#' @param skriv_diagramfil TRUE om diagram ska skrivas till fil.
#' @param demo TRUE om man bara vill se ett exempel på diagrammen i
#'   webbläsaren och inget annat.
#'
#' @return En namngiven lista med `ggplot`-objekt.
#' @export
diag_fek_lve_tidsserie <- function(
    region_vekt = "20",
    diagram_capt = "Källa: Företagens ekonomi (FEK), SCB:s öppna statistikdatabas. Bearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Förädlingsvärde är den faktiska produktionen minus kostnader för köpta varor och tjänster, dock ej löner, sociala avgifter och kostnader för handelsvaror",
    utmapp = rdverktyg::utskriftsmapp(),
    visa_dataetiketter = FALSE,
    cont_klartext = "Förädlingsvärde, mnkr",
    diag_tidsserie = TRUE,
    diag_jmfr_riket = TRUE,
    visa_logga_i_diagram = TRUE,
    logga_sokvag = NA,
    skriv_diagramfil = TRUE,
    demo = FALSE
    ) {

  # om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
  if (demo) {
    demo_url <-
      c("https://region-dalarna.github.io/utskrivna_diagram/fek_Förädlingsvärde_Dalarna_ar2007_2022.png",
        "https://region-dalarna.github.io/utskrivna_diagram/fek_Förädlingsvärde_Dalarna_jmfr_riket_ar2007-2022.png")
    purrr::walk(demo_url, ~ browseURL(.x))
    if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
    rdverktyg::stop_tyst()
  }

  # ========================================== Inställningar ============================================

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  gg_list <- list()

  # Slår ihop två SCB-produkter: den äldre "Regional Basfakta" (finare SNI2007-indelning, år
  # 2007-2021, TAB3513) och den nyare NSEBasfaktaLVEngs07 (grövre bokstavsindelning, år 2022-,
  # TAB6329) - samma TAB6329 som i diag_fek_foradlingsvarde_bransch_lan_scb().
  fek_lve_df <- rdverktyg::suppress_specific_warning({
    historik <- pxweb2r::pxweb2_get_data(
      table = "TAB3513",
      query = list(
        Region = c("00", region_vekt),
        SNI2007 = "*",
        ContentsCode = cont_klartext,
        Tid = "*"
      ), quiet = TRUE) |>
      dplyr::rename(regionkod = region_kod, sni2007kod = `näringsgren sni 2007_kod`, variabel = tabellinnehåll, varde = value)

    ny <- pxweb2r::pxweb2_get_data(
      table = "TAB6329",
      query = list(
        Region = c("00", region_vekt),
        SNI2007 = "*",
        ContentsCode = cont_klartext,
        Tid = "*"
      ), quiet = TRUE) |>
      dplyr::rename(regionkod = region_kod, variabel = tabellinnehåll, varde = value)

    dplyr::bind_rows(historik, ny)
  })

  # ========================================== Läser in branschnyckel ============================================
  # Samma fasta, delade mallfil som i diag_fek_foradlingsvarde_bransch_lan_scb() - validerad en gång
  # här och läst en gång (originalet läste filen två gånger, en per nyckeltyp).
  branschnyckel_sokvag <- "g:/skript/nycklar/Bransch_FEK.xlsx"
  rdverktyg::sakerstall_indatafil(branschnyckel_sokvag)
  branschnyckel_fil <- readxl::read_xlsx(branschnyckel_sokvag)

  # !is.na(Kod)/!is.na(Avdelning) filtreras bort explicit innan join: dplyr::left_join() matchar annars
  # NA mot NA, vilket ger en many-to-many-relation (och därmed uppblåsta, felaktiga summor) så fort
  # nyckelfilen har fler än en rad utan Kod (typiskt raderna som bara har Avdelning ifyllt, och vice
  # versa).
  branschnyckel <- branschnyckel_fil |>
    dplyr::select(Kod, Grupp_kod, Branschgrupp) |>
    dplyr::filter(!is.na(Kod)) |>
    dplyr::distinct()

  bransch_bokstav <- branschnyckel_fil |>
    dplyr::select(Avdelning, Grupp_kod, Branschgrupp) |>
    dplyr::filter(!is.na(Avdelning)) |>
    dplyr::distinct()

  vald_ar <- c(min(fek_lve_df$år), max(fek_lve_df$år))

  if (diag_tidsserie) {
    tidsserie_df <- fek_lve_df |>
      # sni2007kod finns bara för den äldre tabellen (TAB3513 exponerar råa SNI-koder, TAB6329 gör
      # det inte) - filtrerar bort totalraden via klartexten i stället, som finns för båda tabellerna
      # (samma lösning som i diag_fek_foradlingsvarde_bransch_lan_scb()).
      dplyr::filter(!stringr::str_detect(`näringsgren SNI 2007`, "samtliga näringsgrenar"),
             regionkod %in% region_vekt,
             år %in% vald_ar) |>
      dplyr::mutate(bransch_bokstav = stringr::str_sub(`näringsgren SNI 2007`, 1, 1)) |>
      dplyr::left_join(branschnyckel, by = c("sni2007kod" = "Kod")) |>
      dplyr::left_join(dplyr::rename(bransch_bokstav, gk = Grupp_kod, bg = Branschgrupp), by = c("bransch_bokstav" = "Avdelning")) |>
      dplyr::mutate(Grupp_kod = ifelse(is.na(Grupp_kod), gk, Grupp_kod),
             Branschgrupp = ifelse(is.na(Branschgrupp), bg, Branschgrupp)) |>
      dplyr::group_by(år, regionkod, region, Branschgrupp, variabel) |>
      dplyr::summarise(varde = sum(varde, na.rm = TRUE), .groups = "drop")

    region_txt <- rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(unique(tidsserie_df$region)))
    cont_txt <- stringr::str_extract(cont_klartext, "^[^,]*")

    diagramtitel <- glue::glue("{cont_txt} i {region_txt}")
    diagramfil <- glue::glue("fek_{cont_txt}_{region_txt}_ar{min(fek_lve_df$år)}_{max(fek_lve_df$år)}.png")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = tidsserie_df,
      skickad_x_var = "Branschgrupp",
      skickad_y_var = "varde",
      skickad_x_grupp = "år",
      x_axis_sort_value = TRUE,
      diagram_titel = diagramtitel,
      diagram_capt = diagram_capt,
      stodlinjer_avrunda_fem = TRUE,
      filnamn_diagram = diagramfil,
      dataetiketter = visa_dataetiketter,
      manual_y_axis_title = cont_klartext,
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      manual_color = rddiagram::diagramfarger("rus_sex"),
      output_mapp = utmapp,
      lagg_pa_logga = visa_logga_i_diagram,
      logga_path = logga_sokvag,
      skriv_till_diagramfil = skriv_diagramfil
    )

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfil, "\\.png")
  } # slut if-sats om diag_tidsserie

  if (diag_jmfr_riket) {
    jmfr_riket_df <- fek_lve_df |>
      dplyr::filter(!stringr::str_detect(`näringsgren SNI 2007`, "samtliga näringsgrenar")) |>
      dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region)) |>
      dplyr::group_by(år, regionkod, region, variabel) |>
      dplyr::summarise(varde = sum(varde, na.rm = TRUE), .groups = "drop")

    region_txt <- rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(unique(dplyr::pull(dplyr::filter(jmfr_riket_df, regionkod != "00"), region))))
    cont_txt <- stringr::str_extract(cont_klartext, "^[^,]*")

    diagramtitel <- glue::glue("Förändring av {tolower(cont_txt)} i {region_txt} jämfört med riket")
    diagramfil <- glue::glue("fek_{cont_txt}_{region_txt}_jmfr_riket_ar{min(fek_lve_df$år)}-{max(fek_lve_df$år)}.png")

    gg_obj <- rddiagram::SkapaLinjeDiagram(
      skickad_df = dplyr::rename(jmfr_riket_df, !!rlang::sym(cont_klartext) := varde),
                                 skickad_x_var = "år",
                                 skickad_y_var = cont_klartext,
                                 skickad_x_grupp = "region",
                                 diagram_titel = diagramtitel,
                                 diagram_capt = diagram_capt,
                                 stodlinjer_avrunda_fem = TRUE,
                                 filnamn_diagram = diagramfil,
                                 berakna_index = TRUE,
                                 manual_color = rddiagram::diagramfarger("rus_sex"),
                                 output_mapp = utmapp,
                                 lagg_pa_logga = visa_logga_i_diagram,
                                 logga_path = logga_sokvag,
                                 skriv_till_diagramfil = skriv_diagramfil
    )

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfil, "\\.png")
  } # slut if-sats om diag_jmfr_riket

  return(gg_list)
}
