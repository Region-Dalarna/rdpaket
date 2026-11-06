#' Diagram: störst yrken (SSYK3) per geografi
#'
#' Skriv ut de x antal största yrkena SSYK3 (default är 15 största yrken)
#' för valfritt län eller kommun.
#'
#' @param region_vekt Går att skicka med flera. Ett diagram per geografi skrivs ut om man inte skickar med ett värde för `gruppera_namn`, då grupperas de ihop till en geografi.
#' @param gruppera_namn Default är `NA` och då grupperas inget. Skickas namn med så grupperas alla geografier i `region_vekt` ihop till en geografi som döps till värdet för `gruppera_namn`.
#' @param tid_koder Det år man vill få diagram över x antal största yrken för. `"9999"` = senaste år. Skickas flera år med så skrivs ett diagram per år ut.
#' @param konsuppdelat TRUE om man vill ha x största yrken för varje kön. Vid könsuppdelat skrivs ett facetdiagram ut med ett diagram per kön. Då används olika färger per kön också och varje facetdiagram sorteras med störsa yrket överst.
#' @param antal_yrken Default är 15. Hur många yrken som tas med i diagrammet.
#' @param diag_fargvekt Om man vill ha annan färg än default som är Region Dalarnas blå från grafiska profilen.
#' @param manual_color_kon Om man vill ha andra färger för kvinnor och män än standardfärgerna för könsuppdelad statistik.
#' @param kortnamn_lan TRUE så tas "län" bort ur länsnamn, annars inte.
#' @param utmapp Sökväg till mapp där diagrammen skrivs ut. Måste finnas om `skriv_diagramfil = TRUE`.
#' @param returnera_data Returnera datasetet till global environment, bl.a. bra när man skapar Rmarkdown-rapporter.
#' @param ta_bort_diagramtitel FALSE så skrivs ingen diagramtitel ut.
#' @param visa_dataetiketter Visa dataetiketter i diagrammet.
#' @param facet_ovanpa_varandra Lägg facets ovanpå varandra i stället för bredvid varandra.
#' @param storre_text Större text, passar bättre i markdownrapporter.
#' @param visa_logga_i_diagram TRUE om logga ska visas i diagrammet.
#' @param logga_sokvag Sökväg till logga.
#' @param diagram_capt Diagrambeskrivning.
#' @param skriv_diagramfil Om man vill skriva ut diagramfiler, annars returneras endast en lista med ggplot-objekt.
#'
#' @return En namngiven lista med `ggplot`-objekt.
#' @export
diag_storsta_yrke_per_geografi_scb <- function(
                              region_vekt = "20",
                              gruppera_namn = NA,
                              tid_koder = "9999",
                              konsuppdelat = TRUE,
                              antal_yrken = 15,
                              diag_fargvekt = NA,
                              manual_color_kon = NA,
                              kortnamn_lan = TRUE,
                              utmapp = rdverktyg::utskriftsmapp(),
                              returnera_data = FALSE,
                              ta_bort_diagramtitel = FALSE,
                              visa_dataetiketter = FALSE,
                              facet_ovanpa_varandra = FALSE,
                              storre_text = FALSE,
                              visa_logga_i_diagram = TRUE,
                              logga_sokvag = NA,
                              diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
                              skriv_diagramfil = TRUE
                                            ) {

  # ============================================================================================================
  #
  # Skriv ut de x antal största yrkena SSYK3 (default är 15 största yrken) för valfritt län eller kommun.
  #
  # Migrerad till pxweb2r/rddiagram/rdverktyg. hamta_data-repots hamta_yrke_region_ssyk3_kon_tid_scb() slog
  # ihop tre v1-tabeller. Alla tre finns kvar i SCB:s v1-API men motsvaras nu av tre v2-tabeller:
  #   AM0208D/YREG58     (2014-2018)                    -> TAB4396
  #   AM0208D/YREG58N    (RAMS, ny tidsserie 2019-2021) -> TAB3119
  #   AM0208D/YREG58BAS  (BAS 2020-)                     -> TAB4434
  # Används bara av det här skriptet - logiken läggs därför in direkt här i stället för i rdverktyg.
  # ============================================================================================================

  options(dplyr.summarise.inform = FALSE)

  # om ingen färgvektor är medskickad, använd rddiagram::diagramfarger()
  if (all(is.na(diag_fargvekt))) diag_fargvekt <- rddiagram::diagramfarger("rus_sex")
  if (all(is.na(manual_color_kon))) manual_color_kon <- rddiagram::diagramfarger("kon")

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  # ================== hämta data ===================================

  # "9999" måste lösas upp mot det SANNA senaste året över alla tre tabeller i ett svep - annars tolkas
  # "senaste år" olika per tabell (t.ex. 2018 för den äldsta tabellen och 2024 för BAS-tabellen), och man
  # får då både 2018 och 2024 i utdata i stället för bara det verkliga senaste året.
  alla_tabeller <- c("TAB4396", "TAB3119", "TAB4434")
  alla_giltiga_ar <- unlist(purrr::map(alla_tabeller, ~ pxweb2r::pxweb2_get_values(.x, "Tid", quiet = TRUE)$code))
  tid_koder <- if (identical(tid_koder, "9999")) max(alla_giltiga_ar) else tid_koder

  hamta_en_yrkestabell <- function(tabell_id, region_vekt, kon_klartext, tid_koder) {

    giltiga_ar <- pxweb2r::pxweb2_get_values(tabell_id, "Tid", quiet = TRUE)$code
    tid_vekt <- if (identical(tid_koder, "*")) giltiga_ar else as.character(tid_koder)[as.character(tid_koder) %in% giltiga_ar]

    # RAMS ny tidsserie (2019-2021) och BAS (2020-) delar åren 2020-2021 - dessa tas alltid bort ur
    # RAMS-tabellen för att inte räknas dubbelt (samma hantering som i originalskriptet).
    if (tabell_id == "TAB3119") tid_vekt <- tid_vekt[!tid_vekt %in% c("2020", "2021")]
    if (length(tid_vekt) == 0) return(NULL)

    px <- pxweb2r::pxweb2_get_data(
      table = tabell_id,
      query = list(
        Region = region_vekt,
        Yrke2012 = "*",
        Kon = if (all(is.na(kon_klartext))) NA else kon_klartext,
        ContentsCode = "*",
        Tid = tid_vekt
      ),
      on_all_values_invalid = "null", quiet = TRUE)
    if (is.null(px)) return(NULL)

    dplyr::rename(px, regionkod = region_kod, yrkeskod = `yrke (ssyk 2012)_kod`,
                  yrke = `Yrke (SSYK 2012)`, Antal = value) |>
      dplyr::select(-tabellinnehåll)
  }

  px_df <- purrr::map(
    alla_tabeller,
    ~ hamta_en_yrkestabell(.x, region_vekt, if (konsuppdelat) c("män", "kvinnor") else NA, tid_koder)
  ) |>
    purrr::list_rbind()

  if (kortnamn_lan) px_df <- dplyr::mutate(px_df, region = rdverktyg::skapa_kortnamn_lan(region))

  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)

  # om man skickat med gruppera_namn så grupperas alla geografier ihop till en geografi
  if (!is.na(gruppera_namn)) {
    px_df <- px_df |>
      dplyr::group_by(yrkeskod, yrke, kön, år) |>
      dplyr::summarise(Antal = sum(Antal, na.rm = TRUE)) |>
      dplyr::ungroup() |>
      dplyr::mutate(regionkod = "XXXX",
             region = gruppera_namn)
  }

  # bearbeta df:n beroende på om den ska vara könsuppdelad eller inte
  if (konsuppdelat) {
      kon_var <- unique(px_df$kön)             # vektor med båda könen

      # skapa en lista med två vektorer som innehåller yrkeskoder för de antal_yrken största yrkena per kön
      storsta_yrke <- purrr::map(kon_var, ~ px_df |>
                            dplyr::filter(kön == .x,
                                   yrkeskod != "0002") |>
                            dplyr::arrange(dplyr::desc(Antal)) |>
                            dplyr::slice(1:antal_yrken) |>
                            dplyr::pull(yrkeskod))

      # filtrera ut de största yrkena per kön och lägg i en df
      chart_df <- purrr::map2(storsta_yrke, kon_var, ~ dplyr::filter(px_df, yrkeskod %in% .x, kön == .y)) |>
        purrr::list_rbind()

  } else {
    chart_df <- px_df |>
      dplyr::filter(yrkeskod != "0002") |>
      dplyr::group_by(år, regionkod, region, yrkeskod, yrke) |>
      dplyr::summarise(Antal = sum(Antal, na.rm = TRUE)) |>
      dplyr::ungroup() |>
      dplyr::arrange(dplyr::desc(Antal)) |>
      dplyr::slice(1:antal_yrken)
  }

  # returnera datasetet till global environment, bl.a. bra när man skapar Rmarkdown-rapporter
  if(returnera_data == TRUE){
    assign("storsta_yrken_per_geografi_df", chart_df, envir = .GlobalEnv)
  }

  # ========================= Skapa själva diagrammen ==============================================================

  # skriv ut varje geografi för sig (om vi inte grupperat ihop dem ovan, men då hanteras de som en och samma här)
  # och varje år för sig

  for (skickat_ar in unique(chart_df$år)) {
    for (geo_namn in unique(chart_df$region)){

      diagramtitel <- paste0("Störst yrken i ", geo_namn, " år ", skickat_ar)
      filnamn <- paste0(antal_yrken, "_storsta_yrken_", geo_namn, ifelse(konsuppdelat, "_kon", ""), "_", skickat_ar,".png")

      gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = dplyr::filter(chart_df, år == skickat_ar, region == geo_namn),
                        skickad_x_var = "yrke",
                        skickad_y_var = "Antal",
                        diagram_titel = if (ta_bort_diagramtitel) NULL else diagramtitel,
                        diagram_capt = diagram_capt,
                        output_mapp = utmapp,
                        x_var_fokus = if (konsuppdelat) "kön" else NA,         # vi använder fokusvariabel endast i könsuppdelade diagram (för att få grönt för män och gul för kvinnor)
                        x_axis_sort_value = TRUE,
                        x_axis_lutning = 0,
                        manual_x_axis_title = "",
                        manual_y_axis_title = "Antal sysselsatta",
                        filnamn_diagram = filnamn,
                        manual_color = if (konsuppdelat) manual_color_kon else diag_fargvekt,         # grön/gul för könsuppdelat, blå när det inte är det (om man inte väljer andra färgskalor som parameter i funktionen)
                        stodlinjer_avrunda_fem = TRUE,
                        diagram_liggande = TRUE,
                        lagg_pa_logga = visa_logga_i_diagram,
                        logga_path = logga_sokvag,
                        dataetiketter = visa_dataetiketter,
                        facet_grp = if (konsuppdelat) "kön" else NULL,                          # kör facet, ett för varje kön om könsuppdelat, inte könsuppdelalt annars
                        facet_sort = TRUE,
                        diagramfil_hojd = ifelse(storre_text, 11, 7),
                        facet_x_axis_storlek = ifelse(storre_text, 10.5, 8),
                        facet_y_axis_storlek = ifelse(storre_text, 10.5, 8),
                        facet_kolumner = if (facet_ovanpa_varandra) 1 else NULL,
                        facet_scale = "free_y",                                   # konstanthåller skala för antal i yrket men låter vilka yrken som är med vara "free", så att det kan bli två olika uppsättningar av yrken
                        skriv_till_diagramfil = skriv_diagramfil
                        )

      gg_list <- c(gg_list, list(gg_obj))
      names(gg_list)[length(gg_list)] <- stringr::str_remove(filnamn, ".png")

    } # slut for-loop för alla geografier som har skickats med
  } # slut for-loop för alla år som har skickats med

  # sist av allt returnerar vi en lista med diagram
  return(gg_list)

} # slut funktion
