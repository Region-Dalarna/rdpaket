#' Diagram: summerad fruktsamhet, riket jämfört med Dalarnas kommuner
#'
#' Stapeldiagram över summerad fruktsamhet (antal födda barn per kvinna) för
#' riket, länet och länets kommuner, med reproduktionsnivån (2,1) markerad
#' som en referenslinje.
#'
#' \if{html}{\figure{diag_fodelsetal_summerad_fruktsamhet_jmfr_riket_lan_kommuner_scb.png}{options: width="100%" alt="Exempel: diag_fodelsetal_summerad_fruktsamhet_jmfr_riket_lan_kommuner_scb() med standardinställningar"}}
#'
#' @param region_vekt Regioner att visa. `"00"` eller NA ger alla län.
#' @param utmapp Mapp där diagram ska sparas. Måste finnas om `skriv_diagramfil = TRUE`.
#' @param diag_fargvekt Valfri färgvektor, standard: `rddiagram::diagramfarger("rus_sex")`.
#' @param visa_dataetiketter Visa dataetiketter.
#' @param visa_var_xte_etikett Visa var x:te etikett på x-axeln.
#' @param ta_bort_nast_sista_etikett Ta bort näst sista etiketten på x-axeln.
#' @param skriv_diagramfil TRUE om diagram ska skrivas till fil.
#'
#' @return En namngiven lista med ett `ggplot`-objekt.
#' @export
diag_fodelsetal_summerad_fruktsamhet_jmfr_riket_lan_kommuner_scb <- function(
    region_vekt = "20",
    utmapp = rdverktyg::utskriftsmapp(),
    diag_fargvekt = NA,
    visa_dataetiketter = FALSE,
    visa_var_xte_etikett = NA,
    ta_bort_nast_sista_etikett = FALSE,
    skriv_diagramfil = TRUE
  ){

  if (all(is.na(diag_fargvekt))) {
    diag_fargvekt <- rddiagram::diagramfarger("rus_sex")
  }

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  # ============================ Dalarna, riket och länets kommuner ============================

  if (all(region_vekt == "00" | is.na(region_vekt))) {
    hamta_regioner <- rdverktyg::hamtaAllaLan()
  } else {
    lan_koder <- region_vekt[nchar(region_vekt) == 2]
    kommun_koder <- region_vekt[nchar(region_vekt) == 4]

    hamta_regioner <- unique(c(
      rdverktyg::hamtakommuner(unique(stringr::str_sub(lan_koder, 1, 2)), TRUE, TRUE),
      kommun_koder
    ))
  }

  # hamta_data-repots hamta_summerad_fruktsamhet_fodelsetal_scb.R (v1:
  # BE/BE0101/BE0101H/FruktsamhetSum) hämtas här direkt via
  # v2-motsvarigheten TAB4805. Ingen CKM-tabell att slå ihop med -
  # tabellen täcker hela 2000-2025 utan uppdelning.
  fodelsetal_df <- pxweb2r::pxweb2_get_data(
    table = "TAB4805",
    query = list(
      Region = hamta_regioner,
      Kon = "kvinnor",
      ContentsCode = "*",
      Tid = "*"
    ), quiet = TRUE) |>
    dplyr::rename(regionkod = region_kod, Antal = value) |>
    dplyr::select(-tabellinnehåll)

  if (all(nchar(hamta_regioner) == 2)) {

    fodelsetal_df <- fodelsetal_df |>
      dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region, TRUE),
             fokus = dplyr::case_when(
               regionkod != "00" ~ "Län",
               regionkod == "00" ~ "Riket",
               TRUE ~ "Övriga"))

  } else {
    fodelsetal_df <- fodelsetal_df |>
      dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region, TRUE),
             fokus = dplyr::case_when(
               regionkod == "00" ~ "Riket",
               nchar(regionkod) == 2 | regionkod == "0980" ~ "Län",
               nchar(regionkod) == 4 ~ "Kommuner",
               TRUE ~ "Övriga"))
  }

  diagram_capt <- "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"


  diagramtitel <- glue::glue("Antal födda barn per kvinna år {min(fodelsetal_df$år)}-{max(fodelsetal_df$år)}")
  diagramfil <- stringr::str_replace_all(glue::glue("sum_fruktsamhet_riket_dalarna_plus_kommuner_{min(fodelsetal_df$år)}_{max(fodelsetal_df$år)}.png"), "__", "_")


  gg_obj <- rddiagram::SkapaStapelDiagram(
    skickad_df = fodelsetal_df,
    skickad_x_var = "år",
    skickad_y_var = "Antal",
    x_var_fokus = "fokus",
    diagram_titel = diagramtitel,
    diagram_capt = diagram_capt,
    stodlinjer_avrunda_fem = TRUE,
    filnamn_diagram = diagramfil,
    dataetiketter = visa_dataetiketter,
    x_axis_visa_var_xe_etikett = visa_var_xte_etikett,
    x_axis_var_xe_etikett_ta_bort_nast_sista_vardet = ta_bort_nast_sista_etikett,
    manual_y_axis_title = "Summerad fruktsamhet (antal födda barn per kvinna)",
    manual_x_axis_text_vjust = 1,
    manual_x_axis_text_hjust = 1,
    manual_color = rddiagram::diagramfarger("rus_sex")[c(3, 2, 1)],
    fokusera_varden = list(list(geom = "rect", ymin=2.097, ymax=2.103, xmin=0, xmax=Inf, alpha=1, fill="black")),
    output_mapp = utmapp,
    skriv_till_diagramfil = FALSE,
    facet_grp = "region",
    facet_scale = "fixed",
    facet_x_axis_storlek = 5,
    facet_legend_bottom = TRUE
  )

  # Skapa en legend för att visa reproduktionsnivån
  dia_med_legend <- gg_obj +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 2.1, color = "Reproduktionsnivån (2,1 barn per kvinna)"), linewidth = 0.8) +
    ggplot2::scale_color_manual(
      name = "",  # eller "Förklaring" om du vill ha rubrik
      values = c("Reproduktionsnivån (2,1 barn per kvinna)" = "black")
    ) +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(linetype = "solid", linewidth = 0.8))) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.key = ggplot2::element_rect(fill = "white"),
      legend.margin = ggplot2::margin(t = 10)
    ) + ggplot2::guides(fill = "none")

  gg_list <- list(dia_med_legend)
  names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfil, "\\.[^.]+$")

  if (skriv_diagramfil) {
  suppressMessages(
  rddiagram::skriv_till_diagramfil(dia_med_legend,
                        output_mapp = utmapp,
                        filnamn_diagram = diagramfil)
  )}
  return(gg_list)
}
