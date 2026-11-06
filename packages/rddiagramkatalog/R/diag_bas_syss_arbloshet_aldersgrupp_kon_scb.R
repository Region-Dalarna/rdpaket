#' Diagram: arbetslöshet eller sysselsättningsgrad per åldersgrupp och kön (BAS)
#'
#' Ett diagram per kombination av region och innehållsvariabel
#' (arbetslöshet/sysselsättningsgrad), uppdelat på åldersgrupp, kön och
#' födelseregion.
#'
#' @param region_vekt Region.
#' @param vald_alder Åldersgrupper att inkludera.
#' @param vald_cont Innehållsvariabel: `"arbetslöshet"` och/eller `"sysselsättningsgrad"`.
#' @param skriv_diagramfil TRUE om diagram ska skrivas till fil.
#' @param diagram_capt Diagrammets källtext.
#' @param visa_logga_i_diagram TRUE om logga ska visas i diagrammet.
#' @param logga_sokvag Sökväg till logga.
#' @param diag_fargvekt Valfri färgvektor, standard: `rddiagram::diagramfarger("kon")`.
#' @param tid_koder Vilka tidsperioder som ska hämtas. `"*"` = alla, `"9999"` = senaste,
#'   eller `"2021M01:9999"` för "från januari 2021 till och med senaste tillgängliga månad".
#'   Diagrammet visar alltid bara den senaste hämtade perioden, så detta styr
#'   bara hur mycket historik som hämtas, inte vad som visas.
#' @param demo Sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren.
#' @param utmapp Mapp där diagrammet sparas. Måste finnas om `skriv_diagramfil = TRUE`.
#'
#' @return En lista med ett `ggplot`-objekt per kombination av region och innehållsvariabel.
#' @export
diag_bas_syss_arbloshet_aldersgrupp_kon_scb <- function(
    region_vekt = "20",
    vald_alder = c("16-19 år", "20-24 år", "25-29 år", "30-34 år", "35-39 år", "40-44 år",
                   "45-49 år", "50-54 år", "55-59 år", "60-64 år", "65-69 år", "70-74 år"),
    vald_cont = c("arbetslöshet", "sysselsättningsgrad"),
    skriv_diagramfil = TRUE,
    diagram_capt = "Källa: Befolkningens arbetsmarknadsstatus (BAS), SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
    visa_logga_i_diagram = FALSE,
    logga_sokvag = NA,
    diag_fargvekt = NA,
    tid_koder = "*",
    demo = FALSE,
    utmapp = rdverktyg::utskriftsmapp()
    ) {

  if (demo) {
    demo_url <-
      c("https://region-dalarna.github.io/utskrivna_diagram/Inrikes_flyttnetto_alder_Dalarna.png",
        "https://region-dalarna.github.io/utskrivna_diagram/Inrikes_flyttnetto_alder_20-29_%C3%A5r_Dalarna.png")
    purrr::walk(demo_url, ~browseURL(.x))
    if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
    rdverktyg::stop_tyst()
  }

  if (all(is.na(diag_fargvekt))) {
    diag_fargvekt <- rddiagram::diagramfarger("kon")
  }

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  # SCB skriver åldersintervallen med tankstreck ("16–19 år", U+2013), inte
  # vanligt bindestreck. Normaliserar bindestreck mellan siffror till
  # tankstreck här, samma fix som i systerskripten för denna tabell.
  vald_alder_fetch <- gsub("(?<=[0-9])-(?=[0-9])", "–", vald_alder, perl = TRUE)

  # hämta data från SCB via API. Länk till tabell:
  # https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0210__AM0210A/ArbStatusM/
  syss_df <- pxweb2r::pxweb2_get_data(
    table = "TAB6260",
    query = list(
      Region = region_vekt,
      Kon = c("Män", "Kvinnor"),
      Alder = vald_alder_fetch,
      Fodelseregion = "*",
      ContentsCode = vald_cont,
      Tid = intern_tolka_tidskoder("TAB6260", tid_koder)
    ), quiet = TRUE) |>
    dplyr::rename(regionkod = region_kod, variabel = tabellinnehåll, varde = value)

  chart_df <- syss_df |>
    dplyr::filter(!is.na(varde)) |>
    rdverktyg::manader_bearbeta_scbtabeller()

  skriv_diagram <- function(reg, cont_var) {

    vald_region_txt <- rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(region_vekt)$region, T)

    diagramtitel <- glue::glue("{stringr::str_to_sentence(cont_var)} per åldersgrupp och kön i {vald_region_txt} i {unique(dplyr::last(chart_df$månad_år))}")
    diagramfil <- glue::glue("{cont_var}_alder_kon_{vald_region_txt}_{dplyr::last(chart_df$tid)}.png")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = dplyr::filter(chart_df,
                                  regionkod == reg,
                                  variabel == cont_var,
                                  födelseregion != "totalt",
                                  tid == max(tid)),
      skickad_x_var = "ålder",
      skickad_y_var = "varde",
      skickad_x_grupp = "kön",
      diagram_titel = diagramtitel,
      diagram_capt = diagram_capt,
      stodlinjer_avrunda_fem = TRUE,
      procent_0_100_10intervaller = ifelse(stringr::str_detect(cont_var, "antal"), FALSE, TRUE),
      filnamn_diagram = diagramfil,
      dataetiketter = FALSE,
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      manual_color = diag_fargvekt,
      manual_y_axis_title = ifelse(stringr::str_detect(cont_var, "antal"), "", "procent"),
      output_mapp = utmapp,
      skriv_till_diagramfil = skriv_diagramfil,
      lagg_pa_logga = visa_logga_i_diagram,
      logga_path = logga_sokvag,
      facet_grp = "födelseregion",
      facet_scale = "fixed"
    )
  }

  arglist <- list(reg = region_vekt, cont_var = unique(chart_df$variabel))
  crossarg <- expand.grid(arglist)
  # skriv_diagram() returnerar ett enda ggplot-objekt (inte en lista), så
  # map2() ger redan en platt lista - purrr::flatten() på en lista av
  # ggplot-objekt kraschar mot dagens purrr.
  dia_lista <- purrr::map2(crossarg$reg, crossarg$cont_var, ~skriv_diagram(reg = .x, cont_var = .y))

  return(dia_lista)
}
