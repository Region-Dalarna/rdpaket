#' Diagram: BRP per invånare, länsjämförelse och tidsserie
#'
#' Skriver ut diagram med BRP per invånare, dels som en länsjämförelse för
#' senaste året, dels som en tidsserie för valt län jämfört med riket.
#'
#' @param region_vekt Val av region. Finns: `"00"`, `"01"`, `"03"`, `"04"`, `"05"`, `"06"`, `"07"`, `"08"`, `"09"`, `"10"`, `"12"`, `"13"`, `"14"`, `"17"`, `"18"`, `"19"`, `"20"`, `"21"`, `"22"`, `"23"`, `"24"`, `"25"`, `"RIKS1"`-`"RIKS8"`, `"90"`.
#' @param cont_klartext Finns: `"BRP, löpande priser, mnkr"`, `"BRP, volymutveckling i procent"`, `"BRP per invånare, löpande priser, tkr"`, `"BRP per sysselsatt, löpande priser, tkr"`, `"Medelantal sysselsatta, personer i 1000-tal"`, `"Egentlig lön, löpande priser, mnkr"`.
#' @param tid_koder Vilka år som ska hämtas. `"*"` = alla, `"9999"` = senaste,
#'   eller `"2015:9999"` för "från 2015 till och med senaste tillgängliga år".
#' @param diagram_capt Diagrammets källtext.
#' @param visa_dataetiketter Visa dataetiketter.
#' @param diag_fargvekt Valfri färgvektor, standard: `rddiagram::diagramfarger("rus_sex")`.
#' @param visa_logga_i_diagram TRUE om logga ska visas i diagrammet.
#' @param logga_sokvag Sökväg till logga.
#' @param utmapp Mapp där diagram ska sparas. Måste finnas om `skriv_diagramfil = TRUE`.
#' @param skriv_diagramfil TRUE om diagram ska skrivas till fil.
#' @param returnera_data Skall data returneras till global environment.
#' @param excelfil_mapp Anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till.
#' @param excel_filnamn Filnamn för excelfil som exporteras om `excel_filnamn` och `excelfil_mapp` anges.
#' @param demo Sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren.
#' @param diag_lansjmfr_valt_ar Skapa diagram med länsjämförelse för senaste året.
#' @param diag_valt_lan_riket_tidsserie Skapa diagram med tidsserie för valt län jämfört med riket.
#'
#' @return En namngiven lista med `ggplot`-objekt.
#' @export
diag_brp_per_inv_scb <- function(
  region_vekt = "20",
  cont_klartext = "BRP per invånare, löpande priser, tkr",
  tid_koder = "*",
  diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
  visa_dataetiketter = FALSE,
  diag_fargvekt = NA,
  visa_logga_i_diagram = TRUE,
  logga_sokvag = NA,
  utmapp = rdverktyg::utskriftsmapp(),
  skriv_diagramfil = TRUE,
  returnera_data = FALSE,
  excelfil_mapp = NA,
  excel_filnamn = "brp_per_inv.xlsx",
  demo = FALSE,
  diag_lansjmfr_valt_ar = TRUE,
  diag_valt_lan_riket_tidsserie = TRUE
) {

# om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
# demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
if (demo){
  demo_url <-
c("https://region-dalarna.github.io/utskrivna_diagram/brp_per_inv_jmfr_lan_ar2022.png",
"https://region-dalarna.github.io/utskrivna_diagram/brp_per_inv_tidsserie_20_ar2022.png")
  purrr::walk(demo_url, ~browseURL(.x))
  if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
  rdverktyg::stop_tyst()
}

# om ingen färgvektor är medskickad, använd rddiagram::diagramfarger("rus_sex")
if (all(is.na(diag_fargvekt))) {
  diag_fargvekt <- rddiagram::diagramfarger("rus_sex")
}

utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

# hamta_data-repots hamta_brp_lan_region_tid_NR0105ENS2010T01A_scb.R (v1:
# NR/NR0105/NR0105A/NR0105ENS2010T01A) hämtas här direkt via
# v2-motsvarigheten TAB3138. Ingen CKM-uppdelning för den här tabellen
# (ekonomisk statistik, inte BE-området). OBS: originalet hårdkodade
# region_vekt = hamtaAllaLan(TRUE) och cont_klartext = "BRP per invånare,
# löpande priser, tkr" direkt i anropet till hamta_brp_lan_region_tid_scb()
# i stället för att koppla in funktionens egna cont_klartext/tid_koder-
# parametrar (region_vekt-parametern används däremot korrekt längre ner,
# för fokusering/filtrering) - kopplat in nu så att parametrarna faktiskt
# styr vad som hämtas.
brp_lan_df <- rdverktyg::suppress_specific_warning(
  pxweb2r::pxweb2_get_data(
    table = "TAB3138",
    query = list(
      Region = rdverktyg::hamtaAllaLan(TRUE),
      ContentsCode = cont_klartext,
      Tid = intern_tolka_tidskoder("TAB3138", tid_koder)
    ), quiet = TRUE) |>
    dplyr::rename(regionkod = region_kod) |>
    dplyr::rename(!!cont_klartext := value) |>
    dplyr::select(-tabellinnehåll) |>
    dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region, TRUE))
)


if (!is.na(excelfil_mapp) & !is.na(excel_filnamn)){
  openxlsx::write.xlsx(brp_lan_df, paste0(excelfil_mapp, excel_filnamn), overwrite = TRUE)
}

if(returnera_data == TRUE){
  assign("brp_lan_df", brp_lan_df, envir = .GlobalEnv)
}

vald_region_txt <- brp_lan_df |>
  dplyr::distinct(region) |>
  dplyr::pull() |>
  rdverktyg::skapa_kortnamn_lan() |>
  rdverktyg::list_komma_och()


  if (diag_lansjmfr_valt_ar) {

  diagramtitel <- glue::glue("BRP per invånare per län år {max(brp_lan_df$år)}")
  diagramfil <- glue::glue("brp_per_inv_jmfr_lan_ar{max(brp_lan_df$år)}.png")


  gg_obj <- rddiagram::SkapaStapelDiagram(
    skickad_df = brp_lan_df |>
      dplyr::mutate(fokus = dplyr::case_when(regionkod == region_vekt ~ 1,
                               regionkod == "00" ~ 2,
                               TRUE ~ 0)) |>
      dplyr::filter(år == max(år)),
    skickad_x_var = "region",
    skickad_y_var = names(brp_lan_df)[ncol(brp_lan_df)],
    diagram_titel = diagramtitel,
    diagram_capt = diagram_capt,
    x_axis_sort_value = TRUE,
    x_var_fokus = "fokus",
    stodlinjer_avrunda_fem = TRUE,
    filnamn_diagram = diagramfil,
    dataetiketter = visa_dataetiketter,
    manual_x_axis_text_vjust = 1,
    manual_x_axis_text_hjust = 1,
    manual_color = diag_fargvekt,
    output_mapp = utmapp,
    lagg_pa_logga = visa_logga_i_diagram,
    logga_path = logga_sokvag,
    skriv_till_diagramfil = skriv_diagramfil
  )

  ett_diagram <- list(gg_obj)
  names(ett_diagram) <- stringr::str_remove(diagramfil, "\\.png")

  } else ett_diagram <- list()

if (diag_valt_lan_riket_tidsserie) {

  diagramtitel <- glue::glue("Förändring av BRP per invånare år {min(brp_lan_df$år)}-{max(brp_lan_df$år)}")
  diagramfil <- glue::glue("brp_per_inv_tidsserie_{region_vekt}_ar{max(brp_lan_df$år)}.png")


  gg_obj <- rddiagram::SkapaLinjeDiagram(
    skickad_df = brp_lan_df |>
      dplyr::mutate(fokus = dplyr::case_when(regionkod == region_vekt ~ 1,
                               regionkod == "00" ~ 2,
                               TRUE ~ 0)) |>
      dplyr::filter(regionkod %in% c(region_vekt, "00")),
    skickad_x_var = "år",
    skickad_y_var = names(brp_lan_df)[ncol(brp_lan_df)],
    skickad_x_grupp = "region",
    diagram_titel = diagramtitel,
    diagram_capt = diagram_capt,
    stodlinjer_avrunda_fem = TRUE,
    filnamn_diagram = diagramfil,
    berakna_index = TRUE,
    manual_color = diag_fargvekt,
    output_mapp = utmapp,
    lagg_pa_logga = visa_logga_i_diagram,
    logga_path = logga_sokvag,
    skriv_till_diagramfil = skriv_diagramfil
  )

  ett_diagram2 <- list(gg_obj)
  names(ett_diagram2) <- stringr::str_remove(diagramfil, "\\.png")

} else ett_diagram2 <- list()


return(c(ett_diagram, ett_diagram2))
}
