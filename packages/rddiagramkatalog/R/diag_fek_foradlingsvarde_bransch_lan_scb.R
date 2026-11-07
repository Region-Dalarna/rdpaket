#' Diagram: förädlingsvärde per bransch
#'
#' Förädlingsvärde (eller annan variabel ur Företagens ekonomi) per
#' bransch, för en eller flera regioner och år.
#'
#' @param region_vekt Val av region.
#' @param sni2007_klartext Bransch enligt SNI 2007. `"*"` ger samtliga.
#' @param cont_klartext Innehållsvariabel. Finns: `"Antal arbetsställen
#'   (lokala verksamheter)"`, `"Antal anställda"`, `"Nettoomsättning exkl.
#'   merchantingkostnader, mnkr"`, `"Produktionsvärde, mnkr"`,
#'   `"Förädlingsvärde, mnkr"` (standard), `"Totala intäkter, mnkr"`,
#'   `"Totala kostnader, mnkr"`.
#' @param tid_koder `"*"` (standard) ger de tre senaste tillgängliga åren
#'   (grupperade per år i diagrammet), `"9999"` enbart senaste året, eller
#'   ange enskilda år.
#' @param gruppera_namn Namn att gruppera ihop de medskickade regionerna
#'   till en gemensam rad under, eller `NA` (standard) för ingen gruppering.
#' @param diagram_capt Diagrammets källtext.
#' @param visa_dataetiketter TRUE om dataetiketter ska visas i diagrammet.
#' @param diag_fargvekt Vilka färger som ska användas i diagram.
#' @param visa_logga_i_diagram TRUE om logga ska visas i diagrammet.
#' @param logga_sokvag Sökväg till logga.
#' @param utmapp Mapp där diagram sparas. Måste finnas om
#'   `skriv_diagramfil = TRUE`.
#' @param skriv_diagramfil TRUE om diagram ska skrivas till fil.
#' @param returnera_data Skall data returneras till global environment.
#' @param demo TRUE om man bara vill se ett exempel på diagrammet i
#'   webbläsaren och inget annat.
#' @param excelfil_mapp Mapp att spara en excelfil i, om `excel_filnamn`
#'   också anges.
#' @param excel_filnamn Filnamn för excelfil som exporteras, om
#'   `excelfil_mapp` också anges.
#'
#' @return En namngiven lista med ett `ggplot`-objekt.
#' @export
diag_fek_foradlingsvarde_bransch_lan_scb <- function(
  region_vekt = "20",
  sni2007_klartext = "*",
  cont_klartext = "Förädlingsvärde, mnkr",
  tid_koder = "*",
  gruppera_namn = NA,
  diagram_capt = "Källa: Företagens ekonomi i SCB:s öppna statistikdatabas. Bearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Förädlingsvärde är den faktiska produktionen minus kostnader för köpta varor och tjänster, dock ej löner, sociala avgifter och kostnader för handelsvaror.",
  visa_dataetiketter = FALSE,
  diag_fargvekt = NA,
  visa_logga_i_diagram = TRUE,
  logga_sokvag = NA,
  utmapp = rdverktyg::utskriftsmapp(),
  skriv_diagramfil = TRUE,
  returnera_data = FALSE,
  demo = FALSE,
  excelfil_mapp = NA,
  excel_filnamn = "helarsekvivalenter.xlsx"
) {

  # ========================================== Allmän info ============================================
  # Förädlingsvärde (eller annan FEK-variabel) per bransch.
  # ========================================== Inställningar ============================================

  # om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
  if (demo) {
    demo_url <- "https://region-dalarna.github.io/utskrivna_diagram/foradlingsvarde_bransch_20_ar2022.png"
    purrr::walk(demo_url, ~ browseURL(.x))
    if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
    rdverktyg::stop_tyst()
  }

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  if (all(is.na(diag_fargvekt))) diag_fargvekt <- rddiagram::diagramfarger("rus_sex")

  gg_list <- list()

  # TAB6329 (regionala basfakta för verksamhetsnivå enligt Företagens ekonomi) har bara funnits sedan
  # 2022. "9999" (senaste år) är en v1-specifik sentinel som pxweb2r inte förstår - hanteras explicit
  # nedan. "*" ger de tre senaste tillgängliga åren (grupperade per år i diagrammet) - annars, med bara
  # ett enskilt år valt, summeras alla årens värden osynligt ihop till en enda stapel per branschgrupp.
  if (identical(tid_koder, "*")) {
    giltiga_ar <- pxweb2r::pxweb2_get_values("TAB6329", "Tid", quiet = TRUE)$code
    tid_koder <- utils::tail(sort(giltiga_ar), 3)
  } else if (any(tid_koder == "9999")) {
    senaste_ar <- max(pxweb2r::pxweb2_get_values("TAB6329", "Tid", quiet = TRUE)$code)
    tid_koder <- ifelse(tid_koder == "9999", senaste_ar, tid_koder)
  }

  foradl_df <- rdverktyg::suppress_specific_warning(
    pxweb2r::pxweb2_get_data(
      table = "TAB6329",
      query = list(
        Region = region_vekt,
        SNI2007 = sni2007_klartext,
        ContentsCode = cont_klartext,
        Tid = tid_koder
      ), quiet = TRUE) |>
      dplyr::rename(regionkod = region_kod, variabel = tabellinnehåll, varde = value)
  )

  # ========================================== Läser in branschnyckel ============================================
  # Mall för att gruppera SNI-branscher (Avdelning) till färre branschgrupper - en fast, delad mallfil
  # (inte en tidsserie med nya årgångar), så intern_sakerstall_indatafil() valideras direkt mot den
  # exakta sökvägen och skriver ut en konsolvarning om att datan är en manuell ögonblicksbild.
  branschnyckel_sokvag <- "g:/skript/nycklar/Bransch_FEK.xlsx"
  intern_sakerstall_indatafil(branschnyckel_sokvag)
  branschnyckel <- readxl::read_xlsx(branschnyckel_sokvag) |>
    dplyr::select(Avdelning, Grupp_kod, Branschgrupp) |>
    dplyr::distinct()

  chart_df <- foradl_df |>
    dplyr::mutate(branschbokstav = stringr::str_sub(`näringsgren SNI 2007`, 1, 1)) |>
    dplyr::filter(!stringr::str_detect(`näringsgren SNI 2007`, "samtliga näringsgrenar")) |>
    dplyr::left_join(branschnyckel, by = c("branschbokstav" = "Avdelning")) |>
    dplyr::group_by(år, regionkod, region, Grupp_kod, Branschgrupp, variabel) |>
    dplyr::summarise(varde = sum(varde, na.rm = TRUE), .groups = "drop")

  # om man vill gruppera ihop flera kommuner eller län till en större geografisk indelning
  # så anges den med namn i gruppera_namn. Lämnas den tom görs ingenting nedan
  if (!is.na(gruppera_namn)) {
    chart_df <- chart_df |>
      dplyr::group_by(dplyr::across(-c(regionkod, region, varde))) |>
      dplyr::summarise(varde = sum(varde, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(regionkod = "gg",
             region = gruppera_namn) |>
      dplyr::relocate(region, .before = 1) |>
      dplyr::relocate(regionkod, .before = region)

    region_vekt <- "gg"
  }

  if (!is.na(excelfil_mapp) & !is.na(excel_filnamn)) {
    openxlsx::write.xlsx(chart_df, paste0(excelfil_mapp, excel_filnamn), overwrite = TRUE)
  }

  if (returnera_data == TRUE) {
    assign("chart_df", chart_df, envir = .GlobalEnv)
  }

  vald_region_txt <- chart_df |>
    dplyr::distinct(region) |>
    dplyr::pull() |>
    rdverktyg::list_komma_och() |>
    rdverktyg::skapa_kortnamn_lan()

  ar_txt <- chart_df |>
    dplyr::distinct(år) |>
    dplyr::pull() |>
    rdverktyg::list_komma_och()

  flera_ar <- length(unique(chart_df$år)) > 1

  diagramtitel <- glue::glue("Förädlingsvärde i {vald_region_txt} per bransch år {ar_txt}")
  diagramfil <- glue::glue("foradlingsvarde_bransch_{paste0(region_vekt, collapse = '_')}_ar{ar_txt}.png")

  gg_obj <- rddiagram::SkapaStapelDiagram(
    skickad_df = chart_df,
    skickad_x_var = "Branschgrupp",
    skickad_y_var = "varde",
    skickad_x_grupp = if (flera_ar) "år" else NULL,
    diagram_titel = diagramtitel,
    diagram_capt = diagram_capt,
    x_axis_sort_value = TRUE,
    stodlinjer_avrunda_fem = TRUE,
    filnamn_diagram = diagramfil,
    dataetiketter = visa_dataetiketter,
    manual_y_axis_title = cont_klartext,
    manual_x_axis_text_vjust = 1,
    manual_x_axis_text_hjust = 1,
    x_axis_lutning = 45,
    manual_color = if (flera_ar) diag_fargvekt[seq_len(length(unique(chart_df$år)))] else diag_fargvekt,
    output_mapp = utmapp,
    lagg_pa_logga = visa_logga_i_diagram,
    logga_path = logga_sokvag,
    skriv_till_diagramfil = skriv_diagramfil
  )

  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfil, "\\.png")

  return(gg_list)
}
