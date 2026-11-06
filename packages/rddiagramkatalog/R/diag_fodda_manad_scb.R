#' Diagram: befolkningsförändring per månad, senaste sex åren
#'
#' Linjediagram över en vald befolkningsförändringsvariabel per månad, en
#' linje per år, för de senaste sex åren.
#'
#' @param region_vekt Läns- och kommunkoder, det blir ett diagram (och en fil om man skriver bildfiler) per region.
#' @param bef_variabel Går att välja: `"folkmängd"`, `"folkökning"`, `"födda"`, `"döda"`, `"födelseöverskott"`, `"samtliga inflyttningar"`, `"samtliga utflyttningar"`, `"samtliga inrikes inflyttningar"`, `"inflyttningar från kommuner inom länet"`, `"inflyttningar från övriga län"`, `"invandringar"`, `"samtliga inrikes utflyttningar"`, `"utflyttningar till kommuner inom länet"`, `"utflyttningar till övriga län"`, `"utvandringar"`, `"flyttningsöverskott totalt"`, `"flyttningsöverskott inrikes totalt"`, `"flyttningsöverskott eget län"`, `"flyttningsöverskott övriga Sverige"`, `"invandringsöverskott"`, `"justeringspost"`.
#' @param tid_koder Vilka tidsperioder som ska hämtas. `"*"` = alla, `"9999"` = senaste,
#'   eller `"2021M01:9999"` för "från januari 2021 till och med senaste tillgängliga månad".
#' @param diagram_capt Diagrammets källtext.
#' @param utmapp Mapp där diagram (och ev. excelfil) ska sparas. Måste finnas om `skriv_diagramfil = TRUE` eller `skriv_till_excelfil = TRUE`.
#' @param diag_fargvekt Valfri färgvektor.
#' @param visa_logga_i_diagram TRUE om logga ska visas i diagrammet.
#' @param logga_sokvag Sökväg till logga.
#' @param returnera_data Skall data returneras till global environment.
#' @param ta_bort_diagramtitel FALSE så skrivs ingen diagramtitel ut.
#' @param visa_dataetiketter Visa dataetiketter.
#' @param kortnamn_lan Vid TRUE så tas inte "län" i länsnamnet med.
#' @param skriv_diagramfil TRUE om diagram ska skrivas till fil.
#' @param skriv_till_excelfil TRUE om data ska skrivas till en excelfil.
#'
#' @return En namngiven lista med `ggplot`-objekt.
#' @export
diag_fodda_manad_scb <- function(
  region_vekt = "20",
  bef_variabel = "födda",
  tid_koder = "*",
  diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
  utmapp = rdverktyg::utskriftsmapp(),
  diag_fargvekt = NA,
  visa_logga_i_diagram = TRUE,
  logga_sokvag = NA,
  returnera_data = FALSE,
  ta_bort_diagramtitel = FALSE,
  visa_dataetiketter = FALSE,
  kortnamn_lan = TRUE,
  skriv_diagramfil = TRUE,
  skriv_till_excelfil = FALSE
) {

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil || skriv_till_excelfil)

  gg_list <- list()

  # hamta_data-repots hamta_bef_forandringar_region_kon_manad_scb.R (v1:
  # BE0101G/ManadBefStatRegion + MBefStatRegionCKM) hämtas här direkt via
  # v2-motsvarigheterna TAB1625 (historik 2000M01-2024M12) och TAB6473
  # (CKM 2025M01-). OBS: originalskriptet anropade en funktion,
  # hamta_befolkningsforandringar_manad(), som inte längre finns någonstans
  # i funktioner-/hamta_data-reporna (den faktiska funktionen i den
  # sourcade filen heter hamta_bef_forandringar_region_forandringar_kon_
  # tid_scb) - skriptet skulle alltså krascha direkt om det kördes.
  diagram_df_historik <- pxweb2r::pxweb2_get_data(
    table = "TAB1625",
    query = list(
      Region = region_vekt,
      Forandringar = bef_variabel,
      Kon = "totalt",
      ContentsCode = "Befolkning",
      Tid = intern_tolka_tidskoder("TAB1625", tid_koder)
    ),
    on_all_values_invalid = "null", quiet = TRUE)

  # CKM-tabellens klartext för "totalt" är "totalt, samtliga män och
  # kvinnor" (kod "TotSa") - inte bara "totalt" som i historiktabellen.
  diagram_df_ckm <- pxweb2r::pxweb2_get_data(
    table = "TAB6473",
    query = list(
      Region = region_vekt,
      Forandringar = bef_variabel,
      Kon = "TotSa",
      ContentsCode = "Befolkning",
      Tid = intern_tolka_tidskoder("TAB6473", tid_koder)
    ),
    on_all_values_invalid = "null", quiet = TRUE)

  # Skriptet refererade en "år"-kolumn som aldrig fanns i pxweb-uttaget
  # (bara den råa månadskoden, t.ex. "2020M01") - manader_bearbeta_
  # scbtabeller() (samma hjälpfunktion som används i flera andra migrerade
  # skript för den här sortens tabeller) härleder år/månad/etc. från den
  # kolumnen, vilket krävs för att resten av skriptet ska fungera alls.
  diagram_df <- rdverktyg::funktion_upprepa_forsok_om_fel(function()
    dplyr::bind_rows(diagram_df_historik, diagram_df_ckm) |>
      dplyr::rename(regionkod = region_kod, Befolkning = value) |>
      dplyr::select(-tabellinnehåll) |>
      # CKM-tabellens etikett för totalt kön skiljer sig från historiktabellens
      # ("totalt, samtliga män och kvinnor" resp. "totalt") - samma
      # normalisering som redan görs i hamta_bef_forandringar_region_kon_
      # manad_scb.R.
      dplyr::mutate(kön = dplyr::if_else(stringr::str_detect(kön, "totalt"), "totalt", kön)) |>
      rdverktyg::manader_bearbeta_scbtabeller()) |>
    dplyr::mutate(
      fokus = as.integer(5 - (max(as.integer(as.character(år))) - as.integer(as.character(år)))),
      fokus = ifelse(fokus < 0, 0, fokus),  # Sätt alla äldre än 5 år från senaste året till 0
      ar_num = as.integer(år)
    ) |>
    dplyr::filter(ar_num > max(ar_num)-6)

  if (kortnamn_lan) diagram_df <- dplyr::mutate(diagram_df, region = rdverktyg::skapa_kortnamn_lan(region))

  # om ingen färgvektor är medskickad: en färg per år (en linje per år i
  # diagrammet). rus_gradient (sex gröna nyanser) visade sig vara för
  # svårskiljda i praktiken - rus_sex ger sex tydligt olika färger i
  # stället, vilket också råkar sluta på en mörk, tydlig färg för det
  # sista (senaste) året eftersom paletten är ordnad så. Antalet år matchar
  # normalt exakt de sex färgerna (samma "senaste 6 år"-filter som nedan),
  # men rep_len() används defensivt ifall fler år någonsin visas.
  if (all(is.na(diag_fargvekt))) {
    antal_ar <- length(unique(diagram_df$år))
    diag_fargvekt <- rep_len(rddiagram::diagramfarger("rus_sex"), antal_ar)
  }

  # Lägg till en CKM-notering i captionen om CKM-tabellen (TAB6473, data
  # fr.o.m. 2025) faktiskt bidragit med rader till uttaget.
  har_ckm_data <- !is.null(diagram_df_ckm) && nrow(diagram_df_ckm) > 0
  if (har_ckm_data) {
    diagram_capt <- rddiagram::lagg_till_ckm_notering(diagram_capt, TRUE, fran_ar = 2025)
  }

  # returnera datasetet till global environment, bl.a. bra när man skapar Rmarkdown-rapporter
  if(returnera_data == TRUE){
    assign("bef_forandringar_per_manad_df", diagram_df, envir = .GlobalEnv)
  }


  skapa_diagram <- function(skickad_regionkod) {

    diagram_region_df <- dplyr::filter(diagram_df, regionkod %in% skickad_regionkod)

    region_txt <- unique(diagram_region_df$region)
    variabel_txt <- unique(diagram_region_df$förändringar)

    diagramtitel <- glue::glue("{stringr::str_to_sentence(variabel_txt)} i {region_txt}")
    diagramfil <- glue::glue("{variabel_txt}_{region_txt}_ar{dplyr::first(diagram_region_df$år)}-{dplyr::last(diagram_region_df$år)}.png")

    gg_obj <- rddiagram::SkapaLinjeDiagram(
      skickad_df = diagram_region_df,
      skickad_x_var = "månad",
      skickad_y_var = "Befolkning",
      skickad_x_grupp = "år",
      diagram_titel = if (ta_bort_diagramtitel) NULL else diagramtitel,
      diagram_capt = diagram_capt,
      stodlinjer_avrunda_fem = TRUE,
      filnamn_diagram = diagramfil,
      manual_y_axis_title = "",
      manual_color = diag_fargvekt,
      lagg_pa_logga = visa_logga_i_diagram,
      logga_path = logga_sokvag,
      legend_byrow = TRUE,
      output_mapp = utmapp
    ) # slut skriv ggplot_objekt

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfil, ".png")

    return(gg_list)

  } # slut funktion som skapar diagrammet

  retur_list <- purrr::flatten(purrr::map(region_vekt, ~skapa_diagram(skickad_regionkod = .x)))

  # OBS: det här blocket fanns redan i originalskriptet men refererar
  # variabler (region_xlsx, startar_utfall, slutar_prognos, bef_folk_progn)
  # som aldrig definieras någonstans i funktionen - ser ut att vara
  # oavsiktligt kvarglömd kod från ett annat skript. skriv_till_excelfil är
  # FALSE som standard så blocket körs aldrig av misstag, men skulle krascha
  # med "object not found" om man satte det till TRUE. Lämnat oförändrat
  # (samma beteende som originalet) i väntan på besked om vad exportens
  # avsedda innehåll egentligen ska vara.
  if (skriv_till_excelfil) {
      excefilnamn <- glue::glue("befolkning_utfall_progn_{region_xlsx}_ar{startar_utfall}-{slutar_prognos}.xlsx")
      openxlsx::write.xlsx(bef_folk_progn, paste0(utmapp, excefilnamn))
  }

  return(retur_list)

} # slut funktion
