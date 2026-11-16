#' Diagram: förändring av den demografiska försörjningskvoten per region
#'
#' Visar försörjningskvoten (antal personer i yngre och äldre åldrar i
#' förhållande till antalet i arbetsför ålder) för ett startår och ett jämförelseår,
#' en grupp staplar per region. Startåret är senaste år med faktisk
#' befolkningsstatistik. Jämförelseåret ligger `jmfrtid` år framåt (då hämtas
#' det från SCB:s befolkningsprognos) eller bakåt (då hämtas det från faktisk
#' befolkningsstatistik).
#'
#' Kvoten räknas från folkmängd per ålder i SCB:s tabeller TAB638 (till och
#' med 2024) och TAB5557 (från 2025, skyddad med CKM) samt, för prognosår,
#' TAB698. Åldrarna summeras själva ihop - totalkoderna används inte, eftersom
#' CKM-tabellens totaler inte alltid är lika med summan av delarna.
#'
#' @param region_vekt Regioner (läns-/kommunkoder). Standard är Dalarnas län
#'   och dess kommuner. Riket (`"00"`) kan bara användas när jämförelseåret är
#'   ett år med faktisk befolkningsstatistik (negativt `jmfrtid`), eftersom
#'   prognostabellen saknar riket.
#' @param arbetsfor_alder Vilken åldersgrupp som räknas som arbetsför ålder
#'   (nämnaren): `"20-64 år"` (standard, samma avgränsning som SCB:s
#'   demografiska försörjningskvot), `"20-65 år"` (avgränsningen i det
#'   ursprungliga skriptet) eller `"20-66 år"`. Yngre är alltid 0-19 år och
#'   äldre är alla över övre gränsen (65+, 66+ respektive 67+ år).
#' @param jmfrtid Antal år från senaste år med faktisk befolkningsstatistik.
#'   Positivt värde ger ett prognosår (begränsas till sista året i prognosen),
#'   negativt värde ett tidigare år (begränsas till första året i
#'   befolkningsstatistiken). 0 är inte tillåtet.
#' @param sortera_efter `"slut"` (standard) sorterar regionerna efter det
#'   senare av de två åren, `"start"` efter det tidigare.
#' @param svart_linje_vid_ett TRUE om en svart linje ska markera kvoten 1
#'   (lika många yngre och äldre som personer i arbetsför ålder).
#' @param visa_logga_i_diagram TRUE om logga ska visas i diagrammet.
#' @param logga_sokvag Sökväg till logga, `NA` = standardlogga.
#' @param skriv_diagramfil TRUE om diagrammet ska skrivas till fil.
#' @param skriv_till_excelfil TRUE om data ska skrivas till en excelfil i
#'   `utmapp`.
#' @param returnera_data TRUE om data ska skrivas till `.GlobalEnv` som
#'   `forsorjningskvot_per_region`.
#' @param utmapp Mapp där diagram och excelfil sparas. Måste finnas om
#'   `skriv_diagramfil` eller `skriv_till_excelfil` är `TRUE`.
#' @param diag_fargvekt Valfri färgvektor med två färger (tidigare år först),
#'   `NA` = paketets standard.
#' @param diagram_capt Diagrammets källtext. `"auto"` (standard) skapar texten
#'   utifrån vilka tabeller som använts.
#' @param filformat Bildformat, t.ex. `"png"` (standard).
#'
#' @return En namngiven lista med ett `ggplot`-objekt.
#' @export
diag_forsorjningskvot_per_region_scb <- function(
    region_vekt = rdverktyg::hamtakommuner("20", tamedlan = TRUE, tamedriket = FALSE),
    arbetsfor_alder = "20-64 år",
    jmfrtid = 10,
    sortera_efter = "slut",
    svart_linje_vid_ett = TRUE,
    visa_logga_i_diagram = TRUE,
    logga_sokvag = NA,
    skriv_diagramfil = TRUE,
    skriv_till_excelfil = FALSE,
    returnera_data = FALSE,
    utmapp = rdverktyg::utskriftsmapp(),
    diag_fargvekt = NA,
    diagram_capt = "auto",
    filformat = "png"
) {

  # ========================================== Allmän info ============================================
  # Migrerad från func_forsorjningskvot.R (SkapaForsorjningskvot) i källrepot. Befolkningen hämtas från
  # TAB638 (historik, t.o.m. 2024) + TAB5557 (CKM, från 2025) - samma tabellpar som
  # rdhamtadata::hamta_befprognos_diff_data() - och prognosen från SCB:s befolkningsprognos (TAB698) via
  # rdhamtadata::hamta_befprognos_data().
  #
  # Ändringar jämfört med originalet (påverkar vilka tal som kommer ut, se NEWS.md):
  #  * Arbetsför ålder är nu valbar (arbetsfor_alder). Standard är 20-64 år (SCB:s definition); originalet
  #    hade 20-65 år hårdkodat.
  #  * Starten kommer från senaste år i TAB638/TAB5557 i stället för från det gamla BefolkningNy.
  #  * Originalets fokusering av kommuner i ett län (parametern "fokus") är inte migrerad ännu.
  # ========================================== Inställningar ============================================

  options(dplyr.summarise.inform = FALSE)

  if (stringr::str_sub(filformat, 1, 1) == ".") filformat <- stringr::str_sub(filformat, 2)
  region_vekt <- unique(as.character(unlist(region_vekt)))

  if (!arbetsfor_alder %in% c("20-64 år", "20-65 år", "20-66 år")) {
    stop("arbetsfor_alder måste vara ett av \"20-64 år\", \"20-65 år\" eller \"20-66 år\".", call. = FALSE)
  }
  if (!sortera_efter %in% c("slut", "start")) stop("sortera_efter måste vara \"slut\" eller \"start\".", call. = FALSE)
  if (length(jmfrtid) != 1 || is.na(jmfrtid) || jmfrtid == 0) stop("jmfrtid måste vara ett heltal skilt från 0.", call. = FALSE)

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil || skriv_till_excelfil)

  if (all(is.na(diag_fargvekt))) diag_fargvekt <- rev(rddiagram::diagramfarger("rus_sex")[c(1, 2)])

  scb_prognos_url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefProgOsiktRegN"
  ckm_fran_ar <- 2025

  # övre gränsen för arbetsför ålder, t.ex. 64 för "20-64 år"
  arbf_min <- 20
  arbf_max <- as.integer(stringr::str_extract(arbetsfor_alder, "(?<=-)[0-9]+"))

  # ========================================== Vilka år ============================================

  senaste_ar_bef <- max(as.numeric(unlist(purrr::map(c("TAB638", "TAB5557"),
                                                     ~ pxweb2r::pxweb2_get_values(.x, "tid", quiet = TRUE)$code))))
  forsta_ar_bef <- min(as.numeric(unlist(purrr::map(c("TAB638", "TAB5557"),
                                                    ~ pxweb2r::pxweb2_get_values(.x, "tid", quiet = TRUE)$code))))
  startar <- senaste_ar_bef

  if (jmfrtid < 0) {
    slutar <- max(startar + jmfrtid, forsta_ar_bef)
  } else {
    sista_prognosar <- max(as.numeric(pxweb2r::pxweb2_get_values("TAB698", "Tid", quiet = TRUE)$code))
    slutar <- min(startar + jmfrtid, sista_prognosar)
  }

  if (slutar == startar) stop("Jämförelseåret blev samma som startåret (", startar, ") - kontrollera jmfrtid.", call. = FALSE)

  utfall_ar <- unique(c(startar, slutar[slutar <= senaste_ar_bef]))
  prognos_ar_hamta <- slutar[slutar > senaste_ar_bef]

  if (length(prognos_ar_hamta) > 0 && "00" %in% region_vekt) {
    stop("Riket (\"00\") finns inte i prognostabellen. Ta bort \"00\" ur region_vekt eller använd ett negativt jmfrtid.", call. = FALSE)
  }

  # ========================================== Hämta data ============================================

  # Faktisk folkmängd per ålder. Civilstånd och kön summeras ihop. Åldrarna 0-99 hämtas individuellt (inga
  # totalkoder, se funktionsbeskrivningen). Hundraåringarna heter "100+" i TAB638 men "100+1" i TAB5557; båda
  # koderna skickas med och den som inte finns i en tabell hoppas över (on_all_values_invalid = "null").
  # Med bara "100+1" föll hundraåringarna bort för alla år t.o.m. 2024.
  utfall_df <- rdverktyg::funktion_upprepa_forsok_om_fel(function() {
    pxweb2r::pxweb2_get_data(
      table = c("TAB638", "TAB5557"),
      query = list(
        Region = region_vekt,
        Civilstand = c("OG", "G", "SK", "ÄNKL"),
        Alder = c(0:99, "100+", "100+1"),
        Kon = c("män", "kvinnor"),
        ContentsCode = "Folkmängd",
        Tid = as.character(utfall_ar)
      ), quiet = TRUE, on_all_values_invalid = "null")
  })

  region_namn <- utfall_df |>
    dplyr::filter(as.character(`år`) == as.character(max(as.numeric(`år`)))) |>
    dplyr::distinct(regionkod = region_kod, region)

  alder_df <- utfall_df |>
    dplyr::rename(regionkod = region_kod) |>
    dplyr::mutate(alder_num = suppressWarnings(readr::parse_number(as.character(`ålder`)))) |>
    dplyr::summarise(antal = sum(value, na.rm = TRUE),
                     .by = c(regionkod, `år`, alder_num)) |>
    dplyr::mutate(`år` = as.character(`år`), typ = "utfall")

  if (length(prognos_ar_hamta) > 0) {
    prognos_df <- rdverktyg::funktion_upprepa_forsok_om_fel(function() {
      rdhamtadata::hamta_befprognos_data(
        region_vekt = region_vekt,
        tid_vekt = as.character(prognos_ar_hamta),
        url_prognos_vektor = scb_prognos_url
      )
    }, max_forsok = 4)

    if (is.null(prognos_df)) stop("Kunde inte hämta befolkningsprognosen - se felmeddelandet ovan.", call. = FALSE)

    # kolumnen med antal heter Folkmängd eller Antal beroende på tabellversion
    vardekol <- intersect(c("Folkmängd", "Antal"), names(prognos_df))[1]

    prognos_alder_df <- prognos_df |>
      dplyr::mutate(alder_num = suppressWarnings(readr::parse_number(as.character(`ålder`)))) |>
      dplyr::filter(!is.na(alder_num)) |>
      dplyr::summarise(antal = sum(.data[[vardekol]], na.rm = TRUE),
                       .by = c(regionkod, `år`, alder_num)) |>
      dplyr::mutate(`år` = as.character(`år`), typ = "prognos")

    alder_df <- dplyr::bind_rows(alder_df, prognos_alder_df)
    prognos_ar_txt <- unique(prognos_df$prognos_ar)
  } else {
    prognos_ar_txt <- NA_character_
  }

  # ========================================== Beräkna försörjningskvot ============================================

  kvot_df <- alder_df |>
    dplyr::summarise(
      antal_unga = sum(antal[alder_num < arbf_min]),
      antal_arbetsfor = sum(antal[alder_num >= arbf_min & alder_num <= arbf_max]),
      antal_aldre = sum(antal[alder_num > arbf_max]),
      .by = c(regionkod, `år`, typ)) |>
    dplyr::mutate(forsorjningskvot = (antal_unga + antal_aldre) / antal_arbetsfor) |>
    dplyr::left_join(region_namn, by = "regionkod") |>
    dplyr::mutate(region = dplyr::coalesce(region, regionkod)) |>
    dplyr::relocate(region, .after = regionkod) |>
    dplyr::arrange(regionkod, `år`)

  if (returnera_data) assign("forsorjningskvot_per_region", kvot_df, envir = .GlobalEnv)

  # ========================================== Texter ============================================

  tidigt_ar <- as.character(min(startar, slutar))
  sent_ar <- as.character(max(startar, slutar))
  har_prognos <- length(prognos_ar_hamta) > 0
  har_ckm <- any(as.numeric(kvot_df$`år`[kvot_df$typ == "utfall"]) >= ckm_fran_ar)

  region_start <- rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(unique(kvot_df$region)))
  region_txt <- rdverktyg::ar_alla_kommuner_i_ett_lan(unique(kvot_df$regionkod), returnera_text = TRUE, returtext = region_start)
  region_txt <- rdverktyg::ar_alla_lan_i_sverige(unique(kvot_df$regionkod), returnera_text = TRUE, returtext = region_txt)

  if (identical(diagram_capt, "auto")) {
    kalla <- if (har_prognos) {
      paste0("SCB:s öppna statistikdatabas, befolkningsmängd och befolkningsprognos från år ", prognos_ar_txt)
    } else {
      "SCB:s öppna statistikdatabas, befolkningsmängd"
    }
    diagram_capt <- paste0(
      "Källa: ", kalla, "\nBearbetning: Samhällsanalys, Region Dalarna\n",
      "Försörjningskvoten beräknas genom att lägga ihop antalet personer 0-", arbf_min - 1, " år och ", arbf_max + 1,
      "+ år och dividera med antalet personer ", arbetsfor_alder, ".\n",
      "En kvot på 1 innebär att det finns lika många barn, unga och äldre som det finns personer i arbetsför ålder (",
      arbetsfor_alder, ")."
    )
    diagram_capt <- rddiagram::lagg_till_ckm_notering(diagram_capt, har_ckm_data = har_ckm, fran_ar = ckm_fran_ar)
  }

  diagramtitel <- paste0("Förändring av försörjningskvoten i ", region_txt, " mellan år ", tidigt_ar, " och år ", sent_ar)
  region_filnamn <- stringr::str_replace_all(tolower(region_txt), "[^a-z0-9åäö]+", "_")
  filnamn_bas <- paste0("forsorjningskvot_", region_filnamn, "_", tidigt_ar, "_", sent_ar, "_", sub("-", "_", sub(" år", "", arbetsfor_alder)))

  # ========================================== Diagram ============================================

  diagram_df <- kvot_df |>
    dplyr::filter(`år` %in% c(tidigt_ar, sent_ar)) |>
    dplyr::arrange(`år`, regionkod) |>
    dplyr::mutate(ar_etikett = paste0("Försörjningskvot år ", `år`))

  antal_regioner <- dplyr::n_distinct(diagram_df$regionkod)
  liggande <- antal_regioner <= 50

  gg_obj <- rddiagram::SkapaStapelDiagram(
    skickad_df = diagram_df,
    skickad_x_var = "region",
    skickad_y_var = "forsorjningskvot",
    skickad_x_grupp = "ar_etikett",
    diagram_titel = diagramtitel,
    diagram_capt = diagram_capt,
    manual_color = diag_fargvekt,
    manual_y_axis_title = "försörjningskvot",
    sortera_x = if (sortera_efter == "slut") 2 else 1,
    diagram_liggande = liggande,
    stodlinjer_avrunda_fem = FALSE,
    x_axis_lutning = if (liggande) 0 else 90,
    x_axis_storlek = if (liggande) 10.5 else 7,
    manual_x_axis_text_hjust = 1,
    manual_x_axis_text_vjust = 0.5,
    output_mapp = utmapp,
    filnamn_diagram = paste0(filnamn_bas, ".", filformat),
    lagg_pa_logga = visa_logga_i_diagram,
    logga_path = logga_sokvag,
    skriv_till_diagramfil = FALSE
  )

  # linjen läggs på här i stället för i byggfunktionen - filen skrivs först efter att linjen lagts på
  if (svart_linje_vid_ett) {
    gg_obj <- gg_obj + ggplot2::geom_hline(yintercept = 1, color = "black", linewidth = 0.5)
  }

  if (skriv_diagramfil) {
    rddiagram::skriv_till_diagramfil(
      ggplot_objekt = gg_obj,
      output_mapp = utmapp,
      filnamn_diagram = paste0(filnamn_bas, ".", filformat),
      diagramfil_bredd = if (liggande) 12 else 20,
      diagramfil_hojd = if (liggande) max(7, 0.3 * antal_regioner + 2) else 10,
      logga = if (all(is.na(logga_sokvag))) visa_logga_i_diagram else logga_sokvag,
      diagram_bildformat = filformat
    )
  }

  if (skriv_till_excelfil) {
    writexl::write_xlsx(kvot_df, file.path(utmapp, paste0(filnamn_bas, ".xlsx")))
  }

  gg_list <- list(gg_obj)
  names(gg_list) <- filnamn_bas
  return(gg_list)
}
