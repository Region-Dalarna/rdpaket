#' Diagram: befolkningsprognos - förändring per åldersgrupp
#'
#' Visar befolkningsförändring per åldersgrupp mellan ett startår (sista
#' året med faktisk befolkningsstatistik) och ett valfritt antal år framåt,
#' enligt SCB:s befolkningsprognos eller Region Dalarnas egna
#' Profet-prognos. Ett diagram per region, eller ett gemensamt
#' facet-diagram om `facet_variabel` sätts till `"region"`.
#'
#' @param region_vekt Region(er) att hämta prognos för. `"00"` ger riket.
#' @param eget_regionnamn Namn att använda i diagramrubriken. `NA` (standard)
#'   = hämtas automatiskt utifrån `region_vekt`.
#' @param jmfrtid Antal år i jämförelsen, räknat från startåret.
#' @param tabeller_url Antingen en eller flera https-url:er till SCB:s API,
#'   eller sökvägen till en mapp med lokala Profet-/Halland-prognosfiler.
#'   Se [rdhamtadata::hamta_befprognos_data()]. Standard pekar mot
#'   Profet-mappen på G: - faller automatiskt tillbaka till SCB:s API om
#'   den mappen inte finns.
#' @param facet_variabel `"region"` om regionerna ska läggas som facet i
#'   ett gemensamt diagram, annars `NA` (standard) för ett diagram per
#'   region.
#' @param facet_x_axis_storlek Textstorlek på x-axeln i facet-diagram.
#' @param aldersgrupper_vektor Åldersgrupper som används i diagrammet.
#'   Första siffran i varje intervall är startpunkten, så standardvärdet
#'   `c(0, 20, 66, 80)` ger grupperna 0-19, 20-64, 65-79 och 80+ år.
#' @param utmapp Mapp där diagram sparas. Måste finnas om
#'   `skriv_diagramfil = TRUE`.
#' @param gruppera_namn Namn att gruppera ihop de medskickade regionerna
#'   till en gemensam rad under, eller `NA` (standard) för ingen gruppering.
#' @param logga_sokvag Sökväg till logga.
#' @param logga_storlek Loggans storlek i diagrammet.
#' @param facet_scale `"free"` (standard) eller `"fixed"` y-axel i
#'   facet-diagram.
#' @param visa_logga_i_diagram TRUE om logga ska visas i diagrammet.
#' @param skriv_diagramfil TRUE om diagram ska skrivas till fil.
#' @param filformat Bildformat på diagrammet, t.ex. `"png"` (standard).
#' @param konsuppdelat Om `TRUE` delas diagrammet upp på kön. Stöds ännu
#'   inte fullt ut - lämna som `FALSE` (standard).
#' @param ta_bort_diagramtitel TRUE om diagramtitel ska tas bort.
#' @param anvand_senaste_befar Om `TRUE` används senaste tillgängliga år
#'   för befolkningsstatistik som startår, annars (standard) används året
#'   före första prognosår.
#' @param andel_istallet_for_antal Om `TRUE` visas andel i stället för
#'   antal för skillnaden mellan start- och slutår.
#' @param prognos_ar Prognosår att använda bland lokala Profet-filer.
#'   `"9999"` (standard) = senaste. Gäller inte SCB-grenen.
#' @param stodlinjer_avrunda_fem TRUE om stödlinjerna ska avrundas till
#'   närmaste femtal.
#' @param returnera_data Skall data returneras till global environment.
#' @param dataetiketter TRUE om dataetiketter ska visas i diagrammet.
#' @param spara_excelfil TRUE om underliggande data ska sparas som Excel-fil
#'   i `utmapp`.
#' @param diag_fargvekt Valfri färgvektor. `NA` (standard) ger
#'   `rddiagram::diagramfarger("kon")` om `konsuppdelat = TRUE`, annars
#'   `rddiagram::diagramfarger("rus_sex")`.
#' @param diagram_capt Diagrammets källtext. `"auto"` (standard) väljer
#'   text utifrån om data kommer från SCB:s API, Profet, eller båda.
#' @param returnera_figur Skall figur returneras (i en lista).
#'
#' @return En namngiven lista med `ggplot`-objekt.
#' @export
diag_befolkningsprognos_profet_scb <- function(region_vekt = "20",
                                   eget_regionnamn = NA,
                                   jmfrtid = 10,
                                   tabeller_url = "G:/Samhällsanalys/Statistik/Befolkningsprognoser/Profet/datafiler/",
                                   facet_variabel = NA,
                                   facet_x_axis_storlek = 5,
                                   aldersgrupper_vektor = c(0, 20, 66, 80),
                                   utmapp = rdverktyg::utskriftsmapp(),
                                   gruppera_namn = NA,
                                   logga_sokvag = NA,
                                   logga_storlek = 20,
                                   facet_scale = "free",
                                   visa_logga_i_diagram = TRUE,
                                   skriv_diagramfil = TRUE,
                                   filformat = "png",
                                   konsuppdelat = FALSE,
                                   ta_bort_diagramtitel = FALSE,
                                   anvand_senaste_befar = FALSE,
                                   andel_istallet_for_antal = FALSE,
                                   prognos_ar = "9999",
                                   stodlinjer_avrunda_fem = TRUE,
                                   returnera_data = FALSE,
                                   dataetiketter = FALSE,
                                   spara_excelfil = FALSE,
                                   diag_fargvekt = NA,
                                   diagram_capt = "auto",
                                   returnera_figur = TRUE
) {

  # ========================================== Allmän info ============================================
  # Befolkningsprognos per åldersgrupp, över tid eller jämfört med riket. Data hämtas antingen från
  # SCB:s öppna API eller från Region Dalarnas egna Profet-körningar (lokala filer på G:), via
  # rdhamtadata::hamta_befprognos_data(). Jämförelse mot riket kräver en egen tabell (riket saknar
  # regionuppdelning i SCB:s API) - se rdhamtadata::hamta_befprognos_riket_inr_utr_scb().
  # ========================================== Inställningar ============================================

  options(dplyr.summarise.inform = FALSE)
  options(scipen = 999)

  if (stringr::str_sub(filformat, 1, 1) != ".") filformat <- paste0(".", filformat)

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  # om det inte skickats med någon färgvektor så används färgvektorn "rus_sex" från funktionen diagramfärger
  if (all(is.na(diag_fargvekt))) {
    diag_fargvekt <- if (konsuppdelat) rddiagram::diagramfarger("kon") else rddiagram::diagramfarger("rus_sex")
  }

  # om mappen för datafiler från Profet eller Hallands befolkningsprognosskript inte finns så
  # används SCB:s API istället
  if (any(tabeller_url == "G:/Samhällsanalys/Statistik/Befolkningsprognoser/Profet/datafiler/") &
    !dir.exists("G:/Samhällsanalys/Statistik/Befolkningsprognoser/Profet/datafiler/")) {
    tabeller_url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefProgOsiktRegN"
  }

  # om könsuppdelat så får man bara skicka med en prognos, inte flera
  if (konsuppdelat & length(tabeller_url) > 1) stop("Om man skriver ut könsuppdelade diagram så kan endast en prognos användas. Korrigera parametern 'tabeller_url' så att den bara innehåller en url och inte flera.")

  # =============== jämförelse x år framåt från senaste tillgängliga år ==========================

  # Senaste år med befolkningsstatistik (TAB638 = historik, TAB5557 = CKM, se nya metoden för
  # röjandekontroll från 2025 och framåt - båda kollas här).
  senaste_ar_bef <- max(as.numeric(c(
    pxweb2r::pxweb2_get_values("TAB638", "Tid", quiet = TRUE)$code,
    pxweb2r::pxweb2_get_values("TAB5557", "Tid", quiet = TRUE)$code
  )))

  # jmfrtid räknas från "startår" (sista faktiska befolkningsstatistikåret, prognosår - 1) - inte från
  # själva prognosåret. "+N" i hamta_befprognos_data() betyder "prognosår + N", så vi ber om
  # jmfrtid - 1 för att landa på samma slutår (startår + jmfrtid).
  prognos_jmfr_ar <- if (anvand_senaste_befar) {
    senaste_ar_bef + jmfrtid
  } else {
    paste0("+", jmfrtid - 1)
  }

  # ========== Hämta befolkningsprognos för tabell(er) i vektor tabeller_url  ====================

  if (length(region_vekt[region_vekt != "00"]) > 0) {
    befprogn_reg_df <- rdhamtadata::hamta_befprognos_data(region_vekt = region_vekt[region_vekt != "00"],
                                             url_prognos_vektor = tabeller_url,
                                             kon_klartext = c("kvinnor", "män"),
                                             tid_vekt = prognos_jmfr_ar,
                                             cont_klartext = "Folkmängd",
                                             prognos_ar = prognos_ar
    )
  } else befprogn_reg_df <- NULL

  if ("00" %in% region_vekt) {
    hamta_prognos_ar <- min(as.numeric(pxweb2r::pxweb2_get_values("TAB5974", "Tid", quiet = TRUE)$code))

    befprogn_riket_df <- rdhamtadata::hamta_befprognos_riket_inr_utr_scb(
      tid_koder = (senaste_ar_bef + jmfrtid)) |>
      dplyr::mutate(prognos_ar = as.character(hamta_prognos_ar)) |>
      dplyr::select(-fodelseregion) |>
      dplyr::summarise(Folkmängd = sum(Antal), .by = c(regionkod, region, kön, ålder, år, prognos_ar))

    tabeller_url <- if (length(region_vekt) == 1 && region_vekt == "00") "api.scb.se" else c(tabeller_url, "api.scb.se")
  } else befprogn_riket_df <- NULL

  befprogn_df <- list(befprogn_reg_df, befprogn_riket_df) |>
    purrr::compact() |>              # tar bort NULL-element
    dplyr::bind_rows()

  # hantera diagram_capt
  if (diagram_capt == "auto") {
    diagram_capt <- dplyr::case_when(
      any(stringr::str_detect(tabeller_url, "api.scb.se")) & any(stringr::str_detect(tabeller_url, "Profet/datafiler")) ~
        "Källa: SCB:s befolkningsprognos och Region Dalarnas egna befolkningsprognos, bearbetning av Samhällsanalys, Region Dalarna",
      any(stringr::str_detect(tabeller_url, "api.scb.se")) ~
        "Källa: SCB:s befolkningsprognos\nBearbetning: Samhällsanalys, Region Dalarna",
      any(stringr::str_detect(tabeller_url, "Profet/datafiler")) ~
        "Källa: Region Dalarnas egna befolkningsprognos\nBearbetning: Samhällsanalys, Region Dalarna"
    )
  }

  # Här skapar vi en rad med total folkmängd i dfmalar ==========================================
  total_df <- befprogn_df |>
    dplyr::group_by(regionkod, region, kön, år, prognos_ar) |>
    dplyr::summarize(dplyr::across(dplyr::where(is.numeric), ~ sum(.x, na.rm = TRUE))) |>
    dplyr::ungroup() |>
    dplyr::mutate(ålder = "totalt ålder")

  # Lägg på total_df som rad på dfmalar
  befprogn_df <- dplyr::bind_rows(befprogn_df, total_df)

  prognosar <- unique(befprogn_df$prognos_ar)                              # lägg prognosår(en) i en vektor
  startar <- as.character(as.numeric(prognosar) - 1)                       # lägg startår(en) i en vektor
  slutar <- as.character(as.numeric(startar) + jmfrtid)                    # lägg slutår(en) i en vektor

  # =========================== hämta senaste tillgängliga år i befolkingsstatistiken ============================

  # Civilstånd saknar en riktig "totalt"-kod i TAB638 (bara fyra individuella civilstånd) - alla fyra
  # hämtas explicit och summeras ihop. Ålder hämtas som alla individuella åldrar plus en explicit
  # "totalt"-kod. CKM-tabellen (TAB5557) dubblerar annars "totalt"-etiketten över fyra koder
  # (TotSA/TOT1/TOT10/TOT5) - "TotSA" är koden som används konsekvent här.
  hamta_individuella_aldrar <- function(table_id) {
    pxweb2r::pxweb2_get_values(table_id, "Alder", quiet = TRUE) |>
      dplyr::filter(grepl("^[0-9]+\\+? år$", label)) |>
      dplyr::filter(!duplicated(label)) |>
      dplyr::pull(code)
  }
  civilstand_hamta <- c("ogifta", "gifta", "skilda", "änkor/änklingar")

  px_df_bef_historik <- pxweb2r::pxweb2_get_data(
    table = "TAB638",
    query = list(Region = region_vekt, Civilstand = civilstand_hamta,
                 Alder = c(hamta_individuella_aldrar("TAB638"), "tot"), Kon = c("män", "kvinnor"),
                 ContentsCode = "Folkmängd", Tid = startar),
    on_all_values_invalid = "null", quiet = TRUE)
  px_df_bef_ckm <- pxweb2r::pxweb2_get_data(
    table = "TAB5557",
    query = list(Region = region_vekt, Civilstand = civilstand_hamta,
                 Alder = c(hamta_individuella_aldrar("TAB5557"), "TotSA"), Kon = c("män", "kvinnor"),
                 ContentsCode = "Folkmängd", Tid = startar),
    on_all_values_invalid = "null", quiet = TRUE)

  px_df_bef <- dplyr::bind_rows(px_df_bef_historik, px_df_bef_ckm) |>
    dplyr::rename(regionkod = region_kod, Folkmängd = value) |>
    dplyr::select(-tabellinnehåll) |>
    dplyr::mutate(ålder = ifelse(ålder == "totalt, samtliga åldrar", "totalt ålder", ålder),
           # SCB:s klartext för regionkod "00" är "Riket", men rdhamtadata::hamta_befprognos_riket_inr_utr_scb()
           # ger "Sverige" för samma regionkod - normaliserar till "Sverige" här så de hamnar i samma
           # grupp vid group_by(region, ...) + pivot_wider() längre ner (annars blir start-/slutårsdata
           # för riket uppdelade på två separata rader, vilket ger orimliga värden som -100 %/Inf %).
           region = ifelse(regionkod == "00", "Sverige", region)) |>
    dplyr::group_by(dplyr::across(-c(civilstånd, Folkmängd))) |>
    dplyr::summarise(Folkmängd = sum(Folkmängd, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(prognos_ar = as.character(as.numeric(år) + 1))

  # ================= lägg ihop och bearbeta prognos- och befolkningsdata ==================

  # skapa en df med både prognosvärden och befolkningssiffror för senaste året innan prognosen startar
  progn_bef <- dplyr::bind_rows(px_df_bef, befprogn_df)

  suppressWarnings(progn_bef <- dplyr::mutate(progn_bef, aldernum = readr::parse_number(ålder)))

  # Lägg ihop i åldersgrupper
  progn_bef <- progn_bef |>
    dplyr::mutate(aldergrp = rdverktyg::skapa_aldersgrupper(aldernum, aldersgrupper_vektor),
           aldergrp = ifelse(is.na(aldergrp), "totalt", as.character(aldergrp)),
           aldernum = ifelse(is.na(aldernum), -1, aldernum),
           start_ar = as.character(as.numeric(prognos_ar) - 1),
           slut_ar = as.character(as.numeric(start_ar) + jmfrtid),
           ar_beskr = paste0("Förändring ", start_ar, "-", slut_ar, " (prognos våren ",
                              prognos_ar, ")"),
           Folkmängd = round(Folkmängd))

  # skapa en vektor av åldrar som ligger i ordning och där totalt ligger sist
  sort_vekt <- progn_bef |>
    dplyr::group_by(aldergrp) |>
    dplyr::summarise(sort_num = min(aldernum, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(sort_num) |>
    dplyr::pull(aldergrp)

  progn_bef <- dplyr::mutate(progn_bef, aldergrp = factor(aldergrp, levels = sort_vekt))

  # om vi inte vill ha könsuppdelad data (stöds ännu inte fullt ut) så tar vi bort det här
  if (!konsuppdelat) {
    progn_bef <- progn_bef |>
      dplyr::group_by(dplyr::across(-c(kön, Folkmängd))) |>
      dplyr::summarise(Folkmängd = sum(Folkmängd, na.rm = TRUE)) |>
      dplyr::ungroup()
  }

  # om vi vill gruppera ihop regionerna till en så gör vi det här
  if (!is.na(gruppera_namn)) {
    progn_bef <- progn_bef |>
    dplyr::group_by(dplyr::across(c(-region, -regionkod, -Folkmängd))) |>
    dplyr::summarise(Folkmängd = sum(Folkmängd)) |>
    dplyr::mutate(regionkod = "grp", region = gruppera_namn) |>
    dplyr::ungroup()

    eget_regionnamn <- gruppera_namn
  }
  # beräkna skillnad mellan startår och slutår, både som antal och som andel
  prognos_diff_df <- progn_bef |>
    dplyr::group_by(dplyr::across(-c(ålder, aldernum, Folkmängd))) |>
    dplyr::summarise(Folkmängd = sum(Folkmängd, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(names_from = år, values_from = Folkmängd) |>
    dplyr::mutate(antal = rowSums(dplyr::pick(dplyr::all_of(slutar)), na.rm = TRUE) - rowSums(dplyr::pick(dplyr::all_of(startar)), na.rm = TRUE),
           andel = round((rowSums(dplyr::pick(dplyr::all_of(slutar)), na.rm = TRUE) - rowSums(dplyr::pick(dplyr::all_of(startar)), na.rm = TRUE)) /
                           rowSums(dplyr::pick(dplyr::all_of(startar)), na.rm = TRUE) * 100, 1),
           aldergrp = factor(aldergrp, levels = sort_vekt))

  if (returnera_data) {
    assign("bef_progn_nms_df", prognos_diff_df, envir = .GlobalEnv)
  }

  # skriv ut själva diagrammen
  if (!is.na(facet_variabel)) {

    gg_list <- intern_skrivut_befprognos_diagram(skickad_df = prognos_diff_df,
                                   regionkoder = unique(prognos_diff_df$regionkod),
                                   skickad_jmfrtid = jmfrtid,
                                   facet_var = facet_variabel,
                                   facet_scale = facet_scale,
                                   facet_x_axis_storlek = facet_x_axis_storlek,
                                   eget_regionnamn = eget_regionnamn,
                                   farger_diagram = diag_fargvekt,
                                   konsuppdelat = konsuppdelat,
                                   dataetiketter = dataetiketter,
                                   output_fold = utmapp,
                                   skapa_fil = skriv_diagramfil,
                                   filformat = filformat,
                                   spara_excelfil = spara_excelfil,
                                   logga_storlek = logga_storlek,
                                   ta_med_logga = visa_logga_i_diagram,
                                   logga_path = logga_sokvag,
                                   stodlinjer_avrunda_fem = stodlinjer_avrunda_fem,
                                   diagram_capt = diagram_capt,
                                   utan_diagramtitel = ta_bort_diagramtitel,
                                   skickad_andel = andel_istallet_for_antal,
                                   x_var = "aldergrp",
                                   filnamn_typ = "befprogn_")
  } else {
    gg_list <- purrr::flatten(purrr::map(unique(prognos_diff_df$regionkod),
                         ~ intern_skrivut_befprognos_diagram(skickad_df = prognos_diff_df,
                                         regionkoder = .x,
                                         skickad_jmfrtid = jmfrtid,
                                         facet_var = NA,
                                         facet_scale = NA,
                                         facet_x_axis_storlek = facet_x_axis_storlek,
                                         eget_regionnamn = eget_regionnamn,
                                         farger_diagram = diag_fargvekt,
                                         konsuppdelat = konsuppdelat,
                                         dataetiketter = dataetiketter,
                                         output_fold = utmapp,
                                         skapa_fil = skriv_diagramfil,
                                         filformat = filformat,
                                         spara_excelfil = spara_excelfil,
                                         logga_storlek = logga_storlek,
                                         ta_med_logga = visa_logga_i_diagram,
                                         logga_path = logga_sokvag,
                                         stodlinjer_avrunda_fem = stodlinjer_avrunda_fem,
                                         diagram_capt = diagram_capt,
                                         utan_diagramtitel = ta_bort_diagramtitel,
                                         skickad_andel = andel_istallet_for_antal,
                                         x_var = "aldergrp",
                                         filnamn_typ = "befprogn_")
                         ))
  }

  if (returnera_figur == TRUE) return(gg_list)
}
