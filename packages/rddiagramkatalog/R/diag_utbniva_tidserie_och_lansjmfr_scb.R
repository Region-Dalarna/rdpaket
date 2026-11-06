#' Diagram: utbildningsnivå över tid och länsjämförelse
#'
#' Fyra diagram totalt. Data kommer från SCB: UF/UF0506/UF0506B/Utbildning,
#' som i v2 motsvaras av TAB3981 ("Befolkning 16–74 år efter region,
#' utbildningsnivå, ålder och kön. 1985-2025").
#'
#' Tre diagram över hur utbildningsnivån har förändrats över tid + ett
#' diagram där andel eftergymnasialt utbildade jämförs mellan länen för
#' ett år:
#'
#' 1. Andelen högutbildade i befolkningen 25-64 år från 1985 och framåt.
#' 2. Andelen lågutbildade i befolkningen 25-64 år från 1985 och framåt.
#' 3. Andelen per utbildningsnivå (förgymn, gymn, kortare eftergymn,
#'    längre eftergymn) år 1985, 1990, 2000, 2010 och senaste år, eller
#'    annat valfritt år som man skickar med i `valt_ar`.
#' 4. Andel eftergymnasialt utbildade i befolkningen per kön 25-64 år
#'    för valt år, jämförelse mellan länen.
#'
#' @param region_vekt Fler regioner = facet-diagram med en facet per region.
#' @param utmapp Mapp där figurerna sparas. Måste finnas om `skriv_diagramfil = TRUE` eller `spara_data = TRUE`.
#' @param diagram_capt Beskrivning av diagrammet.
#' @param skriv_diagramfil Om man vill skriva diagrammen till png-filer, annars returneras bara ggplot-objekt.
#' @param gruppera_namn `NA` om man vill lägga regioner som facet-diagram, om ett namn skickas med så läggs regionerna ihop till en som döps till namnet som skickas med.
#' @param valt_ar Om man vill ha något annat år än senaste år i diagram.
#' @param diagramtitel_tabort Tar bort diagramtiteln.
#' @param diagram_capt_tabort Tar bort diagram-caption.
#' @param visa_logga_i_diagram TRUE om logga ska visas i diagrammet.
#' @param logga_sokvag Sökväg till logga.
#' @param etiketter_xaxel Visa var x:e etikett för diagram 1 och 2 (högutb/lågutb över tid).
#' @param sverige_istallet_for_riket Byt ut "Riket" mot "Sverige" i regionnamn.
#' @param facet_x_axis_storlek Textstorlek på x-axelns etiketter i facetdiagram.
#' @param region_lagg_forst Om man vill lägga någon eller några regioner först bland facetdiagrammen så skickar man med dem här i den ordning man vill ha dem.
#' @param diag_hogutb_over_tid Diagram 1 ovan, TRUE om man vill ha med det, annars FALSE.
#' @param diag_lagutb_over_tid Diagram 2 ovan, TRUE om man vill ha med det, annars FALSE.
#' @param diag_andel_alla_utbnivaer Diagram 3 ovan, TRUE om man vill ha med det, annars FALSE.
#' @param diag_andel_utbniva_jmfr_lan Diagram 4 ovan, TRUE om man vill ha med det, annars FALSE.
#' @param vald_utb_niva Används i diagram 4, `"eftergymn"` är förvalt men finns också `"hogutb"`, `"gymn"` och `"forgymn"`.
#' @param spara_data TRUE om man vill spara dataseten bakom de diagram som skrivs ut till en Excelfil, en flik per diagram. Fliknamnen byggs av samma namn som diagramfilerna (utan ".png"), trunkerat till Excels gräns på 31 tecken.
#' @param filnamn_data Filnamn för Excelfilen (skrivs till `utmapp`). `NA` = döps automatiskt utifrån vald region.
#'
#' @return En namngiven lista med `ggplot`-objekt.
#' @export
diag_utbniva_tidserie_och_lansjmfr_scb <- function(
                                       region_vekt = c("00", "20"),
                                       utmapp = rdverktyg::utskriftsmapp(),
                                       diagram_capt = "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna",
                                       skriv_diagramfil = TRUE,
                                       gruppera_namn = NA,
                                       valt_ar = NA,
                                       diagramtitel_tabort = FALSE,
                                       diagram_capt_tabort = FALSE,
                                       visa_logga_i_diagram = TRUE,
                                       logga_sokvag = NA,
                                       etiketter_xaxel = NA,
                                       sverige_istallet_for_riket = TRUE,
                                       facet_x_axis_storlek = 8,
                                       region_lagg_forst = NA,
                                       diag_hogutb_over_tid = TRUE,
                                       diag_lagutb_over_tid = FALSE,
                                       diag_andel_alla_utbnivaer = TRUE,
                                       diag_andel_utbniva_jmfr_lan = FALSE,
                                       vald_utb_niva = "eftergymn",
                                       spara_data = FALSE,
                                       filnamn_data = NA){

  # =======================================================================================================
  #
  # Fyra diagram totalt. Data kommer från SCB: UF/UF0506/UF0506B/Utbildning, som i v2 motsvaras av TAB3981
  # ("Befolkning 16–74 år efter region, utbildningsnivå, ålder och kön. 1985-2025").
  #
  # Tre diagram över hur utbildningsnivån har förändrats över tid + ett diagram där andel eftergymnasialt
  # utbildade jämförs mellan länen för ett år
  # =======================================================================================================

  options(dplyr.summarise.inform = FALSE)

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil || spara_data)

  gg_list <- list()       # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  dataset_list <- list()  # dataseten bakom diagrammen, ifyllt bara om spara_data = TRUE

  # bygger ett giltigt och unikt Excel-fliknamn (max 31 tecken, inga otillåtna tecken) utifrån
  # diagramfilnamnet
  saklig_fliknamn <- function(namn, tagna_namn) {
    namn <- gsub("[\\\\/?*\\[\\]:]", "_", namn, perl = TRUE)  # otillåtna tecken i Excel-fliknamn
    namn <- substr(namn, 1, 31)
    unikt_namn <- namn
    i <- 1
    while (unikt_namn %in% tagna_namn) {
      tillagg <- paste0("_", i)
      unikt_namn <- paste0(substr(namn, 1, 31 - nchar(tillagg)), tillagg)
      i <- i + 1
    }
    unikt_namn
  }

  tabell_id <- "TAB3981"

  hamta_utbniva <- function(region_vekt, kon_klartext, alder_vekt, utbildningsniva_klartext, tid_vekt) {
    giltiga_ar <- pxweb2r::pxweb2_get_values(tabell_id, "Tid", quiet = TRUE)$code
    tid_var <- if (identical(tid_vekt, "*")) giltiga_ar else as.character(tid_vekt)[as.character(tid_vekt) %in% giltiga_ar]

    px <- pxweb2r::pxweb2_get_data(
      table = tabell_id,
      query = list(
        Region = region_vekt,
        Alder = alder_vekt,
        UtbildningsNiva = utbildningsniva_klartext,
        Kon = kon_klartext,
        ContentsCode = "UF0506A1",
        Tid = tid_var
      ),
      on_all_values_invalid = "null", quiet = TRUE)

    dplyr::rename(px, regionkod = region_kod, Befolkning = value) |>
      dplyr::select(-tabellinnehåll)
  }

  alla_giltiga_ar <- pxweb2r::pxweb2_get_values(tabell_id, "Tid", quiet = TRUE)$code

  # Bygger en kort, filnamnsvänlig region_txt. Känns urvalet igen som "alla län i Sverige" eller
  # "alla kommuner i ett län" (t.ex. Dalarnas 15 kommuner) blir det en kort etikett i stället för
  # att alla koder radas upp - annars (ett godtyckligt/slumpmässigt urval) listas koderna som
  # tidigare, men bara om det inte blir orimligt långt.
  alla_lan_txt <- rdverktyg::ar_alla_lan_i_sverige(region_vekt, returnera_text = TRUE)
  alla_komm_txt <- if (isFALSE(alla_lan_txt)) rdverktyg::ar_alla_kommuner_i_ett_lan(region_vekt, returnera_text = TRUE) else FALSE
  kant_namn <- if (!isFALSE(alla_lan_txt)) alla_lan_txt else if (!isFALSE(alla_komm_txt)) alla_komm_txt else NA

  if (!is.na(kant_namn)) {
    region_txt <- stringr::str_replace_all(tolower(kant_namn), " ", "_")
    if ("00" %in% region_vekt) region_txt <- paste0(region_txt, "_riket")
  } else {
    region_txt <- paste0(region_vekt, collapse = "_")
    if (nchar(region_txt) > 40) region_txt <- paste0(length(region_vekt), "regioner")
  }

  if (length(valt_ar) > 1) print("Endast ett år kan skickas med i funktionen, bara första året i vektorn kommer att användas.")
  if (is.na(valt_ar[1])) valt_ar <- max(alla_giltiga_ar) else {
    valt_ar <- valt_ar[1]
    if (!valt_ar %in% alla_giltiga_ar) valt_ar <- max(alla_giltiga_ar)
  }

  # ========================================== Läser in data ============================================
  px_df <- hamta_utbniva(region_vekt = region_vekt,
                                kon_klartext = c("män","kvinnor"),
                                alder_vekt = c(as.character(25:64)),
                                utbildningsniva_klartext = "*",
                                tid_vekt = "*")

  px_df_utskrift_kon <- px_df |>
    dplyr::filter(regionkod %in% region_vekt) |>
    dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region, byt_ut_riket_mot_sverige = sverige_istallet_for_riket))

  if (!is.na(gruppera_namn)) {
    px_df_utskrift_kon <- px_df_utskrift_kon |>
      dplyr::group_by(år, ålder, kön, utbildningsnivå) |>
      dplyr::summarise(Befolkning = sum(Befolkning, na.rm = TRUE)) |>
      dplyr::ungroup() |>
      dplyr::mutate(regionkod = "grp",
             region = gruppera_namn) |>
      dplyr::relocate(regionkod, .before = 1) |>
      dplyr::relocate(region, .after = regionkod)

    region_txt <- gruppera_namn                           # för att få rätt regionnamn i titel, filnamn etc.
    region_vekt <- c("grp", region_vekt)

  } # slut if-sats om man vill gruppera

  # om vi vill sortera om facet-diagrammen så gör vi det här
  if (!is.na(region_lagg_forst[1]) & length(unique(px_df_utskrift_kon$region)) > 1){
    # testa om region_lagg_forst-koderna finns i datasetet
    if (any(region_lagg_forst %in% unique(px_df_utskrift_kon$regionkod))) {

      regionkoder_i_df <- unique(px_df_utskrift_kon$regionkod)
      region_i_df <- unique(px_df_utskrift_kon$region)
      # skapa sorteringsvektorer
      reg_ej_fokus <- sort(unique(region_i_df[!regionkoder_i_df %in% region_lagg_forst]))
      reg_fokus <- unique(region_i_df[regionkoder_i_df %in% region_lagg_forst])        # bara de regioner som finns i datasetet kommer med
      reg_fokus_koder <- unique(regionkoder_i_df[regionkoder_i_df %in% region_lagg_forst])        # bara de regioner som finns i datasetet kommer med
      reg_fokus <- reg_fokus[match(region_lagg_forst, reg_fokus_koder)]               # sortera utifrån ordningen i region_lagg_forst
      region_sort <- c(reg_fokus, reg_ej_fokus)

      # sortera med hjälp av region_sort som vi skapade ovan
      px_df_utskrift_kon <- dplyr::mutate(px_df_utskrift_kon, region = factor(region, levels = region_sort))
    } # slut if-sats för att testa om region_lagg_forst-koderna finns i datasetet
  } # slut if-sats om vi skickat med lagg_forst_koder

  # bearbeta datasetet
  px_df_utskrift_kon <- px_df_utskrift_kon |>
    dplyr::mutate(utb_niva = dplyr::case_when(
      utbildningsnivå == "förgymnasial utbildning kortare än 9 år" ~ "Förgymnasial utbildning",
      utbildningsnivå == "förgymnasial utbildning, 9 (10) år" ~ "Förgymnasial utbildning",
      utbildningsnivå == "gymnasial utbildning, högst 2 år" ~ "Gymnasial utbildning",
      utbildningsnivå == "gymnasial utbildning, 3 år" ~ "Gymnasial utbildning",
      utbildningsnivå == "eftergymnasial utbildning, mindre än 3 år" ~ "Eftergymnasial utbildning, mindre än 3 år",
      utbildningsnivå == "eftergymnasial utbildning, 3 år eller mer"~ "Eftergymnasial utbildning, 3 år eller mer",
      utbildningsnivå == "forskarutbildning" ~ "Eftergymnasial utbildning, 3 år eller mer",
      utbildningsnivå == "uppgift om utbildningsnivå saknas" ~ "Uppgift saknas"),
      utb_niva = factor(utb_niva, levels = c("Eftergymnasial utbildning, 3 år eller mer","Eftergymnasial utbildning, mindre än 3 år","Gymnasial utbildning","Förgymnasial utbildning"))) |>
    dplyr::filter(utb_niva != "Uppgift saknas") |>
    dplyr::group_by(år, regionkod, region, kön, utb_niva) |>
    dplyr::summarize(antal = sum(Befolkning, na.rm = TRUE)) |>
    dplyr::mutate(andel = (antal/sum(antal)) * 100) |>
    dplyr::ungroup()

  # Tar bort uppgift saknas och beräknar hur stor andel som har en viss utbildning - uppdelat på kön
  px_df_utskrift <- px_df_utskrift_kon |>
    dplyr::filter(utb_niva != "Uppgift saknas") |>
    dplyr::group_by(år, regionkod, region, utb_niva) |>
    dplyr::summarize(antal = sum(antal, na.rm = TRUE)) |>
    dplyr::mutate(andel = (antal/sum(antal)) * 100) |>
    dplyr::ungroup()

  region_titel <- if (length(region_vekt) > 1) "" else paste0(" i ", unique(px_df_utskrift$region))

    if (diag_hogutb_over_tid) {
    diagramtitel <- stringr::str_wrap(paste0("Andel högutbildade invånare 25-64 år", region_titel))
    diagramfilnamn <- paste0("hogutb_andel_ar", region_txt, "_", min(px_df_utskrift_kon$år), "_", max(px_df_utskrift_kon$år), ".png")

    if (diagram_capt_tabort == TRUE){
      diagram_capt_hogutb <- NULL
    }else{
      diagram_capt_hogutb <- paste0(diagram_capt, "\nDefinitionen av högutbildade är individer med minst 3 års eftergymnasial utbildning.")
    }

    diagram_data <- dplyr::filter(px_df_utskrift_kon,
                                   regionkod %in% region_vekt,
                                   utb_niva == "Eftergymnasial utbildning, 3 år eller mer")

    gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = diagram_data,
                                 skickad_x_var = "år",
                                 skickad_y_var = "andel",
                                 skickad_x_grupp = "kön",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = rddiagram::diagramfarger("kon"),
                                 diagram_titel = if (diagramtitel_tabort) NULL else diagramtitel,
                                 diagram_capt =  diagram_capt_hogutb,
                                 facet_grp = if (length(region_vekt) > 1) "region" else NULL,
                                 facet_scale = "fixed",
                                 facet_x_axis_storlek = facet_x_axis_storlek,
                                 stodlinjer_avrunda_fem = TRUE,
                                 facet_legend_bottom = TRUE,
                                 x_axis_visa_var_xe_etikett = etiketter_xaxel,
                                 lagg_pa_logga = visa_logga_i_diagram,
                                 logga_path = logga_sokvag,
                                 legend_vand_ordning=TRUE,
                                 manual_y_axis_title="procent",
                                 output_mapp = utmapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skriv_diagramfil)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[length(gg_list)] <- stringr::str_remove(diagramfilnamn, ".png")

    if (spara_data) {
      fliknamn <- saklig_fliknamn(stringr::str_remove(diagramfilnamn, ".png"), names(dataset_list))
      dataset_list[[fliknamn]] <- diagram_data
    }
    } # slut if-sats om man vill skriva ut diagram över utvecklingen av högutbildade över tid

  if (diag_lagutb_over_tid) {
    diagramtitel <- stringr::str_wrap(paste0("Andel lågutbildade invånare 25-64 år", region_titel))
    diagramfilnamn <- paste0("lagutb_andel_ar", region_txt, "_", min(px_df_utskrift_kon$år), "_", max(px_df_utskrift_kon$år), ".png")

    if (diagram_capt_tabort == TRUE){
      diagram_capt_lagutb <- NULL
    }else{
      diagram_capt_lagutb <- paste0(diagram_capt, "\nDefinitionen av lågutbildade är individer med endast förgymnasial utbildning.")
    }

    diagram_data <- dplyr::filter(px_df_utskrift_kon,
                                   regionkod %in% region_vekt,
                                   utb_niva == "Förgymnasial utbildning")

    gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = diagram_data,
                                 skickad_x_var = "år",
                                 skickad_y_var = "andel",
                                 skickad_x_grupp = "kön",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = rddiagram::diagramfarger("kon"),
                                 stodlinjer_avrunda_fem = TRUE,
                                 diagram_titel = if (diagramtitel_tabort) NULL else diagramtitel,
                                 diagram_capt =  diagram_capt_lagutb,
                                 facet_grp = if (length(region_vekt) > 1) "region" else NULL,
                                 facet_scale = "fixed",
                                 facet_legend_bottom = TRUE,
                                 facet_x_axis_storlek = facet_x_axis_storlek,
                                 x_axis_visa_var_xe_etikett = etiketter_xaxel,
                                 lagg_pa_logga = visa_logga_i_diagram,
                                 logga_path = logga_sokvag,
                                 legend_vand_ordning=TRUE,
                                 manual_y_axis_title="procent",
                                 output_mapp = utmapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skriv_diagramfil)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[length(gg_list)] <- stringr::str_remove(diagramfilnamn, ".png")

    if (spara_data) {
      fliknamn <- saklig_fliknamn(stringr::str_remove(diagramfilnamn, ".png"), names(dataset_list))
      dataset_list[[fliknamn]] <- diagram_data
    }
  } # slut if-sats om man vill skriva ut diagram över utvecklingen av högutbildade över tid


  if (diag_andel_alla_utbnivaer) {
    diagramtitel <- stringr::str_wrap(paste0("Utbildningsnivå för invånare 25-64 år", region_titel))
    diagramfilnamn <- paste0("utbniva_andel_per_ar_", region_txt, "_", valt_ar, ".png")

    diagram_capt_niva <- if (diagram_capt_tabort == TRUE) NULL else diagram_capt

    diagram_data <- dplyr::mutate(
                       dplyr::filter(px_df_utskrift, regionkod %in% region_vekt,
                              år %in% c("1985", "1990", "2000", "2010", valt_ar)),
                       andel = andel - 0.001)

    gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = diagram_data,
                                 skickad_x_var = "år",
                                 skickad_y_var = "andel",
                                 skickad_x_grupp = "utb_niva",
                                 manual_color = rddiagram::diagramfarger("rd_bla"),
                                 diagram_titel = if (diagramtitel_tabort) NULL else diagramtitel,
                                 diagram_capt =  diagram_capt_niva,
                                 facet_grp = if (length(region_vekt) > 1) "region" else NULL,
                                 facet_legend_bottom = TRUE,
                                 facet_scale = "fixed",
                                 facet_x_axis_storlek = facet_x_axis_storlek,
                                 geom_position_stack = TRUE,
                                 procent_0_100_10intervaller = TRUE,
                                 x_axis_lutning = 0,
                                 lagg_pa_logga = visa_logga_i_diagram,
                                 logga_path = logga_sokvag,
                                 legend_vand_ordning=TRUE,
                                 manual_y_axis_title="procent",
                                 manual_x_axis_title = "år",
                                 output_mapp = utmapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skriv_diagramfil)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[length(gg_list)] <- stringr::str_remove(diagramfilnamn, ".png")

    if (spara_data) {
      fliknamn <- saklig_fliknamn(stringr::str_remove(diagramfilnamn, ".png"), names(dataset_list))
      dataset_list[[fliknamn]] <- diagram_data
    }
  } # slut if-sats om man vill skriva ut diagram över alla utbildningsnivåer

  if (diag_andel_utbniva_jmfr_lan){

    px_df_jmfr_lan <- hamta_utbniva(region_vekt = rdverktyg::hamtaAllaLan(FALSE),
                                    kon_klartext = "*",
                                    alder_vekt = c(as.character(25:64)),
                                    utbildningsniva_klartext = "*",
                                    tid_vekt = valt_ar) |>
      dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region, byt_ut_riket_mot_sverige = sverige_istallet_for_riket))

    # bearbeta datasetet
    px_df_jmfr_lan <- px_df_jmfr_lan |>
      dplyr::mutate(utb_niva = dplyr::case_when(
        utbildningsnivå == "förgymnasial utbildning kortare än 9 år" ~ "Förgymnasial utbildning",
        utbildningsnivå == "förgymnasial utbildning, 9 (10) år" ~ "Förgymnasial utbildning",
        utbildningsnivå == "gymnasial utbildning, högst 2 år" ~ "Gymnasial utbildning",
        utbildningsnivå == "gymnasial utbildning, 3 år" ~ "Gymnasial utbildning",
        utbildningsnivå == "eftergymnasial utbildning, mindre än 3 år" ~ "Eftergymnasial utbildning, mindre än 3 år",
        utbildningsnivå == "eftergymnasial utbildning, 3 år eller mer"~ "Eftergymnasial utbildning, 3 år eller mer",
        utbildningsnivå == "forskarutbildning" ~ "Eftergymnasial utbildning, 3 år eller mer",
        utbildningsnivå == "uppgift om utbildningsnivå saknas" ~ "Uppgift saknas"),
        utb_niva = factor(utb_niva, levels = c("Eftergymnasial utbildning, 3 år eller mer","Eftergymnasial utbildning, mindre än 3 år","Gymnasial utbildning","Förgymnasial utbildning"))) |>
      dplyr::filter(utb_niva != "Uppgift saknas") |>
      dplyr::group_by(år, regionkod, region, kön, utb_niva) |>
      dplyr::summarize(antal = sum(Befolkning, na.rm = TRUE)) |>
      dplyr::mutate(andel = (antal/sum(antal)) * 100) |>
      dplyr::ungroup()

    if (vald_utb_niva == "hogutb") {

      utb_niva_vec = c("Eftergymnasial utbildning, 3 år eller mer")
      dia_titel_txt = "minst 3 års eftergymnasial"
      dia_filnamn_txt = "hog"

    } else if (vald_utb_niva == "eftergymn") {

      utb_niva_vec <- c("Eftergymnasial utbildning, mindre än 3 år",
                        "Eftergymnasial utbildning, 3 år eller mer")
      dia_titel_txt = "eftergymnasial"
      dia_filnamn_txt = "eftergymn_"

    } else if (vald_utb_niva == "gymn") {

      utb_niva_vec <- c("Gymnasial utbildning")
      dia_titel_txt = "gymnasial"
      dia_filnamn_txt = "gymn_"

    } else if (vald_utb_niva == "forgymn") {

      utb_niva_vec <- c("Förgymnasial utbildning")
      dia_titel_txt = "förgymnasial"
      dia_filnamn_txt = "forgymn_"

    } else {
      stop("Felaktig parameter för vald_utb_niva. Välj mellan 'hogutb', 'eftergymn', 'gymn', 'forgymn' eller 'alla'.")
    }

    diagramtitel <- glue::glue("Andel invånare 25-64 år med {dia_titel_txt} utbildning år {valt_ar}")
    diagramfilnamn <- glue::glue("{dia_filnamn_txt}utb_andel_ar_", valt_ar, ".png")

    diagram_capt_jmfr <- if (diagram_capt_tabort == TRUE) NULL else diagram_capt

    diagram_data <- dplyr::filter(px_df_jmfr_lan, utb_niva %in% utb_niva_vec)

    gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = diagram_data,
                                 skickad_x_var = "region",
                                 skickad_y_var = "andel",
                                 skickad_x_grupp = "kön",
                                 diagram_liggande = TRUE,
                                 manual_color = rddiagram::diagramfarger("kon"),
                                 diagram_titel = if (diagramtitel_tabort) NULL else diagramtitel,
                                 diagram_capt =  diagram_capt_jmfr,
                                 stodlinjer_avrunda_fem = TRUE,
                                 facet_legend_bottom = TRUE,
                                 x_axis_lutning = 0,
                                 x_axis_sort_value = TRUE,
                                 lagg_pa_logga = visa_logga_i_diagram,
                                 logga_path = logga_sokvag,
                                 manual_y_axis_title="procent",
                                 output_mapp = utmapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skriv_diagramfil)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[length(gg_list)] <- stringr::str_remove(diagramfilnamn, ".png")

    if (spara_data) {
      fliknamn <- saklig_fliknamn(stringr::str_remove(diagramfilnamn, ".png"), names(dataset_list))
      dataset_list[[fliknamn]] <- diagram_data
    }

  } # slut if-sats om man vill skriva ut länsjämförelsediagram

  if (spara_data && length(dataset_list) > 0) {
    if (is.na(filnamn_data)) filnamn_data <- paste0("utbniva_diagramdata_", region_txt, ".xlsx")
    rdverktyg::excelfil_spara_formaterad(indata = dataset_list, output_mapp = utmapp,
                                         excelfil_namn = filnamn_data)
  }

  return(gg_list)

} # slut funktion
