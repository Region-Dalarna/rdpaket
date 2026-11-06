#' Diagram: förändring i antal förvärvsarbetande per bransch
#'
#' Skapar diagram där antalet förvärvsarbetande i olika branscher jämförs
#' mellan två år, senast och näst senaste år. Jämförelsen görs baserat på
#' den senaste månaden för vilken det finns statistik. Diagrammet kan
#' skapas per vald region/kommun eller som ett facet-diagram för alla
#' regioner/kommuner.
#'
#' @param region_vekt Region vi är intresserade av.
#' @param utmapp Mapp där figuren sparas. Måste finnas om `skriv_diagramfil = TRUE`.
#' @param filnamn_data OBS: oanvänd i originalet (ingen Excel-export finns i funktionen), preserverad oförändrad.
#' @param diag_facet Skapa ett facetdiagram för alla regioner/kommuner i stället för ett diagram per region.
#' @param diag_fargvekt Vilka färger skall användas i diagram.
#' @param skriv_diagramfil Om TRUE sparas figuren till `utmapp`.
#' @param diagram_capt Diagrammets källtext.
#' @param returnera_figur Skall figur returneras (i en lista).
#'
#' @return En namngiven lista med `ggplot`-objekt.
#' @export
diag_sysselsatta_forandring_bransch_scb <- function(region_vekt = "20",
                                                utmapp = rdverktyg::utskriftsmapp(),
                                                filnamn_data = "andel_forvarvsarbetande.xlsx",
                                                diag_facet = FALSE,
                                                diag_fargvekt = rddiagram::diagramfarger("rus_sex"),
                                                skriv_diagramfil = TRUE,
                                                diagram_capt = "Källa: BAS i SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Sysselsatta efter arbetsställets belägenhet (preliminär statistik).",
                                                returnera_figur = TRUE) {

  # ========================================== Allmän info ============================================
  # Skapar diagram där antalet förvärvsarbetande i olika banscher jämförs mellan två år, senast och näst senaste år. Jämförelsen görs
  # baserat på den senast månaden för vilken det finns statistik. Diagrammet kan skapas per vald region/kommun eller som ett facet-diagram för alla regioner/kommuner.
  # Notera att felmeddelandet Caused by error in `curl::curl_fetch_memory() Recv failure: Connection was reset ofta uppstår
  # Detta beror på för många androp på SCB:s API. Vänta en stund och försök igen.
  # ========================================== Inställningar ============================================

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  # Skapar en tom vektor som skall innehålla objektnamn
  objektnamn <- c()

  # =============================================== API-uttag ===============================================
  # Hämtar samma data som tidigare kom via
  # hamta_data/hamta_sysselsatta_region_kon_sni2007_fodelseregion_tid_ArbStDoNMNN_scb.R,
  # men direkt mot SCB:s PxWeb-API v2 med pxweb2r. TAB3784 = "Sysselsatta
  # 15-74 år efter region, kön, näringsgren (SNI 2007) och födelseregion.
  # Preliminär statistik" (månadsvis) - samma tabell (AM0210B/ArbStDoNMNN),
  # nytt id. Ingen CKM-tabell för den här (BAS-registret, inte
  # befolkningsstatistik, verkar inte omfattas av CKM).
  # pxweb2r ger näringsgrenskoden som en egen kolumn automatiskt (Region-
  # mönstret) - branschkoderna (SNI2007) binds sedan ihop med Region
  # Dalarnas egen bransch-/färgnyckel, precis som i originalet.

  # hämta branschnyckel från Region Dalarnas github-repo depot
  branschtabell <- intern_hamta_branschnyckel()

  df <- pxweb2r::pxweb2_get_data(
      "TAB3784",
      query = list(Region = region_vekt, Kon = "totalt", SNI2007 = "*", Fodelseregion = "totalt",
                   ContentsCode = "sysselsatta efter arbetsställets belägenhet", Tid = "*"),
    quiet = TRUE) |>
    dplyr::rename(regionkod = region_kod, branschkod = `näringsgren sni 2007_kod`,
                  `sysselsatta efter arbetsställets belägenhet` = value, tid = månad) |>
    dplyr::mutate(branschkod = ifelse(branschkod == "US", "00", branschkod)) |>
    dplyr::filter(branschkod != "A-U+US") |>
    dplyr::left_join(
      dplyr::select(branschtabell, Br15kod, BrKod, HexCode, bransch = Bransch),
      by = c("branschkod" = "Br15kod")
    ) |>
    dplyr::mutate(år = as.integer(stringr::str_sub(tid, 1, 4)),
           månad_nr = readr::parse_integer(stringr::str_sub(tid, 6,7)),
           månad = format(as.Date(paste(år, stringr::str_sub(tid, 6,7), "1", sep = "-")), "%B"),
           år_månad = paste0(år, " - ", månad),
           månad_år = paste0(månad, " ", år))


  # År att jämföra med (dvs. året innan senaste år)
  jmf_ar = max(df$år)-1
  # Senaste år. Behövs för att döpa kolumner
  senaste_ar = max(df$år)
  # Senaste månad. Behövs för att filtrera data
  senaste_manad = unique(dplyr::pull(dplyr::filter(df, år==max(år), månad == dplyr::last(månad)), månad))

  # Summerar på region och sektor. Lägger till en kolumn senaste och näst senaste, för att kunna beräkna förändring
  df_sum <- df |>
    dplyr::filter(år >= jmf_ar, månad==senaste_manad) |>
    dplyr::group_by(år,regionkod, region, bransch) |>
    dplyr::summarize("Antal" = sum(`sysselsatta efter arbetsställets belägenhet`), .groups = "drop") |>
    dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region, byt_ut_riket_mot_sverige = TRUE),
           år = dplyr::case_when(år == max(år) ~ "Senaste",
                                     TRUE ~ "nast_senaste"))


  # Beräknar förändring
  df_sum <- df_sum |>
    tidyr::pivot_wider(names_from = år, values_from = Antal) |>
      dplyr::mutate(forandring = (`Senaste`-`nast_senaste`)/`nast_senaste`*100) |>
        tidyr::pivot_longer(cols = c(`nast_senaste`,`Senaste`), names_to = "år", values_to = "Antal") |>
          dplyr::mutate(år = dplyr::case_when(år == "nast_senaste" ~ jmf_ar,
                                TRUE ~ senaste_ar))

  # Map-funktion som skapar diagrammen
  skapa_diagram = function(df,vald_region){

    df <- dplyr::filter(df, regionkod %in% vald_region)


    if(length(unique(df$region)) > 1){
      diagram_titel <- paste0("Föränding i antal förvärvsarbetande (16-74 år) per bransch")
      diagramfil <- paste0("förändring_bransch_facet_",dplyr::first(rdverktyg::hamtaregion_kod_namn(vald_region)[2]),".png")

    }else{
      diagram_titel <- paste0("Föränding i antal förvärvsarbetande (16-74 år) per bransch i ",rdverktyg::hamtaregion_kod_namn(vald_region)[2])
      diagramfil <- paste0("förändring_bransch_",rdverktyg::hamtaregion_kod_namn(vald_region)[2],".png")
    }

    diagram_undertitel <- paste0("Mellan ",senaste_manad," ",min(df$år)," och ",senaste_manad," ",max(df$år))
    objektnamn <- c(objektnamn, stringr::str_remove(diagramfil, ".png"))

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = dplyr::mutate(
        dplyr::filter(df, bransch != "Okänt", år==max(år)),
        bransch = stringr::str_wrap(bransch,40)
      ),
      skickad_x_var = "bransch",
      skickad_y_var = "forandring",
      manual_x_axis_text_vjust=1,
      manual_color = diag_fargvekt[1],
      x_axis_sort_value = TRUE,
      diagram_liggande = TRUE,
      manual_y_axis_title = "procent",
      x_axis_lutning = 0,
      stodlinjer_avrunda_fem = FALSE,
      y_axis_minus_plus_samma_axel = TRUE,
      facet_y_axis_storlek = 6,
      facet_sort = TRUE,
      facet_grp = if (length(unique(df$region)) > 1) "region" else NULL,
      facet_scale = "free",
      facet_legend_bottom = TRUE,
      diagram_titel = diagram_titel,
      diagram_undertitel = diagram_undertitel,
      diagram_capt = diagram_capt,
      output_mapp = utmapp,
      filnamn_diagram = diagramfil,
      skriv_till_diagramfil = skriv_diagramfil
    )

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list) <- objektnamn
    return(gg_list)
  }

  if (diag_facet) {
    diag <- skapa_diagram(df_sum,region_vekt)

  } else {
    diag = purrr::flatten(purrr::map(region_vekt, ~skapa_diagram(df_sum,.x)))
  }
  return(diag)
}
