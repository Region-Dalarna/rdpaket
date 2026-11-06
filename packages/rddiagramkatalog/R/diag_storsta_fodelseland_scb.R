#' Diagram: största födelseländer bland utrikes födda i en region
#'
#' Stapeldiagram över de största födelseländerna bland utrikes födda,
#' med möjlighet att jämföra tre år (första, sista och ett jämförelseår)
#' eller bara visa senaste året.
#'
#' @param region_vekt Region (max 1 län).
#' @param utmapp Mapp där diagram ska sparas. Måste finnas om `skriv_diagramfil = TRUE`.
#' @param skriv_diagramfil Sparar figuren till `utmapp`.
#' @param tid_koder Vilka år som ska hämtas. Standard alla från 2000 (`"*"`).
#'   Kan även anges som `"2015:9999"` för "från 2015 till och med senaste
#'   tillgängliga år".
#' @param returnera_figur Returnerar en figur.
#' @param jmf_ar Jämförs mot detta år (utöver första och sista år i `tid_koder`).
#' @param ar_sortering Vilket år länderna ska sorteras på (högst till lägst).
#' @param antal_lander Antal länder som visas i diagrammet.
#' @param diag_fargvekt Valfri färgvektor, standard: `rddiagram::diagramfarger("rus_sex")`.
#' @param visa_logga_i_diagram TRUE om logga ska visas i diagrammet.
#' @param logga_sokvag Sökväg till logga.
#' @param returnera_data Skall data returneras.
#'
#' @return En namngiven lista med ett `ggplot`-objekt.
#' @export
diag_storsta_fodelseland_scb <- function(region_vekt = "20",
                                        utmapp = rdverktyg::utskriftsmapp(),
                                        skriv_diagramfil = FALSE,
                                        tid_koder = "*",
                                        returnera_figur = TRUE,
                                        jmf_ar = 2010,
                                        ar_sortering = 3,
                                        antal_lander = 10,
                                        diag_fargvekt = NA,
                                        visa_logga_i_diagram = FALSE,
                                        logga_sokvag = NA,
                                        returnera_data = FALSE) {

  if (all(is.na(diag_fargvekt))) diag_fargvekt <- rddiagram::diagramfarger("rus_sex")

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  gg_list <- list()
  region_namn <- rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(region_vekt)$region)

  # Hämta data - TAB6030 (historik) + TAB6646 (CKM, senaste året). Kon = NA (inte "*") utelämnar
  # Kon-variabeln ur frågan, vilket - eftersom Kon har elimination = TRUE i båda tabellerna - gör att
  # SCB summerar ihop män och kvinnor åt oss.
  antal_fodelseland_df <- pxweb2r::pxweb2_get_data(
    table = c("TAB6030", "TAB6646"),
    query = list(
      Region = region_vekt,
      Fodelseregion = "*",
      Kon = NA,
      ContentsCode = "*",
      Tid = intern_tolka_tidskoder("TAB6030", tid_koder)
    ), quiet = TRUE) |>
    dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region)) |>
      dplyr::rename(regionkod = region_kod,
             Antal = value)

  top_10_fordelseland_sistaar <- antal_fodelseland_df |>
    dplyr::filter(år == max(år), !(födelseregion %in% c("Samtliga födelseländer", "Sverige"))) |>
    dplyr::arrange(dplyr::desc(Antal)) |>
    head(antal_lander) |>
    dplyr::pull(födelseregion)

  storsta_fodelseland_df <- antal_fodelseland_df |>
    dplyr::filter(födelseregion %in% top_10_fordelseland_sistaar,
           år %in% c(min(år), jmf_ar, max(år)))

  if (returnera_data == TRUE) {
    assign("storsta_fodelseland_df", storsta_fodelseland_df, envir = .GlobalEnv)
  }

  diagram_capt <- paste0("Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\n",
                        "Från och med referensåret 2025 och framåt är en liten kontrollerad slumpmässig\n",
                        "osäkerhet införd i samtliga redovisade uppgifter.Osäkerheten har tillförts för\n",
                        "att skydda enskilda personer så att ingen riskerar att röjas i statistiken.")

  if (length(unique(storsta_fodelseland_df$år)) == 1) {
    diagram_titel <- paste0("Vanligaste födelseland bland utrikes födda i ", region_namn, " år ", unique(storsta_fodelseland_df$år))
    diagramfilnamn <- paste0("storsta_fodelseland_", region_namn, "_", unique(storsta_fodelseland_df$år), ".png")
  } else {
    diagram_titel <- paste0("Vanligaste födelseland bland utrikes födda i ", region_namn)
    diagramfilnamn <- paste0("storsta_fodelseland_", region_namn, ".png")
  }

  gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = storsta_fodelseland_df,
                               skickad_x_var = "födelseregion",
                               skickad_y_var = "Antal",
                               skickad_x_grupp = "år",
                               manual_color = diag_fargvekt,
                               diagram_titel = diagram_titel,
                               manual_x_axis_text_vjust = 1,
                               manual_x_axis_text_hjust = 1,
                               x_axis_sort_value = TRUE,
                               vand_sortering = TRUE,
                               x_axis_sort_grp = ar_sortering,
                               legend_tabort = ifelse(length(unique(storsta_fodelseland_df$år)) == 1, TRUE, FALSE),
                               diagram_capt = diagram_capt,
                               output_mapp = utmapp,
                               stodlinjer_avrunda_fem = TRUE,
                               lagg_pa_logga = visa_logga_i_diagram,
                               logga_path = logga_sokvag,
                               manual_y_axis_title = "",
                               filnamn_diagram = diagramfilnamn,
                               skriv_till_diagramfil = skriv_diagramfil)

  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, ".png")

  if (returnera_figur == TRUE) {
    return(gg_list)
  }
}
