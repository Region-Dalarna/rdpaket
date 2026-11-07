#' Diagram: andel offentligt anställda
#'
#' Andel av förvärvsarbetande (efter arbetsställets belägenhet) som
#' arbetar inom offentlig sektor, totalt eller uppdelat på kön.
#'
#' @param region_vekt Val av kommuner/regioner.
#' @param alder_klartext Åldersspann. Finns: `"15–24 år"`, `"15–74 år"`
#'   (standard), `"16–64 år"`, `"16–65 år"`, `"16–66 år"`, `"20–64 år"`,
#'   `"20–65 år"`, `"20–66 år"`, `"25–54 år"`, `"55–74 år"`. Max ett åt
#'   gången.
#' @param kon_klartext `"totalt"` (standard) för ett diagram utan
#'   könsuppdelning, eller `c("kvinnor", "män")` för uppdelat på kön. Går
#'   bara att välja ettdera.
#' @param utmapp Mapp där figuren sparas. Måste finnas om
#'   `skriv_diagramfil = TRUE`.
#' @param output_mapp_data Mapp där data sparas, om `filnamn_data` också
#'   anges. `NA` (standard) = data sparas inte.
#' @param filnamn_data Filnamn för sparad data.
#' @param diag_fargvekt Vilken färgpalett (namn, se
#'   [rddiagram::diagramfarger()]) som ska användas. Blir alltid `"kon"`
#'   när diagrammet delas upp på kön.
#' @param stodlinjer_avrunda_fem TRUE om stödlinjerna ska avrundas till
#'   närmaste femtal.
#' @param skriv_diagramfil Om TRUE sparas figuren till `utmapp`.
#' @param returnera_figur Om TRUE returneras figuren från funktionen.
#' @param returnera_data Skall data returneras till global environment.
#'
#' @return En namngiven lista med ett `ggplot`-objekt.
#' @export
diag_andel_offentligt_bas_scb <- function(region_vekt = rdverktyg::hamtakommuner("20", tamedlan = TRUE, tamedriket = TRUE),
                                     alder_klartext = "15–74 år",
                                     kon_klartext = "totalt",
                                     utmapp = rdverktyg::utskriftsmapp(),
                                     output_mapp_data = NA,
                                     filnamn_data = "andel_offentligt.xlsx",
                                     diag_fargvekt = "rus_sex",
                                     stodlinjer_avrunda_fem = TRUE,
                                     skriv_diagramfil = TRUE,
                                     returnera_figur = TRUE,
                                     returnera_data = FALSE
) {

  # ========================================== Allmän info ============================================
  # Andelen som arbetar inom offentlig sektor. Funkar med och utan könsuppdelning men enbart för
  # senaste månad. Hämtar data från BAS (Tabell AM0210B/SektorSyssM, v2: TAB2597) - RAMS-varianten
  # (källans diagram_andel_offentligt.R, frusen 2019-2021) migrerades INTE, då källkoden själv
  # flaggar den som ersatt av denna BAS-variant.
  # ========================================== Inställningar ============================================

  if (length(kon_klartext) > 2) {
    stop("Går enbart att välja antingen totalt eller uppdelat på kvinnor och män (kon_klartext)")
  }

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  gg_list <- list() # Skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  objektnamn <- c() # Används för att namnge

  # url till tabellens API: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0210__AM0210B/SektorSyssM/
  df <- pxweb2r::pxweb2_get_data(
    table = "TAB2597",
    query = list(
      Region = region_vekt,
      ArbetsSektor = c("samtliga sektorer", "offentlig förvaltning (staten, kommun, region)"),
      Kon = kon_klartext,
      Alder = alder_klartext,
      Fodelseregion = "totalt",
      ContentsCode = "sysselsatta efter arbetsställets belägenhet",
      Tid = "9999"
    ), quiet = TRUE) |>
    dplyr::rename(varde = value,
           regionkod = region_kod) |>
      dplyr::select(-tabellinnehåll)

  # Pivot wider and calculate share
  andel_df <- df |>
    tidyr::pivot_wider(names_from = `arbetsställets sektortillhörighet`, values_from = varde) |>
    dplyr::mutate(`Offentlig sektor` = ((`offentlig förvaltning (staten, kommun, region)` / `samtliga sektorer`) * 100),
           Övriga = 100 - `Offentlig sektor`) |>
    dplyr::select(-`offentlig förvaltning (staten, kommun, region)`, -`samtliga sektorer`) |>
    tidyr::pivot_longer(cols = c("Offentlig sektor", "Övriga"), names_to = "sektor", values_to = "varde") |>
    dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region, byt_ut_riket_mot_sverige = TRUE),
           varde = varde - 0.01) |>
    tidyr::separate(månad, into = c("år", "månad"), sep = "M") |>
    dplyr::mutate(månad_namn = dplyr::case_when(
      månad == "01" ~ "januari",
      månad == "02" ~ "februari",
      månad == "03" ~ "mars",
      månad == "04" ~ "april",
      månad == "05" ~ "maj",
      månad == "06" ~ "juni",
      månad == "07" ~ "juli",
      månad == "08" ~ "augusti",
      månad == "09" ~ "september",
      månad == "10" ~ "oktober",
      månad == "11" ~ "november",
      månad == "12" ~ "december",
      TRUE ~ "okänd"))

  if (length(unique(andel_df$kön)) > 1) {
    andel_df <- andel_df |>
      dplyr::filter(sektor == "Offentlig sektor")
    diag_fargvekt <- "kon"
  }

  if (returnera_data == TRUE) {
    assign("andel_offentligt", andel_df, envir = .GlobalEnv)
  }

  diagram_titel <- paste0("Andel offentligt anställda (", alder_klartext, ") i ", unique(andel_df$månad_namn), " ", unique(andel_df$år))
  diagramfilnamn <- "andel_offentligt.png"
  diagram_capt <- glue::glue("Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Förvärvsarbetande efter arbetsställets belägenhet. Andel av förvärvsarbetande {alder_klartext} som\narbetar inom offentlig sektor respektive övriga sektorer.")

  objektnamn <- c(objektnamn, "andel_off_totalt")

  gg_obj <- rddiagram::SkapaStapelDiagram(
    skickad_df = andel_df |>
      dplyr::mutate(sektor = factor(`sektor`, levels = c("Offentlig sektor", "Övriga")[2:1])),
    skickad_x_var = "region",
    skickad_y_var = "varde",
    skickad_x_grupp = ifelse(length(unique(andel_df$kön)) > 1, "kön", "sektor"),
    manual_x_axis_text_vjust = 1,
    manual_x_axis_text_hjust = 1,
    manual_color = rddiagram::diagramfarger(diag_fargvekt),
    stodlinjer_avrunda_fem = stodlinjer_avrunda_fem,
    geom_position_stack = ifelse(length(unique(andel_df$kön)) > 1, FALSE, TRUE),
    legend_vand_ordning = ifelse(length(unique(andel_df$kön)) > 1, FALSE, TRUE),
    x_axis_sort_value = TRUE,
    x_axis_sort_grp = 1,
    manual_y_axis_title = "procent",
    vand_sortering = ifelse(length(unique(andel_df$kön)) > 1, TRUE, FALSE),
    diagram_titel = diagram_titel,
    diagram_capt = diagram_capt,
    output_mapp = utmapp,
    filnamn_diagram = diagramfilnamn,
    skriv_till_diagramfil = skriv_diagramfil)

  gg_list <- c(gg_list, list(gg_obj))

  if (!is.na(output_mapp_data) & !is.na(filnamn_data)) {
    openxlsx::write.xlsx(andel_df, paste0(output_mapp_data, filnamn_data))
  }

  names(gg_list) <- c(objektnamn)
  if (returnera_figur == TRUE) return(gg_list)
}
