#' Diagram: arbetslöshet sedan 1976 med kriser markerade (SCB)
#'
#' Ett linjediagram över arbetslösheten (andel arbetslösa av arbetskraften)
#' per år från 1976 till senaste år för valda regioner, med lodräta linjer
#' och etiketter vid starten av 90-talskrisen, finanskrisen, coronapandemin
#' och Rysslands invasion av Ukraina. Data laddas automatiskt ned från SCB:s
#' öppna statistikdatabas (AKU): TAB88 till och med 2004 och TAB2922 från
#' 2005. Diagrammet byggs direkt med ggplot2.
#'
#' @param utmapp Mapp där figuren sparas. Måste finnas om
#'   `skriv_diagramfil = TRUE`.
#' @param region_vekt Vilka regioner (länskoder, ej kommuner) som ska jämföras.
#'   "00" ger riket (visas som Sverige).
#' @param skriv_diagramfil Skall figuren sparas.
#' @param returnera_data Skall data läggas i R-studios globala miljö.
#' @param returnera_figur Skall en lista med figuren returneras.
#' @param diag_fargvekt Vilka färger som ska användas i diagrammet (en per
#'   region).
#' @param filnamn_figur Filnamn för figuren.
#' @param data_namm Namn på dataobjektet som läggs i globala miljön om
#'   `returnera_data = TRUE`.
#'
#' @return En namngiven lista med ett `ggplot`-objekt.
#' @export
diag_arbetsloshet_76_scb <- function(utmapp = rdverktyg::utskriftsmapp(),
                                     region_vekt = c("00", "20"),
                                     skriv_diagramfil = TRUE,
                                     returnera_data = FALSE,
                                     returnera_figur = TRUE,
                                     diag_fargvekt = rddiagram::diagramfarger("rus_sex"),
                                     filnamn_figur = "arbetsloshet_76.png",
                                     data_namm = "arbetsloshet_76") {
  
  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)
  gg_list <- list()
  
  # Till och med 2004
  df_76_04 <- pxweb2r::pxweb2_get_data(
    table = "TAB88",
    query = list(
      Region = region_vekt,
      Kon = c("män", "kvinnor"),
      Arbetskraftstillh = c("SYS", "ALÖS"),
      ContentsCode = "*",
      Tid = "*"),
    quiet = TRUE)
  
  # Från 2005
  df_05_ <- pxweb2r::pxweb2_get_data(
    table = "TAB2922",
    query = list(
      Region = region_vekt,
      Arbetskraftstillh = c("ALÖS", "SYS"),
      Kon = c("män", "kvinnor"),
      ContentsCode = "1000-tal",
      Tid = "*"),
    quiet = TRUE)
  
  # Skydd mot att flera tabellinnehåll summeras ihop (ContentsCode = "*" ovan)
  if (any(c(dplyr::n_distinct(df_76_04$tabellinnehåll),
            dplyr::n_distinct(df_05_$tabellinnehåll)) > 1)) {
    stop("Fler än ett tabellinnehåll returnerades från SCB - välj ett specifikt ContentsCode.")
  }
  
  # Slå ihop tabellerna (utan överlappande år), summera över kön och beräkna arbetslöshet
  df <- dplyr::bind_rows(
    df_76_04 |> dplyr::filter(as.numeric(år) <= 2004),
    df_05_ |> dplyr::filter(as.numeric(år) >= 2005)) |>
    dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region, byt_ut_riket_mot_sverige = TRUE),
                  år = as.numeric(år)) |>
    dplyr::group_by(region, år, arbetskraftstillhörighet) |>
    dplyr::summarise(antal = sum(value), .groups = "drop") |>
    tidyr::pivot_wider(names_from = arbetskraftstillhörighet, values_from = antal) |>
    dplyr::mutate(arbetsloshet = (arbetslösa / (sysselsatta + arbetslösa)) * 100) |>
    dplyr::select(region, år, sysselsatta, arbetslösa, arbetsloshet)
  
  if (returnera_data == TRUE) {
    assign(data_namm, df, envir = .GlobalEnv)
  }
  
  # y-axelns tak: minst 10 procent, annars närmaste 2,5-steg över högsta värdet
  y_tak <- max(10, ceiling(max(df$arbetsloshet, na.rm = TRUE) / 2.5) * 2.5)
  
  # Kriserna markeras med startår (lodrät linje + etikett). Coronapandemin och
  # Ukraina-kriget ligger nära varandra i tid och får därför olika höjd.
  kriser <- dplyr::tibble(
    kris     = c("90-talskrisen", "Finanskrisen 2008-09", "Coronapandemin", "Rysslands invasion av Ukraina"),
    startar  = c(1990, 2008, 2020, 2022),
    etikett_y = y_tak + c(0.6, 0.6, 1.4, 0.6))
  
  # Namngiven färgvektor: en färg per region i den ordning de förekommer i data
  region_niva <- unique(df$region)
  region_farger <- diag_fargvekt[seq_along(region_niva)]
  names(region_farger) <- region_niva
  
  diagram_capt <- paste0("Källa: SCB, arbetskraftsundersökningarna (AKU).\n",
                         "Bearbetning: Samhällsanalys, Region Dalarna.\n",
                         "Från och med oktober 2007 räknas även studenter som aktivt söker ett arbete och är villiga att ta jobb som arbetslösa.\n",
                         "Fram till 2004, arbetslöshet i åldersgruppen 16-64 år, därefter 15-74 år.\nNotera att AKU är en undersökning med betydande osäkerhet på regional nivå. Figuren bör därför enbart användas\nför att titta på övergripande trender. För aktuell data över arbetslöshet, använd SCB-BAS istället.")
  diagramtitel <- paste0("Arbetslöshet i ",
                         glue::glue_collapse(region_niva, sep = ", ", last = " och "),
                         " ", min(df$år), "-", max(df$år))
  
  gg_obj <- ggplot2::ggplot() +
    # Lodrät linje vid varje krisstart
    ggplot2::geom_vline(
      data = kriser,
      ggplot2::aes(xintercept = startar),
      color = "grey60", linewidth = 0.4) +
    # Etiketter för kriserna. geom_label ger vit bakgrund så texten syns där
    # den korsar regionlinjerna.
    ggplot2::geom_label(
      data = kriser,
      ggplot2::aes(x = startar, y = etikett_y, label = kris),
      angle = 90, hjust = 1, vjust = 0.5, size = 3, color = "grey30",
      fill = "white", linewidth = 0, label.padding = ggplot2::unit(0.1, "lines"),
      inherit.aes = FALSE) +
    # Linjerna för regionerna
    ggplot2::geom_line(
      data = df,
      ggplot2::aes(x = år, y = arbetsloshet, color = region),
      linewidth = 1.6) +
    ggplot2::scale_color_manual(
      values = region_farger,
      name = NULL) +
    ggplot2::scale_x_continuous(
      breaks = seq(min(df$år), max(df$år), by = 4),
      expand = ggplot2::expansion(mult = c(0.01, 0.01))) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(x, " %"),
      breaks = seq(0, y_tak, by = 2.5),        # större, etiketterade stödlinjer
      minor_breaks = seq(0, y_tak, by = 0.5),  # tunnare stödlinjer varje 0,5 %
      limits = c(0, y_tak + 2)) +              # utrymme upptill för krisetiketterna
    ggplot2::labs(
      title = diagramtitel,
      x = NULL,
      y = NULL,
      caption = diagram_capt) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggtext::element_textbox_simple(
        size = 20,
        width = ggplot2::unit(0.9, "npc"),
        halign = 0.5,
        margin = ggplot2::margin(7, 0, 7, 0)),
      plot.title.position = "plot",
      axis.text = ggplot2::element_text(color = "black"),
      axis.title = ggplot2::element_text(color = "black"),
      plot.caption = ggplot2::element_text(
        hjust = 0, vjust = 0, size = 11, face = "italic", color = "black"),
      plot.caption.position = "plot",
      plot.margin = ggplot2::margin(t = 10, r = 10, b = 10, l = 0),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_line(color = "grey90", linewidth = 0.3),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "grey75", linewidth = 0.6),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(color = "black"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  if (skriv_diagramfil == TRUE) {
    ggplot2::ggsave(filename = file.path(utmapp, filnamn_figur),
                    plot = gg_obj,
                    width = 12,
                    height = 7,
                    dpi = 300)
  }
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list) <- filnamn_figur |> stringr::str_remove(".png")
  
  if (returnera_figur == TRUE) return(gg_list)
}
