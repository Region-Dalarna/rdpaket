#' Diagram: återstående medellivslängd per utbildningsnivå
#'
#' Linjediagram över återstående medellivslängd vid en given ålder, uppdelat
#' på utbildningsnivå och kön.
#'
#' \if{html}{\figure{diag_aterstaende_medellivslangd_utbniva_lan_scb.png}{options: width="100%" alt="Exempel: diag_aterstaende_medellivslangd_utbniva_lan_scb() med standardinställningar"}}
#'
#' @param region_vekt Region(er).
#' @param tid_koder Vilka år som ska hämtas. `"*"` = alla, `"9999"` = senaste,
#'   eller `"2015:9999"` för "från 2015 till och med senaste tillgängliga år".
#' @param cont_var_klartext Är främst den relevanta variabeln för oss att använda i denna tabell.
#' @param vald_alder Ålder för återstående medellivslängd. Finns: `"30 år"`, `"31 år"`, `"32 år"` ... t.o.m. `"93 år"`, `"94 år"`, `"95+ år"`.
#' @param diag_fargvekt Valbar färgvektor för diagrammet.
#' @param visa_logga_i_diagram TRUE om logga ska visas i diagrammet.
#' @param logga_sokvag Sökväg till logga.
#' @param diagramtitel_tabort TRUE om diagramtitel ska tas bort.
#' @param diagram_capt Diagrammets källtext.
#' @param x_axel_stlk Storlek på x-axelns text.
#' @param y_axel_stlk Storlek på y-axelns text.
#' @param skriv_diagramfil TRUE om diagram ska skrivas till fil.
#' @param ggobjektfilnamn_utan_tid Om TRUE så tas t.ex. året inte med i filnamnet, vilket passar bättre i vissa sammanhang när man vill använda objektsnamnet utan att ändra vid varje uppdatering.
#' @param excel_mapp Mapp där excelfil ska sparas, NA = sparas ingen fil.
#' @param demo Sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren.
#' @param utmapp Mapp där diagrammet sparas. Måste finnas om `skriv_diagramfil = TRUE`.
#'
#' @return En namngiven lista med ett `ggplot`-objekt.
#' @export
diag_aterstaende_medellivslangd_utbniva_lan_scb <- function(
    region_vekt = "20",
    tid_koder = "*",
    cont_var_klartext = "Antal återstående år",
    vald_alder = "30 år",
    diag_fargvekt = NA,
    visa_logga_i_diagram = TRUE,
    logga_sokvag = NA,
    diagramtitel_tabort = FALSE,
    diagram_capt = "Källa: Demografisk analys - Befolkning, SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
    x_axel_stlk = 10.5,
    y_axel_stlk = 12,
    skriv_diagramfil = TRUE,
    ggobjektfilnamn_utan_tid = FALSE,
    excel_mapp = NA,
    demo = FALSE,
    utmapp = rdverktyg::utskriftsmapp()
    ) {

  # =======================================================================================================================
  #
  # Hämta hem data med funktionen hamta_aterstaende_medellivslangd, skriv ut ett diagram. Defaultinställning är
  # återstående medellivslängd vid 30 års ålder för kvinnor respektive män i Dalarna. Det finns fler innehålls-
  # variabler, vilka primärt används för att beräkna dödsrisker och återstående medellivslängd.
  #
  # Länk till SCB-tabell där data hämtas: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__BE__BE0701/LivslUtbLan/
  #
  # =======================================================================================================================

  # om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
  # demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
  if (demo){
    demo_url <-
      c("https://region-dalarna.github.io/utskrivna_diagram/medellivslangd_aterstaende_vid_30 år_alder_Dalarna_ar2012-2016_2019-2023.png")
    purrr::walk(demo_url, ~browseURL(.x))
    if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
    rdverktyg::stop_tyst()
  }

  diag_fargvekt <- if (all(is.na(diag_fargvekt))) rddiagram::diagramfarger("rus_sex")[c(5,1,3)] else diag_fargvekt

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  gg_list <- list()

  # Länk till tabell: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__BE__BE0701/LivslUtbLan5/
  medellivslangd_df <- pxweb2r::pxweb2_get_data(
    table = "TAB4879",
    query = list(
      Region = region_vekt,
      UtbildningsNiva = c("förgymnasial utbildning", "gymnasial utbildning", "eftergymnasial utbildning"),
      Kon = c("män", "kvinnor"),
      Alder = vald_alder,
      ContentsCode = cont_var_klartext,
      Tid = intern_tolka_tidskoder("TAB4879", tid_koder)
    ), quiet = TRUE)

  new_name <- unique(medellivslangd_df$tabellinnehåll)
  medellivslangd_df <- medellivslangd_df |>
    dplyr::rename(regionkod = region_kod, !!new_name := value) |>
    dplyr::select(-tabellinnehåll)

  # om regioner är alla kommuner i ett län eller alla län i Sverige görs revidering, annars inte
  region_start <- rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(unique(medellivslangd_df$region), byt_ut_riket_mot_sverige = TRUE))
  region_txt <- rdverktyg::ar_alla_kommuner_i_ett_lan(unique(medellivslangd_df$regionkod), returnera_text = TRUE, returtext = region_start)
  region_txt <- rdverktyg::ar_alla_lan_i_sverige(unique(medellivslangd_df$regionkod), returnera_text = TRUE, returtext = region_txt)
  regionfil_txt <- region_txt
  region_txt <- paste0(" i ", region_txt)
  regionkod_txt <- if (region_start == region_txt) paste0(unique(medellivslangd_df$regionkod), collapse = "_") else region_txt

  diagramtitel <- glue::glue("Återstående medellivslängd vid {unique(medellivslangd_df$ålder)} års ålder{region_txt}")
  diagramfil <- stringr::str_replace_all(glue::glue("medellivslangd_aterstaende_vid_{unique(medellivslangd_df$ålder)}_alder_{regionfil_txt}_ar{min(medellivslangd_df$årsintervall)}_{max(medellivslangd_df$årsintervall)}.png"), "__", "_")

  chart_df <- medellivslangd_df |>
    dplyr::mutate(utbildningsnivå = factor(utbildningsnivå, levels = c("förgymnasial utbildning", "gymnasial utbildning", "eftergymnasial utbildning")))

  gg_obj <- rddiagram::SkapaLinjeDiagram(
    skickad_df = chart_df,
    skickad_x_var = "årsintervall",
    skickad_y_var = cont_var_klartext,
    skickad_x_grupp = "utbildningsnivå",
    diagram_titel = if(diagramtitel_tabort) NULL else diagramtitel,
    diagram_capt = diagram_capt,
    y_axis_borjar_pa_noll = FALSE,
    filnamn_diagram = diagramfil,
    manual_x_axis_title = "årsintervall",
    manual_color = diag_fargvekt,
    lagg_pa_logga = visa_logga_i_diagram,
    facet_x_axis_storlek = x_axel_stlk,
    facet_y_axis_storlek = y_axel_stlk,
    logga_path = logga_sokvag,
    output_mapp = utmapp,
    facet_grp = "kön",
    facet_scale = "fixed",
    facet_legend_bottom = TRUE,
    skriv_till_diagramfil = skriv_diagramfil
  )

  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfil, "\\.[^.]+$")

  # ta bort tidsbestämning (tex. år) ur objektsnamnet, för användning i tex r-markdownrapporter
  if (ggobjektfilnamn_utan_tid) {
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(names(gg_list)[[length(gg_list)]], "_ar.*$")
  }

  return(gg_list)

}
