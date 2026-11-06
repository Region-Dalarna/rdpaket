#' Diagram: antal invånare per utbildningsnivå, ålder och kön
#'
#' Diagram som visar utbildningsnivå bland inrikes och utrikes födda samt
#' även kön. Tre utbildningsnivåer.
#'
#' @param region_vekt Val av region. Finns: kommunkoder, länskoder eller riket.
#' @param gruppera_namn NA = gör ingenting, annars anges namn på gruppering som medskickade regioner ska grupperas till.
#' @param valt_kon Finns: `"män och kvinnor"`, `"män"`, `"kvinnor"`.
#' @param alder_koder Antingen `"tot16-74"` eller annat intervall, exempelvis `c(as.character(25:64))`, `"*"` ger alla år.
#' @param tid_koder `"*"` = alla år, `"9999"` = senaste,
#'   eller `"2015:9999"` för "från 2015 till och med senaste tillgängliga år".
#' @param vald_utbniva `"*"` = alla. `"förgymnasial utbildning"`, `"gymnasial utbildning"`, `"eftergymnasial utbildning"`, `"utbildningsnivå saknas"`.
#' @param alder_grupper NA = gör ingenting utan åldrar som de är, annars t.ex. `c(20, 25, 30, 35, 40, 45, 50, 55, 60)`, dvs. start i varje åldersgrupp.
#' @param diagram_capt Diagrammets källtext.
#' @param utmapp Mapp där diagram ska sparas. Måste finnas om `skriv_diagramfil = TRUE`.
#' @param excelfil_mapp Mapp där excelfil ska sparas.
#' @param excelfil_namn Filnamn för excelfil.
#' @param returnera_data Skall data returneras till global environment.
#' @param visa_dataetiketter Visa dataetiketter.
#' @param skriv_diagramfil TRUE om diagram ska skrivas till fil.
#' @param diag_fargvekt Valfri färgvektor, standard: `rddiagram::diagramfarger("kon")`.
#'
#' @return En namngiven lista med `ggplot`-objekt.
#' @export
diag_antal_utbniva_alder_kon_scb <- function(
    region_vekt = "20",
    gruppera_namn = NA,
    valt_kon = c("män", "kvinnor"),
    alder_koder =  c(as.character(20:64)),
    tid_koder = "9999",
    vald_utbniva = "förgymnasial utbildning",
    alder_grupper = c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65),
    diagram_capt = "Källa: Utbildningsregistret, SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
    utmapp = rdverktyg::utskriftsmapp(),
    excelfil_mapp = NA,
    excelfil_namn = "utbniva.xlsx",
    returnera_data = FALSE,
    visa_dataetiketter = FALSE,
    skriv_diagramfil = TRUE,
    diag_fargvekt = NA
) {

  # ======================================================================================================
  #
  # Diagram som visar utbildningsnivå bland inrikes och utrikes födda samt även kön. Tre utbildningsnivåer.
  # Skapat 17 okt 2024 av Peter.
  # Senaste ändring: SCB hade bytt variabelnamn från Befolkning till Antal. Rättat på rad 69 Jon 2025-09-24
  # ======================================================================================================

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  gg_list <- list()

  # om ingen färgvektor är medskickad, använd rddiagram::diagramfarger("kon")
  if (all(is.na(diag_fargvekt))) {
    diag_fargvekt <- rddiagram::diagramfarger("kon")
  }

  # Länk till tabell: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__UF__UF0506__UF0506B/Utbniva3/
  utbniva_df <- pxweb2r::pxweb2_get_data(
    table = "TAB3981",
    query = list(
      Region = region_vekt,
      Kon = valt_kon,
      Alder = alder_koder,
      UtbildningsNiva = "*",
      ContentsCode = "UF0506A1",
      Tid = intern_tolka_tidskoder("TAB3981", tid_koder)
    ), quiet = TRUE) |>
    dplyr::rename(regionkod = region_kod,
           utbildningsnivå_alla = utbildningsnivå,
           varde = value) |>
    dplyr::select(-tabellinnehåll) |>
    dplyr::mutate(utbildningsnivå = dplyr::case_when(
      stringr::str_detect(utbildningsnivå_alla, "eftergymnasial|forskar") ~ "eftergymnasial utbildning",
      stringr::str_detect(utbildningsnivå_alla, "förgymnasial utbildning") ~ "förgymnasial utbildning",
      stringr::str_detect(utbildningsnivå_alla, "gymnasial utbildning") ~ "gymnasial utbildning",
      stringr::str_detect(utbildningsnivå_alla, "saknas") ~ "utbildningsnivå saknas",
      TRUE ~ utbildningsnivå_alla # behåller ursprungligt värde om ingen matchning
    )) |>
    dplyr::relocate(utbildningsnivå, .before = utbildningsnivå_alla)

  if (!"ålder" %in% names(utbniva_df)) utbniva_df <- dplyr::mutate(utbniva_df, ålder = "16-74 år")

  if (!all(is.na(alder_grupper))) {
    utbniva_df <- utbniva_df |>
      dplyr::mutate(ålder = rdverktyg::skapa_aldersgrupper(ålder, alder_grupper))
  }

  # om man vill gruppera ihop flera kommuner eller län till en större geografisk indelning
  # så anges den med namn i gruppera_namn. Lämnas den tom görs ingenting nedan
  if (!all(is.na(gruppera_namn))) {
    utbniva_df <- utbniva_df |>
      dplyr::group_by(dplyr::across(-c(regionkod, region, varde))) |>
      dplyr::summarise(varde = sum(varde, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(regionkod = "gg",
             region = gruppera_namn) |>
      dplyr::relocate(region, .before = 1) |>
      dplyr::relocate(regionkod, .before = region)

    region_vekt <- "gg"
  }

  if(returnera_data == TRUE){
    assign("utbniva_kon_alder_df", utbniva_df, envir = .GlobalEnv)
  }

  if (vald_utbniva != "*") {
    utbniva_df <- dplyr::filter(utbniva_df, utbildningsnivå %in% vald_utbniva)
  }

  skapa_diagram <- function(chart_df, skickad_regionkod, valt_ar) {

    chart_df <- chart_df |>
      dplyr::filter(regionkod %in% skickad_regionkod,
             år %in% valt_ar)

    # om regioner är alla kommuner i ett län eller alla län i Sverige görs revidering, annars inte
    region_start <- rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(unique(chart_df$region)))
    region_txt <- rdverktyg::ar_alla_kommuner_i_ett_lan(unique(chart_df$regionkod), returnera_text = TRUE, returtext = region_start)
    region_txt <- rdverktyg::ar_alla_lan_i_sverige(unique(chart_df$regionkod), returnera_text = TRUE, returtext = region_txt)
    regionfil_txt <- region_txt
    region_txt <- paste0(" i ", region_txt)
    regionkod_txt <- if (region_start == region_txt) paste0(unique(chart_df$regionkod), collapse = "_") else region_txt

    ar_txt <- if (min(chart_df$år) == max(chart_df$år)) max(chart_df$år) else paste0(min(chart_df$år), "-", max(chart_df$år))
    utbniva_txt <- paste0(unique(chart_df$utbildningsnivå), collapse = "_")
    utbniva_titel <- if (length(unique(chart_df$utbildningsnivå)) == 1) paste0(" med ", unique(chart_df$utbildningsnivå)) else ""

    diagramtitel <- glue::glue("Invånare{region_txt}{utbniva_titel} år {ar_txt}")
    diagramfil <- stringr::str_replace_all(glue::glue("utbniva_{utbniva_txt}_kon_{regionfil_txt}_ar{ar_txt}.png"), " ", "_")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = chart_df,
    			 skickad_x_var = "ålder",
    			 skickad_y_var = "varde",
    			 skickad_x_grupp = "kön",
    			 diagram_titel = diagramtitel,
    			 diagram_capt = diagram_capt,
    			 stodlinjer_avrunda_fem = TRUE,
    			 diagram_liggande = TRUE,
    			 geom_position_stack = TRUE,
    			 filnamn_diagram = diagramfil,
    			 dataetiketter = visa_dataetiketter,
    			 manual_y_axis_title = "Antal personer",
    			 x_axis_lutning = 0,
    			 manual_color = diag_fargvekt,
    			 output_mapp = utmapp,
    			 skriv_till_diagramfil = skriv_diagramfil,
    			 facet_legend_bottom = TRUE,
    			 facet_grp = if (length(unique(chart_df$utbildningsnivå)) == 1) NULL else "utbildningsnivå",
    			 facet_scale = "fixed",
    )

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfil, ".png")

    return(gg_list)

  } # slut skapa_diagram

    arglist <- list(reg = region_vekt, valt_ar = unique(utbniva_df$år))                               # skapa lista med de två variabler vi vill göra diagram med
    crossarg <- expand.grid(arglist)
    retur_list <- purrr::flatten(purrr::pmap(crossarg, ~skapa_diagram(chart_df = utbniva_df, skickad_regionkod = ..1, valt_ar = ..2)))

  return(retur_list)
} # slut funktion
