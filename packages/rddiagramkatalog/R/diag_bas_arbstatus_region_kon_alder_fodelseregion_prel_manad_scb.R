#' Diagram: förändring i arbetsmarknadsstatus, senaste mot första månaden
#'
#' Stapeldiagram över förändring i arbetsmarknadsstatus (BAS) uppdelat på
#' födelseregion, för den senaste tillgängliga månaden jämfört med januari
#' samma period.
#'
#' @param region_vekt Region.
#' @param gruppera_regioner_namn NA = gör ingenting, annars anges namn på gruppering som medskickade regioner ska grupperas till.
#' @param alder_txt Åldersgrupp, t.ex. `"16-64 år"`.
#' @param kon_txt Kön, `"totalt"` ger inte könsuppdelning.
#' @param cont_txt Innehållsvariabler att hämta.
#' @param tid_koder Vilka tidsperioder som ska hämtas. `"*"` = alla, `"9999"` = senaste,
#'   eller `"2021M01:9999"` för "från januari 2021 till och med senaste tillgängliga månad".
#' @param utmapp Mapp där diagram ska sparas. Måste finnas om `skriv_diagramfil = TRUE`.
#' @param diagram_capt Diagrammets källtext.
#' @param diag_fargvekt Valfri färgvektor, standard: `rddiagram::diagramfarger("rd_primar_nio")`.
#' @param skriv_diagramfil TRUE om diagram ska skrivas till fil.
#' @param demo Sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren.
#'
#' @return En lista med `ggplot`-objekt.
#' @export
diag_bas_arbstatus_region_kon_alder_fodelseregion_prel_manad_scb <- function(region_vekt = "20",
                                                                         gruppera_regioner_namn = NA,
                                                                         alder_txt = "16-64 år",
                                                                         kon_txt = "totalt",
                                                                         cont_txt = c("antal sysselsatta",
                                                                                      "antal arbetslösa",
                                                                                      "antal studerande",
                                                                                      "antal pensionärer",
                                                                                      "antal sjuka",
                                                                                      #"antal totalt",
                                                                                      "antal övriga"
                                                                         ),
                                                                         tid_koder = "*",
                                                                         utmapp = rdverktyg::utskriftsmapp(),
                                                                         diagram_capt = "Källa: Befolkningens arbetsmarknadsstatus (BAS), SCB\nBearbetning: Samhällsanalys, Region Dalarna",
                                                                         diag_fargvekt = NA,
                                                                         skriv_diagramfil = TRUE,
                                                                         demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
                                                                         ) {



# om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
# demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
if (demo){
  demo_url <-
c("https://region-dalarna.github.io/utskrivna_diagram/forandr_arbmstatus_Dalarna_16-64 år.png")
  purrr::walk(demo_url, ~browseURL(.x))
  if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
  rdverktyg::stop_tyst()
}

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  # SCB skriver åldersintervallen med tankstreck ("16–64 år", U+2013), inte
  # vanligt bindestreck. Normaliserar bindestreck mellan siffror till
  # tankstreck här så att både "16-64 år" och "16–64 år" fungerar som
  # klartext (samma fix som gjorts i systerskripten för denna tabell).
  alder_txt <- gsub("(?<=[0-9])-(?=[0-9])", "–", alder_txt, perl = TRUE)

  fodelseregion_txt = c("inrikes född", "utrikes född")
  # om ingen färgvektor är medskickad, använd rddiagram::diagramfarger("rd_primar_nio")
  if (all(is.na(diag_fargvekt))) {
    diag_fargvekt <- rddiagram::diagramfarger("rd_primar_nio")
  }

  # Länk till tabell: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0210__AM0210A/ArbStatusM/
  syss_df <- pxweb2r::pxweb2_get_data(
    table = "TAB6260",
    query = list(
      Region = region_vekt,
      Kon = kon_txt,
      Alder = alder_txt,
      Fodelseregion = fodelseregion_txt,
      ContentsCode = cont_txt,
      Tid = intern_tolka_tidskoder("TAB6260", tid_koder)
    ), quiet = TRUE) |>
    dplyr::rename(regionkod = region_kod, variabel = tabellinnehåll, varde = value)

  # om man vill gruppera ihop de regioner som man skickat med
  if (!is.na(gruppera_regioner_namn)) {
    syss_df <- syss_df |>
      dplyr::group_by(kön, ålder, födelseregion, månad, variabel) |>
      dplyr::summarise(varde = sum(varde, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(regionkod = "grp",
             region = gruppera_regioner_namn) |>
      dplyr::relocate(regionkod, .before = 1) |>
      dplyr::relocate(region, .after = regionkod)

  } # slut if-sats om man vill gruppera


  bakgr_df <- syss_df |>
    dplyr::filter(kön == "totalt" & födelseregion != "totalt") |>
    rdverktyg::manader_bearbeta_scbtabeller()

  forsta_ar <- dplyr::first(bakgr_df$år)
  senaste_ar <- dplyr::last(bakgr_df$år)
  senaste_manad <- dplyr::last(dplyr::pull(dplyr::filter(bakgr_df, år == senaste_ar), månad))

  chart_df <- bakgr_df |>
    dplyr::filter(månad == senaste_manad | år == forsta_ar & månad == "januari") |>
    dplyr::group_by(regionkod, region, kön, ålder, födelseregion, variabel) |>
    dplyr::mutate(forandr = round(((`varde` - dplyr::lag(varde)) / dplyr::lag(varde)) * 100, 1),
           region = rdverktyg::skapa_kortnamn_lan(region, T),                          # T = byt ut Riket mot Sverige om man skickar med Riket
           månad_kort = stringr::str_sub(månad, 1, 3),
           månad_år = paste0(månad_kort, " ", år),
           lbl_x = paste0(dplyr::lag(månad_år), " - ", månad_år))

  bagr_kat <- unique(chart_df$födelseregion)
  reg_txt <- unique(chart_df$region)

  skriv_diagram <- function(data_df, vald_reg, vald_alder, valt_kon) {

    kon_lbl <- if(valt_kon == "totalt") "" else paste0("_", valt_kon)
    kon_rubrik <- if(valt_kon == "totalt") "invånare" else valt_kon

    cont_lbl <- if (length(cont_txt) < 6) paste0("_", paste0(cont_txt, collapse = "_")) else ""

    diag_titel <- glue::glue("Förändring i arbetsmarknadsstatus för {kon_rubrik} {vald_alder} i {vald_reg}")
    diag_filnamn <- glue::glue("forandr_arbmstatus_{vald_reg}_{vald_alder}{kon_lbl}{cont_lbl}.png")

    # skapa df för detta specifika diagram
    diag_df = data_df |>
      dplyr::filter(!is.na(forandr),
             region == vald_reg,
             ålder == vald_alder,
             kön == valt_kon)

    diff <- max(diag_df$forandr, na.rm = TRUE) - min(diag_df$forandr, na.rm = TRUE)
    linje_varde <- diff / 400

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = diag_df,
      skickad_x_var = "lbl_x",
      skickad_y_var = "forandr",
      skickad_x_grupp = "variabel",
      geom_position_stack = TRUE,
      diagram_titel = diag_titel,
      facet_grp = "födelseregion",
      facet_scale = "fixed",
      facet_legend_bottom = TRUE,
      fokusera_varden = list(geom = "rect", ymin=-linje_varde, ymax=linje_varde, xmin=0, xmax=Inf, alpha=1, fill="black"),
      manual_y_axis_title = "procent",
      manual_color = rev(rddiagram::diagramfarger("rd_primar_nio")),
      manual_x_axis_text_hjust = 1,
      manual_x_axis_text_vjust = 1,
      output_mapp = utmapp,
      filnamn_diagram = diag_filnamn,
      skriv_till_diagramfil = skriv_diagramfil
      )
  } # slut funktion skriv_diagram


  arglist <- list(reg = reg_txt, alder = alder_txt, kon = kon_txt)                               # skapa lista med de två variabler vi vill göra diagram med
  crossarg <- expand.grid(arglist)
  # skriv_diagram() returnerar ett enda ggplot-objekt (inte en lista), så
  # pmap() ger redan en platt lista - purrr::flatten() på en lista av
  # ggplot-objekt kraschar numera ("must be a vector, not a <ggplot2::ggplot>
  # object") eftersom flatten() inte längre godtar klassade listor. Samma
  # bugg fanns i originalet (kraschade redan innan migreringen mot dagens
  # purrr-version) - flatten()-anropet är överflödigt och togs bort.
  dia_lista <- purrr::pmap(crossarg, ~skriv_diagram(data_df = chart_df, vald_reg = ..1, vald_alder = ..2, valt_kon = ..3))

  } # slut funktion diag
