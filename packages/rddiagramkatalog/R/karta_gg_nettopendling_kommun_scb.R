#' Karta: nettopendling per kommun
#'
#' Koropletkarta (ggplot) över nettopendling per kommun i ett eller flera län:
#' inpendlare minus utpendlare. Positiva värden (fler som pendlar in än ut)
#' och negativa värden får olika färg, med vitt vid noll.
#'
#' Pendlingsdata finns för åren 1993 och framåt (se
#' [rdhamtadata::hamta_pendling_rams_bas_scb()]), men kartan visar alltid
#' dagens kommunindelning. Äldre år visar därför pendling mellan dagens
#' kommuner.
#'
#' Flödeslinjer från en enskild kommun, som interaktiv karta, finns i
#' [karta_lf_pendlingsfloden_scb()].
#'
#' @param vald_lan_kod Ett eller flera län, som länskod. Standard `"20"`
#'   (Dalarnas län). En kommunkod fungerar också och ger länet kommunen
#'   ligger i.
#' @param valt_ar År, 1993 eller senare. `"9999"` (standard) = senaste
#'   tillgängliga.
#' @param karta_fargvektor Tre färger: negativ nettopendling, mitten (noll)
#'   och positiv nettopendling.
#' @param utmapp Mapp där kartfilerna sparas. Måste finnas om
#'   `skriv_diagramfil` är `TRUE`.
#' @param skriv_diagramfil TRUE om kartan ska skrivas som bildfil.
#' @param returnera_figur TRUE (standard) för att returnera kartorna.
#'
#' @return Osynligt: en namngiven lista av `ggplot`-objekt, en per län. Namnet
#'   är bildfilens namn utan ändelse. Filsökvägarna skrivs ut som meddelanden
#'   när filer skrivs.
#' @seealso [karta_lf_pendlingsfloden_scb()]
#' @export
karta_gg_nettopendling_kommun_scb <- function(
    vald_lan_kod = "20",
    valt_ar = "9999",
    karta_fargvektor = c("darkred", "white", "darkblue"),
    utmapp = rdverktyg::utskriftsmapp(),
    skriv_diagramfil = TRUE,
    returnera_figur = TRUE
) {

  # ========================================== Allmän info ============================================
  # Koropletkarta över nettopendling per kommun, en karta per län. Delar datahämtning med
  # karta_lf_pendlingsfloden_scb() via intern_hamta_pendling().
  # ========================================== Inställningar ============================================

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  lanskoder <- unique(stringr::str_sub(vald_lan_kod, 1, 2))
  kommuner_gis <- rdgis::hamta_karta("kommuner")

  bygg_karta <- function(lanskod) {
    kommunkoder_i_lan <- rdverktyg::hamtakommuner(lanskod, FALSE, FALSE)
    pendling_df <- intern_hamta_pendling(kommunkoder_i_lan, valt_ar)
    unikt_ar <- unique(pendling_df$år)

    # inpendlare (arbetsställekommun = kommunen) minus utpendlare (bostadskommun = kommunen)
    inp <- pendling_df |>
      dplyr::filter(regionkod_arb %in% kommunkoder_i_lan) |>
      dplyr::summarise(inpendlare = sum(pendlare, na.rm = TRUE), .by = regionkod_arb) |>
      dplyr::rename(regionkod = regionkod_arb)
    utp <- pendling_df |>
      dplyr::filter(regionkod_bo %in% kommunkoder_i_lan) |>
      dplyr::summarise(utpendlare = sum(pendlare, na.rm = TRUE), .by = regionkod_bo) |>
      dplyr::rename(regionkod = regionkod_bo)
    netto_df <- dplyr::full_join(inp, utp, by = "regionkod") |>
      dplyr::mutate(dplyr::across(c(inpendlare, utpendlare), ~ tidyr::replace_na(.x, 0)),
                    nettopendling = inpendlare - utpendlare)

    netto_karta <- kommuner_gis |>
      dplyr::filter(knkod %in% kommunkoder_i_lan) |>
      dplyr::left_join(netto_df, by = c("knkod" = "regionkod")) |>
      dplyr::mutate(nettopendling = tidyr::replace_na(nettopendling, 0))

    max_abs <- max(abs(netto_karta$nettopendling), na.rm = TRUE)
    breaks <- pretty(c(-max_abs, max_abs), n = 6)
    lan_namn <- unique(rdverktyg::hamtaregion_kod_namn(lanskod)$region)
    filnamn_bas <- paste0("nettopendling_", lan_namn, "_ar", unikt_ar)

    gg <- rddiagram::skapa_koropletkarta_ggplot(
      sf_objekt = netto_karta,
      vardekolumn = "nettopendling",
      klassindelning = breaks,
      karta_fargvektor = karta_fargvektor,
      legend_titel = "Nettopendling",
      karta_titel = paste0("Nettopendling per kommun i ", lan_namn),
      karta_caption = paste0("år ", unikt_ar, "\nKälla: SCB (RAMS/BAS)\nBearbetning: Samhällsanalys, Region Dalarna\nNettopendling = inpendlare minus utpendlare"),
      filnamn = if (skriv_diagramfil) paste0(filnamn_bas, ".png") else NULL,
      output_mapp = if (skriv_diagramfil) utmapp else NULL
    )
    stats::setNames(list(gg), filnamn_bas)
  }

  resultat_lista <- purrr::list_flatten(purrr::map(lanskoder, bygg_karta))

  if (returnera_figur) invisible(resultat_lista)
}
