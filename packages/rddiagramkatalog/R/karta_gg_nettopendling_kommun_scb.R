#' Karta: nettopendling per kommun
#'
#' Koropletkarta (ggplot) över nettopendling per kommun i ett eller flera län:
#' inpendlare minus utpendlare. Kommuner med fler som pendlar in än ut är gröna
#' och kommuner med fler som pendlar ut än in är röda, med vitt kring noll.
#' Klassgränserna är symmetriska kring noll, med ett mycket smalt vitt
#' mittenintervall (cirka 1 % av största värdet, t.ex. -50 till 50) för
#' kommuner där pendlingen i princip är i balans, och anpassas efter hur stor
#' nettopendlingen är.
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
#' @param karta_fargvektor Färger från negativ via noll till positiv
#'   nettopendling. De tolkas som ankarfärger och interpoleras till en färg per
#'   klass, så mitten bör vara vit. `NULL` (standard) =
#'   `rddiagram::diagramfarger("rd_karta_rod_gron")`.
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
    karta_fargvektor = NULL,
    utmapp = rdverktyg::utskriftsmapp(),
    skriv_diagramfil = TRUE,
    returnera_figur = TRUE
) {

  # ========================================== Allmän info ============================================
  # Koropletkarta över nettopendling per kommun, en karta per län. Delar datahämtning med
  # karta_lf_pendlingsfloden_scb() via intern_hamta_pendling().
  # ========================================== Inställningar ============================================

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  if (is.null(karta_fargvektor) || all(is.na(karta_fargvektor))) {
    karta_fargvektor <- rddiagram::diagramfarger("rd_karta_rod_gron")
  }

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
    breaks <- intern_nollcentrerade_klassgranser(max_abs)
    lan_namn <- unique(rdverktyg::hamtaregion_kod_namn(lanskod)$region)
    filnamn_bas <- paste0("nettopendling_", lan_namn, "_ar", unikt_ar)

    gg <- rddiagram::skapa_koropletkarta_ggplot(
      sf_objekt = netto_karta,
      vardekolumn = "nettopendling",
      klassindelning = breaks,
      karta_fargvektor = karta_fargvektor,
      karta_farg_hogst = NULL,
      legend_position = "top-right",
      karta_titel_avstand = 12,
      karta_caption_storlek = 7,
      legend_titel = "Nettopendling",
      karta_titel = paste0("Nettopendling i ", lan_namn, " år ", unikt_ar),
      karta_caption = "Källa: SCB (RAMS/BAS). Bearbetning: Samhällsanalys, Region Dalarna\nNettopendling = inpendlare minus utpendlare",
      filnamn = if (skriv_diagramfil) paste0(filnamn_bas, ".png") else NULL,
      output_mapp = if (skriv_diagramfil) utmapp else NULL
    )
    stats::setNames(list(gg), filnamn_bas)
  }

  resultat_lista <- purrr::list_flatten(purrr::map(lanskoder, bygg_karta))

  if (returnera_figur) invisible(resultat_lista)
}
