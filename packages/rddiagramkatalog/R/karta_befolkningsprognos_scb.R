#' Karta: prognosticerad befolkningsförändring
#'
#' Choroplettkarta (kommun- eller länsnivå) över procentuell
#' befolkningsförändring enligt befolkningsprognosen, mellan senaste
#' tillgängliga befolkningsstatistik och ett valt framtida år. Returnerar
#' både en ggplot- och en leaflet-version - den förstnämnda kan skrivas
#' till en vanlig bildfil, den senare till en interaktiv html-fil (eller,
#' via [rddiagram::skriv_leaflet_till_fil()], en statisk ögonblicksbild).
#'
#' @param region_vekt Kommun- och/eller länskoder. Länskoder expanderas
#'   till alla kommuner i länet.
#' @param start_ar Startår för jämförelsen. `"9999"` (standard) = senaste
#'   tillgängliga år med befolkningsstatistik.
#' @param till_ar Slutår. `"+10"` (standard) = 10 år efter startåret, eller
#'   ett enskilt årtal.
#' @param utmapp Mapp där kartfilerna sparas. Måste finnas om
#'   `skriv_diagramfil` eller `skriv_leaflet_fil` är `TRUE`.
#' @param skriv_diagramfil TRUE om ggplot-kartan ska skrivas som bildfil.
#' @param skriv_leaflet_fil TRUE om leaflet-kartan ska skrivas till fil.
#' @param leaflet_filformat `"html"` (standard, interaktiv) eller `"png"`
#'   (statisk ögonblicksbild via webshot2 - kräver det paketet samt en
#'   lokalt installerad Chrome/Chromium, se [rddiagram::skriv_leaflet_till_fil()]).
#' @param filnamns_prefix Eget filnamn (utan filändelse), eller `NA`
#'   (standard) för automatisk namngivning.
#' @param kommuner_egetnamn Eget visningsnamn för de valda kommunerna i
#'   titeln, eller `NA` för automatiskt.
#' @param prognosskapare Vem som gjort prognosen, för källtexten.
#' @param diagram_capt,diagram_capt_leaflet Källtext för respektive
#'   kartversion. `<prognos_ar>`/`<prognosskapare>` byts ut automatiskt.
#' @param returnera_figur Skall kartorna returneras (i en lista).
#'
#' @return En namngiven lista: `ggplot` (ett `ggplot`-objekt) och
#'   `leaflet` (ett `leaflet`-objekt).
#' @export
karta_befolkningsprognos_scb <- function(
    region_vekt = "20",
    start_ar = "9999",
    till_ar = "+10",
    utmapp = rdverktyg::utskriftsmapp(),
    skriv_diagramfil = TRUE,
    skriv_leaflet_fil = TRUE,
    leaflet_filformat = "html",
    filnamns_prefix = NA,
    kommuner_egetnamn = NA,
    prognosskapare = "Region Dalarna",
    diagram_capt = "Källa: <prognosskapare>s befolkningsprognos från år <prognos_ar>\nBearbetning: Samhällsanalys, Region Dalarna",
    diagram_capt_leaflet = "Källa: <prognosskapare>s befolkningsprognos från år <prognos_ar>, bearbetning av Samhällsanalys, Region Dalarna",
    returnera_figur = TRUE
) {

  # ========================================== Allmän info ============================================
  # Choroplettkarta över prognosticerad befolkningsförändring (procent), per kommun. Både ggplot- och
  # leaflet-version byggs alltid - ggplot via den delade byggfunktionen rddiagram::skapa_koropletkarta_
  # ggplot() (samma mall/profil som alla andra kartor i paketet), leaflet handbyggt (ingen delad
  # leaflet-byggfunktion finns ännu).
  # ========================================== Inställningar ============================================

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil || skriv_leaflet_fil)

  kommunnyckel <- rdverktyg::hamtaregtab() |>                     # hämta alla kommunkoder
    dplyr::filter(nchar(regionkod) == 4) |>
    dplyr::rename(Kommun = region,
           Kommunkod = regionkod)

  region_kommuner <- region_vekt[nchar(region_vekt) == 4]            # lägg alla kommunkoder i en vektor
  region_lan <- region_vekt[nchar(region_vekt) == 2]                 # lägg alla länskoder i en vektor
  kommunkoder <- rdverktyg::hamtakommuner(region_lan, FALSE, FALSE) |>       # hämta alla kommuner som finns i län med länskod
    c(region_kommuner)                                               # och lägg ihop med skickade kommunkoder

  kommunkoder <- kommunkoder[kommunkoder %in% kommunnyckel$Kommunkod]      # behåll bara kommunkoder som är giltiga

  filnamn_kommuner <- rdverktyg::ar_alla_kommuner_i_ett_lan(kommunkoder, returnera_text = TRUE, returtext = paste0(kommunkoder, collapse = "_")) |> stringr::str_replace_all(" ", "_")

  if (is.na(kommuner_egetnamn)) {
    retur_kommuner <- rdverktyg::hamtaregion_kod_namn(kommunkoder)$region |> rdverktyg::list_komma_och()
    kommuner_egetnamn <- rdverktyg::ar_alla_kommuner_i_ett_lan(kommunkoder, returnera_text = TRUE, returtext = retur_kommuner)
  }

  # "+N" ska betyda "N år efter startåret" - hamta_befprognos_data() tolkar "+N" som prognosår + N, så
  # vi ber om "+/-(N-1)" för att landa på samma målår.
  till_ar_hamta <- if (stringr::str_detect(till_ar, "^[+-][0-9]+$")) {
    tecken <- stringr::str_sub(till_ar, 1, 1)
    paste0(tecken, as.numeric(stringr::str_sub(till_ar, 2)) - 1)
  } else till_ar

  if (any(stringr::str_sub(kommunkoder, 1, 2) != "20")) {
    befprogn <- rdhamtadata::hamta_befprognos_data(region_vekt = kommunkoder,
                                      url_prognos_vektor = "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefProgOsiktRegN",
                                      tid_vekt = till_ar_hamta)
  } else {
    befprogn <- rdhamtadata::hamta_befprognos_data(region_vekt = kommunkoder,
                                      tid_vekt = till_ar_hamta)
  }

  # Hjälpfunktion: kod för en given ålder i en given tabell - TAB638 och TAB5557 har olika koder för
  # samma ålder (t.ex. är öppna åldersklassen "100+" i TAB638 men "100+1" i TAB5557, och "totalt"-koden
  # heter "tot" respektive "TotSA"). aldrar = NA ger bara totalkoden, "*" ger alla individuella åldrar,
  # en karaktärsvektor med specifika åldrar ("0".."99"/"100+") ger just dem.
  hamta_alderskoder <- function(table_id, aldrar) {
    totalkod <- if (table_id == "TAB638") "tot" else "TotSA"
    if (all(is.na(aldrar))) return(totalkod)
    v <- pxweb2r::pxweb2_get_values(table_id, "Alder", quiet = TRUE)
    v <- v[grepl("^[0-9]+\\+? år$", v$label), ]
    v <- v[!duplicated(v$label), ]
    if (identical(aldrar, "*")) return(v$code)
    label_sokt <- ifelse(aldrar == "100+", "100+ år", paste0(aldrar, " år"))
    v$code[match(label_sokt, v$label)]
  }

  # Civilstånd saknar en riktig "totalt"-kod i TAB638 - alla fyra hämtas explicit och summeras ihop.
  # "9999" (senaste tillgängliga år) hanteras här själva, eftersom det bara var en konvention i den
  # gamla v1-funktionen och inte förstås av pxweb2r.
  civilstand_hamta <- c("ogifta", "gifta", "skilda", "änkor/änklingar")
  hamta_bef_folkmangd_v2 <- function(region_vekt, tid, aldrar) {
    if (identical(tid, "9999")) {
      tid <- as.character(max(as.numeric(c(
        pxweb2r::pxweb2_get_values("TAB638", "Tid", quiet = TRUE)$code,
        pxweb2r::pxweb2_get_values("TAB5557", "Tid", quiet = TRUE)$code
      ))))
    }
    historik <- pxweb2r::pxweb2_get_data(
      table = "TAB638",
      query = list(Region = region_vekt, Civilstand = civilstand_hamta,
                   Alder = hamta_alderskoder("TAB638", aldrar), Kon = c("män", "kvinnor"),
                   ContentsCode = "Folkmängd", Tid = tid),
      on_all_values_invalid = "null", quiet = TRUE)
    ckm <- pxweb2r::pxweb2_get_data(
      table = "TAB5557",
      query = list(Region = region_vekt, Civilstand = civilstand_hamta,
                   Alder = hamta_alderskoder("TAB5557", aldrar), Kon = c("män", "kvinnor"),
                   ContentsCode = "Folkmängd", Tid = tid),
      on_all_values_invalid = "null", quiet = TRUE)

    dplyr::bind_rows(historik, ckm) |>
      dplyr::rename(regionkod = region_kod, Folkmängd = value) |>
      dplyr::select(-tabellinnehåll) |>
      dplyr::mutate(ålder = ifelse(ålder %in% c("totalt, samtliga åldrar", "tot", "TotSA"), "totalt ålder", ålder)) |>
      dplyr::group_by(dplyr::across(-c(civilstånd, Folkmängd))) |>
      dplyr::summarise(Folkmängd = sum(Folkmängd, na.rm = TRUE), .groups = "drop")
  }

  befutfall <- hamta_bef_folkmangd_v2(region_vekt = kommunkoder, tid = start_ar, aldrar = "*") |>
    dplyr::mutate(prognos_ar = "utfall")

  bef_diff <- befutfall |>
    dplyr::bind_rows(befprogn) |>
    dplyr::group_by(år, regionkod, region, ålder, prognos_ar) |>
    dplyr::summarise(Folkmängd = sum(Folkmängd, na.rm = TRUE), .groups = "drop")

  min_ar <- min(bef_diff$år)
  max_ar <- max(bef_diff$år)
  prognos_ar <- unique(befprogn$prognos_ar)

  if (is.na(filnamns_prefix)) {
    filnamn_pre <- glue::glue("befprogn_karta_{filnamn_kommuner}_ar{min_ar}-{max_ar}")
  } else {
    filnamn_pre <- filnamns_prefix
  }

  bef_diff_tot <- bef_diff |>
    dplyr::filter(ålder != "totalt ålder") |>
    dplyr::group_by(år, regionkod, region) |>
    dplyr::summarise(Folkmängd = sum(Folkmängd, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(aldergrp = "totalt")

  bef_df <- bef_diff_tot |>
    tidyr::pivot_wider(names_from = "år", values_from = "Folkmängd") |>
    dplyr::mutate(diff = .data[[max_ar]] - .data[[min_ar]],
           diff_proc = (.data[[max_ar]] - .data[[min_ar]]) / .data[[min_ar]] * 100)

  kommuner_sf <- rdgis::hamta_karta("kommuner", regionkoder = kommunkoder)

  bef_karta <- kommuner_sf |>
    dplyr::left_join(bef_df, by = c("knkod" = "regionkod")) |>
    dplyr::filter(aldergrp == "totalt")

  # ================= ggplot-karta (delad byggfunktion) =================

  diagram_capt_utfyllt <- diagram_capt |>
    stringr::str_replace_all("<prognos_ar>", "{prognos_ar}") |>
    stringr::str_replace_all("<prognosskapare>", "{prognosskapare}") |>
    glue::glue()

  # Diskreta, nollcentrerade klasser (i stället för en kontinuerlig skala) - skapa_koropletkarta_
  # ggplot()s kontinuerliga läge (scale_fill_gradientn) centrerar mittfärgen på MITTEN AV DATAINTERVALLET,
  # inte på 0, vilket hade varit missvisande för ökning/minskning om intervallet inte råkar vara
  # symmetriskt. max_abs + pretty() ger symmetriska klassgränser som alltid inkluderar 0 som gräns.
  max_abs <- max(abs(bef_karta$diff_proc), na.rm = TRUE)
  diff_breaks <- pretty(c(-max_abs, max_abs), n = 6)

  befprognos_gg <- rddiagram::skapa_koropletkarta_ggplot(
    sf_objekt = bef_karta,
    vardekolumn = "diff_proc",
    klassindelning = diff_breaks,
    karta_fargvektor = c("#F15060", "white", "#0e5a4c"),
    legend_titel = "Förändring (%)",
    karta_titel = paste0("Prognosticerad befolkningsutveckling i ", kommuner_egetnamn),
    karta_caption = paste0("år ", min_ar, "-", max_ar, "\n", diagram_capt_utfyllt),
    filnamn = if (skriv_diagramfil) paste0(filnamn_pre, ".png") else NULL,
    output_mapp = if (skriv_diagramfil) utmapp else NULL
  )

  # ========= leaflet-karta =========

  diagram_capt_leaflet_utfyllt <- diagram_capt_leaflet |>
    stringr::str_replace_all("<prognos_ar>", "{prognos_ar}") |>
    stringr::str_replace_all("<prognosskapare>", "{prognosskapare}") |>
    glue::glue()

  fargskala <- leaflet::colorNumeric(
    palette = c("#F15060", "white", "#0e5a4c"),
    domain = c(-max_abs, max_abs)
  )

  bef_leaflet <- bef_karta |>
    sf::st_transform(crs = 4326)

  befprognos_leaflet <- leaflet::leaflet(bef_leaflet) |>
    rddiagram::lagg_till_bakgrundskarta() |>
    leaflet::addPolygons(
      fillColor = ~fargskala(diff_proc),
      weight = 1,
      color = "black",
      fillOpacity = 0.8,
      label = ~glue::glue("{knnamn}: {round(diff_proc, 1)} % ({diff} invånare)"),
      labelOptions = leaflet::labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "13px",
        direction = "auto")
    ) |>
    leaflet::addLegend(
      pal = fargskala,
      values = bef_karta$diff_proc,
      title = htmltools::HTML(glue::glue("Förändring (%)<br/><span style='font-size:11px; font-weight:normal'>år {min_ar}-{max_ar} enligt<br>befolkningsprognos</span>")),
      position = "bottomright"
    ) |>
    leaflet::addControl(
      html = diagram_capt_leaflet_utfyllt,
      position = "bottomleft"
    )

  if (skriv_leaflet_fil) {
    rddiagram::skriv_leaflet_till_fil(befprognos_leaflet, utmapp, filnamn_pre,
                                       karta_filformat = leaflet_filformat)
  }

  if (returnera_figur) {
    list(ggplot = befprognos_gg, leaflet = befprognos_leaflet)
  }
}
