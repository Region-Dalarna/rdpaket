#' Leaflet-karta: prognosticerad befolkningsförändring
#'
#' Interaktiv leaflet-karta på kommunnivå över procentuell
#' befolkningsförändring enligt befolkningsprognosen, med samma klasser och färger som
#' [karta_gg_befolkningsprognos_kommun_scb()], mellan senaste
#' tillgängliga befolkningsstatistik och ett valt framtida år. Håll muspekaren
#' över en kommun för att se förändringen i procent och antal invånare. Samma
#' karta som bild (ggplot) finns i [karta_gg_befolkningsprognos_kommun_scb()].
#'
#' @param region_vekt Kommun- och/eller länskoder. Länskoder expanderas
#'   till alla kommuner i länet.
#' @param start_ar Startår för jämförelsen. `"9999"` (standard) = senaste
#'   tillgängliga år med befolkningsstatistik.
#' @param till_ar Slutår. `"+10"` (standard) = 10 år efter startåret, eller
#'   ett enskilt årtal.
#' @param karta_fargvektor Färger från minskning via noll till ökning, tolkade
#'   som ankarfärger och interpolerade till en färg per klass. `NULL`
#'   (standard) = `rddiagram::diagramfarger("rd_karta_rod_gron")`.
#' @param utmapp Mapp där kartfilen sparas. Måste finnas om
#'   `skriv_leaflet_fil` är `TRUE`.
#' @param skriv_leaflet_fil TRUE om kartan ska skrivas till fil.
#' @param leaflet_filformat `"html"` (standard, interaktiv) eller `"png"`
#'   (statisk ögonblicksbild via webshot2 - kräver det paketet samt en
#'   lokalt installerad Chrome/Chromium, se [rddiagram::skriv_leaflet_till_fil()]).
#' @param filnamns_prefix Eget filnamn (utan filändelse), eller `NA`
#'   (standard) för automatisk namngivning.
#' @param kommuner_egetnamn Eget visningsnamn för de valda kommunerna i
#'   titeln, eller `NA` för automatiskt.
#' @param prognosskapare Vem som gjort prognosen, för källtexten.
#' @param diagram_capt_leaflet Källtext. `<prognos_ar>`/`<prognosskapare>`
#'   byts ut automatiskt.
#' @param returnera_figur TRUE (standard) för att returnera kartan. Minst en av
#'   `skriv_leaflet_fil` och `returnera_figur` måste vara `TRUE`.
#'
#' @return Osynligt: en namngiven lista med ett `leaflet`-objekt. Namnet är
#'   filens namn utan ändelse. Filsökvägen skrivs ut som ett meddelande
#'   när filen skrivs.
#' @seealso [karta_gg_befolkningsprognos_kommun_scb()]
#' @export
karta_lf_befolkningsprognos_kommun_scb <- function(
    region_vekt = "20",
    start_ar = "9999",
    till_ar = "+10",
    karta_fargvektor = NULL,
    utmapp = rdverktyg::utskriftsmapp(),
    skriv_leaflet_fil = TRUE,
    leaflet_filformat = "html",
    filnamns_prefix = NA,
    kommuner_egetnamn = NA,
    prognosskapare = "Region Dalarna",
    diagram_capt_leaflet = "Källa: <prognosskapare>s befolkningsprognos från år <prognos_ar>. Bearbetning: Samhällsanalys, Region Dalarna",
    returnera_figur = TRUE
) {

  intern_kontrollera_utdata(skriv_leaflet_fil, returnera_figur, "skriv_leaflet_fil")
  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_leaflet_fil)

  d <- intern_befprognos_kartdata(region_vekt, start_ar, till_ar, filnamns_prefix, kommuner_egetnamn)
  bef_karta <- d$bef_karta; min_ar <- d$min_ar; max_ar <- d$max_ar; prognos_ar <- d$prognos_ar
  filnamn_pre <- d$filnamn_pre; max_abs <- d$max_abs


  diagram_capt_leaflet_utfyllt <- diagram_capt_leaflet |>
    stringr::str_replace_all("<prognos_ar>", "{prognos_ar}") |>
    stringr::str_replace_all("<prognosskapare>", "{prognosskapare}") |>
    glue::glue()

  if (is.null(karta_fargvektor) || all(is.na(karta_fargvektor))) {
    karta_fargvektor <- rddiagram::diagramfarger("rd_karta_rod_gron")
  }

  # samma klasser och färger som i ggplot-kartan
  klassgranser <- intern_nollcentrerade_klassgranser(max_abs, minsta_mitt = 0.1)
  fargskala <- leaflet::colorBin(
    palette = grDevices::colorRampPalette(karta_fargvektor)(length(klassgranser) - 1),
    domain = range(klassgranser),
    bins = klassgranser
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
      labFormat = function(type, cuts, p) {
        tal <- scales::number(cuts, accuracy = if (all(cuts == round(cuts))) 1 else 0.1, big.mark = " ", decimal.mark = ",")
        paste0(tal[-length(tal)], " – ", tal[-1])
      },
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

  resultat_lista <- stats::setNames(list(befprognos_leaflet), filnamn_pre)
  if (returnera_figur) invisible(resultat_lista)
}
