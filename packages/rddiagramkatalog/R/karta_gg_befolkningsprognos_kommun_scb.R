#' Karta: prognosticerad befolkningsförändring
#'
#' Koropletkarta (ggplot) på kommunnivå över procentuell befolkningsförändring
#' enligt befolkningsprognosen, mellan senaste tillgängliga
#' befolkningsstatistik och ett valt framtida år. Samma karta som interaktiv
#' leaflet-karta finns i [karta_lf_befolkningsprognos_kommun_scb()].
#'
#' @param region_vekt Kommun- och/eller länskoder. Länskoder expanderas
#'   till alla kommuner i länet.
#' @param start_ar Startår för jämförelsen. `"9999"` (standard) = senaste
#'   tillgängliga år med befolkningsstatistik.
#' @param till_ar Slutår. `"+10"` (standard) = 10 år efter startåret, eller
#'   ett enskilt årtal.
#' @param utmapp Mapp där kartfilen sparas. Måste finnas om
#'   `skriv_diagramfil` är `TRUE`.
#' @param skriv_diagramfil TRUE om kartan ska skrivas som bildfil.
#' @param filnamns_prefix Eget filnamn (utan filändelse), eller `NA`
#'   (standard) för automatisk namngivning.
#' @param kommuner_egetnamn Eget visningsnamn för de valda kommunerna i
#'   titeln, eller `NA` för automatiskt.
#' @param prognosskapare Vem som gjort prognosen, för källtexten.
#' @param diagram_capt Källtext. `<prognos_ar>`/`<prognosskapare>` byts ut
#'   automatiskt.
#' @param returnera_figur TRUE (standard) för att returnera kartan.
#'
#' @return Osynligt: en namngiven lista med ett `ggplot`-objekt. Namnet är
#'   bildfilens namn utan ändelse. Filsökvägen skrivs ut som ett meddelande
#'   när filen skrivs.
#' @seealso [karta_lf_befolkningsprognos_kommun_scb()]
#' @export
karta_gg_befolkningsprognos_kommun_scb <- function(
    region_vekt = "20",
    start_ar = "9999",
    till_ar = "+10",
    utmapp = rdverktyg::utskriftsmapp(),
    skriv_diagramfil = TRUE,
    filnamns_prefix = NA,
    kommuner_egetnamn = NA,
    prognosskapare = "Region Dalarna",
    diagram_capt = "Källa: <prognosskapare>s befolkningsprognos från år <prognos_ar>\nBearbetning: Samhällsanalys, Region Dalarna",
    returnera_figur = TRUE
) {

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  d <- intern_befprognos_kartdata(region_vekt, start_ar, till_ar, filnamns_prefix, kommuner_egetnamn)
  bef_karta <- d$bef_karta; min_ar <- d$min_ar; max_ar <- d$max_ar; prognos_ar <- d$prognos_ar
  kommuner_egetnamn <- d$kommuner_egetnamn; filnamn_pre <- d$filnamn_pre; max_abs <- d$max_abs


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

  resultat_lista <- stats::setNames(list(befprognos_gg), filnamn_pre)
  if (returnera_figur) invisible(resultat_lista)
}
