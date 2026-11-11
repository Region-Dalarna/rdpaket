#' Karta: prognosticerad befolkningsförändring
#'
#' Koropletkarta (ggplot) på kommunnivå över procentuell befolkningsförändring
#' enligt befolkningsprognosen, i samma stil som [karta_gg_nettopendling_kommun_scb()]: grönt
#' för ökning och rött för minskning, med ett smalt vitt mittenintervall kring noll, mellan senaste tillgängliga
#' befolkningsstatistik och ett valt framtida år. Samma karta som interaktiv
#' leaflet-karta finns i [karta_lf_befolkningsprognos_kommun_scb()].
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
#' @param legend_position Teckenförklaringens placering. `"auto"` (standard)
#'   väljer ett hörn där den inte hamnar ovanpå någon kommun, annars utanför
#'   kartan. Se [rddiagram::skapa_koropletkarta_ggplot()].
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
#' @param returnera_figur TRUE (standard) för att returnera kartan. Minst en av
#'   `skriv_diagramfil` och `returnera_figur` måste vara `TRUE`.
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
    karta_fargvektor = NULL,
    legend_position = "auto",
    utmapp = rdverktyg::utskriftsmapp(),
    skriv_diagramfil = TRUE,
    filnamns_prefix = NA,
    kommuner_egetnamn = NA,
    prognosskapare = "Region Dalarna",
    diagram_capt = "Källa: <prognosskapare>s befolkningsprognos från år <prognos_ar>. Bearbetning: Samhällsanalys, Region Dalarna",
    returnera_figur = TRUE
) {

  intern_kontrollera_utdata(skriv_diagramfil, returnera_figur, "skriv_diagramfil")
  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  d <- intern_befprognos_kartdata(region_vekt, start_ar, till_ar, filnamns_prefix, kommuner_egetnamn)
  bef_karta <- d$bef_karta; min_ar <- d$min_ar; max_ar <- d$max_ar; prognos_ar <- d$prognos_ar
  kommuner_egetnamn <- d$kommuner_egetnamn; filnamn_pre <- d$filnamn_pre; max_abs <- d$max_abs


  diagram_capt_utfyllt <- diagram_capt |>
    stringr::str_replace_all("<prognos_ar>", "{prognos_ar}") |>
    stringr::str_replace_all("<prognosskapare>", "{prognosskapare}") |>
    glue::glue()

  if (is.null(karta_fargvektor) || all(is.na(karta_fargvektor))) {
    karta_fargvektor <- rddiagram::diagramfarger("rd_karta_rod_gron")
  }

  # Diskreta klasser, symmetriska kring noll med ett smalt vitt mittenintervall (se
  # intern_nollcentrerade_klassgranser()). En kontinuerlig skala hade centrerat mittfärgen på mitten av
  # dataintervallet, inte på 0, vilket vore missvisande för minskning och ökning.
  befprognos_gg <- rddiagram::skapa_koropletkarta_ggplot(
    sf_objekt = bef_karta,
    vardekolumn = "diff_proc",
    klassindelning = intern_nollcentrerade_klassgranser(max_abs, minsta_mitt = 0.1),
    karta_fargvektor = karta_fargvektor,
    karta_farg_hogst = NULL,
    legend_titel = "Förändring (%)",
    legend_position = legend_position,
    legend_titel_storlek = 10, legend_text_storlek = 8, legend_objekt_storlek = 1,
    karta_titel = paste0("Prognosticerad befolkningsutveckling i ", kommuner_egetnamn, " år ", min_ar, "–", max_ar),
    karta_titel_avstand = 12,
    karta_caption = paste0(diagram_capt_utfyllt, "\nFörändring i procent mellan år ", min_ar, " och ", max_ar),
    karta_caption_storlek = 7,
    filnamn = if (skriv_diagramfil) paste0(filnamn_pre, ".png") else NULL,
    output_mapp = if (skriv_diagramfil) utmapp else NULL
  )

  resultat_lista <- stats::setNames(list(befprognos_gg), filnamn_pre)
  if (returnera_figur) invisible(resultat_lista)
}
