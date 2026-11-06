#' Diagram: arbetslöshet i procent, facet per utbildningsnivå eller -inriktning
#'
#' Facetdiagram över arbetslöshet i procent (inskrivna arbetslösa på
#' Arbetsförmedlingen / arbetskraften, dvs. förvärvsarbetande plus
#' arbetslösa) per utbildningsnivå eller utbildningsinriktning, över tid
#' (år). Från SCB:s
#' tabell TAB6367 (Arbetsmarknadsställning (U1a) i befolkningen 20-64 år,
#' nattbefolkning).
#'
#' TAB6367 har en enda utbildningsdimension (Utbildngrupp) som blandar
#' utbildningsnivå, utbildningsinriktning OCH ca 120 detaljerade
#' utbildningsgrupper (SUN-koder) i samma dimension - alldeles för många för
#' ett facetdiagram. Funktionen fokuserar därför på de två grova
#' indelningarna: `utb_typ = "niva"` (4 nivåer: förgymnasial, gymnasial,
#' eftergymnasial <3 år, eftergymnasial 3+ år) och `utb_typ = "inriktning"`
#' (8 inriktningar: pedagogik, humaniora/konst, samhällsvetenskap m.fl.,
#' naturvetenskap/data, teknik, lant-/skogsbruk, hälso-/sjukvård, tjänster).
#' Vill man ha andra/fler koder (t.ex. `"09"` okänd nivå, `"9"` okänd
#' inriktning, eller någon av de ~120 detaljerade SUN-grupperna) går det att
#' skicka med egna koder i `utb_vekt`.
#'
#' TAB6367 täcker 2019-2024 (uppdateras). TAB5400 är samma tabelltyp (samma
#' dimensioner, samma ContentsCode-texter) men med äldre, numera frysta data
#' (2006-2018, "uppdateras ej" enligt SCB). Som standard hämtas båda och
#' slås ihop till en sammanhängande tidsserie 2006-2024
#' (`ta_med_tidigare_data = FALSE` hämtar bara TAB6367, dvs. 2019-2024).
#'
#' \if{html}{\figure{diag_arbloshet_procent_utbildning_facet_ar_scb.png}{options: width="100%" alt="Exempel: diag_arbloshet_procent_utbildning_facet_ar_scb() med standardinställningar"}}
#'
#' @param region_vekt En eller flera regioner, t.ex. `c("20", "17", "21")`. Flera regioner jämförs då som linjer inom varje facet.
#' @param utb_typ Vilken utbildningsindelning som ska facet:as på: `"niva"` eller `"inriktning"`.
#' @param utb_vekt NA = default-koder för `utb_typ`, annars egna Utbildngrupp-koder.
#' @param ta_med_tidigare_data TRUE = slå ihop med TAB5400 (2006-2018) till en tidsserie 2006-2024.
#' @param lagga_till_punkter TRUE lägger till punkter vid varje observation.
#' @param titel_bredd Radbrytningsbredd för diagramtiteln.
#' @param tid_koder Vilka år som ska hämtas, per tabell. `"*"` = alla, `"9999"` = senaste,
#'   eller `"2015:9999"` för "från 2015 till och med senaste tillgängliga år".
#'   Om intervallet inte täcker TAB5400 (2006-2018) hoppas den tabellen
#'   automatiskt över, även om `ta_med_tidigare_data = TRUE`.
#' @param diagram_capt Diagrammets källtext.
#' @param utmapp Mapp där diagram ska sparas. Måste finnas om `skriv_diagramfil = TRUE`.
#' @param diag_fargvekt Valfri färgvektor, standard: `rddiagram::diagramfarger("rus_sex")`.
#' @param logga_sokvag NA om man vill köra med standardloggan, bra att sätta en lokal om man är på resande fot.
#' @param visa_logga_i_diagram TRUE om logga ska visas i diagrammet.
#' @param skriv_diagramfil TRUE om diagram ska skrivas till fil.
#' @param facet_scale `"fixed"` gör att nivåerna/inriktningarna går att jämföra rakt av, `"free_y"` ger varje facet egen skala.
#' @param demo Sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren.
#'
#' @return En namngiven lista med ett `ggplot`-objekt.
#' @export
diag_arbloshet_procent_utbildning_facet_ar_scb <- function(
    region_vekt = "20",
    utb_typ = c("niva", "inriktning"),
    utb_vekt = NA,
    ta_med_tidigare_data = TRUE,
    lagga_till_punkter = FALSE,
    titel_bredd = 80,
    tid_koder = "*",
    diagram_capt = "Källa: SCB:s öppna statistikdatabas, Arbetsmarknadsställning (U1a)\nBearbetning: Samhällsanalys, Region Dalarna",
    utmapp = rdverktyg::utskriftsmapp(),
    diag_fargvekt = NA,
    logga_sokvag = NA,
    visa_logga_i_diagram = TRUE,
    skriv_diagramfil = TRUE,
    facet_scale = "fixed",
    demo = FALSE
    ) {

  utb_typ <- match.arg(utb_typ)

  # om parametern demo är satt till TRUE: inga exempelbilder är uppladdade för det här skriptet ännu.
  if (demo){
    cat("Inga exempelbilder är uppladdade för det här skriptet ännu.\n")
    rdverktyg::stop_tyst()
  }

  # Default-koder för Utbildngrupp beroende på utb_typ. Koderna är SUN2020-nivå/inriktningskoder,
  # se https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__UF__UF0506/Educ3MM/ - "00N" (samtliga
  # utbildningsnivåer) och "00I" (samtliga utbildningsinriktningar) är totalsummor och "09"/"9" är
  # okänd nivå/inriktning - utelämnas som standard men kan läggas till via utb_vekt.
  if (all(is.na(utb_vekt))) {
    utb_vekt <- if (utb_typ == "niva") {
      c("02", "03", "04", "05")
    } else {
      c("1", "2", "3", "4", "5", "6", "7", "8")
    }
  }

  # Radbryter långa utbildningsnamn ("samhällsvetenskap, juridik, handel, administration",
  # "eftergymnasial utbildning, mindre än 3 år" m.fl.) vid ordgränser så att facet-rubrikerna
  # radbryts i stället för att klippas av i de smala facet-panelerna. "/" och "," räknas inte som
  # ordgränser av stringr::str_wrap(), så ett mellanslag läggs till efter dem innan radbrytningen.
  wrap_kat <- function(x) stringr::str_wrap(stringr::str_replace_all(x, "([/,])(?=\\S)", "\\1 "), width = 25)

  # rus_sex är standardpaletten för RD:s diagram - används även här om inget annat skickas med.
  if (all(is.na(diag_fargvekt))) {
    diag_fargvekt <- rddiagram::diagramfarger("rus_sex")
  }

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  # Länk till tabell: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0208__AM0208C/ArbStallnU1Ar/
  # ContentsCode hämtas som text (inte kod) - A = förvärvsarbetande, B = inskrivna arbetslösa på
  # Arbetsförmedlingen. Arbetskraften = A + B (C, "inte i arbetskraften", räknas INTE in i nämnaren).
  hamta_tabell <- function(tabell, tid) {
    pxweb2r::pxweb2_get_data(
      table = tabell,
      query = list(
        Region = region_vekt,
        Utbildngrupp = utb_vekt,
        ContentsCode = c("Förvärvsarbetande (A)", "Inskrivna arbetslösa på Arbetsförmedlingen (B)"),
        Tid = tid
      ), quiet = TRUE)
  }

  # TAB6367 (2019-2024) och - om ta_med_tidigare_data - TAB5400 (2006-2018, fryst historisk data)
  # har identiska dimensioner/kolumnnamn och överlappar inte i tid, så de kan radbindas direkt.
  arbl_raw <- hamta_tabell("TAB6367", intern_tolka_tidskoder("TAB6367", tid_koder))
  if (ta_med_tidigare_data) {
    # Om det begärda intervallet inte täcker TAB5400 alls (t.ex. tid_koder = "2020:9999")
    # blir tid_5400 en tom vektor - hoppa då över tabellen i stället för att skicka en
    # tom Tid-fråga till API:et.
    tid_5400 <- intern_tolka_tidskoder("TAB5400", tid_koder)
    if (length(tid_5400) > 0) {
      arbl_raw <- dplyr::bind_rows(arbl_raw, hamta_tabell("TAB5400", tid_5400))
    }
  }

  arbl_df <- arbl_raw |>
    dplyr::rename(regionkod = region_kod) |>
    tidyr::pivot_wider(names_from = tabellinnehåll, values_from = value) |>
    dplyr::rename(forvarvsarbetande = `Förvärvsarbetande (A)`,
                  arbetslosa = `Inskrivna arbetslösa på Arbetsförmedlingen (B)`) |>
    dplyr::mutate(andel_arbetslosa = round(arbetslosa / (forvarvsarbetande + arbetslosa) * 100, 1),
                  utbildning = wrap_kat(utbildning),
                  # Facet-panelerna ska komma i kodordning (samma ordning som utb_vekt), inte
                  # bokstavsordning på klartexten - utbildning görs om till en factor vars nivåer
                  # är de wrap:ade klartextnamnen men ordnade efter utbildning_kod:s plats i utb_vekt.
                  utbildning = factor(utbildning, levels = unique(utbildning[order(match(utbildning_kod, utb_vekt))]))) |>
    dplyr::arrange(år)

  vald_region <- rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(region_vekt)$region, byt_ut_riket_mot_sverige = TRUE))

  utb_typ_titel <- if (utb_typ == "niva") "utbildningsnivå" else "utbildningsinriktning"
  diagram_titel <- stringr::str_wrap(paste0("Arbetslöshet efter ", utb_typ_titel, " i ", vald_region, ", nattbefolkning 20-64 år"), titel_bredd)
  diagramfil <- paste0("arbloshet_procent_utb_", utb_typ, "_facet_", paste0(region_vekt, collapse = "_"), ".png")

  flera_regioner <- length(unique(arbl_df$regionkod)) > 1

  gg_obj <- rddiagram::SkapaLinjeDiagram(
    skickad_df = arbl_df,
    skickad_x_var = "år",
    skickad_y_var = "andel_arbetslosa",
    skickad_x_grupp = if (flera_regioner) "region" else NULL,
    lagga_till_punkter = lagga_till_punkter,
    diagram_titel = diagram_titel,
    diagram_capt = diagram_capt,
    manual_y_axis_title = "procent",
    manual_color = if (flera_regioner) diag_fargvekt else diag_fargvekt[1],
    legend_vand_ordning = TRUE,
    legend_byrow = TRUE,
    facet_grp = "utbildning",
    facet_scale = facet_scale,
    facet_x_axis_storlek = 8,
    facet_legend_bottom = TRUE,
    stodlinjer_avrunda_fem = TRUE,
    skriv_till_diagramfil = skriv_diagramfil,
    lagg_pa_logga = visa_logga_i_diagram,
    logga_path = logga_sokvag,
    output_mapp = utmapp,
    filnamn_diagram = diagramfil)

  retur_list <- list(gg_obj)
  names(retur_list) <- stringr::str_remove(diagramfil, ".png")

  return(retur_list)
} # slut funktion
