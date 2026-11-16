#' Diagram: arbetslöshet eller sysselsättningsgrad, facet per utbildningsnivå eller -inriktning
#'
#' Facetdiagram över arbetslöshet eller sysselsättningsgrad i procent per utbildningsnivå eller
#' utbildningsinriktning, över tid (år). Från SCB:s tabell TAB6367 (Arbetsmarknadsställning (U1a) i
#' befolkningen 20-64 år, nattbefolkning).
#'
#' Måttet väljs med `matt`:
#' * `"arbetsloshet"` (standard): inskrivna arbetslösa på Arbetsförmedlingen i procent av arbetskraften,
#'   dvs. förvärvsarbetande plus arbetslösa.
#' * `"sysselsattningsgrad"`: förvärvsarbetande i procent av alla i åldern 20-64 år (förvärvsarbetande,
#'   inskrivna arbetslösa och de som inte är i arbetskraften).
#' * `"antal_sysselsatta"`: antalet förvärvsarbetande (sysselsatta) i åldern 20-64 år. Grupperna skiljer sig
#'   mycket i storlek, så `facet_scale = "free_y"` ger ofta ett mer läsbart diagram.
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
#' Funktionen hette tidigare `diag_arbloshet_procent_utbildning_facet_ar_scb()`, som finns kvar som ett
#' föråldrat alias.
#'
#' \if{html}{\figure{diag_arbloshet_syss_utbildning_facet_ar_scb.png}{options: width="100%" alt="Exempel: diag_arbloshet_syss_utbildning_facet_ar_scb() med standardinställningar"}}
#'
#' @param region_vekt En eller flera regioner, t.ex. `c("20", "17", "21")`. Flera regioner jämförs då som linjer inom varje facet.
#' @param matt Vilket mått som visas: `"arbetsloshet"` (standard), `"sysselsattningsgrad"` eller `"antal_sysselsatta"`.
#' @param utb_typ Vilken utbildningsindelning som ska facet:as på: `"niva"` eller `"inriktning"`.
#' @param utb_vekt NA = default-koder för `utb_typ`, annars egna Utbildngrupp-koder.
#' @param ta_med_tidigare_data TRUE = slå ihop med TAB5400 (2006-2018) till en tidsserie 2006-2024.
#' @param lagga_till_punkter TRUE lägger till punkter vid varje observation.
#' @param titel_bredd Radbrytningsbredd för diagramtiteln.
#' @param tid_koder Vilka år som ska hämtas, per tabell. `"*"` = alla, `"9999"` = senaste,
#'   `"2015:9999"` för "från 2015 till och med senaste tillgängliga år" eller enskilda år (t.ex. `"2024"`
#'   eller `c("2010", "2020")`; de år som saknas i en tabell hoppas över).
#'   Om intervallet inte täcker TAB5400 (2006-2018) hoppas den tabellen
#'   automatiskt över, även om `ta_med_tidigare_data = TRUE`.
#' @param diagram_capt Diagrammets källtext. `NA` (standard) = källa och bearbetning, samt en rad som förklarar måttet.
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
diag_arbloshet_syss_utbildning_facet_ar_scb <- function(
    region_vekt = "20",
    matt = c("arbetsloshet", "sysselsattningsgrad", "antal_sysselsatta"),
    utb_typ = c("niva", "inriktning"),
    utb_vekt = NA,
    ta_med_tidigare_data = TRUE,
    lagga_till_punkter = FALSE,
    titel_bredd = 80,
    tid_koder = "*",
    diagram_capt = NA,
    utmapp = rdverktyg::utskriftsmapp(),
    diag_fargvekt = NA,
    logga_sokvag = NA,
    visa_logga_i_diagram = TRUE,
    skriv_diagramfil = TRUE,
    facet_scale = "fixed",
    demo = FALSE
    ) {

  utb_typ <- match.arg(utb_typ)
  matt <- match.arg(matt)

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
  # Arbetsförmedlingen, A+B+C = alla i åldern 20-64 år. Arbetslöshet = B / (A + B) (C, "inte i arbetskraften",
  # räknas INTE in i nämnaren); sysselsättningsgrad = A / (A + B + C).
  hamta_tabell <- function(tabell, tid) {
    pxweb2r::pxweb2_get_data(
      table = tabell,
      query = list(
        Region = region_vekt,
        Utbildngrupp = utb_vekt,
        ContentsCode = c("Förvärvsarbetande (A)", "Inskrivna arbetslösa på Arbetsförmedlingen (B)",
                         "Totalt antal personer (A+B+C)"),
        Tid = tid
      ), quiet = TRUE)
  }

  # Vilka år som ska hämtas från respektive tabell. Intervall ("2015:9999") och "*" tolkas av
  # intern_tolka_tidskoder(); enskilda år (t.ex. "2024" eller c("2010", "2020")) skärs mot de år som
  # finns i tabellen, så att en tabell som inte har året hoppas över i stället för att ge fel.
  # "9999" (senaste året) finns alltid i TAB6367 och hämtas inte från TAB5400.
  tid_for_tabell <- function(tabell) {
    tid <- intern_tolka_tidskoder(tabell, tid_koder)
    if (!any(tid_koder %in% c("*", "9999")) && length(tid_koder) > 0) {
      tid <- intersect(tid, pxweb2r::pxweb2_get_values(tabell, "Tid", quiet = TRUE)$code)
    }
    tid
  }
  tid_6367 <- tid_for_tabell("TAB6367")
  tid_5400 <- if (ta_med_tidigare_data && !identical(tid_koder, "9999")) tid_for_tabell("TAB5400") else character(0)
  if (length(tid_6367) == 0 && length(tid_5400) == 0) {
    stop("Inga av de begärda åren (tid_koder) finns i SCB:s tabeller för arbetsmarknadsställning efter utbildning.",
         call. = FALSE)
  }

  # TAB6367 (2019-2024) och - om ta_med_tidigare_data - TAB5400 (2006-2018, fryst historisk data)
  # har identiska dimensioner/kolumnnamn och överlappar inte i tid, så de kan radbindas direkt.
  # En tabell utan år i det begärda urvalet hoppas över i stället för att skicka en tom Tid-fråga.
  arbl_raw <- dplyr::bind_rows(
    if (length(tid_6367) > 0) hamta_tabell("TAB6367", tid_6367),
    if (length(tid_5400) > 0) hamta_tabell("TAB5400", tid_5400))

  arbl_df <- arbl_raw |>
    dplyr::rename(regionkod = region_kod) |>
    tidyr::pivot_wider(names_from = tabellinnehåll, values_from = value) |>
    dplyr::rename(forvarvsarbetande = `Förvärvsarbetande (A)`,
                  arbetslosa = `Inskrivna arbetslösa på Arbetsförmedlingen (B)`,
                  totalt = `Totalt antal personer (A+B+C)`) |>
    dplyr::mutate(andel = switch(matt,
                    arbetsloshet = round(arbetslosa / (forvarvsarbetande + arbetslosa) * 100, 1),
                    sysselsattningsgrad = round(forvarvsarbetande / totalt * 100, 1),
                    antal_sysselsatta = forvarvsarbetande),
                  utbildning = wrap_kat(utbildning),
                  # Facet-panelerna ska komma i kodordning (samma ordning som utb_vekt), inte
                  # bokstavsordning på klartexten - utbildning görs om till en factor vars nivåer
                  # är de wrap:ade klartextnamnen men ordnade efter utbildning_kod:s plats i utb_vekt.
                  utbildning = factor(utbildning, levels = unique(utbildning[order(match(utbildning_kod, utb_vekt))]))) |>
    dplyr::arrange(år)

  vald_region <- rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(region_vekt)$region, byt_ut_riket_mot_sverige = TRUE))

  utb_typ_titel <- if (utb_typ == "niva") "utbildningsnivå" else "utbildningsinriktning"
  matt_titel <- switch(matt, arbetsloshet = "Arbetslöshet", sysselsattningsgrad = "Sysselsättningsgrad",
                       antal_sysselsatta = "Antal sysselsatta")
  if (all(is.na(diagram_capt))) {
    diagram_capt <- paste0(
      "Källa: SCB:s öppna statistikdatabas, Arbetsmarknadsställning (U1a)\nBearbetning: Samhällsanalys, Region Dalarna",
      switch(matt,
        arbetsloshet = "\nArbetslöshet = inskrivna arbetslösa på Arbetsförmedlingen i procent av arbetskraften",
        sysselsattningsgrad = "\nSysselsättningsgrad = förvärvsarbetande i procent av alla i åldern 20-64 år",
        antal_sysselsatta = "\nSysselsatta = förvärvsarbetande i åldern 20-64 år"))
  }
  diagram_titel <- stringr::str_wrap(paste0(matt_titel, " efter ", utb_typ_titel, " i ", vald_region, ", nattbefolkning 20-64 år"), titel_bredd)
  diagramfil <- paste0(switch(matt, arbetsloshet = "arbloshet_procent", sysselsattningsgrad = "sysselsattningsgrad_procent",
                       antal_sysselsatta = "antal_sysselsatta"), "_utb_", utb_typ, "_facet_", paste0(region_vekt, collapse = "_"), ".png")

  flera_regioner <- length(unique(arbl_df$regionkod)) > 1

  gg_obj <- rddiagram::SkapaLinjeDiagram(
    skickad_df = arbl_df,
    skickad_x_var = "år",
    skickad_y_var = "andel",
    skickad_x_grupp = if (flera_regioner) "region" else NULL,
    lagga_till_punkter = lagga_till_punkter,
    diagram_titel = diagram_titel,
    diagram_capt = diagram_capt,
    manual_y_axis_title = if (matt == "antal_sysselsatta") "antal" else "procent",
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

#' @rdname diag_arbloshet_syss_utbildning_facet_ar_scb
#' @description `diag_arbloshet_procent_utbildning_facet_ar_scb()` är det gamla namnet och ger samma diagram som
#'   `diag_arbloshet_syss_utbildning_facet_ar_scb(matt = "arbetsloshet")`.
#' @param ... Skickas vidare till `diag_arbloshet_syss_utbildning_facet_ar_scb()`.
#' @export
diag_arbloshet_procent_utbildning_facet_ar_scb <- function(...) {
  .Deprecated("diag_arbloshet_syss_utbildning_facet_ar_scb")
  diag_arbloshet_syss_utbildning_facet_ar_scb(..., matt = "arbetsloshet")
}
