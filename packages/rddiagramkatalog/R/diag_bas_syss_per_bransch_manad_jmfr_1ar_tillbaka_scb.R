#' Diagram: skillnad i antal sysselsatta per bransch, jämfört med tidigare månad
#'
#' Diagram för skillnad i antal sysselsatta per bransch, jämfört med ett
#' valbart antal månader tillbaka (standard 12 månader/ett år).
#'
#' @param region_vekt Region.
#' @param kon_klartext Välj bland `"kvinnor"`, `"män"`, `"totalt"` - vilka som ska visas.
#' @param diagram_capt Diagrammets källtext.
#' @param utmapp Mapp där diagram ska sparas. Måste finnas om `skriv_diagramfil = TRUE`.
#' @param diag_fargvekt Valfri färgvektor.
#' @param tid_koder `"9999"` = senaste månad.
#' @param visa_logga_i_diagram TRUE om logga ska visas i diagrammet.
#' @param logga_sokvag Sökväg till logga.
#' @param returnera_data Skall data returneras till global environment.
#' @param manader_diagramtitel_kortnamn Förkorta månadsnamnen i diagramtiteln.
#' @param jamfor_antal_manader_bakat Hur många månader tillbaka som jämförelsen görs mot.
#' @param dagbefolkning Om FALSE visas nattbefolkning (boende), TRUE = dagbefolkning (arbetsställe).
#' @param stodlinjer_avrunda_fem Avrunda stödlinjerna till närmaste femtal.
#' @param skriv_diagramfil TRUE om diagram ska skrivas till fil.
#' @param ta_bort_diagramtitel FALSE så skrivs ingen diagramtitel ut.
#' @param visa_dataetiketter Visa dataetiketter i diagrammet.
#' @param demo Sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren.
#'
#' @return En namngiven lista med ett `ggplot`-objekt.
#' @export
diag_bas_syss_per_bransch_manad_jmfr_1ar_tillbaka_scb <- function(
    region_vekt = "20",
    kon_klartext = c("kvinnor", "män"),
    diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
    utmapp = rdverktyg::utskriftsmapp(),
    diag_fargvekt = NA,
    tid_koder = "9999",
    visa_logga_i_diagram = TRUE,
    logga_sokvag = NA,
    returnera_data = FALSE,
    manader_diagramtitel_kortnamn = TRUE,
    jamfor_antal_manader_bakat = 12,
    dagbefolkning = TRUE,
    stodlinjer_avrunda_fem = TRUE,
    skriv_diagramfil = TRUE,
    ta_bort_diagramtitel = FALSE,
    visa_dataetiketter = FALSE,
    demo = FALSE
    ) {

  # om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
  # demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
  if (demo){
    demo_url <-
      c("https://region-dalarna.github.io/utskrivna_diagram/bas_syss_Dalarna_augusti_ar2024_jmfrt_med_augusti_ar2024.png")
    purrr::walk(demo_url, ~browseURL(.x))
    if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
    rdverktyg::stop_tyst()
  }

  if (!all(kon_klartext %in% c("kvinnor", "män", "totalt"))) {
    stop("kon_klartext måste vara en delmängd av c(\"kvinnor\", \"män\", \"totalt\").")
  }

  # om ingen färgvektor är medskickad väljs en utifrån kon_klartext: rus_sex om bara totalt visas (då
  # passar inte tvåfärgspaletten "kon"), kon_och_total om alla tre visas, annars "kon" (standardfallet
  # kvinnor+män)
  if (all(is.na(diag_fargvekt))) {
    diag_fargvekt <- if (setequal(kon_klartext, "totalt")) {
      rddiagram::diagramfarger("rus_sex")[1]
    } else if (setequal(kon_klartext, c("kvinnor", "män", "totalt"))) {
      rddiagram::diagramfarger("kon_och_total")
    } else {
      rddiagram::diagramfarger("kon")
    }
  }

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  gg_list <- list()

  variabel_dag_nattbefolkning <- if (dagbefolkning) "sysselsatta efter arbetsställets belägenhet" else "sysselsatta efter bostadens belägenhet"

  # Länk till tabell: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0210__AM0210B/ArbStDoNMNN/
  # (samma tabell, TAB3784, som används i diag_sysselsatta_forandring_bransch_scb/_kommun_scb)
  # Räknar ut vilken månad som ska jämföras mot (N månader bakåt), samma
  # teknik som i diag_bas_arbloshet_manad_jmfr_1ar_tillbaka_scb.
  giltiga_manader <- pxweb2r::pxweb2_get_values("TAB3784", "Tid", quiet = TRUE)$code
  tid_nu <- if (tid_koder == "9999") max(giltiga_manader) else tid_koder
  idx_nu <- which(giltiga_manader == tid_nu)
  idx_da <- idx_nu - jamfor_antal_manader_bakat
  tid_hamta <- unique(c(tid_nu, giltiga_manader[idx_da]))

  bas_syss_df <- pxweb2r::pxweb2_get_data(
    table = "TAB3784",
    query = list(
      Region = region_vekt,
      Kon = "*",
      SNI2007 = "*",
      Fodelseregion = "*",
      ContentsCode = variabel_dag_nattbefolkning,
      Tid = tid_hamta
    ), quiet = TRUE) |>
    dplyr::rename(regionkod = region_kod, sni2007kod = `näringsgren sni 2007_kod`) |>
    dplyr::rename(!!variabel_dag_nattbefolkning := value) |>
    dplyr::select(-tabellinnehåll) |>
    rdverktyg::manader_bearbeta_scbtabeller()

  # hämta branschnyckel från Region Dalarnas github-repo depot
  nyckel_bransch <- intern_hamta_branschnyckel()

  manad_nu <- as.character(dplyr::last(bas_syss_df$tid))
  manad_da <- as.character(dplyr::first(bas_syss_df$tid))

  chart_df <- bas_syss_df |>
    dplyr::select(-c(år, månad, månad_år, år_månad)) |>
    tidyr::pivot_wider(names_from = tid, values_from = {{variabel_dag_nattbefolkning}}) |>
    dplyr::mutate(diff = .data[[manad_nu]] - .data[[manad_da]]) |>
    dplyr::filter(kön %in% kon_klartext,
           `näringsgren SNI 2007` != "Total",
           födelseregion != "totalt") |>
    dplyr::left_join(dplyr::select(nyckel_bransch, Br15kod, Bransch), by = c("sni2007kod" = "Br15kod")) |>
    dplyr::mutate(Bransch = ifelse(is.na(Bransch), stringr::str_to_sentence(`näringsgren SNI 2007`), Bransch))

  # returnera datasetet till global environment, bl.a. bra när man skapar Rmarkdown-rapporter
  if(returnera_data == TRUE){
    assign("chart_df", chart_df, envir = .GlobalEnv)
  }

  # om regioner är alla kommuner i ett län eller alla län i Sverige görs revidering, annars inte
  region_start <- rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(unique(bas_syss_df$region)))
  region_txt <- rdverktyg::ar_alla_kommuner_i_ett_lan(unique(bas_syss_df$regionkod), returnera_text = TRUE, returtext = region_start)
  region_txt <- rdverktyg::ar_alla_lan_i_sverige(unique(bas_syss_df$regionkod), returnera_text = TRUE, returtext = region_txt)
  regionfil_txt <- region_txt
  region_txt <- paste0(" i ", region_txt)
  regionkod_txt <- if (region_start == region_txt) paste0(unique(bas_syss_df$regionkod), collapse = "_") else region_txt

  manad_start <- as.character(dplyr::first(dplyr::pull(dplyr::distinct(bas_syss_df, månad))))

  if (manader_diagramtitel_kortnamn) manad_start <- stringr::str_sub(manad_start, 1, 3)

  manad_slut <- as.character(dplyr::last(dplyr::pull(dplyr::distinct(bas_syss_df, månad))))

  if (manader_diagramtitel_kortnamn) manad_slut <- stringr::str_sub(manad_slut, 1, 3)

  ar_start <- as.character(dplyr::first(dplyr::pull(dplyr::distinct(bas_syss_df, år))))

  ar_slut <- as.character(dplyr::last(dplyr::pull(dplyr::distinct(bas_syss_df, år))))

  # ändra diagramtitel baserat på om det är dag- eller nattbefolkning
  region_txt <- if(dagbefolkning) {
    region_ny <- stringr::str_replace(region_txt, " i ", " på ")
    region_ny <- glue::glue("{region_ny}s arbetsmarknad")
  } else {
    region_ny <- glue::glue(" och boende{region_txt}")
  }

  # ändra diagramfilnamn baserat på om det är dag- eller nattbefolkning
  dagnatt_filnamn <- if(dagbefolkning) "dagbef" else "nattbef"

  diagramtitel <- glue::glue("Skillnad i antal sysselsatta 15-74 år {region_txt} i {manad_slut} år {ar_slut} jämfört med {manad_start} år {ar_start}")
  # filnamnet får en extra kon_klartext-markör bara om urvalet avviker från standarden (kvinnor+män), så
  # att befintliga filnamn inte påverkas av den nya parametern
  kon_filnamn <- if (setequal(kon_klartext, c("kvinnor", "män"))) "" else paste0("_", paste(sort(kon_klartext), collapse = "_"))
  diagramfil <- glue::glue("bas_syss_{regionfil_txt}_{manad_slut}_ar{ar_slut}_jmfrt_med_{manad_start}_ar{ar_start}_{dagnatt_filnamn}{kon_filnamn}.png")


  gg_obj <- rddiagram::SkapaStapelDiagram(
    skickad_df = chart_df,
    skickad_x_var = "Bransch",
    skickad_y_var = "diff",
    skickad_x_grupp = "kön",
    diagram_titel = if (ta_bort_diagramtitel) NULL else diagramtitel,
    diagram_capt = diagram_capt,
    diagram_liggande = TRUE,
    stodlinjer_avrunda_fem = stodlinjer_avrunda_fem,
    filnamn_diagram = diagramfil,
    dataetiketter = visa_dataetiketter,
    manual_y_axis_title = "",
    x_axis_lutning = 0,
    manual_color = diag_fargvekt,
    output_mapp = utmapp,
    lagg_pa_logga = visa_logga_i_diagram,
    logga_path = logga_sokvag,
    skriv_till_diagramfil = skriv_diagramfil,
    facet_grp = "födelseregion",
    facet_scale = "fixed",
    facet_legend_bottom = TRUE
  )

  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfil, ".png")

  return(gg_list)
}
