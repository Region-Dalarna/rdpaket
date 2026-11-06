#' Diagram: lediga jobb per sektor över tid
#'
#' Diagram över lediga jobb (Konjunkturstatistik över vakanser) uppdelat
#' på sektor, över tid.
#'
#' @param region_vekt Val av region. Finns bl.a. `"00"`, `"FA00"`-`"FA60"`, samt kommunkoder.
#' @param utmapp Mapp där figuren sparas. Måste finnas om `skriv_diagramfil = TRUE`.
#' @param tid_koder `"*"` = alla år eller kvartal, `"9999"` = senaste.
#' @param kon_klartext Finns: `"män"`, `"kvinnor"`, `"totalt"`.
#' @param sektor_klartext Finns: `"offentlig sektor"`, `"privat sektor"`, `"totalt"`, `"hela ekonomin"`, `"näringslivet och hushållens icke-vinstdrivande organisationer"`, `"offentlig förvaltning"`.
#' @param cont_klartext Finns: `"Lediga jobb"`.
#' @param kvartal_klartext Finns `1`, `2`, `3`, `4`. `"9999"` = senaste tillgängliga kvartal, `NA` = alla kvartal.
#' @param skriv_diagramfil Skall diagrammet sparas.
#' @param returnera_data Skall data returneras.
#' @param returnera_figur Skall figuren returneras.
#'
#' @return En lista med `ggplot`-objekt.
#' @export
diag_lediga_jobb_tid_sektor_scb <- function(region_vekt = "20",
                                              utmapp = rdverktyg::utskriftsmapp(),
                                              tid_koder = "*",
                                              kon_klartext = "totalt",
                                              sektor_klartext = c("offentlig sektor", "privat sektor"),
                                              cont_klartext = "Lediga jobb",
                                              kvartal_klartext = "9999",
                                              skriv_diagramfil = TRUE,
                                              returnera_data = FALSE,
                                              returnera_figur = TRUE){

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  diagram_capt <- "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
  visa_dataetiketter <- FALSE
  gg_list <- list()

  # ====================================================================================================
  # Hämta data direkt via pxweb2r
  # TAB6424 = nya tabellen (kvartal 2024K2-), TAB4429 = gamla tabellen (historik 2006K1-2024K1, uppdateras ej).
  # De båda tabellerna har olika klartext-etiketter för sektor och innehåll (ContentsCode). Vi bygger
  # därför en frågevektor som täcker båda tabellernas varianter - on_all_values_invalid = "null" gör
  # att en tabell hoppas över (ger NULL) i stället för att hela uttaget stoppas, om inget av de begärda
  # värdena finns i just den tabellen. pxweb2r har (till skillnad från den gamla wrappern func_pxweb2.R)
  # inget inbyggt stöd för att hämta flera tabeller i ett anrop, så de hämtas var för sig här och slås
  # ihop med bind_rows() - samma mönster som används för CKM-uppdelade tabeller i andra skript.
  # ====================================================================================================

  sektor_varden <- sektor_klartext
  if (any(stringr::str_detect(tolower(sektor_klartext), "offentlig"))) {
    sektor_varden <- c(sektor_varden, "offentlig sektor", "offentlig förvaltning")
  }
  if (any(stringr::str_detect(tolower(sektor_klartext), "privat|näringslivet"))) {
    sektor_varden <- c(sektor_varden, "privat sektor", "näringslivet och hushållens icke-vinstdrivande organisationer")
  }
  if (any(stringr::str_detect(tolower(sektor_klartext), "totalt|hela ekonomin"))) {
    sektor_varden <- c(sektor_varden, "totalt", "hela ekonomin")
  }
  sektor_varden <- unique(sektor_varden)

  cont_varden <- cont_klartext
  if (any(stringr::str_detect(tolower(cont_klartext), "lediga jobb"))) {
    cont_varden <- c(cont_varden, "Lediga jobb", "Lediga jobb, totalt")
  }
  cont_varden <- unique(cont_varden)

  hamta_lediga_jobb_tabell <- function(tabell_id) {
    df <- pxweb2r::pxweb2_get_data(
      table = tabell_id,
      query = list(
        Region = region_vekt,
        Sektor = sektor_varden,
        ContentsCode = cont_varden,
        Tid = tid_koder
      ),
      on_all_values_invalid = "null", quiet = TRUE)
    if (!is.null(df)) df$tabell_id <- tabell_id
    df
  }

  dataset_df <- dplyr::bind_rows(
    hamta_lediga_jobb_tabell("TAB6424"),
    hamta_lediga_jobb_tabell("TAB4429"))

  if (is.null(dataset_df) || nrow(dataset_df) == 0) {
    stop("Ingen data kunde hämtas för lediga jobb (E1) - kontrollera region/sektor/tid-urvalet.")
  }

  # --- normalisera kolumnnamn så att resten av skriptet är oberoende av SCB:s exakta etiketter -------

  # Tidskolumnen heter normalt "kvartal" i dessa tabeller, men vi känner av det defensivt eftersom
  # pxweb2r döper kolumnen efter SCB:s etikett för variabeln (skulle SCB byta etikett bryts annars koden).
  tid_kol <- intersect(c("kvartal", "tid"), names(dataset_df))[1]
  if (is.na(tid_kol)) {
    stop("Hittade ingen tidskolumn (kvartal/tid) i dataset_df. Kolumner: ", paste(names(dataset_df), collapse = ", "))
  }
  if (tid_kol != "kvartal") dataset_df <- dataset_df |> dplyr::rename(kvartal = !!tid_kol)

  # Innehållskolumnen (ContentsCode) är den kolumn som blir kvar utöver de kända kolumnerna.
  # OBS: om SCB döper ContentsCode-dimensionen till något annat än vi förväntar oss dyker den upp
  # här ändå eftersom vi letar efter "det som blir över" - men döp gärna av vid felsökning med
  # names(dataset_df) om något inte stämmer.
  kanda_kolumner <- c("tabell_id", "region_kod", "region", "sektor", "kvartal", "value")
  innehall_kol <- setdiff(names(dataset_df), kanda_kolumner)

  if (length(innehall_kol) == 1) {
    dataset_df <- dataset_df |>
      dplyr::rename(innehall = !!innehall_kol) |>
      dplyr::mutate(innehall = ifelse(innehall %in% c("Lediga jobb", "Lediga jobb, totalt"), "Lediga jobb", innehall)) |>
      dplyr::filter(innehall == "Lediga jobb") |>
      dplyr::select(-innehall)
  }

  # döp om värdekolumnen till "varde" (samma konvention som övriga diagramskript använder)
  dataset_df <- dataset_df |> dplyr::rename(varde = value)

  # döp om sektor så att det stämmer mellan de båda tabellerna
  sektor_namnvektor <- c(
    "offentlig förvaltning" = "offentlig sektor",
    "näringslivet och hushållens icke-vinstdrivande organisationer" = "privat sektor",
    "hela ekonomin" = "totalt"
  )
  dataset_df <- dataset_df |>
    dplyr::mutate(sektor = dplyr::recode(sektor, !!!sektor_namnvektor))

  # om samma kvartal skulle finnas i båda tabellerna: behåll raden från TAB6424 (den nyare tabellen)
  if ("tabell_id" %in% names(dataset_df)) {
    prioritetsordning <- c("TAB6424", "TAB4429")
    dataset_df <- dataset_df |>
      dplyr::mutate(.prioritet = match(tabell_id, prioritetsordning)) |>
      dplyr::arrange(.prioritet) |>
      dplyr::distinct(region_kod, sektor, kvartal, .keep_all = TRUE) |>
      dplyr::select(-.prioritet, -tabell_id)
  }

  lediga_jobb_df <- dataset_df |>
    dplyr::mutate(
      ar = stringr::str_sub(kvartal, 1, 4),
      kvartal_num = as.numeric(stringr::str_sub(kvartal, 6, 6)),
      kvartal_txt = paste0("Kvartal ", kvartal_num)
    ) |>
    dplyr::arrange(kvartal, sektor)

  # filtrera på specifikt/senaste kvartal om kvartal_klartext är satt (NA = behåll alla kvartal)
  if (!all(is.na(kvartal_klartext))) {
    kvartal_senaste <- if ("9999" %in% as.character(kvartal_klartext)) {
      lediga_jobb_df |> dplyr::filter(kvartal == max(kvartal)) |> dplyr::pull(kvartal_num) |> unique()
    } else numeric(0)

    kvartal_siffror <- as.character(kvartal_klartext)
    kvartal_siffror <- as.numeric(kvartal_siffror[kvartal_siffror %in% c("1", "2", "3", "4")])

    kvartal_valda <- unique(c(kvartal_senaste, kvartal_siffror))

    lediga_jobb_df <- lediga_jobb_df |> dplyr::filter(kvartal_num %in% kvartal_valda)
  }

  if(returnera_data == TRUE){
    assign("lediga_jobb_E1_df", lediga_jobb_df, envir = .GlobalEnv)
  }

  # om regioner är alla kommuner i ett län eller alla län i Sverige görs revidering, annars inte
  region_start <- rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(unique(lediga_jobb_df$region)))
  region_txt <- rdverktyg::ar_alla_kommuner_i_ett_lan(unique(lediga_jobb_df$region_kod), returnera_text = TRUE, returtext = region_start)
  region_txt <- rdverktyg::ar_alla_lan_i_sverige(unique(lediga_jobb_df$region_kod), returnera_text = TRUE, returtext = region_txt)
  regionfil_txt <- region_txt
  region_txt <- paste0(" i ", region_txt)
  regionkod_txt <- if (region_start == region_txt) paste0(unique(lediga_jobb_df$region_kod), collapse = "_") else region_txt

  kvartal_titel <- dplyr::case_when(length(unique(lediga_jobb_df$kvartal_num)) > 1 ~ paste0("kvartal ", rdverktyg::list_komma_och(unique(lediga_jobb_df$kvartal_num))),
                             unique(lediga_jobb_df$kvartal_num) == 1 ~ "första kvartalet",
                             unique(lediga_jobb_df$kvartal_num) == 2 ~ "andra kvartalet",
                             unique(lediga_jobb_df$kvartal_num) == 3 ~ "tredje kvartalet",
                             unique(lediga_jobb_df$kvartal_num) == 4 ~ "fjärde kvartalet",
                             TRUE ~ "")

  diagramtitel <- glue::glue("Lediga jobb{region_txt} {kvartal_titel} varje år")
  diagramfil <- stringr::str_replace_all(
    glue::glue("lediga_jobb_{regionfil_txt}_E1_{dplyr::first(lediga_jobb_df$ar)}_{dplyr::last(lediga_jobb_df$ar)}_kvartal{unique(lediga_jobb_df$kvartal_num)}.png"),
    "__", "_")

  chart_df <- lediga_jobb_df

  gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = chart_df,
                               skickad_x_var = "ar" ,
                               skickad_y_var = "varde",
                               skickad_x_grupp = if ("sektor" %in% names(chart_df) & length(unique(chart_df$sektor)) > 1) "sektor" else NA,
                               x_axis_sort_value = FALSE,
                               diagram_titel = diagramtitel,
                               diagram_capt = diagram_capt,
                               stodlinjer_avrunda_fem = TRUE,
                               filnamn_diagram = diagramfil,
                               dataetiketter = visa_dataetiketter,
                               geom_position_stack = TRUE,
                               manual_y_axis_title = "",
                               manual_x_axis_text_vjust = 1,
                               manual_x_axis_text_hjust = 1,
                               manual_color = if ("sektor" %in% names(chart_df) & length(unique(chart_df$sektor)) > 1) rddiagram::diagramfarger("rus_sex") else rddiagram::diagramfarger("rus_sex")[1],
                               output_mapp = utmapp,
                               skriv_till_diagramfil = skriv_diagramfil,
  )

  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfil, ".png")
  if (returnera_figur == TRUE) return(gg_list)
}
