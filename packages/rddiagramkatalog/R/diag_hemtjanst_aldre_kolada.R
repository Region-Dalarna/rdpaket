#' Diagram: andel äldre med hemtjänst (Kolada)
#'
#' Andel av invånarna i åldersgrupperna 65-79 år och 80+ år som har hemtjänst
#' i ordinärt boende (Kolada N21700 och N21704), antingen som en jämförelse
#' mellan regioner ett visst år (`diagramtyp = "regioner"`) eller som en
#' tidsserie (`diagramtyp = "tidsserie"`). Nyckeltalen finns bara för
#' kommuner och riket i Kolada, så länets värde räknas ut här (se Details).
#'
#' @param region_vekt Regionkoder: kommuner, `"00"` för riket och länskod (t.ex. `"20"`).
#'   Länet beräknas som ett viktat värde (se Details). Standard är
#'   Dalarnas kommuner och riket. Länskoden `"20"` tas bort, eftersom
#'   nyckeltalen saknar värden för län.
#' @param diagramtyp `"regioner"` (standard) jämför regionerna ett år,
#'   `"tidsserie"` visar utvecklingen över tid (en ruta per region om flera).
#' @param aldersgrupp `"bada"` (standard), `"65-79"` eller `"80+"`.
#' @param konsuppdelat Om `TRUE` delas värdena upp på kvinnor och män. Med båda
#'   åldersgrupperna får varje åldersgrupp en egen ruta i regiondiagrammet och
#'   ett eget diagram i tidsserien.
#' @param tid_koder Vilka år som ska hämtas, som `"start:slut"` (där `"9999"`
#'   betyder "till och med senaste år") eller en heltalsvektor.
#' @param ar Vilket år som visas när `diagramtyp = "regioner"`. `"9999"`
#'   (standard) = senaste året.
#' @param output_mapp_data Mapp där data sparas som Excelfil. `NA` (standard)
#'   = data sparas inte.
#' @param filnamn_data Filnamn för Excelfilen.
#' @param utmapp Mapp där diagram sparas. Måste finnas om
#'   `skriv_diagramfil = TRUE`.
#' @param skriv_diagramfil TRUE om diagram ska skrivas till fil.
#' @param returnera_figur Skall figur returneras (i en lista).
#' @param returnera_data Skall data returneras (till R-studios globala miljö,
#'   som `hemtjanst_aldre`).
#' @param diag_fargvekt Färger för de två åldersgrupperna. Standard:
#'   `rddiagram::diagramfarger("rus_sex")`.
#'
#' @details Länsvärdet finns inte i Kolada. Det räknas här som summan av
#' kommunernas antal hemtjänsttagare (N21703 för 65-79 år, N21705 för 80+)
#' dividerat med summan av kommunernas folkmängd i åldersgruppen (SCB). Det
#' är alltså viktat efter befolkning, till skillnad från Koladas ovägda
#' medelvärde av kommuner. Antalet är från 2021 en kommunindividuell median
#' över årets månader. År där någon kommun saknar värde utelämnas för länet.
#'
#' @return En namngiven lista med ett `ggplot`-objekt.
#' @export
diag_hemtjanst_aldre_kolada <- function(region_vekt = rdverktyg::hamtakommuner("20", tamedlan = TRUE, tamedriket = TRUE),
                                        diagramtyp = c("regioner", "tidsserie"),
                                        aldersgrupp = c("bada", "65-79", "80+"),
                                        konsuppdelat = FALSE,
                                        tid_koder = "2011:9999",
                                        ar = "9999",
                                        output_mapp_data = NA,
                                        filnamn_data = "hemtjanst_aldre.xlsx",
                                        utmapp = rdverktyg::utskriftsmapp(),
                                        skriv_diagramfil = TRUE,
                                        returnera_figur = TRUE,
                                        returnera_data = FALSE,
                                        diag_fargvekt = NA) {

  diagramtyp <- match.arg(diagramtyp)
  aldersgrupp <- match.arg(aldersgrupp)
  if (all(is.na(diag_fargvekt))) diag_fargvekt <- rddiagram::diagramfarger("rus_sex")
  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  # Kolada har inga värden för län: länsvärdet räknas ut själv (se intern_hemtjanst_lan_viktat())
  lan_vekt <- region_vekt[nchar(region_vekt) <= 2 & region_vekt != "00"]
  kolada_vekt <- setdiff(region_vekt, lan_vekt)

  # ========================================== Hämta data ============================================

  aldersgrupper <- c(N21700 = "65-79 år", N21704 = "80+ år")
  if (aldersgrupp == "65-79") aldersgrupper <- aldersgrupper["N21700"]
  if (aldersgrupp == "80+") aldersgrupper <- aldersgrupper["N21704"]
  flera_aldrar <- length(aldersgrupper) > 1

  hemtjanst_aldre <- rdverktyg::hamta_kolada_df(kpi_id = names(aldersgrupper),
                                                valda_kommuner = kolada_vekt,
                                                valda_ar = intern_tolka_ar_intervall(tid_koder),
                                                konsuppdelat = konsuppdelat) |>
    dplyr::filter(!is.na(varde)) |>
    dplyr::mutate(aldersgrupp = factor(unname(aldersgrupper[variabelkod]), levels = unname(aldersgrupper)),
                  region = rdverktyg::skapa_kortnamn_lan(region),
                  ar = as.integer(ar)) |>
    dplyr::select(ar, regionkod, region, kon, aldersgrupp, varde)

  for (lan in lan_vekt) {
    hemtjanst_aldre <- dplyr::bind_rows(
      hemtjanst_aldre,
      intern_hemtjanst_lan_viktat(lan, ar_vekt = unique(hemtjanst_aldre$ar), konsuppdelat = konsuppdelat,
                                  aldersgrupper = unname(aldersgrupper)))
  }

  if (nrow(hemtjanst_aldre) == 0) stop("Inga data från Kolada för valda regioner och år.", call. = FALSE)
  if (konsuppdelat && !all(unique(hemtjanst_aldre$kon) %in% c("Kvinnor", "Män"))) {
    stop("Kolada saknar könsuppdelade värden för nyckeltalet.", call. = FALSE)
  }

  if (returnera_data) assign("hemtjanst_aldre", hemtjanst_aldre, envir = .GlobalEnv)

  if (!is.na(output_mapp_data) && !is.na(filnamn_data)) {
    openxlsx::write.xlsx(hemtjanst_aldre, paste0(output_mapp_data, filnamn_data))
  }

  diagram_capt <- paste0("Källa: Kolada (RKA)\nBearbetning: Samhällsanalys, Region Dalarna\n",
                         "Diagramförklaring: Personer i åldersgruppen med hemtjänst i ordinärt boende, dividerat med antal invånare i åldersgruppen den 31/12.",
                         if (length(lan_vekt) > 0) "\nLänet är summan av kommunernas antal med hemtjänst dividerat med summan av kommunernas invånare (SCB), inte ett medelvärde av kommunerna." else "")
  gg_list <- list()

  # Vad som skiljs åt med färg och vad som delas i rutor:
  #  * könsuppdelat: kön i färg, åldersgrupp i egna rutor (om båda åldersgrupperna visas)
  #  * annars: åldersgrupp i färg
  grupp_var <- if (konsuppdelat) "kon" else if (flera_aldrar) "aldersgrupp" else NA
  farger <- if (konsuppdelat) rddiagram::diagramfarger("kon") else if (flera_aldrar) diag_fargvekt[1:2] else diag_fargvekt[1]
  aldrar_facet <- if (konsuppdelat && flera_aldrar) "aldersgrupp" else NULL
  titel_ald <- if (flera_aldrar) "" else paste0(" ", aldersgrupper, "")
  filnamn_del <- paste0(if (konsuppdelat) "_kon" else "", if (!flera_aldrar) paste0("_", gsub("[^0-9]", "", aldersgrupp)) else "")

  # =============================================== Diagram ===============================================

  if (diagramtyp == "regioner") {
    valt_ar <- if (identical(as.character(ar), "9999")) max(hemtjanst_aldre$ar) else as.integer(ar)
    plot_df <- dplyr::filter(hemtjanst_aldre, ar == valt_ar)
    if (nrow(plot_df) == 0) stop("Inga data för år ", valt_ar, ".", call. = FALSE)
    filnamn <- paste0("hemtjanst_aldre_regioner", filnamn_del, "_", valt_ar, ".png")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = plot_df,
      skickad_x_var = "region",
      skickad_y_var = "varde",
      skickad_x_grupp = grupp_var,
      facet_grp = aldrar_facet,
      facet_scale = "free",
      facet_kolumner = if (!is.null(aldrar_facet)) 1 else NULL,
      x_axis_sort_value = TRUE,
      diagram_titel = paste0("Andel invånare", titel_ald, " med hemtjänst ", valt_ar),
      diagram_capt = diagram_capt,
      manual_y_axis_title = "procent",
      manual_color = farger,
      x_axis_lutning = 45,
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      output_mapp = utmapp,
      filnamn_diagram = filnamn,
      skriv_till_diagramfil = skriv_diagramfil,
      diagramfil_hojd = if (!is.null(aldrar_facet)) 10 else 7
    )
  } else {
    flera <- length(unique(hemtjanst_aldre$regionkod)) > 1
    # Könsuppdelat med båda åldersgrupperna blir ett diagram per åldersgrupp (annars blir den ena
    # åldersgruppens linjer helt platta på en gemensam y-axel)
    delar <- if (!is.null(aldrar_facet)) split(hemtjanst_aldre, hemtjanst_aldre$aldersgrupp) else list(hemtjanst_aldre)

    for (del_df in delar) {
      ald_txt <- if (!is.null(aldrar_facet)) paste0(" ", unique(del_df$aldersgrupp)) else titel_ald
      ald_fil <- if (!is.null(aldrar_facet)) paste0("_", gsub("[^0-9]", "", unique(del_df$aldersgrupp))) else ""
      filnamn <- paste0("hemtjanst_aldre_tidsserie", filnamn_del, ald_fil, "_",
                        if (flera) "regioner" else unique(del_df$regionkod), ".png")

      gg_obj <- rddiagram::SkapaLinjeDiagram(
        skickad_df = dplyr::mutate(del_df, ar = as.character(ar)),
        skickad_x_var = "ar",
        skickad_y_var = "varde",
        skickad_x_grupp = grupp_var,
        facet_grp = if (flera) "region" else NULL,
        facet_scale = "fixed",
        facet_kolumner = if (flera) 4 else NULL,
        facet_legend_bottom = flera,
        diagram_titel = if (flera) paste0("Andel invånare", ald_txt, " med hemtjänst") else
          paste0("Andel invånare", ald_txt, " med hemtjänst i ", unique(del_df$region)),
        diagram_capt = diagram_capt,
        manual_y_axis_title = "procent",
        manual_color = farger,
        lagga_till_punkter = !flera,
        x_axis_lutning = 45,
        output_mapp = utmapp,
        filnamn_diagram = filnamn,
        skriv_till_diagramfil = skriv_diagramfil,
        diagramfil_hojd = if (flera) 12 else 7
      )
      gg_list[[sub("\\.png$", "", filnamn)]] <- gg_obj
    }
  }

  if (diagramtyp == "regioner") gg_list[[sub("\\.png$", "", filnamn)]] <- gg_obj
  if (returnera_figur) return(gg_list)
}

# Länets andel med hemtjänst, viktad: summa antal hemtjänsttagare / summa folkmängd i åldersgruppen.
# Antal från Kolada (N21703 = 65-79 år, N21705 = 80+ år), folkmängd från SCB (31/12).
# Returnerar samma kolumner som Kolada-uttaget: ar, regionkod, region, kon, aldersgrupp, varde.
intern_hemtjanst_lan_viktat <- function(lan, ar_vekt, konsuppdelat, aldersgrupper) {
  kommuner <- rdverktyg::hamtakommuner(lan, tamedlan = FALSE, tamedriket = FALSE)
  antal_kpi <- c("N21703" = "65-79 år", "N21705" = "80+ år")
  antal_kpi <- antal_kpi[antal_kpi %in% aldersgrupper]

  antal_df <- rdverktyg::hamta_kolada_df(kpi_id = names(antal_kpi), valda_kommuner = kommuner,
                                         valda_ar = ar_vekt, konsuppdelat = konsuppdelat) |>
    dplyr::mutate(aldersgrupp = unname(antal_kpi[variabelkod]), ar = as.integer(ar),
                  kon = if (konsuppdelat) kon else "Båda könen")

  pop_df <- pxweb2r::pxweb2_get_data(
    table = c("TAB638", "TAB5557"),
    query = list(Region = kommuner,
                 Civilstand = c("OG", "G", "SK", "ÄNKL"),
                 Alder = c(65:99, "100+", "100+1", "100+10"),   # kodnamnen för 100+ skiljer mellan tabellerna
                 Kon = c("män", "kvinnor"), ContentsCode = "Folkmängd",
                 Tid = as.character(unique(antal_df$ar))),
    quiet = TRUE, on_all_values_invalid = "null") |>
    dplyr::distinct(table_id, region_kod, civilstånd, ålder, kön, år, .keep_all = TRUE) |>   # 100+ kan komma med två koder
    dplyr::mutate(aldersgrupp = ifelse(readr::parse_number(ålder) >= 80, "80+ år", "65-79 år"),
                  kon = if (konsuppdelat) ifelse(kön == "kvinnor", "Kvinnor", "Män") else "Båda könen",
                  ar = as.integer(år)) |>
    dplyr::group_by(regionkod = region_kod, ar, kon, aldersgrupp) |>
    dplyr::summarise(pop = sum(value, na.rm = TRUE), .groups = "drop")

  antal_df |>
    dplyr::filter(!is.na(varde)) |>
    dplyr::select(ar, regionkod, kon, aldersgrupp, antal = varde) |>
    dplyr::left_join(pop_df, by = c("ar", "regionkod", "kon", "aldersgrupp")) |>
    dplyr::group_by(ar, kon, aldersgrupp) |>
    # län-året utelämnas om någon kommun saknar antal eller folkmängd
    dplyr::filter(dplyr::n() == length(kommuner), !any(is.na(pop))) |>
    dplyr::summarise(varde = sum(antal) / sum(pop) * 100, .groups = "drop") |>
    dplyr::mutate(regionkod = lan,
                  region = rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(lan)$region),
                  aldersgrupp = factor(aldersgrupp, levels = unname(aldersgrupper))) |>
    dplyr::select(ar, regionkod, region, kon, aldersgrupp, varde)
}
