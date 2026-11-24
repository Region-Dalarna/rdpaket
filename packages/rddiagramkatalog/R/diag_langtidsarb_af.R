#' Diagram: långtidsarbetslöshet (Arbetsförmedlingen)
#'
#' Två diagram över långtidsarbetslöshet bland inskrivna arbetslösa: ett
#' stapeldiagram som jämför andelen långtidsarbetslösa i länets kommuner (för
#' senaste år eller vart femte år), och ett stapeldiagram som jämför hur länge
#' de arbetslösa i länet varit utan arbete över tid. Årsvärden är medelvärden
#' av månadsdata. Data laddas automatiskt ned som Excelfil från
#' Arbetsförmedlingens sida för tidigare statistik och tidsserier.
#'
#' @param utmapp Mapp där figuren sparas. Måste finnas om
#'   `skriv_diagramfil = TRUE`.
#' @param region Vilket län som ska väljas (kortnamn, t.ex. "Dalarna"). Endast
#'   ett län åt gången.
#' @param skriv_diagramfil Skall figurerna sparas.
#' @param diag_jmf_kommun Skapa diagram: långtidsarbetslöshet per kommun i
#'   valt län.
#' @param diag_jmf_olika_langd Skapa diagram: tid som arbetslös i valt län
#'   över tid.
#' @param returnera_data Skall data läggas i R-studios globala miljö.
#' @param returnera_figur Skall en lista med figurer returneras.
#' @param diag_fargvekt Vilka färger som ska användas i diagrammen.
#' @param jmf_ar Om `TRUE` jämförs kommunerna vart femte år från första år
#'   samt senaste år, annars visas endast senaste år.
#' @param tid_cols Definition av långtidsarbetslöshet i kommundiagrammet. En
#'   åt gången: "Utan arbete mer än 6 månader", "Utan arbete mer än 12
#'   månader" eller "Utan arbete mer än 24 månader".
#' @param start_ar Första år som ska tas med i diagrammen (heltal, t.ex.
#'   2009). `NULL` ger alla tillgängliga år.
#' @param data_namm_kommun Namn på dataobjektet för kommundiagrammet som läggs
#'   i globala miljön om `returnera_data = TRUE`.
#' @param data_namm_olika_langd Namn på dataobjektet för diagrammet över tid
#'   som arbetslös som läggs i globala miljön om `returnera_data = TRUE`.
#'
#' @return En namngiven lista med `ggplot`-objekt.
#' @export
diag_langtidsarb_af <- function(utmapp = rdverktyg::utskriftsmapp(),
                                region = "Dalarna",
                                skriv_diagramfil = TRUE,
                                diag_jmf_kommun = TRUE,
                                diag_jmf_olika_langd = FALSE,
                                returnera_data = FALSE,
                                returnera_figur = TRUE,
                                diag_fargvekt = rddiagram::diagramfarger("rus_sex"),
                                jmf_ar = TRUE,
                                tid_cols = "Utan arbete mer än 6 månader",
                                start_ar = NULL,
                                data_namm_kommun = "langtidsarbetsloshet_kommun_df",
                                data_namm_olika_langd = "langtidsarbetsloshet_olika_langd_df") {

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)
  gg_list <- list()

  if (length(region) != 1) stop("`region` får bara innehålla ett län.")
  if (!is.null(start_ar) && (!is.numeric(start_ar) || length(start_ar) != 1)) {
    stop("`start_ar` måste vara ett år (t.ex. 2009) eller NULL.", call. = FALSE)
  }
  if (length(tid_cols) != 1) stop("`tid_cols` får bara innehålla en definition av långtidsarbetslöshet.")

  # ========================================== Läser in data ============================================
  # Arbetsförmedlingens sida för tidigare statistik/tidsserier (Excel)
  af_url <- "https://arbetsformedlingen.se/statistik/sok-statistik/tidigare-statistik-tidsserier"

  # 1. Läs in webbsidan
  af_sida <- tryCatch(
    rvest::read_html(af_url),
    error = function(e) {
      stop("Kunde inte läsa in Arbetsförmedlingens statistiksida (", af_url, ").\n",
           "Kontrollera internetanslutningen och att sidan finns kvar.\n",
           "Ursprungligt fel: ", conditionMessage(e), call. = FALSE)
    })
  
  af_url_lankar <- af_sida |>
    rvest::html_elements("a") |>
    rvest::html_attr("href") |>
    purrr::discard(is.na)
  
  if (length(af_url_lankar) == 0) {
    stop("Inga länkar hittades på ", af_url, ". Sidans struktur kan ha ändrats.", call. = FALSE)
  }
  
  avkodade_lankar <- purrr::map_chr(af_url_lankar, utils::URLdecode)
  
  # 2. Hitta länken till filen med tid utan arbete per län och kommun (ej årsgenomsnitt)
  af_index <- which(
    stringr::str_detect(avkodade_lankar, "web-inskrivna-arbetslosa-tid-utan-arbete-lan-kom") &
      !stringr::str_detect(avkodade_lankar, "arsgenomsnitt"))
  
  if (length(af_index) == 0) {
    stop("Hittade ingen länk till filen med tid utan arbete (\"web-inskrivna-arbetslosa-tid-utan-arbete-lan-kom\") på ",
         af_url, ".\nArbetsförmedlingen kan ha bytt namn på filen eller flyttat den.", call. = FALSE)
  }
  if (length(af_index) > 1) {
    message("Hittade ", length(af_index), " matchande länkar på Arbetsförmedlingens sida - använder den första.")
  }
  
  inlasfil <- af_url_lankar[af_index[1]]
  if (!startsWith(inlasfil, "http")) inlasfil <- paste0("https://arbetsformedlingen.se", inlasfil)
  
  # 3. Ladda ned Excelfilen
  tmpfile <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmpfile), add = TRUE)
  
  tryCatch(
    curl::curl_download(inlasfil, tmpfile),
    error = function(e) {
      stop("Kunde inte ladda ned Excelfilen från Arbetsförmedlingen:\n", inlasfil, "\n",
           "Ursprungligt fel: ", conditionMessage(e), call. = FALSE)
    })
  
  # 4. Läs in fliken "Total"
  flikar <- tryCatch(
    readxl::excel_sheets(tmpfile),
    error = function(e) {
      stop("Den nedladdade filen kunde inte läsas som Excelfil:\n", inlasfil, "\n",
           "Ursprungligt fel: ", conditionMessage(e), call. = FALSE)
    })
  
  if (!"Total" %in% flikar) {
    stop("Fliken \"Total\" saknas i Arbetsförmedlingens fil. Tillgängliga flikar: ",
         paste(flikar, collapse = ", "), ".", call. = FALSE)
  }
  
  af_df <- readxl::read_xlsx(tmpfile, sheet = "Total", skip = 4, col_types = "text")
  
  # 5. Kontrollera att förväntade kolumner finns (fångar t.ex. ändrat antal rubrikrader)
  forvantade_kol <- c("PERIOD", "LÄN", "KOMMUN", "ARBETSLÖSA",
                      "Utan arbete mer än 6 månader",
                      "Utan arbete mer än 12 månader",
                      "Utan arbete mer än 24 månader")
  saknade_kol <- setdiff(forvantade_kol, names(af_df))
  
  if (length(saknade_kol) > 0) {
    stop("Följande kolumner saknas i Arbetsförmedlingens fil: ", paste(saknade_kol, collapse = ", "), ".\n",
         "Inlästa kolumner: ", paste(names(af_df), collapse = ", "), ".\n",
         "Filens layout kan ha ändrats (t.ex. antal rubrikrader, nu skip = 4).", call. = FALSE)
  }

  # Gemensam bearbetning: år/månad, numeriska kolumner och kortnamn.
  # Filtrerar på valt län redan här (länssumman beräknas per län, så resultatet påverkas inte).
  df_base <- af_df |>
    tidyr::separate(PERIOD, c("Ar", "Manad"), sep = "-") |>
    dplyr::mutate(dplyr::across(c(ARBETSLÖSA, starts_with("Utan arbete mer än")), as.numeric),
                  Ar = as.integer(Ar),
                  LÄN = rdverktyg::skapa_kortnamn_lan(LÄN, byt_ut_riket_mot_sverige = TRUE),
                  KOMMUN = rdverktyg::skapa_kortnamn_lan(KOMMUN, byt_ut_riket_mot_sverige = TRUE)) |>
    dplyr::filter(LÄN %in% region)
  
  if (nrow(df_base) == 0) stop("Hittade ingen data för region = \"", region, "\".", call. = FALSE)
  
  # Valfritt startår (filtreras före kontrollen nedan, så att bara år som
  # faktiskt visas kontrolleras)
  if (!is.null(start_ar)) {
    if (start_ar > max(df_base$Ar, na.rm = TRUE)) {
      stop("`start_ar` (", start_ar, ") är senare än senaste år i data (",
           max(df_base$Ar, na.rm = TRUE), ").", call. = FALSE)
    }
    df_base <- df_base |>
      dplyr::filter(Ar >= start_ar)
  }
  
  # Kontroll: kommun-månader där ARBETSLÖSA finns men tid utan arbete saknas
  # (t.ex. sekretessade små tal). De räknas som 0 i länssumman nedan, vilket
  # gör länets andel något för låg. Skriver ut omfattningen per år.
  tid_kol_alla <- c("Utan arbete mer än 6 månader",
                    "Utan arbete mer än 12 månader",
                    "Utan arbete mer än 24 månader")
  
  saknas_df <- df_base |>
    dplyr::mutate(saknas = !is.na(ARBETSLÖSA) & dplyr::if_any(all_of(tid_kol_alla), is.na))
  
  if (any(saknas_df$saknas)) {
    paverkan <- saknas_df |>
      dplyr::group_by(Ar) |>
      dplyr::summarise(
        rader = sum(saknas),
        andel = sum(ARBETSLÖSA[saknas], na.rm = TRUE) / sum(ARBETSLÖSA, na.rm = TRUE) * 100,
        .groups = "drop") |>
      dplyr::filter(rader > 0)
    
    berorda_kommuner <- saknas_df |>
      dplyr::filter(saknas) |>
      dplyr::pull(KOMMUN) |>
      unique() |>
      sort()
    
    message("OBS! Saknade värden i Arbetsförmedlingens data för ", region, ":\n",
            sum(saknas_df$saknas), " kommun-månader har värde för ARBETSLÖSA men saknar värde för tid utan arbete ",
            "(troligen sekretess).\n",
            "De räknas som 0 i länssumman, vilket gör länets andel långtidsarbetslösa något för låg.\n",
            "Berörda kommuner: ", paste(berorda_kommuner, collapse = ", "), "\n",
            "Per år (antal kommun-månader, andel av länets arbetslösa i dessa rader):\n",
            paste0("  ", paverkan$Ar, ": ", paverkan$rader, " (",
                   format(round(paverkan$andel, 1), nsmall = 1, decimal.mark = ","), " %)",
                   collapse = "\n"))
  } else {
    message("Kontroll OK: inga saknade värden för tid utan arbete i ", region, ".")
  }
  
  # Länssumma som egen "kommun" och åldersgrupper
  df_lan_kom <- df_base |>
    dplyr::bind_rows(
      df_base |>
        dplyr::group_by(Ar, Manad, LÄN) |>
        dplyr::summarise(
          dplyr::across(c(ARBETSLÖSA, starts_with("Utan arbete mer än")), \(x) sum(x, na.rm = TRUE)),
          .groups = "drop") |>
        dplyr::mutate(KOMMUN = LÄN)) |>
    dplyr::mutate(
      åldersgrupper = dplyr::case_when(
        Ar <= 2022              ~ "Inskrivna arbetslösa 16-64 år",
        Ar >= 2023 & Ar <= 2025 ~ "Inskrivna arbetslösa 16-65 år",
        Ar >= 2026              ~ "Inskrivna arbetslösa 16-66 år",
        TRUE                    ~ NA_character_)) |>
    dplyr::filter(KOMMUN != "Uppgift saknas")

  if (diag_jmf_kommun == TRUE) {

    df <- df_lan_kom |>
      tidyr::pivot_longer(cols = all_of(tid_cols), names_to = "langtidsdefinition", values_to = "tid_varde") |>
      dplyr::mutate(andel_langtidsarbetslos = (tid_varde / ARBETSLÖSA) * 100) |>
      dplyr::group_by(Ar, LÄN, KOMMUN, åldersgrupper, langtidsdefinition) |>
      dplyr::summarise(
        andel_langtidsarbetslos = mean(andel_langtidsarbetslos, na.rm = TRUE),
        .groups = "drop")

    if (jmf_ar == TRUE) {
      # Vart femte år från första år, plus senaste år
      years_to_plot <- union(seq(min(df$Ar), max(df$Ar), by = 5), max(df$Ar))
      utskrift_df <- df |>
        dplyr::filter(Ar %in% years_to_plot)
      vand_sortering <- TRUE
      diagram_titel <- paste0("Långtidsarbetslöshet i ", region)
    } else {
      utskrift_df <- df |>
        dplyr::filter(Ar == max(Ar))
      vand_sortering <- FALSE
      diagram_titel <- paste0("Långtidsarbetslöshet i ", region, " år ", max(df$Ar))
    }

    if (returnera_data == TRUE) {
      assign(data_namm_kommun, utskrift_df, envir = .GlobalEnv)
    }

    manader <- stringr::str_extract(tid_cols, "\\d+")
    ar_max <- max(df$Ar)

    diagram_capt <- paste0("Källa: Arbetsförmedlingen.",
                           "\nBearbetning: Samhällsanalys, Region Dalarna.",
                           "\nDiagramförklaring: Månadsdata. Diagrammet visar årsmedelvärden; för ", ar_max, " visas medelvärdet för",
                           "\nhittills tillgängliga månader. Andel av arbetslösa som varit öppet arbetslösa eller i program med aktivitetsstöd",
                           "\ni minst ", manader, " månader. Till och med 2022 avser statistiken åldersgruppen 16-64 år, mellan 2023 och 2025",
                           "\n16-65 år och från 2026 16-66 år.")

    diagramfilnamn <- "langtidsarbetsloshet.png"

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = utskrift_df,
      skickad_x_var = "KOMMUN",
      skickad_y_var = "andel_langtidsarbetslos",
      skickad_x_grupp = ifelse(jmf_ar == TRUE, "Ar", NA),
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      manual_color = diag_fargvekt,
      x_axis_sort_value = TRUE,
      x_axis_sort_grp = ifelse(jmf_ar == TRUE, length(unique(utskrift_df$Ar)), NA),
      vand_sortering = vand_sortering,
      dataetiketter = FALSE,
      manual_y_axis_title = "procent",
      diagram_titel = diagram_titel,
      diagram_capt = diagram_capt,
      procent_0_100_10intervaller = TRUE,
      stodlinjer_avrunda_fem = TRUE,
      output_mapp = utmapp,
      filnamn_diagram = diagramfilnamn,
      skriv_till_diagramfil = skriv_diagramfil)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn |> stringr::str_remove(".png")
  }

  if (diag_jmf_olika_langd == TRUE) {

    # Delar upp de arbetslösa i intervall för tid utan arbete (summerar till ARBETSLÖSA)
    df <- df_lan_kom |>
      dplyr::filter(KOMMUN %in% region) |>
      dplyr::mutate(
        `0 - 6 månader`   = ARBETSLÖSA - `Utan arbete mer än 6 månader`,
        `6 - 12 månader`  = `Utan arbete mer än 6 månader` - `Utan arbete mer än 12 månader`,
        `12 - 24 månader` = `Utan arbete mer än 12 månader` - `Utan arbete mer än 24 månader`,
        `24 månader -`    = `Utan arbete mer än 24 månader`) |>
      tidyr::pivot_longer(
        cols = c(`0 - 6 månader`, `6 - 12 månader`, `12 - 24 månader`, `24 månader -`),
        names_to = "langtidsdefinition", values_to = "tid_varde") |>
      dplyr::mutate(andel_langtidsarbetslos = (tid_varde / ARBETSLÖSA) * 100) |>
      dplyr::group_by(Ar, LÄN, KOMMUN, åldersgrupper, langtidsdefinition) |>
      dplyr::summarise(
        andel_langtidsarbetslos = mean(andel_langtidsarbetslos, na.rm = TRUE),
        .groups = "drop")

    if (returnera_data == TRUE) {
      assign(data_namm_olika_langd, df, envir = .GlobalEnv)
    }

    diagram_capt <- paste0("Källa: Arbetsförmedlingen.",
                           "\nBearbetning: Samhällsanalys, Region Dalarna.",
                           "\nDiagramförklaring: Till och med 2022 avser statistiken åldersgruppen 16-64 år, mellan 2023 och 2025",
                           "\n16-65 år och från 2026 16-66 år.")

    diagram_titel <- paste0("Tid som arbetslös i ", region)
    diagramfilnamn <- "langtidsarbetsloshet_jmf_langd_tid.png"

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = df |>
        dplyr::mutate(Ar = as.character(Ar),
                      langtidsdefinition = factor(langtidsdefinition,
                                                  levels = c("0 - 6 månader", "6 - 12 månader", "12 - 24 månader", "24 månader -"))),
      skickad_x_var = "Ar",
      skickad_y_var = "andel_langtidsarbetslos",
      skickad_x_grupp = "langtidsdefinition",
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      manual_color = diag_fargvekt,
      dataetiketter = FALSE,
      manual_y_axis_title = "procent",
      diagram_titel = diagram_titel,
      diagram_capt = diagram_capt,
      procent_0_100_10intervaller = TRUE,
      stodlinjer_avrunda_fem = TRUE,
      output_mapp = utmapp,
      filnamn_diagram = diagramfilnamn,
      skriv_till_diagramfil = skriv_diagramfil)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn |> stringr::str_remove(".png")
  }

  if (returnera_figur == TRUE) return(gg_list)
}