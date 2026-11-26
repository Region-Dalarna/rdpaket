#' Diagram: gymnasieantagningen per program eller över tid
#'
#' Visar antagna, förstahandssökande, platser eller outnyttjade platser i
#' gymnasieantagningen i Dalarna, antingen som en jämförelse mellan alla
#' program ett visst år (`diagramtyp = "program"`) eller som en tidsserie
#' (`diagramtyp = "tidsserie"`) för alla program sammantaget eller ett valt
#' program. Data hämtas från tabellen `dkf.gymnasieantagna` i databasen
#' `oppna_data`, samma källa som utbildningsappen.
#'
#' Antagna och förstahandssökande kan delas upp på kön. Platser och
#' outnyttjade platser finns bara totalt i källan.
#'
#' @param matt Vilket mått som ska visas: `"antagna"` (standard),
#'   `"forstahandssokande"`, `"platser"` eller `"outnyttjade"`.
#' @param konsuppdelat Om `TRUE` delas diagrammet upp på kvinnor och män. Går
#'   bara för `"antagna"` och `"forstahandssokande"`.
#' @param diagramtyp `"program"` (standard) jämför alla program ett år,
#'   `"tidsserie"` visar utvecklingen över tid.
#' @param ar Vilket år som visas i programdiagrammet. `"9999"` (standard) =
#'   senaste året i data. Används inte för tidsserien.
#' @param program Valfritt: ett eller flera program (eller programinriktningar
#'   om `niva = "inriktning"`) som tidsserien ska visas för, ett diagram per
#'   program. `NULL` (standard) = alla program sammantaget. I
#'   programdiagrammet begränsas urvalet till de angivna programmen.
#' @param niva `"program"` (standard) eller `"inriktning"`
#'   (`program_inriktning` i källan).
#' @param kommun Valfritt: kommunkoder eller kommunnamn att begränsa till,
#'   t.ex. `c("2080", "2081")` eller `"Falun"`. `NULL` (standard) = hela
#'   Dalarna.
#' @param utmapp Mapp där figuren sparas. Måste finnas om
#'   `skriv_diagramfil = TRUE`.
#' @param output_mapp_data Mapp där data sparas, om `filnamn_data` också
#'   anges. `NA` (standard) = data sparas inte.
#' @param filnamn_data Filnamn för sparad Excel-data.
#' @param diag_fargvekt Vilka färger som ska användas i diagram.
#' @param skriv_diagramfil Om TRUE sparas figuren till `utmapp`.
#' @param diagram_capt Diagrammets källtext. `"auto"` (standard) väljer text
#'   utifrån vilket mått som visas.
#' @param returnera_figur Skall figur returneras (i en lista).
#' @param returnera_data Skall data returneras (till R-studios globala
#'   miljö, som `gymnasieantagning_df`).
#'
#' @return En namngiven lista med `ggplot`-objekt.
#' @export
diag_gymnasieantagna_antal <- function(matt = c("antagna", "forstahandssokande", "platser", "outnyttjade"),
                                       konsuppdelat = FALSE,
                                       diagramtyp = c("program", "tidsserie"),
                                       ar = "9999",
                                       program = NULL,
                                       niva = c("program", "inriktning"),
                                       kommun = NULL,
                                       utmapp = rdverktyg::utskriftsmapp(),
                                       output_mapp_data = NA,
                                       filnamn_data = "gymnasieantagning.xlsx",
                                       diag_fargvekt = rddiagram::diagramfarger("rus_sex"),
                                       skriv_diagramfil = TRUE,
                                       diagram_capt = "auto",
                                       returnera_figur = TRUE,
                                       returnera_data = FALSE) {

  # ========================================== Allmän info ============================================
  # Läser dkf.gymnasieantagna i oppna_data (samma tabell och samma kolumnnamn som utbildningsappens
  # hamta_gymnasiedata()). Raderna är en per utbildning/skola/kommun och år och summeras här.
  # Platser (org) och outnyttjade platser (led_pl) saknar könsuppdelning i källan.
  # ========================================== Inställningar ============================================

  matt <- match.arg(matt)
  diagramtyp <- match.arg(diagramtyp)
  niva <- match.arg(niva)

  if (konsuppdelat && matt %in% c("platser", "outnyttjade")) {
    warning("Platser och outnyttjade platser finns inte uppdelade på kön - visar totalt.", call. = FALSE)
    konsuppdelat <- FALSE
  }

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  matt_info <- list(
    antagna = list(tot = "ant_tot", kv = "ant_kv", man = "ant_män", etikett = "Antagna elever",
                   titel = "Antagna gymnasieelever", fil = "antagna",
                   forklaring = "Antagna i början av september"),
    forstahandssokande = list(tot = "1a_tot", kv = "1a_kv", man = "1a_män", etikett = "Förstahandssökande",
                              titel = "Förstahandssökande till gymnasiet", fil = "forstahandssokande",
                              forklaring = "Förstahandssökande i början av september"),
    platser = list(tot = "org", etikett = "Antal platser", titel = "Gymnasieplatser", fil = "platser",
                   forklaring = "Platser när antagningsprocessen startar i januari"),
    outnyttjade = list(tot = "led_pl", etikett = "Outnyttjade platser", titel = "Outnyttjade gymnasieplatser",
                       fil = "outnyttjade", forklaring = "Skillnaden mellan platser och antagna")
  )[[matt]]

  if (identical(diagram_capt, "auto")) {
    diagram_capt <- paste0("Källa: Gymnasieantagningen, Dalarnas kommunförbund\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: ",
                           matt_info$forklaring)
  }

  # =============================================== Läser in data ===============================================

  rad <- rdpostgres::oppnadata_hamta("dkf", "gymnasieantagna")
  df <- intern_gymnasieantagna_bearbeta(rad, matt_info, konsuppdelat, niva, kommun)
  kommun_namn <- if (is.null(kommun)) NULL else unique(rad$kommun[as.character(rad$kommkod) %in% kommun | rad$kommun %in% kommun])

  if (nrow(df) == 0) stop("Inga rader i dkf.gymnasieantagna för valt urval.", call. = FALSE)

  if (!is.null(program)) {
    saknas <- setdiff(program, unique(df$program))
    if (length(saknas) > 0) {
      stop("Hittar inte: ", paste(saknas, collapse = ", "), ". Finns: ",
           paste(sort(unique(df$program)), collapse = "; "), call. = FALSE)
    }
  }

  if (returnera_data) assign("gymnasieantagning_df", df, envir = .GlobalEnv)

  if (!is.na(output_mapp_data) && !is.na(filnamn_data)) {
    openxlsx::write.xlsx(df, paste0(output_mapp_data, filnamn_data))
  }

  geo_txt <- if (is.null(kommun)) "i Dalarna" else paste0("i ", rdverktyg::list_komma_och(kommun_namn))
  kon_farger <- rddiagram::diagramfarger("kon")
  gg_list <- list()

  # =============================================== Programdiagram ===============================================

  if (diagramtyp == "program") {
    valt_ar <- if (identical(as.character(ar), "9999")) max(df$ar) else as.integer(ar)
    plot_df <- dplyr::filter(df, ar == valt_ar)
    if (!is.null(program)) plot_df <- dplyr::filter(plot_df, program %in% !!program)
    if (nrow(plot_df) == 0) stop("Inga data för år ", valt_ar, ".", call. = FALSE)

    filnamn <- paste0("gymnasieantagning_", matt_info$fil, if (konsuppdelat) "_kon" else "", "_", valt_ar, ".png")
    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = plot_df,
      skickad_x_var = "program",
      skickad_y_var = "varde",
      skickad_x_grupp = if (konsuppdelat) "Kon" else NA,
      manual_x_axis_text_vjust = 0,
      manual_x_axis_text_hjust = 0.6,
      manual_color = if (konsuppdelat) kon_farger else diag_fargvekt[1],
      diagram_titel = paste0(matt_info$titel, " ", geo_txt, " ", valt_ar, " per ", if (niva == "program") "program" else "programinriktning"),
      x_axis_sort_value = TRUE,
      diagram_capt = diagram_capt,
      x_axis_lutning = 0,
      diagram_liggande = TRUE,
      manual_y_axis_title = matt_info$etikett,
      stodlinjer_avrunda_fem = TRUE,
      output_mapp = utmapp,
      filnamn_diagram = filnamn,
      skriv_till_diagramfil = skriv_diagramfil
    )
    gg_list[[sub("\\.png$", "", filnamn)]] <- gg_obj
  }

  # =============================================== Tidsserie ===============================================

  if (diagramtyp == "tidsserie") {
    # NULL = alla program sammantaget (ett diagram), annars ett diagram per valt program
    urval <- if (is.null(program)) list(NULL) else as.list(program)

    for (prog in urval) {
      plot_df <- if (is.null(prog)) df else dplyr::filter(df, program == prog)
      plot_df <- plot_df |>
        dplyr::group_by(dplyr::across(dplyr::any_of(c("ar", "Kon")))) |>
        dplyr::summarise(varde = sum(varde, na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(ar = as.character(ar))

      namn_del <- if (is.null(prog)) "alla_program" else gsub("^_+|_+$", "", gsub("[^a-z0-9]+", "_", chartr("åäöéü", "aaoeu", tolower(prog))))
      filnamn <- paste0("gymnasieantagning_", matt_info$fil, if (konsuppdelat) "_kon" else "", "_tidsserie_", namn_del, ".png")
      titel_del <- if (is.null(prog)) "alla program" else prog

      gg_obj <- rddiagram::SkapaLinjeDiagram(
        skickad_df = plot_df,
        skickad_x_var = "ar",
        skickad_y_var = "varde",
        skickad_x_grupp = if (konsuppdelat) "Kon" else NA,
        manual_color = if (konsuppdelat) kon_farger else diag_fargvekt[1],
        diagram_titel = paste0(matt_info$titel, " ", geo_txt, ", ", titel_del),
        diagram_capt = diagram_capt,
        manual_y_axis_title = matt_info$etikett,
        lagga_till_punkter = TRUE,
        x_axis_lutning = 45,
        output_mapp = utmapp,
        filnamn_diagram = filnamn,
        skriv_till_diagramfil = skriv_diagramfil
      )
      gg_list[[sub("\\.png$", "", filnamn)]] <- gg_obj
    }
  }

  if (returnera_figur) return(gg_list)
}

# Råtabellen -> en rad per år, program (eller inriktning) och ev. kön, med kolumnen `varde`.
# Egen funktion så att den kan testas utan databas.
intern_gymnasieantagna_bearbeta <- function(rad, matt_info, konsuppdelat, niva, kommun_urval) {
  # Gy25 förde in Anpassad gymnasieskola i samma tabell - en annan skolform som inte ska blandas in
  # (samma filter som i utbildningsappen).
  if ("skolform" %in% names(rad)) rad <- dplyr::filter(rad, is.na(skolform) | skolform != "Anpassad gymnasieskola")

  rad$kommkod <- as.character(rad$kommkod)
  if (!is.null(kommun_urval)) rad <- dplyr::filter(rad, kommkod %in% !!kommun_urval | kommun %in% !!kommun_urval)

  programkol <- if (niva == "program") "program" else "program_inriktning"
  rad <- dplyr::mutate(rad, ar = as.integer(ar), program = .data[[programkol]])
  rad <- dplyr::filter(rad, !is.na(program))

  if (konsuppdelat) {
    df <- tidyr::pivot_longer(
      dplyr::select(rad, ar, program, Kvinnor = dplyr::all_of(matt_info$kv), Män = dplyr::all_of(matt_info$man)),
      c(Kvinnor, Män), names_to = "Kon", values_to = "varde")
    df <- dplyr::group_by(df, ar, program, Kon)
  } else {
    df <- dplyr::transmute(rad, ar, program, varde = .data[[matt_info$tot]])
    df <- dplyr::group_by(df, ar, program)
  }
  dplyr::summarise(df, varde = sum(varde, na.rm = TRUE), .groups = "drop")
}
