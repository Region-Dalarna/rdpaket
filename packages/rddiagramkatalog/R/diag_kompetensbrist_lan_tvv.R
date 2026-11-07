#' Diagram: upplevd kompetensbrist per län
#'
#' Andel småföretag som anser att tillgång till lämplig arbetskraft är ett
#' stort hinder för tillväxt, jämfört mellan ett valt tidigare år och
#' senaste tillgängliga år. Data hämtas från en lokal, manuellt
#' nedladdad Excel-fil (Tillväxtverkets "Företagens villkor och
#' verklighet").
#'
#' @param diagram_capt Diagrammets källtext.
#' @param utmapp Mapp där figuren sparas. Måste finnas om
#'   `skriv_diagramfil = TRUE`.
#' @param skriv_diagramfil Om TRUE sparas figuren till `utmapp`.
#' @param start_ar År som senaste år ska jämföras med. Finns 2011, 2014,
#'   2017, 2020 och 2023.
#' @param returnera_data Skall data returneras till global environment.
#' @param returnera_figur Skall figur returneras (i en lista).
#'
#' @return En namngiven lista med ett `ggplot`-objekt.
#' @export
diag_kompetensbrist_lan_tvv <- function(diagram_capt = "Källa: Tillväxtverket: Företagens villkor och verklighet.\nBearbetning:Samhällsanalys, Region Dalarna\nDiagramförklaring: Andel småföretag som anser att tillgång till lämplig arbetskraft är ett stort hinder för tillväxt",
                                utmapp = rdverktyg::utskriftsmapp(),
                                skriv_diagramfil = TRUE,
                                start_ar = c("2020", "2023"),
                                returnera_data = FALSE,
                                returnera_figur = TRUE
){

  # ========================================== Allmän info ============================================
  # Ett diagram som visar upplevd kompetensbrist i Sveriges regioner.
  # Ingen SCB-hämtning här - datan kommer från en lokal Excel-fil (Tillväxtverkets "Företagens villkor
  # och verklighet", manuellt nedladdad).
  # ========================================== Inställningar ============================================

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)

  # ========================================== Läser in data ============================================
  # Data laddas hem från: https://tillvaxtdata.tillvaxtverket.se/statistik#page=ed8c6ae0-6f98-46fb-bd3b-66b69839e14a
  # Välj ladda ner Excel-fil strax under den andra figuren (linjediagram med alla regioner).
  # Senaste filen som matchar mönstret i mappen (sökt rekursivt, eftersom nedladdningar kan hamna i
  # undermappar) väljs automatiskt utifrån ändringsdatum - i stället för ett hårdkodat filnamn, så att
  # skriptet automatiskt plockar upp en ny nedladdning. intern_hamta_senaste_fil() stoppar med ett
  # tydligt felmeddelande om mappen saknas eller ingen fil matchar, och skriver ut en konsolvarning om
  # att datan är en manuell ögonblicksbild.
  kompetensbrist_mapp <- "G:/skript/projekt/data/kompetensforsorjning/"
  kompetensbrist_fil <- intern_hamta_senaste_fil(kompetensbrist_mapp, "Tillgång till lämplig arbetskraft.*\\.xlsx$", recursive = TRUE)

  # OBS: filen har fyra beskrivande/filterrader överst (t.ex. "Hinder för tillväxt: ...", "Valda
  # filter: ...") innan den riktiga tabellrubriken ("Formulär år" / "Nivå 1" / "Viktad andel") på rad
  # 5 - därav startRow = 5. Kontrollera gärna radnumret om filens layout ändras igen. check.names =
  # FALSE så att kolumnnamnen behåller mellanslag exakt som i Excel-filen.
  kompetensbrist_df <- openxlsx::read.xlsx(kompetensbrist_fil,
                                           startRow = 5,
                                           check.names = FALSE) |>
    dplyr::rename(År = `Formulär.år`,
                  Region = `Nivå.1`,
                  Andel = `Viktad.andel`) |>
    dplyr::mutate(
      År = as.character(År),
      Region = rdverktyg::skapa_kortnamn_lan(Region)
    )

  # Andelen kan komma in som andel (0.24) eller som procenttal (24) beroende på hur cellerna är
  # formaterade i Excel-filen - normaliserar till procent (0-100) här, vilket är vad diagrammet nedan
  # förväntar sig.
  if (max(kompetensbrist_df$Andel, na.rm = TRUE) <= 1) {
    kompetensbrist_df <- kompetensbrist_df |>
      dplyr::mutate(Andel = Andel * 100)
  }

  if (returnera_data == TRUE) {
    assign("kompetensbrist", kompetensbrist_df, envir = .GlobalEnv)
  }

  diagram_titel <- paste0("Upplevd kompetensbrist i Sveriges län")
  diagramfil <- "kompetensbrist.png"

  gg_obj <- rddiagram::SkapaStapelDiagram(
    skickad_df = kompetensbrist_df |>
      dplyr::filter(År %in% c(start_ar, max(År))) |>
      dplyr::mutate(Region = ifelse(toupper(Region) %in% c("TOTAL", "TOTALT"), "Sverige", Region)),
    skickad_x_var = "Region",
    skickad_y_var = "Andel",
    skickad_x_grupp = "År",
    manual_x_axis_text_vjust = 1,
    manual_x_axis_text_hjust = 1,
    manual_color = rddiagram::diagramfarger("rus_sex"),
    diagram_titel = diagram_titel,
    x_axis_sort_value = TRUE,
    x_axis_sort_grp = 3,
    y_axis_100proc = TRUE,
    diagram_capt = diagram_capt,
    x_axis_lutning = 45,
    manual_y_axis_title = "procent",
    output_mapp = utmapp,
    filnamn_diagram = diagramfil,
    skriv_till_diagramfil = skriv_diagramfil)
  gg_list <- c(gg_list, list(gg_obj))

  names(gg_list) <- "kompetensbrist"
  if (returnera_figur == TRUE) return(gg_list)
}
