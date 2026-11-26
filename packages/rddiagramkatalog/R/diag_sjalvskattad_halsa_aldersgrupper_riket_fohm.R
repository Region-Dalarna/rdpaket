#' Diagram: andel med bra eller mycket bra självskattad hälsa efter ålder, riket
#'
#' Tidsserie för andel med bra eller mycket bra självskattad hälsa i olika åldersgrupper, från
#' Hälsa på lika villkor-enkäten hos Folkhälsomyndigheten (tabellen halsgodaald.px). Tabellen finns
#' bara för riket, inte för län eller kommuner. Åldersgrupperna är 16-29, 30-44, 45-64, 65-84 och
#' 85- år, och värdena är 4-årsmedelvärden.
#'
#' @param aldersgrupper Åldersgrupper att visa, som i FoHM:s tabell. Standard är de äldre,
#'   `c("65-84 år", "85- år")`. `"*"` ger de fem
#'   åldersgrupperna (utan totalraderna).
#' @param kon_klartext `c("Kvinnor", "Män")` (finns också `"Totalt"`), eller `NA` för ingen
#'   könsuppdelning (då används "Totalt").
#' @param tid_koder `"*"` = alla perioder (standard), `"9999"` = senaste, annars valda år.
#' @param diag_fargvekt Valfri färgvektor.
#' @param utmapp Mapp där diagrammet sparas. Måste finnas om `skriv_diagramfil = TRUE`.
#' @param skriv_diagramfil Skriv diagrammet till fil i `utmapp`.
#' @param returnera_data Skall data returneras till global environment.
#' @param diagram_capt Diagrammets källtext.
#' @param diagram_0_till_100_procent TRUE så går skalan alltid från 0 till 100 procent, annars anpassas skalan efter data.
#' @param logga_sokvag Sökväg till logga, `NULL` för att köra utan logga.
#'
#' @return En namngiven lista med `ggplot`-objekt.
#' @export
diag_sjalvskattad_halsa_aldersgrupper_riket_fohm <- function(
    aldersgrupper = c("65-84 år", "85- år"),
    kon_klartext = c("Kvinnor", "Män"),
    tid_koder = "*",
    diag_fargvekt = NA,
    utmapp = rdverktyg::utskriftsmapp(),
    skriv_diagramfil = TRUE,
    returnera_data = FALSE,
    diagram_capt = "Källa: Folkhälsomyndighetens öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: 4-årsmedelvärden, riket",
    diagram_0_till_100_procent = TRUE,
    logga_sokvag = NA
) {

  # Folkhälsomyndigheten har bara ett v1-PXWeb-API (inget pxweb2r-stöd), så uttaget görs med pxweb.
  # Åldersuppdelad självskattad hälsa finns bara för riket (halsgodaald.px), inte för län.

  if (!requireNamespace("pxweb", quietly = TRUE)) install.packages("pxweb")

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  konsuppdelat <- !all(is.na(kon_klartext))
  if (!konsuppdelat) kon_klartext <- "Totalt"
  if (all(is.na(diag_fargvekt))) diag_fargvekt <- rddiagram::diagramfarger(if (konsuppdelat) "kon" else "rus_sex")

  url <- "https://fohm-app.folkhalsomyndigheten.se/Folkhalsodata/api/v1/sv/A_Folkhalsodata/A_Mo8/Halsoutfall/01Overgrip/01.01halsgod/halsgodaald.px"
  px_meta <- pxweb::pxweb_get(url)
  hamta_variabel <- function(text) px_meta$variables[[which(vapply(px_meta$variables, `[[`, "", "text") == text)]]
  kod_for_klartext <- function(variabel_text, klartext_vekt) {
    var <- hamta_variabel(variabel_text)
    if (identical(klartext_vekt, "*")) return(var$values)
    kod <- var$values[match(tolower(klartext_vekt), tolower(var$valueTexts))]
    if (anyNA(kod)) stop("Hittar inte ", paste(klartext_vekt[is.na(kod)], collapse = ", "), " i ", variabel_text, ". Giltiga värden: ",
                         paste(var$valueTexts, collapse = ", "))
    kod
  }

  giltiga_ar <- hamta_variabel("År")$values
  tid_vekt <- if (identical(tid_koder, "*")) giltiga_ar else if (identical(tid_koder, "9999")) max(giltiga_ar) else tid_koder[tid_koder %in% giltiga_ar]

  alder_var <- hamta_variabel("Ålder")
  halsa_df <- as.data.frame(pxweb::pxweb_get(url = url, query = list(
    "Hälsotillstånd" = "*",
    "Andel och konfidensintervall" = kod_for_klartext("Andel och konfidensintervall", "Andel"),
    "Ålder" = if (identical(aldersgrupper, "*")) alder_var$values[grepl("^[0-9]", alder_var$valueTexts)] else kod_for_klartext("Ålder", aldersgrupper),
    "Kön" = kod_for_klartext("Kön", kon_klartext),
    "År" = tid_vekt
  )))
  y_var <- grep("Andel$", names(halsa_df), value = TRUE)[1]
  halsa_df <- halsa_df[!is.na(halsa_df[[y_var]]), ]

  if (returnera_data) assign("sjalvskattad_halsa_aldersgrupper_df", halsa_df, envir = .GlobalEnv)

  flera_aldrar <- length(unique(halsa_df$Ålder)) > 1
  tid_txt <- paste0(min(halsa_df$År), "_", max(halsa_df$År))
  alder_txt <- if (identical(aldersgrupper, "*")) "alla_aldrar" else
    sub("_$", "", gsub("_+", "_", gsub("[^0-9a-z]+", "_", tolower(paste(sub(" år$", "", aldersgrupper), collapse = "_")))))
  diagramfil <- paste0("sjalvskattad_halsa_aldersgrupper_riket_", alder_txt, if (konsuppdelat) "_kon" else "", "_", tid_txt, ".png")
  diagramtitel <- if (flera_aldrar) "Andel med bra eller mycket bra självskattad hälsa efter ålder, riket" else
    paste0("Andel med bra eller mycket bra självskattad hälsa, ", unique(halsa_df$Ålder), ", riket")

  gg_obj <- rddiagram::SkapaLinjeDiagram(skickad_df = halsa_df,
                             skickad_x_var = "År",
                             skickad_y_var = y_var,
                             skickad_x_grupp = if (konsuppdelat) "Kön" else if (flera_aldrar) "Ålder" else NA,
                             facet_grp = if (konsuppdelat && flera_aldrar) "Ålder" else NULL,
                             facet_scale = "fixed",
                             facet_legend_bottom = konsuppdelat && flera_aldrar,
                             diagram_titel = diagramtitel,
                             diagram_capt = diagram_capt,
                             stodlinjer_avrunda_fem = TRUE,
                             filnamn_diagram = diagramfil,
                             manual_y_axis_title = "procent",
                             procent_0_100_10intervaller = diagram_0_till_100_procent,
                             lagga_till_punkter = TRUE,
                             x_axis_lutning = 45,
                             manual_color = diag_fargvekt,
                             lagg_pa_logga = if (is.null(logga_sokvag)) FALSE else TRUE,
                             logga_path = logga_sokvag,
                             output_mapp = utmapp,
                             skriv_till_diagramfil = skriv_diagramfil)

  gg_list <- list(gg_obj)
  names(gg_list) <- sub("\\.png$", "", diagramfil)
  gg_list
}
