#' Diagram: antal företag för 50 procent av lönesumman per kommun
#'
#' Hur många företag som krävs för att lönesumman i en kommun skall uppgå
#' till 50 procent (kumulativt). För tillfället enbart för Dalarna. Vill
#' man ha annat län måste data hämtas via MONA.
#'
#' @param utmapp Mapp där figuren sparas. Måste finnas om `skriv_diagramfil = TRUE`.
#' @param skriv_diagramfil Om TRUE sparas figuren till `utmapp`.
#' @param returnera_data Skall data returneras till global environment.
#'
#' @return En namngiven lista med ett `ggplot`-objekt.
#' @export
diag_50proc_lonesumma_nms <- function(utmapp = rdverktyg::utskriftsmapp(),
                                  skriv_diagramfil = TRUE,
                                  returnera_data = FALSE
                                  ){

  # ========================================== Allmän info ============================================
  # Hur många företag krävs för att lönesumman i en kommun skall uppgå till 50 % (kumulativt).
  # För tillfället enbart för Dalarna. Vill man ha annat län måste data hämtas via MONA. Skript finns på:
  # R-skript som skapar data finns på MONA under /Jon/Sårbarhetsanalys/Nya databasen/Ftg_50procent_lonesumma_ny_databas.R
  # ========================================== Inställningar ============================================

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  # Text till diagram
  diagram_capt_kommun <- c("Källa: SCB.\nBearbetning: Samhällsanalys, Region Dalarna.")

  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)

  # ========================================== Läser in data ============================================
  # Läser in data från Excel (ursprung NMS-databasen) - senaste filen som matchar mönstret i mappen väljs
  # automatiskt utifrån ändringsdatum. intern_hamta_senaste_fil() stoppar med ett tydligt felmeddelande
  # om mappen saknas eller ingen fil matchar, i stället för det kryptiska felet som annars uppstår, och
  # skriver ut en konsolvarning om att datan är en manuell ögonblicksbild.
  input_mapp <- "G:/skript/projekt/data/sarbarhetsanalys/"
  latest_file <- intern_hamta_senaste_fil(input_mapp, "*50_procent_lonesumma")

  kommun_df <- openxlsx::read.xlsx(latest_file,sheet=1)

  if(returnera_data == TRUE){
    assign("lonesumma_andel_50proc", kommun_df, envir = .GlobalEnv)
  }

  diagramtitel <- paste0("Summan av antalet företag i respektive kommun som utgör 50 procent av lönesumman ",max(kommun_df$Ar))
  diagramtitel <- stringr::str_wrap(diagramtitel,55)
  diagramfilnamn <- "50proc_lonesumma.png"

  gg_obj <- rddiagram::SkapaStapelDiagram(
    skickad_df = kommun_df,
    skickad_x_var = "LAKommun_namn",
    skickad_y_var = "cnum",
    manual_x_axis_text_vjust=1,
    manual_x_axis_text_hjust=1,
    manual_color = rddiagram::diagramfarger("rus_sex")[1],
    diagram_titel = diagramtitel,
    diagram_capt =  diagram_capt_kommun,
    manual_y_axis_title = "Antal företag",
    stodlinjer_avrunda_fem = TRUE,
    dataetiketter = TRUE,
    x_axis_lutning = 45,
    x_axis_sort_value = TRUE,
    output_mapp = utmapp,
    filnamn_diagram = diagramfilnamn,
    skriv_till_diagramfil = skriv_diagramfil)

  gg_list <- c(gg_list, list(gg_obj))

  names(gg_list) <- "50_proc_lonesumma"
  return(gg_list)
}
