#' Diagram: examinerade från Högskolan Dalarna
#'
#' Skapar diagram för examinerade från Högskolan Dalarna, för antingen ett
#' valt år eller ett antal år.
#'
#' @param utmapp Mapp där figuren sparas. Måste finnas om `skriv_diagramfil = TRUE`.
#' @param output_mapp_data Mapp där data sparas.
#' @param filnamn_data Filnamn på sparad data.
#' @param filnamn_figur Filnamn på sparad figur.
#' @param diag_fargvekt Vilka färger skall användas i diagram.
#' @param valda_ar Vilka läsår som skall väljas. `"99"` ger första läsåret, `"9999"` ger senaste läsåret.
#' @param diagram_capt Diagrammets källtext.
#' @param skriv_diagramfil Om TRUE sparas figuren till `utmapp`.
#' @param returnera_figur Skall figur returneras (i en lista).
#' @param returnera_data Skall data returneras (till R-studios globala miljö).
#'
#' @return En namngiven lista med ett `ggplot`-objekt.
#' @export
diag_examen_hogskolan_nms <- function(utmapp = rdverktyg::utskriftsmapp(),
                                  output_mapp_data = NA,
                                  filnamn_data = "hogsoleexamen.xlsx",
                                  filnamn_figur = "hogskoleexamen.png",
                                  diag_fargvekt = rddiagram::diagramfarger("rus_sex"),
                                  valda_ar = c(2010,2015,"9999"),
                                  diagram_capt = "Källa: NMS-databasen (SCB), högskoleregistret\nBearbetning:Samhällsanalys, Region Dalarna\nDiagramförklaring: Enbart program/områden med minst 5 examinerade",
                                  skriv_diagramfil = TRUE,
                                  returnera_figur = TRUE,
                                  returnera_data = FALSE){

  # ========================================== Allmän info ================================================
  # 1: Skapar diagram för examinerade från Högskolan Dalarna, för antingen ett valt år eller ett antal år
  # Data hämtas från regiondatabasen (SCB): /Jon/kompetensförsörjning/Ny databas/hogskoleexamen_ny_databas.R
  # ========================================== Inställningar ==============================================

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)

  # =============================================== Läs in data ===============================================
  # Läs in datafil med högskoleexamen - senaste filen som matchar mönstret i mappen väljs automatiskt
  # utifrån ändringsdatum. intern_hamta_senaste_fil() stoppar med ett tydligt felmeddelande om mappen
  # saknas eller ingen fil matchar, och skriver ut en konsolvarning om att datan är en manuell
  # ögonblicksbild.
  folder_path <- "G:/skript/projekt/data/kompetensforsorjning/"
  latest_file <- intern_hamta_senaste_fil(folder_path, "Dalarna_hogskoleexamen")

  df <- openxlsx::read.xlsx(latest_file)

  # Mall för att få klartext (utbildningsnamn) - en fast, delad mallfil (inte en tidsserie med nya
  # årgångar som högskoleexamen-datan ovan), så intern_sakerstall_indatafil() används här i stället
  # för intern_hamta_senaste_fil().
  mall_sun_sokvag <- "G:/skript/projekt/data/kompetensforsorjning/mall_sun2020inr.xlsx"
  intern_sakerstall_indatafil(mall_sun_sokvag)
  mall_sun <- openxlsx::read.xlsx(mall_sun_sokvag, sheet = 1)
  mall_sun_2siffer <- openxlsx::read.xlsx(mall_sun_sokvag, sheet = 2) |>
    dplyr::mutate("SUN2020Inr_2siffer" = as.character(SUN2020Inr_2siffer))

  # Alla utbildningar
  # Plockar ut de två första siffrorna i sun-koden (för att matcha mot en bredare mall)
  df <- df |>
    dplyr::mutate("SUN2020Inr_2siffer" = substr(SUN2020Inr, 1, 2)) |>
    dplyr::left_join(mall_sun, by = "SUN2020Inr") |>
    dplyr::left_join(mall_sun_2siffer, by = "SUN2020Inr_2siffer") |>
    dplyr::relocate(antal, .after = SUN2020Inr_2siffer_namn)

  if("99" %in% valda_ar) valda_ar <- replace(valda_ar,valda_ar=="99",min(df$LAr))
  if("9999" %in% valda_ar) valda_ar <- replace(valda_ar,valda_ar=="9999",max(df$LAr))

  # Grupperar på år och den bredare utbildningsnivån (2 siffror).
  df <- df |>
    dplyr::filter(LAr %in% valda_ar) |>
    dplyr::group_by(LAr, SUN2020Inr_2siffer_namn) |>
    dplyr::summarize(antal = sum(antal), .groups = "drop")

  # Returnerar data till R globala miljö
  if(returnera_data == TRUE){
    assign("hogskoleexamen_df", df, envir = .GlobalEnv)
  }

  # =============================================== Diagram ===============================================

  if(length(valda_ar) == 1) diagram_titel <- paste0("Antal examinerade från Högskolan Dalarna låsåret ",max(df$LAr)," per program") else diagram_titel <- "Antal examinerade från Högskolan Dalarna per program"
  diagramfil <- filnamn_figur

  # Program/områden med minst 5 examinerade det senaste året i urvalet
  program_minst_5 <- df |>
    dplyr::filter(LAr == max(LAr)) |>
    dplyr::filter(antal > 4) |>
    dplyr::pull(SUN2020Inr_2siffer_namn) |>
    unique()

  gg_obj <- rddiagram::SkapaStapelDiagram(
    skickad_df = dplyr::filter(df, SUN2020Inr_2siffer_namn %in% program_minst_5),
    skickad_x_var = "SUN2020Inr_2siffer_namn",
    skickad_y_var = "antal",
    skickad_x_grupp = ifelse(length(valda_ar) == 1,NA,"LAr"),
    manual_color = diag_fargvekt,
    diagram_titel = diagram_titel,
    x_axis_sort_value = TRUE,
    diagram_capt = diagram_capt,
    x_axis_lutning = 0,
    diagram_liggande = TRUE,
    stodlinjer_avrunda_fem = TRUE,
    manual_y_axis_title = "",
    manual_x_axis_title = "",
    output_mapp = utmapp,
    filnamn_diagram = diagramfil,
    skriv_till_diagramfil = skriv_diagramfil
  )


  gg_list <- c(gg_list, list(gg_obj))

  names(gg_list) <- "hogskoleexamen"

  # OBS: originalet returnerade (vid returnera_figur = TRUE, standardvärdet)
  # INNAN Excel-exporten nedan, vilket gjorde att output_mapp_data/
  # filnamn_data aldrig skrevs i praktiken. Exporten görs nu alltid innan
  # returneringen.
  if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
    openxlsx::write.xlsx(df,paste0(output_mapp_data,filnamn_data))
  }

  if(returnera_figur == TRUE) return(gg_list)

}
