#' Diagram: BNP-förändring
#'
#' Ett stapeldiagram över den procentuella förändringen i Sveriges BNP
#' jämfört med föregående kvartal (säsongrensad volymförändring). Data
#' laddas automatiskt ned från SCB:s öppna statistikdatabas.
#'
#' @param utmapp Mapp där figuren sparas. Måste finnas om
#'   `skriv_diagramfil = TRUE`.
#' @param skriv_diagramfil Skall figuren sparas.
#' @param returnera_data Skall data läggas i R-studios globala miljö.
#' @param returnera_figur Skall en lista med figuren returneras.
#' @param ta_bort_diagramtitel TRUE om diagramtiteln ska tas bort.
#' @param ta_bort_caption TRUE om diagramtexten (caption) ska tas bort.
#' @param diag_fargvekt Vilka färger som ska användas i diagrammet.
#' @param startar Startår för diagrammet. Data finns från 1980.
#' @param visa_etikett Antal x-etiketter som visas. 8 ger vartannat år.
#' @param x_axis_lutning Lutning på x-axelns etiketter (grader).
#' @param data_namm Namn på dataobjektet som läggs i globala miljön om
#'   `returnera_data = TRUE`.
#'
#' @return En namngiven lista med ett `ggplot`-objekt.
#' @export
diag_BNP_forandring_SCB <- function(utmapp = rdverktyg::utskriftsmapp(),
                                    skriv_diagramfil = TRUE,
                                    returnera_data = FALSE,
                                    returnera_figur = TRUE,
                                    ta_bort_diagramtitel = FALSE,
                                    ta_bort_caption = FALSE,
                                    diag_fargvekt = rddiagram::diagramfarger("rus_sex"),
                                    startar = "2016",
                                    visa_etikett = 8,
                                    x_axis_lutning = 45,
                                    data_namm = "BNP_df")  {
  
  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)
  gg_list <- list()
  
  BNP_df <- pxweb2r::pxweb2_get_data(
    table = "TAB4553",
    query = list(
      Anvandningstyp = "- BNP till marknadspris",
      ContentsCode   = "Säsongrensad, volymförändring föregående period, procent",
      Tid            = "*"
    ), quiet = TRUE) %>%
    dplyr::rename("Säsongrensad, volymförändring föregående period, procent" = value) %>%
    dplyr::select(-tabellinnehåll)
  
  BNP_df <- BNP_df %>%
    dplyr::mutate(ar = substr(kvartal, 1, 4),
                  ar_kvartal = kvartal,
                  kvartal = substr(kvartal, 5, 6)) %>%
    dplyr::rename("Sasongsransad_forandring" = `Säsongrensad, volymförändring föregående period, procent`) %>%
    dplyr::mutate(kvartal_ar = paste0(kvartal, " ", ar)) %>%
    dplyr::mutate(kvartal = dplyr::case_when(
      kvartal == "K1" ~ "kvartal ett",
      kvartal == "K2" ~ "kvartal två",
      kvartal == "K3" ~ "kvartal tre",
      TRUE            ~ "kvartal fyra"))
  
  if (returnera_data == TRUE) {
    assign(data_namm, BNP_df, envir = .GlobalEnv)
  }
  
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."
  diagramtitel <- "Procentuell förändring i Sveriges BNP jämfört med föregående kvartal"
  diagramtitel <- stringr::str_wrap(diagramtitel, 40)
  diagramfilnamn <- "BNP.png"
  
  if (ta_bort_diagramtitel == TRUE) {
    diagramtitel <- NULL
  }
  if (ta_bort_caption == TRUE) {
    diagram_capt <- NULL
  }
  
  BNP_df$kvartal_ar <- factor(BNP_df$kvartal_ar,
                              levels = unique(BNP_df$kvartal_ar),  # behåll ordning i första förekomst
                              ordered = TRUE)
  
  gg_obj <- rddiagram::SkapaStapelDiagram(
    skickad_df = BNP_df %>% dplyr::filter(ar >= startar),
    skickad_x_var = "kvartal_ar",
    skickad_y_var = "Sasongsransad_forandring",
    stodlinjer_avrunda_fem = TRUE,
    manual_color = diag_fargvekt,
    diagram_titel = diagramtitel,
    x_axis_sort_value = FALSE,
    manual_y_axis_title = "procent",
    x_axis_lutning = x_axis_lutning,
    x_axis_visa_var_xe_etikett = visa_etikett,
    diagram_capt = diagram_capt,
    output_mapp = utmapp,
    filnamn_diagram = diagramfilnamn,
    skriv_till_diagramfil = skriv_diagramfil)
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list) <- diagramfilnamn %>% stringr::str_remove(".png")
  
  if (returnera_figur == TRUE) return(gg_list)
}