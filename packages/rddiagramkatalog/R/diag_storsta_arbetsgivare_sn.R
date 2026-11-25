#' Diagram: största arbetsgivare (Svenskt Näringsliv)
#'
#' Liggande stapeldiagram över de tio största arbetsgivarna per vald region
#' (län och/eller kommun), ett diagram per region. Data kommer från en
#' Excelfil som mejlats från Svenskt Näringsliv (foretagsklimat@svensktnaringsliv.se)
#' och läses in från `input_mapp`, där den senaste filen (högst årtal i
#' filnamnet) används. Antal anställda är medelvärdet av ett spann om 50 och
#' därför inte exakt. Hanteringen av företagsnamn är anpassad för Dalarna.
#'
#' @param utmapp Mapp där figuren sparas. Måste finnas om
#'   `skriv_diagramfil = TRUE`.
#' @param region_vekt Vilka regioner (länskoder och/eller kommunkoder) som ska
#'   väljas. Ett diagram skapas per region.
#' @param skriv_diagramfil Skall figurerna sparas.
#' @param skapa_diagram Skall diagram skapas. `FALSE` kan användas tillsammans
#'   med `returnera_data = TRUE` för att bara hämta data.
#' @param returnera_data Skall data läggas i R-studios globala miljö.
#' @param returnera_figur Skall en lista med figurer returneras.
#' @param diag_fargvekt Vilka färger som ska användas i diagrammen.
#' @param privata_arbetsgivare Om `TRUE` visas bara privata arbetsgivare,
#'   annars både privata och offentliga.
#' @param input_mapp Mapp där Excelfilerna från Svenskt Näringsliv ligger.
#'   Filnamnen ska innehålla "samtliga" och årtal.
#' @param diagram_capt Text under diagrammen.
#' @param data_namm Namn på dataobjektet som läggs i globala miljön om
#'   `returnera_data = TRUE`.
#'
#' @return En namngiven lista med `ggplot`-objekt.
#' @export
diag_storsta_arbetsgivare_sn <- function(utmapp = rdverktyg::utskriftsmapp(),
                                         region_vekt = "20",
                                         skriv_diagramfil = TRUE,
                                         skapa_diagram = TRUE,
                                         returnera_data = FALSE,
                                         returnera_figur = TRUE,
                                         diag_fargvekt = rddiagram::diagramfarger("rus_sex"),
                                         privata_arbetsgivare = FALSE,
                                         input_mapp = "G:/Samhällsanalys/Statistik/Företagsstatistik/Största arbetsgivare/www_foretagsklimat_se/mail_fran_foretagsklimat/",
                                         diagram_capt = "Källa: Svenskt Näringsliv (www.foretagsklimat.se)\nBearbetning: Samhällsanalys, Region Dalarna\nNotera att antal anställda är medelvärdet av ett spann om 50 och således inte exakta.",
                                         data_namm = "storsta_arbetsgivare") {
  
  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)
  gg_list <- list()
  
  # ========================================== Läser in data ============================================
  if (!dir.exists(input_mapp)) {
    stop("Mappen `input_mapp` finns inte: ", input_mapp, call. = FALSE)
  }
  
  filer <- list.files(input_mapp, pattern = "samtliga", full.names = TRUE)
  
  if (length(filer) == 0) {
    stop("Hittade inga filer med \"samtliga\" i namnet i ", input_mapp, call. = FALSE)
  }
  
  # Senaste filen = högst årtal i filnamnet (ändringsdatum används bara om inget filnamn har årtal)
  fil_ar <- as.integer(stringr::str_extract(basename(filer), "\\d{4}"))
  
  if (all(is.na(fil_ar))) {
    senaste_fil <- filer[which.max(file.info(filer)$mtime)]
    warning("Inget filnamn innehåller ett årtal - använder den senast ändrade filen: ", basename(senaste_fil),
            call. = FALSE)
  } else {
    senaste_fil <- filer[which.max(fil_ar)]
  }
  
  ar_fil <- stringr::str_extract(basename(senaste_fil), "\\d{4}")
  
  data_list <- list()
  
  for (flik in c("Län", "Kommun")) {
    df <- readxl::read_xlsx(senaste_fil, sheet = flik, skip = 3)
    
    if (!all(c("FöretagsNamn", "RedigeratAntalAnställda") %in% names(df))) {
      stop("Fliken \"", flik, "\" i ", basename(senaste_fil),
           " saknar kolumnerna FöretagsNamn och/eller RedigeratAntalAnställda.\n",
           "Inlästa kolumner: ", paste(names(df), collapse = ", "), call. = FALSE)
    }
    
    # Kolumn 1-2 = regionkod och regionnamn (namnen varierar mellan filerna)
    names(df)[1:2] <- c("regionkod", "region_namn")
    
    data_list[[flik]] <- df |>
      dplyr::select(regionkod, region_namn, FöretagsNamn, RedigeratAntalAnställda) |>
      dplyr::rename(`antal anställda` = RedigeratAntalAnställda) |>
      dplyr::mutate(
        # Kod kan läsas in som tal - återställ inledande nollor (län 2 siffror, kommun 4)
        regionkod = stringr::str_pad(as.character(regionkod), width = ifelse(flik == "Län", 2, 4), pad = "0"),
        `antal anställda` = as.numeric(`antal anställda`),
        # Delar upp i offentlig och privat verksamhet (heuristik på namnet)
        sektor = dplyr::if_else(
          stringr::str_detect(FöretagsNamn, stringr::regex(
            paste0("region|kommun|församling|verket|landsting|högskola|universitet|myndighet|",
                   "kriminalvård|försvarsmakt|försäkringskassa|polis|utbildningsförbund|räddningstjänst"),
            ignore_case = TRUE)),
          "Offentlig",
          "Privat"),
        år = ar_fil)
  }
  
  arbgiv_df <- dplyr::bind_rows(data_list) |>
    dplyr::rename(Arbetsgivare = FöretagsNamn) |>
    dplyr::filter(regionkod %in% region_vekt)
  
  if (nrow(arbgiv_df) == 0) {
    stop("Ingen data för region_vekt = ", paste(region_vekt, collapse = ", "), " i ", basename(senaste_fil), ".",
         call. = FALSE)
  }
  
  privat_arb_txt <- ""
  privat_arb_fil <- ""
  
  if (privata_arbetsgivare == TRUE) {
    arbgiv_df <- arbgiv_df |> dplyr::filter(sektor == "Privat")
    privat_arb_txt <- "privata "
    privat_arb_fil <- "_privata"
  }
  
  # ========================================== Företagsnamn ============================================
  # Gör om namn som är skrivna helt i versaler till normal skrivning
  fixa_namn <- function(x,
                        behall_versaler = c("AB", "HB", "KB", "SSAB", "IKEA", "ICA", "NKT",
                                            "MVUS", "UPA", "VBU", "3M"),
                        sma_ord = c("i", "och", "and", "av", "för", "på", "till", "med", "län", "kommun"),
                        undantag = c(
                          "OK-Q8 AB"                    = "OK-Q8 AB",
                          "E.B. HEDLUND & CO AKTIEBOLAG" = "E.B. Hedlund & Co AB",
                          "F:a GREFS AB"                = "F:a Grefs AB",
                          "CARLBORGSONS GÅRD/ VÄSTANVIKS FOLKH.SKOLA EK FÖR" =
                            "Carlborgsons Gård / Västanviks Folkhögskola",
                          "Hedemora ASSistansservice Ekonomisk förening" =
                            "Hedemora Assistansservice Ekonomisk förening")) {
    
    alla_versaler <- x == stringr::str_to_upper(x, locale = "sv")
    
    ny <- stringr::str_to_title(x, locale = "sv")
    
    # Små ord ska vara gemener (men inte första ordet)
    ny <- stringr::str_replace_all(
      ny,
      stringr::regex(paste0("(?<=\\s)(", paste(sma_ord, collapse = "|"), ")(?=\\s|$)"), ignore_case = TRUE),
      stringr::str_to_lower)
    
    # Förkortningar ska vara versaler
    ny <- stringr::str_replace_all(
      ny,
      stringr::regex(paste0("\\b(", paste(behall_versaler, collapse = "|"), ")\\b"), ignore_case = TRUE),
      stringr::str_to_upper)
    
    # Bara namn som var helt i versaler ändras, övriga behåller sin skrivning
    ny <- dplyr::if_else(alla_versaler, ny, x)
    
    # Mellanslag efter kommatecken: "Utbildningsförbund,VBU" -> "Utbildningsförbund, VBU"
    ny <- stringr::str_replace_all(ny, ",(?=\\S)", ", ")
    
    # Enhetlig "AB" i stället för "Aktiebolag" (rör inte "Aktiebolaget")
    ny <- stringr::str_replace_all(ny, "\\bAktiebolag\\b", "AB")
    
    # Manuella undantag (matchas mot originalnamnet) vinner alltid
    dplyr::coalesce(unname(undantag[x]), ny)
  }
  
  arbgiv_df <- arbgiv_df |>
    dplyr::mutate(Arbetsgivare = fixa_namn(Arbetsgivare))
  
  if (returnera_data == TRUE) {
    assign(data_namm, arbgiv_df |> dplyr::rename(antal_anstallda = `antal anställda`), envir = .GlobalEnv)
  }
  
  # ========================================== Diagram ============================================
  if (skapa_diagram == TRUE) {
    
    regioner <- unique(arbgiv_df$region_namn)
    diagramfilnamn <- paste0("Storsta_arbetsgivare", privat_arb_fil, "_", regioner, "_", ar_fil, ".png")
    
    gg_list <- purrr::map2(regioner, diagramfilnamn, \(vald_region, filnamn) {
      rddiagram::SkapaStapelDiagram(
        skickad_df = arbgiv_df |>
          dplyr::filter(region_namn == vald_region) |>
          # Topp 10, men alla som delar plats 10 tas med (antal anställda är avrundat i spann om 50)
          dplyr::slice_max(`antal anställda`, n = 10, with_ties = TRUE),
        skickad_x_var = "Arbetsgivare",
        skickad_y_var = "antal anställda",
        x_axis_sort_value = TRUE,
        diagram_liggande = TRUE,
        stodlinjer_avrunda_fem = TRUE,
        x_axis_lutning = 0,
        manual_color = diag_fargvekt,
        diagram_titel = paste0("Största ", privat_arb_txt, "arbetsgivare i ", vald_region, " år ", ar_fil),
        diagram_capt = diagram_capt,
        output_mapp = utmapp,
        filnamn_diagram = filnamn,
        skriv_till_diagramfil = skriv_diagramfil)
    }) |>
      purrr::set_names(diagramfilnamn |> stringr::str_remove(".png"))
  }
  
  if (returnera_figur == TRUE) return(gg_list)
}
