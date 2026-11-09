#' Hämta befolkningsprognosdata, från SCB eller lokala prognosfiler
#'
#' Enhetlig hämtare för befolkningsprognosdata - avgör automatiskt om
#' `url_prognos_vektor` pekar mot SCB:s öppna API (en https-url) eller mot en
#' lokal mapp med prognosfiler, och normaliserar resultatet till samma
#' kolumnstruktur oavsett källa. Lokala filer kan vara antingen CSV-filer
#' från organisationens egen Profet-körning eller Excel-filer från Hallands
#' befolkningsprognosskript - båda läses in och döps om till samma
#' kolumnnamn som SCB-grenen använder.
#'
#' @param region_vekt Region(er) att hämta, som kommun- eller länskoder.
#' @param alder_list Åldrar att inkludera. `"*"` (standard) ger alla åldrar.
#' @param kon_klartext `c("kvinnor", "män")` (standard), eller ett av könen.
#' @param cont_klartext Innehållsvariabel. Finns: `"Folkmängd"` (standard),
#'   `"Födda"`, `"Döda"`, `"Inrikes inflyttning"`, `"Inrikes utflyttning"`,
#'   `"Invandring"`, `"Utvandring"`. `"*"` ger samtliga (bara SCB-grenen).
#' @param tid_vekt År att hämta. `"*"` (standard) ger alla år. Kan anges
#'   relativt prognosåret med `"+"`/`"-"`, t.ex. `"+0"` för prognosåret
#'   självt eller `paste0("+", 0:10)` för prognosåret och tio år framåt.
#' @param url_prognos_vektor Antingen en eller flera https-url:er till SCB:s
#'   API, eller sökvägen till en mapp med lokala prognosfiler (Profet-CSV
#'   och/eller Hallands-Excel). Standard pekar mot Profet-mappen på G:.
#' @param prognos_ar Vilket/vilka prognosår som ska användas bland de lokala
#'   filerna i mappen. `"9999"` (standard) = senaste tillgängliga. Gäller
#'   inte SCB-grenen, där url:en i sig styr prognosåret.
#' @param long_format Om `TRUE` returneras datan i long-format (en rad per
#'   variabel), annars (standard) i wide-format.
#'
#' @return En `tibble`/`data.frame` med kolumnerna `regionkod`, `region`,
#'   `kön`, `ålder`, `år`, `prognos_ar` samt de begärda innehållsvariablerna.
#' @export
hamta_befprognos_data <- function(
    region_vekt = "20",
    alder_list = "*",
    kon_klartext = c("kvinnor", "män"),
    cont_klartext = "Folkmängd",
    tid_vekt = "*",
    url_prognos_vektor = "G:/Samhällsanalys/Statistik/Befolkningsprognoser/Profet/datafiler/",
    prognos_ar = "9999",
    long_format = FALSE
) {

  options(scipen = 999)

  # v2-motsvarigheten till SCB:s v1-tabell BE0401A/BefProgOsiktRegN (befolkningsprognos per
  # region/kön/ålder/innehåll) - den enda SCB-url som i praktiken används.
  url_scbtabell <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefProgOsiktRegN"
  scb_tabell_id <- "TAB698"

  before_n <- length(url_prognos_vektor)

  # byt ut endast om det inte är https:// OCH inte en befintlig mapp
  to_replace <- !stringr::str_detect(url_prognos_vektor, "^https://") & !dir.exists(url_prognos_vektor)

  url_prognos_vektor[to_replace] <- url_scbtabell
  url_prognos_vektor <- unique(url_prognos_vektor)

  if (any(to_replace)) cat("Ersatte", sum(to_replace), "värde(n) med SCB-url.\n")
  if (length(url_prognos_vektor) < before_n) cat("Tog bort", before_n - length(url_prognos_vektor), "dubblett(er).\n")

  # Lokala Profet-/Halland-mappar är manuellt underhållna filer, till skillnad från SCB-grenen
  # (live-API) - samma typ av konsolvarning som alla andra diag_*()-funktioner i rddiagramkatalog ger
  # när de läser en manuell fil, så att ingen missar att datan är en ögonblicksbild.
  lokala_mappar <- url_prognos_vektor[!stringr::str_detect(url_prognos_vektor, "^https://")]
  for (mapp in lokala_mappar) {
    message(
      "Data hämtas från en manuellt underhållen mapp:\n  ", mapp,
      "\nDetta är INTE en live-hämtning mot ett API - innehållet är en ",
      "ögonblicksbild som kan vara inaktuell och måste uppdateras manuellt ",
      "vid behov."
    )
  }

  hamta_url_rad_i_vektor <- function(hamta_url) {

    # ================================== beräkna rätt tid_vekt ==============================================

    if (any(tid_vekt != "*")) {               # om inte "*" är valt
      jmfr_vekt <- as.numeric(tid_vekt[stringr::str_detect(tid_vekt, "\\+|\\-")])              # alla år som ska beräknas utifrån prognosår
      andra_ar_vekt <- as.numeric(tid_vekt[!stringr::str_detect(tid_vekt, "\\+|\\-")])         # övriga år = år som skickas med som de är, tex. "2015"

      if (stringr::str_detect(hamta_url, "https://api.scb.se")) {

        valt_ar <- min(as.numeric(pxweb2r::pxweb2_get_values(scb_tabell_id, "Tid", quiet = TRUE)$code))
        if ("9999" %in% prognos_ar) prognos_ar <- as.numeric(stringr::str_replace(prognos_ar[prognos_ar == "9999"], "9999", as.character(valt_ar)))

      } else {

        filsokvagar <- list.files(hamta_url, pattern = ".csv|.xlsx", full.names = TRUE)
        filsokvagar_csv <- filsokvagar[stringr::str_detect(filsokvagar, ".csv")]
        filsokvagar_xlsx <- filsokvagar[stringr::str_detect(filsokvagar, ".xlsx")]

        # behåll de som finns i region_vekt i filskokvagar_csv (dvs. från Profet)
        if (!"20" %in% region_vekt) filsokvagar_csv <- filsokvagar_csv[!stringr::str_detect(filsokvagar_csv, "lan")]                        # ta bort länsfiler ur sokvagsvektorn om inte "lan" är med i sökvägen
        if (!any(rdverktyg::hamtakommuner(lan = "20", FALSE, FALSE, FALSE) %in% region_vekt)) filsokvagar_csv <- filsokvagar_csv[!stringr::str_detect(filsokvagar_csv, "kommun")]      # ta bort kommunfiler ur sokvagsvektorn om ingen av Dalarnas kommuners kommunkoder är med

        # och så sätter vi ihop csv- och xlsx-sökvägar igen
        filsokvagar <- c(filsokvagar_csv, filsokvagar_xlsx)

        # kontrollera vilka prognosår som finns bland profet-filerna i mappen
        existerande_ar <- unique(purrr::map_int(filsokvagar, ~ readr::parse_number(.)))
        if ("9999" %in% prognos_ar) prognos_ar <- as.numeric(stringr::str_replace(prognos_ar, "9999", as.character(max(existerande_ar))))
        prognos_ar <- prognos_ar[prognos_ar %in% existerande_ar]
      }

      start_ar <- prognos_ar
      jmfr_ar <- start_ar + jmfr_vekt
      hamta_tid_vekt <- if (length(jmfr_ar) > 0) c(jmfr_ar, andra_ar_vekt) else andra_ar_vekt

    } else hamta_tid_vekt <- tid_vekt                   # om tid_Vekt == "*" så blir hamta_tid_vekt det också

    hamta_data_fran_tabell <- function(url_prognos) {

      if (stringr::str_detect(url_prognos, "https://api.scb.se")) {

        valt_ar <- min(as.numeric(pxweb2r::pxweb2_get_values(scb_tabell_id, "Tid", quiet = TRUE)$code))

        if (all(cont_klartext == "*")) cont_klartext <- pxweb2r::pxweb2_get_values(scb_tabell_id, "ContentsCode", quiet = TRUE)$label

        # Ålder = "0" om bara Födda efterfrågas (Födda finns bara registrerat på nyfödda, dvs. ålder 0),
        # annars den medskickade åldersvektorn rakt av, eller (om alder_list = "*") utelämnad helt ur
        # query_list nedan - att utelämna en variabel i pxweb2r ger alla individuella värden.
        alder_vekt <- if (all(cont_klartext == "Födda")) "0" else if (!identical(alder_list, "*")) unlist(alder_list) else NULL

        # Kön/innehåll skickas som klartext rakt in i frågan - pxweb2r slår själv upp rätt kod.
        query_list <- purrr::compact(list(Region = region_vekt,
                           Alder = alder_vekt,
                           Kon = kon_klartext,
                           ContentsCode = cont_klartext,
                           Tid = as.character(hamta_tid_vekt)))

        # quiet = TRUE tystar både pxweb2r:s "include_aggregations = auto"-info och de "ogiltiga värden
        # borttagna"-notiser som on_all_values_invalid = "null" kan ge upphov till.
        px_df <- pxweb2r::pxweb2_get_data(table = scb_tabell_id, query = query_list, on_all_values_invalid = "null", quiet = TRUE) |>
          dplyr::rename(regionkod = region_kod) |>
          dplyr::mutate(prognos_ar = as.character(valt_ar))

        if (long_format) {
          px_df <- dplyr::rename(px_df, variabel = tabellinnehåll, varde = value)
        } else {
          px_df <- tidyr::pivot_wider(px_df, names_from = tabellinnehåll, values_from = value)
        }
        retur_df <- px_df

      } else {                    # om vi inte har en url som innehåller en adress till SCB:s API så utgår vi från att det är en profet-fil

        filsokvagar <- list.files(hamta_url, pattern = ".csv|.xlsx", full.names = TRUE)

        alla_prognos_ar <- unique(purrr::map_chr(filsokvagar, ~ as.character(readr::parse_number(.))))
        sok_prognos_ar <- paste0(prognos_ar, collapse = "|")               # för att kunna använda nedan i str_detect
        # ta ut rätt år utifrån användarens val
        if (all(prognos_ar == "9999")) filsokvagar <- filsokvagar[stringr::str_detect(filsokvagar, max(alla_prognos_ar))]
        if (all(!is.na(prognos_ar) & prognos_ar != "9999")) filsokvagar <- filsokvagar[stringr::str_detect(filsokvagar, sok_prognos_ar)]
        if (any(stringr::str_detect(prognos_ar, "9999"))) prognos_ar <- stringr::str_replace(alla_prognos_ar, "9999", max(alla_prognos_ar))

        retur_profet <- NULL
        befskript_df <- NULL
        filsokvagar_xlsx <- filsokvagar[stringr::str_detect(filsokvagar, ".xlsx")]
        filsokvagar_csv <- filsokvagar[stringr::str_detect(filsokvagar, ".csv")]
        # här läser vi in filer som är skapade med Hallands skript
        if (length(filsokvagar_xlsx) > 0) {

          # kontrollera vilka prognosår som finns bland profet-filerna i mappen som ska användas
          progn_ar <- purrr::map_chr(filsokvagar_xlsx, ~ as.character(readr::parse_number(.)))

          # vektor för att döpa om kolumner i profetfilen så att de blir samma som i pxwebs befolkningsprognostabeller
          rename_befskript <- c("regionkod" = "lan_kod", "regionkod" = "kommun_kod", "ålder" = "alder", "kön" = "kon", "år" = "ar",
                                "Folkmängd" = "total_folkmangd", "Födda" = "fodda", "Döda" = "doda", "Inrikes inflyttning" = "inrikes_inflyttning",
                                "Inrikes utflyttning" = "inrikes_utflyttning", "Invandring" = "invandring", "Utvandring" = "utvandring")

          contvar_vekt <- c("Folkmängd", "Födda", "Döda", "Inrikes inflyttning", "Inrikes utflyttning", "Invandring", "Utvandring")
          if (all(cont_klartext == "*")) cont_klartext <- contvar_vekt

          # Felhantering: de lokala Profet-filerna (Hallands skript-varianten) har bara de sju
          # innehållsvariablerna i contvar_vekt ovan - saknar t.ex. helt uppdelning på inrikes/utrikes
          # födda. Stoppa här med ett begripligt felmeddelande om något annat efterfrågas, i stället för
          # att fortsätta med tom/felaktig data.
          saknade_contvar <- setdiff(cont_klartext, contvar_vekt)
          if (length(saknade_contvar) > 0) {
            stop("hamta_befprognos_data(): cont_klartext innehåller variabler som ännu inte finns i de ",
                 "lokala Profet-filerna (", hamta_url, "): ", paste(saknade_contvar, collapse = ", "),
                 ". Tillgängliga variabler i filerna är: ", paste(contvar_vekt, collapse = ", "),
                 ". Hämta i stället direkt från SCB:s API (skicka en https://-url i url_prognos_vektor) ",
                 "om du behöver denna variabel.")
          }

          befskript_df <- purrr::list_rbind(purrr::map2(filsokvagar_xlsx, progn_ar, ~ dplyr::mutate(readxl::read_xlsx(.x),
                                 prognos_ar = as.character(.y),
                                 ar = as.character(ar))))

          # Varje rad får sina jämförelseår utifrån sin egen prognos_ar (filerna kan ha olika prognosår).
          # "+N" betyder prognos_ar + N i alla grenar (SCB, Profet-csv och Hallands xlsx), se tid_vekt överst.
          if (all(hamta_tid_vekt != "*")) {
            befskript_df <- befskript_df |>
              dplyr::rowwise() |>
              dplyr::filter({
                fil_start_ar <- as.numeric(prognos_ar)
                fil_jmfr_ar <- fil_start_ar + jmfr_vekt
                fil_hamta_tid_vekt <- if (length(fil_jmfr_ar) > 0) c(fil_jmfr_ar, andra_ar_vekt) else andra_ar_vekt
                as.numeric(ar) %in% fil_hamta_tid_vekt
              }) |>
              dplyr::ungroup()
          }            # ta bara ut jämförelseåret

          if (all(cont_klartext != "*")) tabort_contvar <- contvar_vekt[!contvar_vekt %in% cont_klartext]

          befskript_df <- dplyr::rename(befskript_df, dplyr::any_of(rename_befskript))

          befskript_df <- dplyr::mutate(befskript_df,
                           kön = dplyr::case_when(kön == 1 ~ "män",
                                          kön == 2 ~ "kvinnor",
                                          TRUE ~ as.character(kön)),
                           ålder = ifelse(ålder > 99, "100+ år", paste0(ålder, " år")),
                           år = as.character(år),
                           # regionkod kan vara numerisk i xlsx-filen ("20", 180) - gör om till SCB:s textkoder ("20", "0180")
                           regionkod = as.character(regionkod),
                           regionkod = stringr::str_pad(regionkod, width = ifelse(nchar(regionkod) <= 2, 2, 4), pad = "0"))
          # lägg till regionnamn, som i csv-grenen (diagramskripten grupperar på region) - men bara om
          # filen inte redan har en egen region-kolumn, annars krockar namnen i left_join() (blir
          # region.x/region.y i stället för region) och relocate() nedan kraschar.
          if (!"region" %in% names(befskript_df)) {
            befskript_df <- dplyr::left_join(befskript_df, rdverktyg::hamtaregtab(), by = "regionkod")
          }
          befskript_df <- dplyr::relocate(befskript_df, region, .after = regionkod)
          data.table::setDT(befskript_df)
          befskript_df <- befskript_df[, lapply(.SD, sum, na.rm = TRUE), by = setdiff(names(befskript_df), names(befskript_df)[sapply(befskript_df, is.numeric)])]

          # ta bort variabler som användaren inte valt
          befskript_df <- dplyr::select(befskript_df, -dplyr::all_of(tabort_contvar))

          if (all(cont_klartext == "Födda")) befskript_df <- dplyr::select(dplyr::filter(befskript_df, ålder == "0 år"), -ålder)

          if (long_format) {
            befskript_df <- tidyr::pivot_longer(befskript_df, cols = dplyr::any_of(contvar_vekt), names_to = "variabel", values_to = "varde")
          }

        } # slut test om det finns .xlsx-filer (= filer från Hallands skript)

        if (length(filsokvagar_csv) > 0) {
          # hämta regionnyckel
          regionnyckel <- rdverktyg::hamtaregtab()

          # behåll de som finns i region_vekt
          if (!"20" %in% region_vekt) filsokvagar_csv <- filsokvagar_csv[!stringr::str_detect(filsokvagar_csv, "lan")]                        # ta bort länsfiler ur sokvagsvektorn om inte "20" är med som regionkod
          if (!any(rdverktyg::hamtakommuner(lan = "20", FALSE, FALSE, FALSE) %in% region_vekt)) filsokvagar_csv <- filsokvagar_csv[!stringr::str_detect(filsokvagar_csv, "kommun")]      # ta bort kommunfiler ur sokvagsvektorn om ingen av Dalarnas kommuners kommunkoder är med

          # kontrollera vilka prognosår som finns bland profet-filerna i mappen som ska användas
          progn_ar <- purrr::map_chr(filsokvagar_csv, ~ as.character(readr::parse_number(.)))

          # begränsa till det/de prognosår som prognos_ar-parametern faktiskt efterfrågar
          prognos_ar_vekt <- if ("9999" %in% prognos_ar) {
            max(as.numeric(progn_ar))                       # "9999" = senaste tillgängliga prognosår
          } else if (all(is.na(prognos_ar))) {
            as.numeric(progn_ar)                             # NA = alla prognosår
          } else {
            as.numeric(prognos_ar)                            # specifikt/specifika år
          }

          behall_fil <- progn_ar %in% as.character(prognos_ar_vekt)
          if (!any(behall_fil)) {
            stop(paste0("Inget prognosår i mappen matchar efterfrågat prognos_ar (",
                        paste(prognos_ar, collapse = ", "),
                        "). Tillgängliga prognosår i mappen: ", paste(unique(progn_ar), collapse = ", ")))
          }
          filsokvagar_csv <- filsokvagar_csv[behall_fil]
          progn_ar <- progn_ar[behall_fil]

          # vektor för att döpa om kolumner i profetfilen så att de blir samma som i pxwebs befolkningsprognostabeller
          rename_profet <- c("regionkod" = "lan_kod", "regionkod" = "kommun_kod", "regionkod", "region", "ålder" = "age", "kön" = "kon", "år" = "year",
                             "Folkmängd" = "pop", "Födda" = "fodda", "Döda" = "doda", "Inrikes inflyttning" = "inrikes_inflyttade",
                             "Inrikes utflyttning" = "inrikes_utflyttade", "Invandring" = "immigranter", "Utvandring" = "emigranter")

          contvar_vekt <- c("Folkmängd", "Födda", "Döda", "Inrikes inflyttning", "Inrikes utflyttning", "Invandring", "Utvandring")
          if (all(cont_klartext == "*")) cont_klartext <- contvar_vekt

          saknade_contvar <- setdiff(cont_klartext, contvar_vekt)
          if (length(saknade_contvar) > 0) {
            stop("hamta_befprognos_data(): cont_klartext innehåller variabler som ännu inte finns i de ",
                 "lokala Profet-filerna (", hamta_url, "): ", paste(saknade_contvar, collapse = ", "),
                 ". Tillgängliga variabler i filerna är: ", paste(contvar_vekt, collapse = ", "),
                 ". Hämta i stället direkt från SCB:s API (skicka en https://-url i url_prognos_vektor) ",
                 "om du behöver denna variabel.")
          }

          las_in_profet_fil <- function(profetfil_sokvag, fil_prognosar) {

            if (any(hamta_tid_vekt != "*")) {
              # "+N" betyder prognos_ar + N i alla grenar (SCB, Profet-csv och Hallands xlsx), se tid_vekt överst.
              fil_start_ar <- as.numeric(fil_prognosar)
              fil_jmfr_ar <- fil_start_ar + jmfr_vekt
              fil_hamta_tid_vekt <- if (length(fil_jmfr_ar) > 0) c(fil_jmfr_ar, andra_ar_vekt) else andra_ar_vekt
            } else fil_hamta_tid_vekt <- "*"

            profet_df <- readr::read_csv(profetfil_sokvag, show_col_types = FALSE)
            kolnamn <- names(profet_df)                                                        # hämta kolumnnamn för att kunna avgöra om det är kommuner eller län

            # kolla om det är kommunfil, i så fall lägger vi ihop in- och utflyttning inom län och från utanför län
            if (all(c("utomin", "inomin", "utomut", "inomut") %in% kolnamn)) {
              profet_df <- dplyr::mutate(profet_df,
                       inrikes_inflyttade = inomin + utomin,
                       inrikes_utflyttade = inomut + utomut)
              profet_df <- dplyr::select(profet_df, -c(inomin, utomin, inomut, utomut))
            }

            if (all(fil_hamta_tid_vekt == "*")) fil_hamta_tid_vekt <- (as.numeric(fil_prognosar)):2100

            profet_df <- dplyr::rename(profet_df, dplyr::any_of(rename_profet))                                                    # döp om kolumner så de heter samma som i pxweb-befolkningsprognoserna

            regionkod_len <- if (any(stringr::str_detect(kolnamn, "lan_kod"))) 2 else 4                 # kolla om det är län eller kommuner, väljer längd på regionkoden utifrån vad vi har i datasetet

            profet_df <- dplyr::mutate(profet_df,
                     regionkod = stringr::str_sub(regionkod, 1, regionkod_len),
                     kön = ifelse(kön == 1, "män", "kvinnor"),
                     ålder = ifelse(ålder == 100, paste0(ålder, "+ år"), paste0(ålder, " år")),
                     år = as.character(år),
                     prognos_ar = fil_prognosar)
            profet_df <- dplyr::filter(profet_df, år %in% as.character(fil_hamta_tid_vekt))
            profet_df <- dplyr::relocate(dplyr::left_join(profet_df, regionnyckel, by = "regionkod"), region, .after = regionkod)

            if (all(cont_klartext != "*")) tabort_contvar <- contvar_vekt[!contvar_vekt %in% cont_klartext]                      # ta bort de innehållsvariabler som användaren inte valt ur vektorn, använd den för att ta bort variabler

            # ta bort variabler som användaren inte valt
            profet_df <- dplyr::select(profet_df, -dplyr::all_of(tabort_contvar))

            if (all(cont_klartext == "Födda")) profet_df <- dplyr::select(dplyr::filter(profet_df, ålder == "0 år"), -ålder)

            if (long_format) {
              profet_df <- tidyr::pivot_longer(profet_df, cols = dplyr::any_of(contvar_vekt), names_to = "variabel", values_to = "varde")
            }
            return(profet_df)
          } # slut funktion för att läsa in profetfiler

          retur_profet <- purrr::list_rbind(purrr::map2(filsokvagar_csv, progn_ar, ~ las_in_profet_fil(.x, .y)))
        } # slut test om det finns csv-filer att läsa in (= filer från Profet)

        retur_filer <- dplyr::bind_rows(retur_profet, befskript_df)

        return(retur_filer)

    } # if-sats, else-delen som är om det är en sökväg till en profet-fil
    } # funktion att hämta data från url:er (scb-api:er eller profet-filer)

    # hamta_url är redan en enskild url/sökväg här - ingen map_dfr behövs för att hämta den.
    retur_df <- dplyr::filter(hamta_data_fran_tabell(url_prognos = hamta_url), regionkod %in% region_vekt)

    return(retur_df)

  } # slut funktion för varje url-rad i url-vektorn

  purrr::list_rbind(purrr::map(url_prognos_vektor, ~ hamta_url_rad_i_vektor(hamta_url = .x)))
}

#' Beräkna förändring i befolkningsprognos mellan start- och slutår
#'
#' Hämtar befolkningsprognos via [hamta_befprognos_data()] samt senaste
#' faktiska befolkningssiffror (via SCB:s API), och räknar ut skillnaden i
#' antal och procent per åldersgrupp (0-19, 20-64, 65-79, 80+ år) mellan
#' utgångsåret och ett valfritt antal år framåt.
#'
#' @param region_vekt Region(er) att hämta, som kommun- eller länskoder.
#' @param eget_regionnamn Namn att använda i diagramrubriken hos anropande
#'   funktion. `NA` (standard) = inget eget namn satt.
#' @param jmfrtid Antal år i jämförelsen, räknat från sista året med
#'   faktisk befolkningsstatistik (= året före första prognosår).
#' @param tabeller_url Antingen en eller flera https-url:er till SCB:s API,
#'   eller sökvägen till en mapp med lokala prognosfiler. Se
#'   [hamta_befprognos_data()]. Standard pekar mot Profet-mappen på G:.
#' @param output_fold Mapp att eventuellt skriva diagram till hos anropande
#'   funktion - används inte av den här funktionen själv.
#' @param gruppera_namn Namn att gruppera ihop de medskickade regionerna
#'   till en gemensam rad under, eller `NA` (standard) för ingen gruppering.
#' @param konsuppdelat Om `TRUE` behålls uppdelning på kön. Stöds ännu inte
#'   fullt ut - lämna som `FALSE` (standard).
#' @param anvand_senaste_befar Om `TRUE` används senaste tillgängliga år för
#'   befolkningsstatistik som startår, annars (standard) används året före
#'   första prognosår.
#' @param prognos_ar Prognosår att använda bland lokala Profet-filer.
#'   `"9999"` (standard) = senaste. Gäller inte SCB-grenen.
#'
#' @return En `tibble`/`data.frame` med en rad per region/åldersgrupp
#'   (och ev. kön), med kolumnerna `antal` och `andel` för förändringen
#'   mellan start- och slutår.
#' @export
hamta_befprognos_diff_data <- function(region_vekt = "20",
                                   eget_regionnamn = NA,
                                   jmfrtid = 10,
                                   tabeller_url = "G:/Samhällsanalys/Statistik/Befolkningsprognoser/Profet/datafiler/",
                                   output_fold = "G:/Samhällsanalys/API/Fran_R/Utskrift/",
                                   gruppera_namn = NA,
                                   konsuppdelat = FALSE,
                                   anvand_senaste_befar = FALSE,
                                   prognos_ar = "9999"
                                   ) {

  options(dplyr.summarise.inform = FALSE)
  options(scipen = 999)

  # =============== jämförelse x år framåt från senaste tillgängliga år ==========================

  folkmangd_url <- c("TAB638", "TAB5557")

  senaste_ar_bef <- as.numeric(max(purrr::map(folkmangd_url, ~ pxweb2r::pxweb2_get_values(.x, "tid", quiet = TRUE)$code) |> unlist()))
  # jmfrtid räknas från startåret (prognosår - 1, se startar/slutar nedan), medan "+N" i
  # hamta_befprognos_data() betyder prognosår + N. Därför "+(jmfrtid - 1)" för att landa på startår + jmfrtid.
  prognos_jmfr_ar <- if (anvand_senaste_befar) senaste_ar_bef + jmfrtid else paste0("+", jmfrtid - 1)

  # ========== Hämta befolkningsprognos för tabell(er) i vektor tabeller_url  ====================

  befprogn_df <- hamta_befprognos_data(region_vekt = region_vekt,
                                       url_prognos_vektor = tabeller_url,
                                       kon_klartext = c("kvinnor", "män"),
                                       tid_vekt = prognos_jmfr_ar,
                                       cont_klartext = "Folkmängd",
                                       prognos_ar = prognos_ar           # prognos_ar funkar bara för profet-uttag (för uttag från SCB:s API styr url:en vilket år som hämtas men i Profet kan flera år hämtas med samma url om det finns data för flera år i mappen)
                                       )

  # Här skapar vi en rad med total folkmängd ====================================================
  total_df <- befprogn_df |>
    dplyr::group_by(regionkod, region, kön, år, prognos_ar) |>
    dplyr::summarize(dplyr::across(dplyr::where(is.numeric), ~ sum(.x, na.rm = TRUE))) |>
    dplyr::ungroup() |>
    dplyr::mutate(ålder = "totalt ålder")

  # Lägg på total_df som rad
  befprogn_df <- dplyr::bind_rows(befprogn_df, total_df)

  prognosar <- befprogn_df$prognos_ar |> unique()                         # lägg prognosår(en) i en vektor
  startar <- as.character(as.numeric(prognosar) - 1)    # lägg startår(en) i en vektor
  slutar <- as.character(as.numeric(startar) + jmfrtid)   # lägg slutår(en) i en vektor

  # =========================== hämta senaste tillgängliga år i befolkingsstatistiken ============================

  # Folkmängden efter region, civilstånd, ålder och kön. År 1968-  (äldre tidsserie + CKM)
  px_df_bef <- pxweb2r::pxweb2_get_data(
    table = c("TAB638", "TAB5557"),
    query = list(
      Region = region_vekt,
      Civilstand = c("OG", "G", "SK", "ÄNKL"),
      Alder = c(0:99, "100+1", "TOT1"),
      Kon = c("män", "kvinnor"),
      ContentsCode = "Folkmängd",
      Tid = startar
    ), quiet = TRUE, on_all_values_invalid = "null") |>
    dplyr::group_by(regionkod = region_kod, region, ålder, kön, år) |>
    dplyr::summarise(Folkmängd = sum(value, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(prognos_ar = as.character(as.numeric(år) + 1))

  # ================= lägg ihop och bearbeta prognos- och befolkningsdata ==================

  # skapa en df med både prognosvärden och befolkningssiffror för senaste året innan prognosen startar
  progn_bef <- dplyr::bind_rows(px_df_bef, befprogn_df)

  suppressWarnings(progn_bef <- progn_bef |>              # skippa felmeddelanden då det inte är någon fara (kan inte översätta NA-värden till numeric)
                     dplyr::mutate(aldernum = readr::parse_number(ålder)))                # Skapa en numerisk åldersvariabel

  # Lägg ihop i åldersgrupper
  progn_bef <- progn_bef |>
    dplyr::mutate(aldergrp = dplyr::case_when(aldernum < 20 ~ "0-19 år",
                                dplyr::between(aldernum, 20, 64) ~ "20-64 år",
                                dplyr::between(aldernum, 65, 79) ~ "65-79 år",
                                aldernum > 79 ~ "80+ år",
                                TRUE ~ "totalt"),
                  start_ar = as.character(as.numeric(prognos_ar) - 1),
                  slut_ar = as.character(as.numeric(start_ar) + jmfrtid),
           ar_beskr = paste0("Förändring ", start_ar, "-", slut_ar, " (prognos våren ",
                              prognos_ar, ")"),
           Folkmängd = round(Folkmängd))

  # om vi inte vill ha könsuppdelad data (stöds ännu inte fullt ut) så tar vi bort det här
  if (!konsuppdelat) {
    progn_bef <- progn_bef |>
      dplyr::group_by(dplyr::across(-c(kön, Folkmängd))) |>
      dplyr::summarise(Folkmängd = sum(Folkmängd, na.rm = TRUE)) |>
      dplyr::ungroup()
  }

  # om vi vill gruppera ihop regionerna till en så gör vi det här
  if (!is.na(gruppera_namn)) {
    progn_bef <- progn_bef |>
    dplyr::group_by(dplyr::across(c(-region, -regionkod, -Folkmängd))) |>
    dplyr::summarise(Folkmängd = sum(Folkmängd)) |>
    dplyr::mutate(regionkod = "grp", region = gruppera_namn) |>
    dplyr::ungroup()

    eget_regionnamn <- gruppera_namn
  }

  # beräkna skillnad mellan startår och slutår, både som antal och som andel
  prognos_diff_df <- progn_bef |>
    dplyr::group_by(dplyr::across(-c(ålder, aldernum, Folkmängd))) |>
    dplyr::summarise(Folkmängd = sum(Folkmängd, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(names_from = år, values_from = Folkmängd)

  # Kontrollera att de förväntade start-/slutårskolumnerna faktiskt finns i datan
  # (pxweb2-uttaget returnerar inte alltid exakt lika många år som "+jmfrtid" tidigare gav).
  tillgangliga_ar <- names(prognos_diff_df) |> purrr::keep(~ stringr::str_detect(.x, "^[0-9]{4}$"))
  saknas_slut <- setdiff(slutar, tillgangliga_ar)
  saknas_start <- setdiff(startar, tillgangliga_ar)

  if (length(saknas_slut) > 0 || length(saknas_start) > 0) {
    senaste <- max(as.numeric(tillgangliga_ar))
    aldst <- min(as.numeric(tillgangliga_ar))
    warning(paste0(
      "Förväntade årskolumner saknas i hämtad prognosdata: ",
      paste(c(saknas_start, saknas_slut), collapse = ", "),
      ". Tillgängliga år är ", aldst, "-", senaste,
      ". Kontrollera tid_vekt/jmfrtid-hanteringen i hamta_befprognos_data()."
    ))
    slutar <- as.character(senaste)
    if (length(saknas_start) > 0) startar <- as.character(aldst)
  }

  prognos_diff_df <- dplyr::mutate(prognos_diff_df,
                                   antal = rowSums(dplyr::select(prognos_diff_df, dplyr::all_of(slutar)), na.rm = TRUE) -
                                     rowSums(dplyr::select(prognos_diff_df, dplyr::all_of(startar)), na.rm = TRUE),
                                   andel = round((rowSums(dplyr::select(prognos_diff_df, dplyr::all_of(slutar)), na.rm = TRUE) -
                                                    rowSums(dplyr::select(prognos_diff_df, dplyr::all_of(startar)), na.rm = TRUE)) /
                                                   rowSums(dplyr::select(prognos_diff_df, dplyr::all_of(startar)), na.rm = TRUE) * 100, 1),
                                   aldergrp = factor(aldergrp, levels = c("totalt", "0-19 år", "20-64 år", "65-79 år", "80+ år"))
  )

  return(prognos_diff_df)
}

#' Riket-nivå befolkningsprognos uppdelat på inrikes/utrikes födda
#'
#' Hämtar SCB:s befolkningsprognos för riket, uppdelat på ålder, kön och
#' födelseregion (inrikes/utrikes född). Till skillnad från
#' [hamta_befprognos_data()] finns ingen regionuppdelning i källtabellen -
#' den täcker bara riket. Motsvarar v1-tabellen BE0401A/BefolkprognRevNb,
#' vars v2-id verifierades (2026) via identisk variabelstruktur
#' (InrikesUtrikes/Alder/Kon/ContentsCode/Tid) och identiska etiketter
#' (`"inrikes födda"`/`"utrikes födda"`/`"inrikes och utrikes födda"`,
#' ContentsCode `"Antal"`): TAB5974, "Folkmängd efter inrikes/utrikes född,
#' ålder och kön. År 2026-2120".
#'
#' Tabellen saknar en egen totalkod för ålder - samtliga individuella åldrar
#' (0-104 år samt "105+ år") hämtas därför explicit, på samma sätt som på
#' flera andra ställen i paketet (se t.ex. `diag_utrikes_fodda_tidsserie_scb`
#' i rddiagramkatalog).
#'
#' Ett smalt, specialiserat hjälpanrop - används av rddiagramkatalogs
#' befolkningsprognosdiagram när prognosen jämförs mot riket.
#'
#' @param tid_koder Vilka år som ska hämtas. `"*"` (standard) ger alla
#'   tillgängliga år.
#' @param kon_klartext `c("män", "kvinnor")` (standard).
#'
#' @return En `tibble` med kolumnerna `regionkod` (alltid `"00"`), `region`
#'   (alltid `"Sverige"`), `kön`, `ålder`, `fodelseregion`, `år`, `Antal`.
#' @export
hamta_befprognos_riket_inr_utr_scb <- function(tid_koder = "*", kon_klartext = c("män", "kvinnor")) {

  alder_varden <- pxweb2r::pxweb2_get_values("TAB5974", "Alder", quiet = TRUE)
  alder_koder <- alder_varden$code[alder_varden$type == "Variable"]

  tid_vekt <- if (identical(tid_koder, "*")) {
    pxweb2r::pxweb2_get_values("TAB5974", "Tid", quiet = TRUE)$code
  } else {
    as.character(tid_koder)
  }

  pxweb2r::pxweb2_get_data(
    table = "TAB5974",
    query = list(
      InrikesUtrikes = c("inrikes födda", "utrikes födda"),
      Alder = alder_koder,
      Kon = kon_klartext,
      ContentsCode = "Antal",
      Tid = tid_vekt
    ),
    on_all_values_invalid = "null", quiet = TRUE
  ) |>
    dplyr::rename(Antal = value, fodelseregion = `inrikes/utrikes född`) |>
    dplyr::select(-tabellinnehåll) |>
    dplyr::mutate(regionkod = "00", region = "Sverige") |>
    dplyr::relocate(regionkod, region)
}
