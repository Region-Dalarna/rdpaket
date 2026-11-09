#' Kör en diagramfunktion för varje region och lägg till i en ppt-lista
#'
#' Kör en diagramfunktion en gång per region i `region_vekt`, och lägger
#' till en rad per genererad bild i `ppt_lista` - redo att byggas till en
#' presentation med [skapa_ppt_fran_lista()].
#'
#' Ange antingen `funktion` (en riktig R-funktion, t.ex.
#' `rddiagramkatalog::diag_befolkningsprognos_profet_scb`) eller
#' `source_url` (en URL eller lokal sökväg till ett skript som `source()`:as
#' och vars funktion sedan anropas) - inte båda. `funktion` är att föredra
#' när diagrammet redan finns som en riktig paketfunktion (se
#' `rddiagramkatalog`); `source_url` finns kvar som alternativ för diagram
#' som ännu inte är migrerade dit, eller som bara finns som ett löst skript
#' (t.ex. på G:).
#'
#' @param ppt_lista Den ppt-lista som ska fyllas på, se [initiera_ppt_df()].
#' @param funktion En riktig R-funktion (eller en sträng med dess namn,
#'   inklusive paketnamn om det behövs, t.ex.
#'   `"rddiagramkatalog::diag_befolkningsprognos_profet_scb"`) att anropa
#'   direkt. Föredras framför `source_url`. Ange bara en av de två.
#' @param source_url En URL eller lokal sökväg till ett skript som
#'   `source()`:as, vars funktion sedan anropas - alternativet till
#'   `funktion`. Ange bara en av de två.
#' @param parameter_argument En namngiven lista med parametervärden till
#'   funktionen (t.ex. `list(output_mapp = "c:/utskrift/")`). Bara de
#'   parametrar som faktiskt finns i funktionens formals skickas med -
#'   övriga ignoreras tyst.
#' @param region_vekt De kommun-/regionkoder funktionen ska köras för, en
#'   bild per region.
#' @param utmapp_bilder Mappen där bilderna sparas (måste matcha vad
#'   funktionen själv sparar till).
#' @param vald_pptmall Vilken mall-layout (se [skapa_ppt_fran_lista()])
#'   som ska användas för de tillagda raderna.
#' @param funktion_kor Bara relevant med `source_url`, om källfilen
#'   innehåller fler än en funktion: vilken som ska köras. `NA`
#'   (standard) = den första.
#' @param regionvariabel Namnet på funktionens parameter för region/kommun,
#'   om det inte är `"region_vekt"`.
#' @param source_encoding Teckenkodning för `source_url`, om den inte är
#'   `"utf-8"`.
#' @param felmeddelande_loggfil_mapp Mapp att skriva en loggfil med
#'   eventuella fel till, eller `NULL` för ingen loggfil.
#' @param felmeddelande_filnamn Filnamn på loggfilen (se
#'   `felmeddelande_loggfil_mapp`), eller `NULL` för ingen loggfil.
#'
#' @return `ppt_lista` med nya rader tillagda (eller oförändrad, vid fel -
#'   se `felmeddelande_loggfil_mapp`).
#' @export
ppt_lista_fyll_pa <- function(
    ppt_lista,
    funktion = NULL,
    source_url = NULL,
    parameter_argument,
    region_vekt,
    utmapp_bilder,
    vald_pptmall = "Bild utan text och rubrik",
    funktion_kor = NA,
    regionvariabel = "region_vekt",
    source_encoding = "utf-8",
    felmeddelande_loggfil_mapp = NULL,
    felmeddelande_filnamn = NULL
) {

  if (is.null(funktion) && is.null(source_url)) {
    stop("Ange antingen funktion eller source_url till ppt_lista_fyll_pa().", call. = FALSE)
  }
  if (!is.null(funktion) && !is.null(source_url)) {
    stop("Ange bara en av funktion eller source_url till ppt_lista_fyll_pa(), inte b\u00e5da.", call. = FALSE)
  }

  felmeddelande_skriv_till_loggfil <- !is.null(felmeddelande_loggfil_mapp) && !is.null(felmeddelande_filnamn)

  rapportera_fel <- function(meddelande) {
    cat(meddelande, "\n")

    if (isTRUE(felmeddelande_skriv_till_loggfil)) {
      tid_txt <- format(lubridate::now(), "_%d_%h_%Y")
      loggfil <- paste0(felmeddelande_loggfil_mapp, felmeddelande_filnamn) |>
        stringr::str_remove("\\.txt$") |>
        paste0(tid_txt, ".txt")

      write(
        paste(Sys.time(), "-", meddelande),
        file = loggfil,
        append = TRUE
      )
    }
  }

  retur <- tryCatch({

    if (!is.null(funktion)) {
      # Direktanrop - ingen source() behövs. funktion kan vara en riktig funktion eller en sträng
      # (eval(parse()) hanterar både ett bart funktionsnamn och "paket::funktion"-strängar enhetligt).
      funk_obj <- if (is.function(funktion)) funktion else eval(parse(text = funktion))
      funktion_kor_namn <- if (is.function(funktion)) {
        namn_forsok <- tryCatch(deparse(substitute(funktion)), error = function(e) NA_character_)
        if (is.na(namn_forsok) || !grepl("^[a-zA-Z0-9_.:]+$", namn_forsok)) "diagram" else namn_forsok
      } else funktion
    } else {
      source(source_url, encoding = source_encoding, echo = FALSE)
      funktion_i_fil <- rdverktyg::hitta_funktioner_i_fil_ej_inuti_andra_funktioner(source_url)
      stopifnot(length(funktion_i_fil) >= 1)

      if (is.na(funktion_kor)) {
        funktion_kor_namn <- funktion_i_fil[1]
      } else {
        funktion_skickad <- funktion_kor
        funktion_kor_namn <- funktion_i_fil[funktion_i_fil %in% funktion_kor]
        if (length(funktion_kor_namn) == 0) stop(glue::glue("Funktionen '{funktion_skickad}' finns inte i source-filen"))
      }
      funk_obj <- match.fun(funktion_kor_namn)
    }

    nya_rader <- purrr::map(region_vekt, ~ {

      parametrar_funktion <- utils::modifyList(parameter_argument, stats::setNames(list(.x), regionvariabel))
      parametrar_filter <- parametrar_funktion[names(parametrar_funktion) %in% names(formals(funk_obj))]

      funktion_retur <- rdverktyg::skriptrader_upprepa_om_fel({
        do.call(funk_obj, parametrar_filter)
      },
      exportera_till_globalenv = FALSE,
      vila_sek = 3,
      upprepa_vid_felmeddelande_som_innehaller =
        c("recv failure", "connection was reset", "curl_fetch_memory", "timeout", "cannot open the connection")
      )

      if (is.null(funktion_retur)) return(NULL)
      # vissa diagramfunktioner returnerar inte en lista utan ett bart ggplot-objekt - gör om till
      # en lista av det i så fall så att returobjektet alltid har samma struktur.
      if (ggplot2::is.ggplot(funktion_retur)) funktion_retur <- list(funktion_retur)

      if (is.null(names(funktion_retur))) names(funktion_retur) <- paste0(funktion_kor_namn, "_", .x)

      dplyr::tibble(
        dia_path = glue::glue("{utmapp_bilder}{names(funktion_retur)}.png"),
        mall = vald_pptmall
      )
    }, .progress = TRUE) |> purrr::list_rbind()

    if (nrow(nya_rader) > 0) {
      retur_lista <- dplyr::bind_rows(ppt_lista, nya_rader)
      cat(funktion_kor_namn, "klar.\n")
    } else {
      retur_lista <- ppt_lista
    }

    retur_lista

  }, error = function(e) {

    # Bugfix (bekräftad genom test): deparse(funktion) på ett riktigt funktionsobjekt (inte en
    # symbol/sträng) ger en flerradig teckenvektor (en rad per kodrad) - glue::glue() kraschar då med
    # "All unnamed arguments must be length 1" i stället för att bara logga felet, vilket i praktiken
    # dolde det ursprungliga felet bakom ett nytt, missvisande. Identifierare för loggraden byggs nu
    # av en enda, garanterat kort sträng.
    funktion_identifierare <- if (!is.null(funktion)) {
      if (is.function(funktion)) "en angiven funktion" else funktion
    } else source_url

    rapportera_fel(
      glue::glue("Fel i ppt_lista_fyll_pa(): {conditionMessage(e)} (funktion: {funktion_identifierare})")
    )

    ppt_lista
  })

  return(retur)
}
