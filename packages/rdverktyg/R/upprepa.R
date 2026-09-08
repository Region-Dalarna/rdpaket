# Funktioner för att upprepa försök vid fel eller tills ett villkor är uppfyllt.
# Utbrutna ur func_API.R.

#' Kör en funktion om och om igen tills den returnerar TRUE
#'
#' Praktiskt för att vänta in en server eller ett jobb som blir klart.
#'
#' @param funktion En funktion utan argument som returnerar `TRUE`/`FALSE`.
#' @param max_forsok Max antal försök.
#' @param vanta_minuter Minuter att vänta mellan försöken.
#'
#' @return `TRUE` om något försök lyckades, annars `FALSE`.
#' @export
funktion_upprepa_forsok_tills_retur_TRUE <- function(funktion, max_forsok = 15, vanta_minuter = 2) {
  for (forsok in seq_len(max_forsok)) {
    if (isTRUE(funktion())) {
      message("Funktionen lyckades på försök ", forsok, ".")
      return(TRUE)
    }
    if (forsok == max_forsok) {
      message("Max antal försök nått. Funktionen stoppas.")
      return(FALSE)
    }
    message("Försök ", forsok, " misslyckades. Försöker igen om ", vanta_minuter, " minuter.")
    Sys.sleep(vanta_minuter * 60)
  }
}

#' Kör en funktion om och om igen tills den inte ger något fel
#'
#' @param funktion En funktion utan argument.
#' @param max_forsok Max antal försök.
#' @param vanta_sekunder Sekunder att vänta mellan försöken.
#' @param meddelanden Om `TRUE` skrivs försök och fel ut.
#' @param hoppa_over Om `TRUE` körs `funktion()` en gång utan felhantering.
#' @param loggfil Sökväg till en `.txt`-fil att spara felmeddelanden i vid
#'   slutgiltigt misslyckande. `NULL` = ingen logg.
#' @param returnera_vid_fel Vad som returneras när alla försök misslyckats.
#'
#' @return Funktionens returvärde vid framgång, annars `returnera_vid_fel`.
#' @export
funktion_upprepa_forsok_om_fel <- function(funktion,
                                           max_forsok = 10,
                                           vanta_sekunder = 1,
                                           meddelanden = FALSE,
                                           hoppa_over = FALSE,
                                           loggfil = NULL,
                                           returnera_vid_fel = NULL) {

  if (isTRUE(hoppa_over)) return(funktion())

  funktionsnamn <- tryCatch(
    paste0(deparse(substitute(funktion)), collapse = " "),
    error = function(e) "funktion()"
  )

  unika_fel <- character(0)

  for (forsok in seq_len(max_forsok)) {
    resultat <- try(funktion(), silent = TRUE)

    if (!inherits(resultat, "try-error")) return(resultat)

    felmeddelande <- as.character(resultat)
    unika_fel <- unique(c(unika_fel, felmeddelande))

    if (meddelanden) {
      cat("Försök ", forsok, " med ", funktionsnamn, " misslyckades med fel: ", felmeddelande)
    }

    if (forsok == max_forsok) {
      cat("Max antal försök nått. ", funktionsnamn, " stoppas.\n\n")
      if (!is.null(loggfil)) {
        loggtext <- paste0(
          Sys.time(), " | Funktion: ", funktionsnamn, "\nFelmeddelanden:\n",
          paste(unika_fel, collapse = "\n"), "\n\n"
        )
        write(loggtext, file = loggfil, append = TRUE)
        cat(paste0(unika_fel, "\n"))
      }
      return(returnera_vid_fel)
    }

    if (meddelanden) message("Försöker igen om ", vanta_sekunder, " sekunder.")
    Sys.sleep(vanta_sekunder)
  }
}

#' Kör en eller flera skriptrader igen om felmeddelandet matchar
#'
#' Fångar uttrycket `expr` (utan att evaluera det direkt) och kör om det vid
#' fel vars meddelande innehåller något av orden i
#' `upprepa_vid_felmeddelande_som_innehaller`.
#'
#' En rad: `df <- skriptrader_upprepa_om_fel(hamta_data())`.
#' Flera rader: lägg dem inom `{ ... }`.
#'
#' @param expr Uttryck att köra (fångas med `substitute()`).
#' @param max_forsok Max antal försök.
#' @param vila_sek Sekunder mellan försöken.
#' @param exportera_till_globalenv Om `TRUE` läggs objekt som skapas i `expr`
#'   ut i den globala miljön.
#' @param returnera_vid_fel Vad som returneras vid slutgiltigt fel. `NULL`
#'   betyder att felet kastas vidare.
#' @param upprepa_vid_felmeddelande_som_innehaller Teckenvektor med delsträngar
#'   som gör att ett nytt försök görs.
#'
#' @return Värdet av `expr`, eller de skapade objekten som en lista.
#' @export
skriptrader_upprepa_om_fel <- function(expr,
                                       max_forsok = 5,
                                       vila_sek = 1,
                                       exportera_till_globalenv = TRUE,
                                       returnera_vid_fel = NULL,
                                       upprepa_vid_felmeddelande_som_innehaller =
                                         c("recv failure", "connection was reset",
                                           "curl_fetch_memory", "timeout")) {

  expr_sub <- substitute(expr)
  is_fun_input <- is.symbol(expr_sub) &&
    is.function(get0(as.character(expr_sub), envir = parent.frame(), inherits = TRUE))

  for (i in seq_len(max_forsok)) {
    env <- new.env(parent = parent.frame())

    out <- tryCatch(
      {
        if (is_fun_input) {
          eval(expr_sub, envir = parent.frame())()
        } else {
          res <- eval(expr_sub, envir = env)
          obj_namn <- ls(env, all.names = TRUE)
          if (length(obj_namn)) {
            obj_lista <- mget(obj_namn, envir = env)
            if (exportera_till_globalenv) list2env(obj_lista, envir = globalenv())
            obj_lista
          } else {
            res
          }
        }
      },
      error = function(e) {
        msg <- tolower(conditionMessage(e))
        message("Fel vid försök ", i, ": ", conditionMessage(e))

        ska_forsoka_igen <- is.null(upprepa_vid_felmeddelande_som_innehaller) ||
          any(vapply(
            tolower(upprepa_vid_felmeddelande_som_innehaller),
            function(p) grepl(p, msg, fixed = TRUE), logical(1)
          ))

        if (i < max_forsok && ska_forsoka_igen) {
          Sys.sleep(vila_sek)
          return(NULL)
        }
        if (is.null(returnera_vid_fel)) stop(e) else returnera_vid_fel
      }
    )

    if (!is.null(out)) return(out)
  }
}
