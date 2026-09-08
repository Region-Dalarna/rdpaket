# Allmänna R-verktyg - utbrutna ur func_API.R.

#' Ladda en funktions standardargument in i den globala miljön
#'
#' Praktiskt vid felsökning: kör funktionens kropp rad för rad med
#' standardvärdena satta. Skriver över variabler med samma namn.
#'
#' @param funktion Funktionen (skickas som namn, inte anrop).
#' @param meddelanden Om `TRUE` rapporteras argument som saknar standardvärde.
#'
#' @return Inget (osynligt `NULL`).
#' @export
ladda_funk_parametrar <- function(funktion, meddelanden = FALSE) {
  funktionsnamn <- sub("\\(\\)$", "", deparse(substitute(funktion)))
  st_var <- formals(funktionsnamn)

  for (varname in names(st_var)) {
    if (!is.symbol(st_var[[varname]]) || nzchar(as.character(st_var[[varname]]))) {
      if (is.symbol(st_var[[varname]]) && !nzchar(as.character(st_var[[varname]]))) {
        if (meddelanden) message("Parametern ", varname, " saknar standardvärde och laddades inte.")
        next
      }
      assign(varname, eval(st_var[[varname]]), envir = .GlobalEnv)
    } else if (meddelanden) {
      message("Parametern ", varname, " saknar standardvärde och laddades inte.")
    }
  }
  invisible(NULL)
}

#' Lista funktionsnamn i en eller flera R-filer
#'
#' @param filsokvag_eller_github_url En eller flera sökvägar eller URL:er till
#'   R-filer.
#'
#' @return En teckenvektor med unika funktionsnamn.
#' @export
lista_funktioner_i_skript <- function(filsokvag_eller_github_url) {
  namn <- unlist(lapply(filsokvag_eller_github_url, function(kalla) {
    rader <- readLines(kalla, warn = FALSE)
    if (length(rader) == 0) return(character(0))
    traffar <- stringr::str_extract_all(rader, "\\b\\w+\\s*(?=\\s*<-\\s*function)")
    stringr::str_trim(unique(unlist(traffar)))
  }))
  namn[!is.na(namn) & nzchar(namn)]
}

#' Lista bara toppnivåfunktioner i en R-fil
#'
#' Som [lista_funktioner_i_skript()] men tar bara med funktioner som inte är
#' definierade inuti andra funktioner.
#'
#' @param filnamn Sökväg eller URL till en R-fil.
#'
#' @return En teckenvektor med funktionsnamn.
#' @export
hitta_funktioner_i_fil_ej_inuti_andra_funktioner <- function(filnamn) {
  rader <- readLines(filnamn, warn = FALSE)

  funktionsrader <- stringr::str_which(rader, "\\bfunction\\b")
  yttre <- integer(0)
  balans <- 0L

  for (i in funktionsrader) {
    balans_till_rad <- sum(stringr::str_count(rader[seq_len(i)], "\\{")) -
      sum(stringr::str_count(rader[seq_len(i)], "\\}"))
    if (balans == 0L) yttre <- c(yttre, i)
    balans <- balans_till_rad
  }

  namn <- stringr::str_extract(rader[yttre], "\\b\\w+\\b(?=\\s*<-\\s*function)")
  namn[!is.na(namn)]
}

#' Stoppa skriptet utan felmeddelande
#'
#' @return Inget - avbryter körningen.
#' @export
stop_tyst <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}

#' Dämpa varningar som matchar en text
#'
#' @param expr Uttrycket som ska köras.
#' @param warn_text Delsträng - varningar vars meddelande innehåller den dämpas.
#'
#' @return Värdet av `expr`.
#' @export
suppress_specific_warning <- function(expr, warn_text = "NAs introduced by coercion") {
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (grepl(warn_text, conditionMessage(w))) invokeRestart("muffleWarning")
    }
  )
}

#' Filtrera fram jämförelseperioder utifrån offset
#'
#' T.ex. samma månad 1, 2 och 3 år bakåt: `period_vekt = c(-12, -24, -36)`.
#'
#' @param period_kolumn Hela periodkolumnen, t.ex. `df$månad`.
#' @param vald_period En eller flera perioder att räkna från.
#' @param period_vekt Antal enheter bakåt (negativt) eller framåt (positivt).
#' @param inkludera_vald_period Om `vald_period` ska ingå i resultatet.
#'
#' @return En vektor med perioder.
#' @export
period_jmfr_filter <- function(period_kolumn, vald_period, period_vekt, inkludera_vald_period = TRUE) {
  retur_vekt <- unlist(lapply(vald_period, function(period) {
    valda_pos <- period_vekt[period_vekt > 0]
    valda_neg <- period_vekt[period_vekt < 0]

    tidigare <- sort(unique(period_kolumn[period_kolumn < period]), decreasing = TRUE)[abs(valda_neg)]
    senare   <- sort(unique(period_kolumn[period_kolumn > period]), decreasing = FALSE)[abs(valda_pos)]
    c(senare, tidigare)
  }))

  if (isTRUE(inkludera_vald_period)) retur_vekt <- c(vald_period, retur_vekt)
  retur_vekt
}

#' Dynamisk avrundning
#'
#' Stora tal rundas till en "fin" grund (10, 100, ...), medelstora till en
#' decimal och små till två.
#'
#' @param x Numerisk vektor.
#' @param grans_stora,grans_medel Gränser för stora respektive medelstora tal.
#' @param dec_stora,dec_medel,dec_sma Antal decimaler i respektive intervall.
#'
#' @return En numerisk vektor.
#' @export
avrundning_dynamisk <- function(x, grans_stora = 10, grans_medel = 1,
                                dec_stora = 0, dec_medel = 1, dec_sma = 2) {
  vapply(x, function(v) {
    if (is.na(v)) return(NA_real_)
    if (abs(v) >= grans_stora) {
      multipel <- 10^floor(log10(abs(v)))
      round(v / multipel) * multipel
    } else if (abs(v) >= grans_medel) {
      round(v, dec_medel)
    } else {
      round(v, dec_sma)
    }
  }, numeric(1))
}

#' Dela upp värden i ett antal intervall
#'
#' Delar spannet i `skickad_kolumn` i `antal_intervaller` steg avrundade till
#' "fina" tal. Praktiskt för legender och storleksklasser.
#'
#' @param skickad_kolumn Numerisk vektor.
#' @param antal_intervaller Antal intervall (standard 5).
#'
#' @return En numerisk vektor med intervallgränser.
#' @export
skapa_intervaller <- function(skickad_kolumn, antal_intervaller = 5) {
  runda_till_bra_tal <- function(tal) 10^round(log10(tal))

  min_varde <- min(skickad_kolumn, na.rm = TRUE)
  max_varde <- max(skickad_kolumn, na.rm = TRUE)

  intervall_steg <- runda_till_bra_tal((max_varde - min_varde) / (antal_intervaller - 1))

  min_rundat <- floor(min_varde / intervall_steg) * intervall_steg
  if (min_rundat <= 0) min_rundat <- intervall_steg / 2
  min_rundat <- max(floor(min_varde / intervall_steg) * intervall_steg, min_varde)
  max_rundat <- min(ceiling(max_varde / intervall_steg) * intervall_steg, max_varde)

  if (max_rundat > max_varde * 1.1) {
    max_rundat <- floor(max_varde / intervall_steg) * intervall_steg + intervall_steg / 2
  }
  if ((max_rundat - min_rundat) < (antal_intervaller - 1) * intervall_steg) {
    max_rundat <- max_rundat + intervall_steg
  }

  avrundning_dynamisk(seq(min_rundat, max_rundat, length.out = antal_intervaller))
}

#' Välj ut jämnt spridda värden ur en vektor
#'
#' Returnerar en vektor i originalordning där bara ett antal jämnt fördelade
#' värden (inkl. min och max) är kvar, resten är `""`. Praktiskt för
#' axeletiketter.
#'
#' @param skickad_vektor Numerisk vektor (eller något som går att göra numeriskt).
#' @param antal_varden Antal värden att behålla.
#'
#' @return En teckenvektor lika lång som `skickad_vektor`.
#' @export
varden_jamnt_spridda_valj_ut <- function(skickad_vektor, antal_varden = 4) {
  num_vec <- as.numeric(skickad_vektor)
  n <- length(num_vec)
  if (antal_varden >= n) return(as.character(skickad_vektor))

  valda <- sort(num_vec)[round(seq(1, n, length.out = antal_varden))]
  ifelse(num_vec %in% valda, as.character(num_vec), "")
}

#' Gör en vektor till en citattecken-separerad textrad
#'
#' `vektor_till_text(c("a", "b"))` skriver ut `"a", "b"` och kopierar det till
#' urklipp - praktiskt för att klistra in i kod.
#'
#' @param skickad_vektor Vektorn.
#' @param till_urklipp Om `TRUE` kopieras texten till urklipp (kräver `clipr`).
#'
#' @return Textraden (osynligt).
#' @export
vektor_till_text <- function(skickad_vektor, till_urklipp = TRUE) {
  retur_text <- paste0('"', skickad_vektor, '"', collapse = ", ")
  cat(retur_text)
  if (isTRUE(till_urklipp)) urklipp(retur_text)
  invisible(retur_text)
}

#' Skriv ut tal 1-20 som svenska ord
#'
#' @param x Heltal 1-20 (vektor). Övriga tal tas bort med en varning.
#'
#' @return En teckenvektor.
#' @export
nummer_till_text <- function(x) {
  ord <- c("ett", "två", "tre", "fyra", "fem", "sex", "sju", "åtta", "nio", "tio",
           "elva", "tolv", "tretton", "fjorton", "femton", "sexton", "sjutton",
           "arton", "nitton", "tjugo")

  retur_x <- ifelse(x >= 1 & x <= 20, ord[x], NA_character_)
  if (anyNA(retur_x)) warning("Funktionen kan bara hantera talen 1-20, övriga tas bort.")
  retur_x <- retur_x[!is.na(retur_x)]
  if (length(retur_x) == 0) stop("Funktionen kan bara hantera tal som är 1-20.")
  retur_x
}

#' Lägg till avslutande snedstreck om det saknas
#'
#' @param x En eller flera sökvägar/strängar.
#'
#' @return `x` med avslutande `/`.
#' @export
slash_lagg_till <- function(x) {
  ifelse(stringr::str_ends(x, "/"), x, paste0(x, "/"))
}

#' Sökväg till det skript som körs
#'
#' Försöker i tur och ordning: `this.path`, `source()`-kontext,
#' RStudio-dokumentet, och till sist arbetskatalogen.
#'
#' @return En sökväg som slutar med `/`.
#' @export
sokvag_for_skript_hitta <- function() {
  retur_varde <- tryCatch(
    dirname(this.path::this.path()),
    error = function(e) NULL
  )

  if (is.null(retur_varde) && !is.null(sys.frame(1)$ofile)) {
    retur_varde <- dirname(normalizePath(sys.frame(1)$ofile))
  }
  if (is.null(retur_varde) && interactive() && requireNamespace("rstudioapi", quietly = TRUE)) {
    retur_varde <- tryCatch(
      dirname(rstudioapi::getActiveDocumentContext()$path),
      error = function(e) NULL
    )
  }
  if (is.null(retur_varde) || !nzchar(retur_varde)) retur_varde <- getwd()

  slash_lagg_till(retur_varde)
}

#' Lägg något i urklipp
#'
#' Använder `clipr` om det finns (hanterar även data.frames), annars ett
#' plattforms-fallback (Windows/macOS/xclip/xsel).
#'
#' @param x Det som ska kopieras.
#' @param sep Radseparator för teckenvektorer i fallback-läget.
#'
#' @return `x` (osynligt).
#' @export
urklipp <- function(x, sep = "\n") {
  if (requireNamespace("clipr", quietly = TRUE) && isTRUE(try(clipr::clipr_available(), silent = TRUE))) {
    clipr::write_clip(x)
    return(invisible(x))
  }

  txt <- if (is.data.frame(x)) {
    paste(utils::capture.output(
      utils::write.table(x, sep = "\t", row.names = FALSE, quote = FALSE)
    ), collapse = "\n")
  } else {
    ch <- as.character(x)
    if (length(ch) == 1) ch else paste(ch, collapse = sep)
  }

  os <- Sys.info()[["sysname"]]
  if (identical(os, "Windows")) {
    utils::writeClipboard(enc2native(txt))
  } else if (identical(os, "Darwin")) {
    con <- pipe("pbcopy", "w"); on.exit(close(con)); writeChar(enc2utf8(txt), con, eos = NULL, useBytes = TRUE)
  } else if (nzchar(Sys.which("xclip"))) {
    con <- pipe("xclip -selection clipboard", "w"); on.exit(close(con)); writeChar(enc2utf8(txt), con, eos = NULL, useBytes = TRUE)
  } else if (nzchar(Sys.which("xsel"))) {
    con <- pipe("xsel --clipboard --input", "w"); on.exit(close(con)); writeChar(enc2utf8(txt), con, eos = NULL, useBytes = TRUE)
  } else {
    warning("Ingen urklippsmetod hittades. Installera 'clipr' eller xclip/xsel.")
  }
  invisible(x)
}

#' Skapa åldersgrupper från en åldersvektor
#'
#' `skapa_aldersgrupper(alder, c(19, 35, 50, 65, 80))` ger grupperna
#' 0-18, 19-34, 35-49, 50-64, 65-79, 80+ år.
#'
#' @param alder Vektor med åldrar (numerisk eller text).
#' @param aldergrupp_vekt Startålder för varje ny grupp.
#' @param konv_fran_txt Om `TRUE` plockas första talet ut ur textvärden.
#' @param returnera_faktorvariabel Om `TRUE` returneras en faktor, annars text.
#'
#' @return En faktor (eller teckenvektor) med åldersgrupp per element.
#' @export
skapa_aldersgrupper <- function(alder, aldergrupp_vekt, konv_fran_txt = TRUE,
                                returnera_faktorvariabel = TRUE) {
  if (konv_fran_txt && is.character(alder)) {
    alder <- as.numeric(sub(",", ".", stringr::str_extract(alder, "-?[0-9]+([.,][0-9]+)?"), fixed = TRUE))
  }

  min_alder <- suppressWarnings(min(alder, na.rm = TRUE))
  max_alder <- suppressWarnings(max(alder, na.rm = TRUE))

  if (!is.infinite(aldergrupp_vekt[[1]])) aldergrupp_vekt <- c(-Inf, aldergrupp_vekt)
  if (!is.infinite(utils::tail(aldergrupp_vekt, 1))) aldergrupp_vekt <- c(aldergrupp_vekt, Inf)

  aldergrupp_vekt[1] <- min(aldergrupp_vekt[1], min_alder)
  aldergrupp_vekt[length(aldergrupp_vekt)] <- max(aldergrupp_vekt[length(aldergrupp_vekt)], max_alder + 1)

  labels <- character(length(aldergrupp_vekt) - 1)
  for (i in seq_along(labels)) {
    lower <- max(aldergrupp_vekt[i], min_alder)
    upper <- min(aldergrupp_vekt[i + 1] - 1, max_alder)

    labels[i] <- if (i == length(labels) &&
                     max_alder > max(aldergrupp_vekt[is.finite(aldergrupp_vekt)])) {
      paste0(lower, "+ år")
    } else if (lower == upper) {
      paste0(lower, " år")
    } else {
      paste0(lower, "-", upper, " år")
    }
  }

  retur_vekt <- cut(alder, breaks = aldergrupp_vekt, labels = labels,
                    right = FALSE, include.lowest = TRUE)
  if (!returnera_faktorvariabel) retur_vekt <- as.character(retur_vekt)
  retur_vekt
}
