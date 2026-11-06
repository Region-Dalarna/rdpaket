# Enkel cache för sökindexet - bygger på installerade .Rd-filer (via
# tools::Rd_db()), inte källkoden, så den fungerar oavsett hur paketet
# installerats. Byggs om bara om man explicit ber om det (t.ex. efter att
# fler diagram lagts till och paketet laddats om i samma session).
.rddiagramkatalog_cache <- new.env(parent = emptyenv())

#' @noRd
intern_rd_text <- function(rd, tag) {
  idx <- which(tools:::RdTags(rd) == tag)
  if (length(idx) == 0) return(NA_character_)
  trimws(paste(unlist(rd[idx]), collapse = " "))
}

# tools::Rd_db() läser paketets ihoppackade lazy-load-hjälpdatabas
# (help/rddiagramkatalog.rdb) - en resurs som kan bli tillfälligt låst eller
# skadad oberoende av koden här (t.ex. av OneDrive-synk eller ett
# antivirusprogram som håller filen låst mitt i en installation på Windows).
# Ger ett begripligt, handlingsbart felmeddelande i stället för R:s kryptiska
# "lazy-load database is corrupt"/"internal error in R_decompress1".
#' @noRd
intern_rd_db <- function() {
  tryCatch(
    tools::Rd_db("rddiagramkatalog"),
    error = function(e) {
      stop(
        "Kunde inte läsa rddiagramkatalogs hjälpdatabas (", conditionMessage(e), ").\n",
        "Prova först att starta om R/RStudio och försök igen - det brukar bero på en\n",
        "tillfällig låsning av hjälpfilerna. Om felet består, installera om paketet:\n",
        "  remove.packages(\"rddiagramkatalog\")\n",
        "  rdverktyg::rdpaket_uppdatera_alla(installera_saknade_paket = TRUE)",
        call. = FALSE
      )
    }
  )
}

# Renderar en funktions fullständiga, formaterade hjälptext (samma innehåll
# som ?funktionsnamn) till en vanlig textsträng - används av
# diagramkatalog_utforska()s "Visa hjälp"-knapp, som visar texten i en egen
# modalruta i stället för att öppna RStudios Hjälp-ruta (vilken annars kan
# hamna bakom gadgetens dialogfönster). underline_titles = FALSE undviker
# Rd2txt()s annars inkluderade "_T_e_c_k_e_n backspace"-understrykning, som
# bara är tänkt för en riktig terminal, inte för textvisning i en HTML-ruta.
#' @noRd
intern_funktion_hjalptext <- function(fn) {
  rd_db <- intern_rd_db()
  rd <- rd_db[[paste0(fn, ".Rd")]]
  if (is.null(rd)) return(paste("Ingen hjälptext hittades för", fn, "."))

  con <- textConnection("buffer", "w", local = TRUE)
  tools::Rd2txt(rd, out = con, options = list(underline_titles = FALSE))
  close(con)
  paste(buffer, collapse = "\n")
}

# Samma hjälptext som intern_funktion_hjalptext(), men som HTML med
# rubrikraderna ("Description:", "Arguments:", "Usage:" osv. - precis de
# rader Rd2txt() skriver utan inledande blanksteg och med ett avslutande
# kolon) i fetstil. Används av diagramkatalog_utforska()s hjälpruta -
# övriga rader HTML-eskapas manuellt eftersom de injiceras rått via
# shiny::HTML() för att <strong>-taggarna ska tolkas som markup.
#' @noRd
intern_funktion_hjalptext_html <- function(fn) {
  text <- intern_funktion_hjalptext(fn)
  rader <- strsplit(text, "\n", fixed = TRUE)[[1]]
  rader <- gsub("&", "&amp;", rader, fixed = TRUE)
  rader <- gsub("<", "&lt;", rader, fixed = TRUE)
  rader <- gsub(">", "&gt;", rader, fixed = TRUE)
  ar_rubrik <- grepl("^[A-Za-zÅÄÖåäö][A-Za-zÅÄÖåäö ]*:$", rader)
  rader[ar_rubrik] <- paste0("<strong>", rader[ar_rubrik], "</strong>")
  paste(rader, collapse = "\n")
}

# Bygger sökindexet: en rad per exporterad diag_*()-funktion, med titel,
# beskrivning och parameterdokumentation (allt från .Rd-filen, dvs. samma
# text som visas av ?funktionsnamn) hopslaget till en sökbar textsträng.
#' @noRd
intern_diagram_index <- function() {
  rd_db <- intern_rd_db()
  # Enbart diag_*()-funktionerna - inte paketets egna katalogverktyg
  # (sok_diagram(), diagramkatalog_utforska()), som också är exporterade men
  # inte är diagram att söka fram.
  funktioner <- sort(grep("^diag_", getNamespaceExports("rddiagramkatalog"), value = TRUE))

  rader <- lapply(funktioner, function(fn) {
    rd <- rd_db[[paste0(fn, ".Rd")]]
    if (is.null(rd)) {
      return(data.frame(funktion = fn, titel = NA_character_,
                         sokbar_text = fn, stringsAsFactors = FALSE))
    }

    titel <- intern_rd_text(rd, "\\title")
    beskrivning <- intern_rd_text(rd, "\\description")
    arg_text <- intern_rd_text(rd, "\\arguments")

    data.frame(
      funktion = fn,
      titel = titel,
      sokbar_text = paste(fn, titel, beskrivning, arg_text, sep = " "),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rader)
}

#' @noRd
intern_diagram_index_cached <- function(forcera_omladdning = FALSE) {
  if (forcera_omladdning || is.null(.rddiagramkatalog_cache$index)) {
    .rddiagramkatalog_cache$index <- intern_diagram_index()
  }
  .rddiagramkatalog_cache$index
}

# Kopierar en eller flera funktionsnamn (en per rad) till urklippshanteraren.
# Misslyckas tyst (returnerar FALSE) om paketet clipr inte är installerat
# eller om ingen urklippshanterare finns tillgänglig (t.ex. en headless
# Linux-miljö utan xclip/xsel) - urklippskopiering är bara en bekvämlighet
# och ska aldrig få ett sok_diagram()-anrop att krascha.
#' @noRd
intern_kopiera_till_urklipp <- function(funktionsnamn) {
  if (!requireNamespace("clipr", quietly = TRUE) || !clipr::clipr_available()) {
    return(invisible(FALSE))
  }
  tryCatch({
    clipr::write_clip(paste(funktionsnamn, collapse = "\n"))
    invisible(TRUE)
  }, error = function(e) invisible(FALSE))
}

#' Sök bland diagramfunktionerna i katalogen
#'
#' Söker igenom titel, beskrivning och parameterdokumentation för alla
#' diagramfunktioner i paketet (samma text som `?funktionsnamn` visar) och
#' returnerar de som matchar. Sökningen är skiftlägesokänslig och matchar
#' delsträngar var som helst i dokumentationen - inte bara i funktionsnamnet.
#'
#' @param sokord Ett eller flera sökord. Om flera ord anges måste alla matcha
#'   (var och en som delsträng någonstans i funktionens dokumentation, i
#'   valfri ordning och på valfri plats).
#' @param forcera_omladdning Om `TRUE` byggs sökindexet om från paketets
#'   `.Rd`-filer igen, i stället för att återanvända den cachade versionen
#'   från föregående anrop i samma session.
#' @param funktionsnamn_lagg_i_urklippshanteraren Om `TRUE` (standard) läggs
#'   de matchande funktionsnamnen (en per rad) i urklippshanteraren, så de är
#'   redo att klistras in. Kräver paketet `clipr`
#'   (`install.packages("clipr")`) - om det inte finns installerat, eller
#'   ingen urklippshanterare är tillgänglig, hoppas kopieringen bara tyst
#'   över. Standardvärdet styrs av `options(rddiagramkatalog.kopiera_funktionsnamn = ...)`,
#'   så man kan stänga av det permanent (t.ex. i sin `.Rprofile`) utan att
#'   behöva ange parametern vid varje anrop:
#'   `options(rddiagramkatalog.kopiera_funktionsnamn = FALSE)`.
#'
#' @return En `data.frame` med kolumnerna `funktion` och `titel`, en rad per
#'   matchande funktion. Skriv `?funktionsnamn` för att läsa mer om en
#'   specifik funktion, eller `diagramkatalog_utforska()` för att söka och
#'   fylla i parametrar i ett grafiskt gränssnitt.
#'
#' @examples
#' \dontrun{
#' sok_diagram("arbetslöshet")
#' sok_diagram(c("scb", "kön"))
#' sok_diagram("gini", funktionsnamn_lagg_i_urklippshanteraren = FALSE)
#' }
#' @export
sok_diagram <- function(sokord, forcera_omladdning = FALSE,
                         funktionsnamn_lagg_i_urklippshanteraren = getOption("rddiagramkatalog.kopiera_funktionsnamn", TRUE)) {
  index <- intern_diagram_index_cached(forcera_omladdning)
  matchar <- Reduce(`&`, lapply(sokord, function(ord) {
    grepl(ord, index$sokbar_text, ignore.case = TRUE)
  }))
  resultat <- index[matchar, c("funktion", "titel")]
  rownames(resultat) <- NULL

  if (funktionsnamn_lagg_i_urklippshanteraren && nrow(resultat) > 0) {
    intern_kopiera_till_urklipp(resultat$funktion)
  }

  resultat
}
