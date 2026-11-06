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

# Bygger sökindexet: en rad per exporterad diag_*()-funktion, med titel,
# beskrivning och parameterdokumentation (allt från .Rd-filen, dvs. samma
# text som visas av ?funktionsnamn) hopslaget till en sökbar textsträng.
#' @noRd
intern_diagram_index <- function() {
  rd_db <- tools::Rd_db("rddiagramkatalog")
  funktioner <- sort(getNamespaceExports("rddiagramkatalog"))

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
#'
#' @return En `data.frame` med kolumnerna `funktion` och `titel`, en rad per
#'   matchande funktion. Skriv `?funktionsnamn` för att läsa mer om en
#'   specifik funktion, eller `diagramkatalog_bladdra()` för att söka och
#'   fylla i parametrar i ett grafiskt gränssnitt.
#'
#' @examples
#' \dontrun{
#' sok_diagram("arbetslöshet")
#' sok_diagram(c("scb", "kön"))
#' }
#' @export
sok_diagram <- function(sokord, forcera_omladdning = FALSE) {
  index <- intern_diagram_index_cached(forcera_omladdning)
  matchar <- Reduce(`&`, lapply(sokord, function(ord) {
    grepl(ord, index$sokbar_text, ignore.case = TRUE)
  }))
  resultat <- index[matchar, c("funktion", "titel")]
  rownames(resultat) <- NULL
  resultat
}
