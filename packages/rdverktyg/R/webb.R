# Webbhämtning - utbrutna ur func_API.R.

#' Extrahera länkar från en webbsida som matchar sökord
#'
#' Läser alla `<a href>` på sidan och behåller de som matchar samtliga
#' `sokord`. Ett sökord som börjar med `!` betyder "får INTE finnas". Relativa
#' länkar görs absoluta med `bas_url`.
#'
#' @param skickad_url URL till sidan.
#' @param sokord Teckenvektor med sökord (regex). `!`-prefix = negation.
#' @param bas_url Bas-URL för relativa länkar.
#'
#' @return En teckenvektor med matchande (absoluta) URL:er.
#' @export
webbsida_extrahera_url_med_sokord <- function(skickad_url,
                                              sokord = c("varsel", "lan", "!bransch", ".xlsx"),
                                              bas_url = "https://arbetsformedlingen.se") {
  if (!requireNamespace("rvest", quietly = TRUE)) stop("Paketet 'rvest' krävs.", call. = FALSE)

  lankar <- rvest::read_html(skickad_url) |>
    rvest::html_elements("a") |>
    rvest::html_attr("href")
  lankar <- lankar[!is.na(lankar)]

  matchar <- Filter(function(lank) {
    all(vapply(sokord, function(s) {
      if (startsWith(s, "!")) {
        !grepl(substring(s, 2), lank, ignore.case = TRUE)
      } else {
        grepl(s, lank, ignore.case = TRUE)
      }
    }, logical(1)))
  }, lankar)

  matchar <- unlist(matchar, use.names = FALSE)
  ifelse(grepl("^https?://", matchar), matchar, paste0(bas_url, matchar))
}

#' Hitta och ladda ner en fil från en webbsida via sökord
#'
#' Letar bland länkarna på `url_webbsida` efter den (eller de) som matchar alla
#' `sokord` och laddar ner den till en temporär fil.
#'
#' @param url_webbsida URL till sidan med länkar.
#' @param sokord Sökord som identifierar rätt fil (se
#'   [webbsida_extrahera_url_med_sokord()]).
#' @param bas_url Bas-URL för relativa länkar.
#'
#' @return En teckenvektor med sökväg(ar) till de nedladdade temporära filerna.
#' @export
filhamtning_med_url_och_sokord <- function(
    url_webbsida = "https://arbetsformedlingen.se/statistik/sok-statistik/tidigare-statistik",
    sokord = c("web-platser", ".xlsx"),
    bas_url = "https://arbetsformedlingen.se") {

  url_nedladdning <- webbsida_extrahera_url_med_sokord(url_webbsida, sokord, bas_url)
  if (length(url_nedladdning) == 0) stop("Inga URL:er matchar angivna sökord.")

  vapply(url_nedladdning, function(u) {
    fil <- tempfile(fileext = ".xlsx")
    resp <- httr::GET(u, httr::write_disk(fil, overwrite = TRUE))
    if (httr::status_code(resp) != 200) {
      stop("Nedladdning misslyckades (HTTP ", httr::status_code(resp), ") för ", u)
    }
    fil
  }, character(1), USE.NAMES = FALSE)
}

#' Svarar en webbsida med HTTP 200?
#'
#' @param skickad_url URL att testa.
#'
#' @return `TRUE` om ett HEAD-anrop ger status 200.
#' @export
url_finns_webbsida <- function(skickad_url) {
  identical(httr::status_code(httr::HEAD(skickad_url)), 200L)
}
