# Tolkar ett intervallmönster i en tid_koder-parameter, t.ex. "2015:9999"
# ("från 2015 till senaste tillgängliga period") eller "2015M01:9999" för
# månadstabeller. pxweb2r stödjer inte intervall mot API:t (bara "*", enskilda
# koder eller "9999" som ensam sentinel för senaste period), så vi hämtar
# tabellens giltiga koder och filtrerar själva.
#
# Allt annat än ett enda "start:slut"-mönster (dvs. "*", en enskild kod, eller
# en vektor med flera specifika koder som i jmf_ar-stilen) skickas igenom
# oförändrat - detta ändrar alltså inget befintligt beteende, det lägger
# bara till en ny tolkning av ett mönster som annars hade varit en ogiltig kod.
#' @noRd
intern_tolka_tidskoder <- function(tabell, tid_koder) {
  if (length(tid_koder) != 1 || !grepl("^[^:]+:[^:]+$", tid_koder)) {
    return(tid_koder)
  }

  delar <- strsplit(tid_koder, ":", fixed = TRUE)[[1]]
  start <- delar[1]
  slut <- delar[2]

  giltiga <- sort(pxweb2r::pxweb2_get_values(tabell, "Tid", quiet = TRUE)$code)

  if (slut == "9999") {
    giltiga[giltiga >= start]
  } else {
    giltiga[giltiga >= start & giltiga <= slut]
  }
}

# Samma "start:9999"-mönster som intern_tolka_tidskoder(), men för
# datakällor som tar en ren heltalsvektor av år (t.ex. Kolada via
# rdverktyg::hamta_kolada_df()) i stället för pxweb2r-kodade perioder. Här
# finns ingen "giltiga koder"-uppslagning att göra - API:et returnerar helt
# enkelt inga rader för år som inte är publicerade än, oavsett hur långt
# fram slutåret sätts, så "9999" ersätts bara med ett passande långt fram i
# tiden-årtal.
#' @noRd
intern_tolka_ar_intervall <- function(tid_koder, ar_till_om_9999 = 2100) {
  if (length(tid_koder) != 1 || !grepl("^[0-9]+:[0-9]+$", tid_koder)) {
    return(tid_koder)
  }
  delar <- strsplit(tid_koder, ":", fixed = TRUE)[[1]]
  start <- as.integer(delar[1])
  slut <- if (delar[2] == "9999") ar_till_om_9999 else as.integer(delar[2])
  start:slut
}

# Vilka tidskoder ur tid_koder som ska skickas till EN av flera tabeller som delar
# på tidsserien (t.ex. historiktabell + ny tabell). SCB ger fel om en Tid-kod saknas i
# tabellen, så enskilda år skärs mot tabellens egna koder. Returnerar character(0) om
# tabellen inte har något av åren - anroparen ska då hoppa över tabellen.
# - "*" skickas vidare som det är.
# - "9999" (senaste) hämtas bara från tabellen med ger_senaste = TRUE (den nyaste). Eftersom
#   nyaste periodens kod kan finnas utan värden (NA) skickas de senaste tre åren (15 perioder
#   för månadstabeller) i stället för "9999" - anroparen MÅSTE då köra
#   intern_behall_senaste_tid() på det hopslagna uttaget.
# - Intervall ("2015:9999") tolkas av intern_tolka_tidskoder().
#' @noRd
intern_tid_for_tabell <- function(tabell, tid_koder, ger_senaste = FALSE) {
  tid_koder <- as.character(tid_koder)
  if ("*" %in% tid_koder) return("*")

  tid <- intern_tolka_tidskoder(tabell, tid_koder)
  vill_ha_senaste <- "9999" %in% tid
  tid <- setdiff(tid, "9999")
  if (length(tid) > 0 || (vill_ha_senaste && ger_senaste)) {
    koder <- sort(pxweb2r::pxweb2_get_values(tabell, "Tid", quiet = TRUE)$code)
    tid <- intersect(tid, koder)
    if (vill_ha_senaste && ger_senaste) {
      antal <- if (all(grepl("^[0-9]{4}$", koder))) 3 else 15
      tid <- union(tid, utils::tail(koder, antal))
    }
  }
  tid
}

# Stoppar med ett tydligt fel om ingen av tabellerna har något av de begärda åren
# (tid_lista = lista med resultatet av intern_tid_for_tabell() per tabell).
#' @noRd
intern_stoppa_om_ingen_tid <- function(tid_lista, amne = "uttaget") {
  if (all(lengths(tid_lista) == 0)) {
    stop("Inga av de begärda perioderna (tid_koder) finns i SCB:s tabeller för ", amne, ".", call. = FALSE)
  }
  invisible(NULL)
}

# "9999" betyder senaste året med data. SCB:s nyaste tabell kan ha en period som
# finns som Tid-kod men ännu saknar värden (NA), och historiktabellen ger annars sitt
# sista år vid sidan av det nya. Efter att tabellerna slagits ihop tas därför rader
# utan värde bort och bara det senaste året (plus eventuella uttryckligen begärda år)
# behålls. Utan "9999" i tid_koder returneras df oförändrad.
#' @noRd
intern_behall_senaste_tid <- function(df, tid_koder, tidkol = "år", varde = "value") {
  if (is.null(df) || nrow(df) == 0 || !"9999" %in% tid_koder || !tidkol %in% names(df)) return(df)
  if (varde %in% names(df)) df <- df[!is.na(df[[varde]]), , drop = FALSE]
  if (nrow(df) == 0) return(df)
  df[df[[tidkol]] %in% c(setdiff(tid_koder, "9999"), max(df[[tidkol]])), , drop = FALSE]
}
