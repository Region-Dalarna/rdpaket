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
