# Klassgränser för värden som kan vara både negativa och positiva, t.ex. nettopendling: symmetriska
# kring noll, med ett mittenintervall som sträcker sig lika långt åt båda hållen (-s/2 till s/2) så
# att "ungefär noll" blir en egen, vit klass. Stegbredden s är ett "snyggt" tal (1, 2, 5 gånger
# en tiopotens) som ger ungefär antal_per_sida klasser på varje sida om mittenintervallet.
#' @noRd
intern_nollcentrerade_klassgranser <- function(max_abs, antal_per_sida = 5) {
  if (!is.finite(max_abs) || max_abs <= 0) return(c(-1, 1))
  ra <- max_abs / (antal_per_sida + 0.5)
  tiopotens <- 10^floor(log10(ra))
  steg <- c(1, 2, 5, 10, 20, 50) * tiopotens
  # största steget som ger minst ungefär antal_per_sida klasser per sida, men aldrig fler än 11 klasser
  s <- max(steg[steg <= ra * 1.25])
  while (ceiling((max_abs - s / 2) / s) * 2 + 1 > 11) s <- steg[which(steg > s)[1]]
  k <- ceiling((max_abs - s / 2) / s)
  seq(-(k + 0.5) * s, (k + 0.5) * s, by = s)
}
