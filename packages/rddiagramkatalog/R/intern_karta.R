# Klassgränser för värden som kan vara både negativa och positiva, t.ex. nettopendling: symmetriska
# kring noll, med ett mycket smalt mittenintervall (cirka 1 % av största värdet, avrundat till ett
# snyggt tal) som är en egen, vit klass för "i princip oförändrat". Utanför det följer klasser med
# jämn stegbredd s (1, 2 eller 5 gånger en tiopotens), så få och breda som krävs för att inte få fler
# än 11 klasser totalt. Första klassen utanför mitten går alltså från mittenintervallets
# kant till s.
#' @noRd
intern_nollcentrerade_klassgranser <- function(max_abs, mitt_andel = 0.01) {
  if (!is.finite(max_abs) || max_abs <= 0) return(c(-1, 1))
  # största "snygga" tal (1, 2 eller 5 gånger en tiopotens) som är högst x
  snyggt <- function(x) {
    steg <- c(1, 2, 5, 10) * 10^floor(log10(x))
    max(steg[steg <= x])
  }
  mitt <- snyggt(max_abs * mitt_andel)

  # minsta snygga stegbredd som ger högst 11 klasser totalt
  potens <- floor(log10(max_abs))
  kandidater <- sort(as.vector(outer(c(1, 2, 5), 10^((potens - 3):(potens + 1)))))
  s <- kandidater[ceiling(max_abs / kandidater) * 2 + 1 <= 11][1]

  k <- ceiling(max_abs / s)
  positiva <- unique(c(mitt, seq(s, k * s, by = s)))
  c(-rev(positiva), positiva)
}
