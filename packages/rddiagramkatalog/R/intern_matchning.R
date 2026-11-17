# Översätter klartext till SCB:s koder för en variabel i en tabell (t.ex. utbildningsgrupper eller
# kön/ålder/födelseland i de regionala matchningsindikatorerna). Hittas klartexten flera gånger (t.ex. "totalt"
# finns som två koder) används den första. Stannar med tydligt fel om någon klartext saknas.
#' @noRd
intern_matchning_kod <- function(tabell, variabel, klartext) {
  varden <- pxweb2r::pxweb2_get_values(tabell, variabel, quiet = TRUE)
  koder <- varden$code[match(klartext, varden$label)]
  if (anyNA(koder)) {
    stop("Hittade inte ", paste0("\"", klartext[is.na(koder)], "\"", collapse = ", "), " i variabeln ", variabel,
         " (", tabell, "). Giltiga värden hämtas med pxweb2r::pxweb2_get_values(\"", tabell, "\", \"", variabel,
         "\").", call. = FALSE)
  }
  koder
}
