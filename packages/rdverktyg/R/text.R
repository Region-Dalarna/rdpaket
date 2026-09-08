# Textfunktioner - utbrutna ur func_text.R i Region-Dalarna/funktioner.

intern_lista_med_bindeord <- function(vektor, bindeord) {
  vektor <- as.character(vektor)
  n <- length(vektor)
  if (n == 0) return(character(0))
  if (n == 1) return(vektor)
  paste0(paste(vektor[-n], collapse = ", "), " ", bindeord, " ", vektor[n])
}

#' Sätt ihop en vektor till en text med kommatecken och "och"
#'
#' `list_komma_och(c("a", "b", "c"))` ger `"a, b och c"`.
#'
#' @param skickad_vektor En vektor som sätts ihop till en textsträng.
#'
#' @return En sträng med längd 1 (eller den oförändrade vektorn om den har
#'   0-1 element).
#' @export
list_komma_och <- function(skickad_vektor) {
  intern_lista_med_bindeord(skickad_vektor, "och")
}

#' Sätt ihop en vektor till en text med kommatecken och "eller"
#'
#' `list_komma_eller(c("a", "b", "c"))` ger `"a, b eller c"`.
#'
#' @inheritParams list_komma_och
#' @return En sträng med längd 1.
#' @export
list_komma_eller <- function(skickad_vektor) {
  intern_lista_med_bindeord(skickad_vektor, "eller")
}

#' Sätt ihop en vektor till en text med kommatecken och "samt"
#'
#' `list_komma_samt(c("a", "b", "c"))` ger `"a, b samt c"`.
#'
#' @inheritParams list_komma_och
#' @return En sträng med längd 1.
#' @export
list_komma_samt <- function(skickad_vektor) {
  intern_lista_med_bindeord(skickad_vektor, "samt")
}

#' Dela upp en sträng i flera rader
#'
#' Bryter en sträng till flera rader så att ingen rad blir längre än
#' `max_langd`. Praktiskt för diagramrubriker. För det vanliga fallet
#' (radbrytning vid mellanslag) används [stringr::str_wrap()]; annars görs
#' brytningen vid `sokstrang`.
#'
#' @param strang En sträng, eller en vektor av strängar.
#' @param max_langd Ungefärlig maxlängd (bredd) per rad.
#' @param sokstrang Tecken att bryta vid. Standard är mellanslag.
#'
#' @return En vektor av samma längd som `strang`, med `\n` inlagda.
#' @export
dela_upp_strang_radbryt <- function(strang, max_langd, sokstrang = " ") {
  if (identical(sokstrang, " ")) {
    return(stringr::str_wrap(strang, width = max_langd))
  }

  vapply(strang, function(strang_elem) {
    positioner <- stringr::str_locate_all(strang_elem, stringr::fixed(sokstrang))[[1]][, 2]
    if (length(positioner) == 0) return(strang_elem)

    forra_brytning <- 0L
    for (pos in positioner) {
      if (pos - forra_brytning > max_langd) {
        traff <- positioner[positioner > forra_brytning & positioner < pos]
        if (length(traff) == 0) next
        brytpos <- max(traff)
        substr(strang_elem, brytpos, brytpos) <- "\n"
        forra_brytning <- brytpos
      }
    }
    strang_elem
  }, character(1), USE.NAMES = FALSE)
}

#' Ersätt svenska tecken med ASCII
#'
#' Translittererar å/ä/ö (och andra diakriter som é, ü) till närmaste
#' ASCII-tecken via [stringi::stri_trans_general()].
#'
#' @param text En sträng eller vektor av strängar.
#'
#' @return `text` utan icke-ASCII-tecken.
#' @export
byt_ut_svenska_tecken <- function(text) {
  stringi::stri_trans_general(text, "Latin-ASCII")
}

#' Omvandla ett procenttal till text
#'
#' `procent_till_text(50)` ger `"varannan"`, `procent_till_text(25)` ger
#' `"var fjärde"` och så vidare. Värden utan en bra textform returneras som
#' `"<x>%"`.
#'
#' @param procent Ett procenttal (0-100).
#'
#' @return En sträng.
#' @export
procent_till_text <- function(procent) {

  tolerans <- 0.5

  dplyr::case_when(
    procent == 0 ~ "ingen",
    procent > 0 & procent < 0.5 - tolerans ~ "nästan ingen",
    procent >= 0.5 & procent < 2.5 - tolerans ~ "väldigt få",
    procent >= 2.5 & procent < 5 - tolerans ~ "knappt var tjugonde",
    procent >= 5 - tolerans & procent <= 5 + tolerans ~ "var tjugonde",
    procent > 5 + tolerans & procent < 6.3 - tolerans ~ "drygt var tjugonde",
    procent >= 6.3 & procent < 6.6 - tolerans ~ "knappt var femtonde",
    procent >= 6.6 - tolerans & procent <= 6.6 + tolerans ~ "var femtonde",
    procent > 6.6 + tolerans & procent < 7.5 - tolerans ~ "drygt var femtonde",
    procent > 7.5 + tolerans & procent < 10 - tolerans ~ "knappt var tionde",
    procent >= 10 - tolerans & procent <= 10 + tolerans ~ "var tionde",
    procent > 10 + tolerans & procent < 12.3 - tolerans ~ "drygt var tionde",
    procent >= 12.3 - tolerans & procent < 14.3 - tolerans ~ "knappt var sjunde",
    procent >= 14.3 - tolerans & procent <= 14.3 + tolerans ~ "var sjunde",
    procent > 14.3 + tolerans & procent < 17.2 + tolerans ~ "drygt var sjunde",
    procent >= 17.2 - tolerans & procent < 20 - tolerans ~ "knappt var femte",
    procent >= 20 - tolerans & procent <= 20 + tolerans ~ "var femte",
    procent > 20 + tolerans & procent < 22.5 - tolerans ~ "drygt var femte",
    procent >= 22.5 - tolerans & procent < 25 - tolerans ~ "knappt var fjärde",
    procent >= 25 - tolerans & procent <= 25 + tolerans ~ "var fjärde",
    procent > 25 + tolerans & procent < 29.2 - tolerans ~ "drygt var fjärde",
    procent >= 29.2 - tolerans & procent < 33.3 - tolerans ~ "knappt var tredje",
    procent >= 33.3 - tolerans & procent <= 33.3 + tolerans ~ "var tredje",
    procent > 33.3 + tolerans & procent < 37 - tolerans ~ "drygt var tredje",
    procent >= 37 - tolerans & procent < 40 - tolerans ~ "knappt två av fem",
    procent >= 40 - tolerans & procent <= 40 + tolerans ~ "två av fem",
    procent > 40 + tolerans & procent < 45 - tolerans ~ "drygt två av fem",
    procent >= 45 - tolerans & procent < 50 - tolerans ~ "knappt varannan",
    procent >= 50 - tolerans & procent <= 50 + tolerans ~ "varannan",
    procent > 50 + tolerans & procent < 56 - tolerans ~ "drygt varannan",
    procent >= 56 - tolerans & procent < 60 - tolerans ~ "knappt tre av fem",
    procent >= 60 - tolerans & procent <= 60 + tolerans ~ "tre av fem",
    procent > 60 + tolerans & procent < 65 - tolerans ~ "drygt tre av fem",
    procent >= 65 - tolerans & procent < 66.6 - tolerans ~ "knappt två av tre",
    procent >= 66.6 - tolerans & procent <= 66.6 + tolerans ~ "två av tre",
    procent > 66.6 + tolerans & procent < 70 - tolerans ~ "drygt två av tre",
    procent >= 70 - tolerans & procent < 75 - tolerans ~ "knappt tre av fyra",
    procent >= 75 - tolerans & procent <= 75 + tolerans ~ "tre av fyra",
    procent > 75 + tolerans & procent < 77.5 - tolerans ~ "drygt tre av fyra",
    procent >= 77.5 - tolerans & procent < 80 - tolerans ~ "knappt fyra av fem",
    procent >= 80 - tolerans & procent <= 80 + tolerans ~ "fyra av fem",
    procent > 80 + tolerans & procent < 85 - tolerans ~ "drygt fyra av fem",
    procent >= 85 - tolerans & procent < 90 - tolerans ~ "knappt nio av tio",
    procent >= 90 - tolerans & procent <= 90 + tolerans ~ "nio av tio",
    procent >= 90 + tolerans & procent < 95 + tolerans ~ "drygt nio av tio",
    procent >= 95 ~ "nästan alla",
    procent == 100 ~ "alla",
    TRUE ~ paste0(procent, "%")
  )
}

#' Beskriv en förändring mellan två värden i text
#'
#' `forandring_till_text(100, 150)` ger `"ökat väsentligt"`.
#'
#' @param varde1 Startvärde.
#' @param varde2 Slutvärde.
#'
#' @return En sträng som beskriver förändringen.
#' @export
forandring_till_text <- function(varde1, varde2) {
  procent_skillnad <- ((varde2 - varde1) / abs(varde1)) * 100

  dplyr::case_when(
    abs(procent_skillnad) <= 1 ~ "varit oförändrad",
    procent_skillnad > 1 & procent_skillnad <= 5 ~ "ökat något",
    procent_skillnad < -1 & procent_skillnad >= -5 ~ "minskat något",
    procent_skillnad > 5 & procent_skillnad <= 10 ~ "ökat",
    procent_skillnad < -5 & procent_skillnad >= -10 ~ "minskat",
    procent_skillnad > 10 & procent_skillnad <= 20 ~ "ökat väsentligt",
    procent_skillnad < -10 & procent_skillnad >= -20 ~ "minskat väsentligt",
    procent_skillnad > 20 & procent_skillnad <= 90 ~ "ökat mycket",
    procent_skillnad < -20 & procent_skillnad >= -90 ~ "minskat mycket",
    procent_skillnad > 90 & procent_skillnad <= 110 ~ "fördubblats",
    procent_skillnad < -90 & procent_skillnad >= -110 ~ "halverats",
    procent_skillnad > 110 & procent_skillnad <= 190 ~ "mer än fördubblats",
    procent_skillnad < -110 & procent_skillnad >= -190 ~ "minskat med mer än hälften",
    procent_skillnad > 190 & procent_skillnad <= 210 ~ "tredubblats",
    procent_skillnad < -190 & procent_skillnad >= -210 ~ "minskat till en tredjedel",
    procent_skillnad > 210 & procent_skillnad <= 990 ~ paste0("ökat ", round(procent_skillnad / 100, 1), " gånger"),
    procent_skillnad < -210 & procent_skillnad >= -990 ~ paste0("minskat till ", round(100 / abs(procent_skillnad), 1), " procent"),
    procent_skillnad >= 990 ~ "ökat mycket kraftigt",
    procent_skillnad <= -990 ~ "minskat mycket kraftigt",
    TRUE ~ "okänd förändring"
  )
}
