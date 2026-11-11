# Delade hjälpare.
#
# Två lägen genomgående:
#   "publik"  -> skriv till adminshiny-tabellerna i sekretess-databasen;
#                adminportal-lyssnaren synkar till servern inom sekunder.
#   "intern"  -> direkta lokala anrop (körs redan på RP0003).

intern_validera_target <- function(target) {
  target <- target[1]
  if (!target %in% c("publik", "intern")) {
    stop("Ogiltigt värde för target: '", target, "'. Måste vara 'publik' eller 'intern'.",
         call. = FALSE)
  }
  target
}

intern_validera_namn <- function(namn) {
  if (!is.character(namn) || length(namn) == 0 || any(!nzchar(namn))) {
    stop("namn måste vara en icke-tom teckenvektor.", call. = FALSE)
  }
  ogiltiga <- namn[!grepl("^[a-zA-Z0-9_-]+$", namn)]
  if (length(ogiltiga) > 0) {
    stop("Ogiltiga app-namn (bara bokstäver/siffror/_/- tillåtna): ",
         paste(ogiltiga, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

# Namn på poster på landningssidan (exkludering, ikon, beskrivning): ett app-/rapportnamn enligt
# intern_validera_namn(), eller en pdf-rapport "<pdf_mapp>/<filnamn>.pdf". Filnamnet får innehålla
# bokstäver (även å, ä, ö), siffror, mellanslag och vanliga skiljetecken, men inga snedstreck,
# kontrolltecken eller "..". Namnen skickas alltid som parametrar till SQL och blir aldrig en sökväg.
# Nedladdningsfunktionerna (som gör namnet till en sökväg) använder den strängare intern_validera_namn().
#' @noRd
intern_validera_landningssidenamn <- function(namn, pdf_mapp = "pdf-rapporter") {
  if (!is.character(namn) || length(namn) == 0 || any(is.na(namn)) || any(!nzchar(namn))) {
    stop("namn måste vara en icke-tom teckenvektor.", call. = FALSE)
  }
  pdf_monster <- paste0("^", pdf_mapp, "/[^/\\\\[:cntrl:]]+\\.pdf$")
  ar_pdf <- grepl(pdf_monster, namn, ignore.case = TRUE) & !grepl("\\.\\.", namn)
  ar_vanlig <- grepl("^[a-zA-Z0-9_-]+$", namn)
  ogiltiga <- namn[!(ar_pdf | ar_vanlig)]
  if (length(ogiltiga) > 0) {
    stop("Ogiltiga namn (bara bokstäver/siffror/_/- eller en pdf-rapport '", pdf_mapp,
         "/<filnamn>.pdf' tillåtna): ", paste(ogiltiga, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

# DB-anslutning (läs/skriv) mot sekretess-databasen via rdshinyappar.
intern_con_las <- function() {
  con <- rdshinyappar::shiny_uppkoppling_las(db_name = "sekretess",
                                             db_user = "shiny_las_sekretess")
  if (is.null(con)) stop("Kunde inte ansluta till sekretess-databasen (läs).", call. = FALSE)
  con
}

intern_con_skriv <- function() {
  con <- rdshinyappar::shiny_uppkoppling_skriv(db_name = "sekretess",
                                               db_user = "shiny_skriv_sekretess")
  if (is.null(con)) stop("Kunde inte ansluta till sekretess-databasen (skriv).", call. = FALSE)
  con
}

# Lokalt sudo-anrop (bara "intern").
intern_lokalt_anrop <- function(skript, args = character(0)) {
  resultat <- system2("sudo", args = c(skript, args), stdout = TRUE, stderr = TRUE)
  status <- attr(resultat, "status")
  if (!is.null(status) && status != 0) {
    stop("Lokalt anrop misslyckades (", skript, "):\n",
         paste(resultat, collapse = "\n"), call. = FALSE)
  }
  resultat
}

# Kör om landningssidan lokalt (bara "intern").
intern_regenerera_landningssida <- function() {
  resultat <- system2("/usr/local/bin/generera_landningssida.sh", stdout = TRUE, stderr = TRUE)
  status <- attr(resultat, "status")
  if (!is.null(status) && status != 0) {
    stop("generera_landningssida.sh misslyckades: ", paste(resultat, collapse = "\n"),
         call. = FALSE)
  }
  invisible(resultat)
}

# Efter en ändring: regenerera (intern) eller informera om automatisk synk (publik).
intern_efter_andring <- function(target, meddelande_intern, meddelande_publik) {
  if (target == "intern") {
    intern_regenerera_landningssida()
    message(meddelande_intern, " Landningssidan är regenererad.")
  } else {
    message(meddelande_publik, " Ändringen synkas automatiskt inom några sekunder.")
  }
  invisible(TRUE)
}
