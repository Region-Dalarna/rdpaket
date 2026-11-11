# Synk av landningssida_app mot det verkliga filsystemet på servern.

#' Synka app-listan mot filsystemet på DENNA server
#'
#' Körs lokalt (aldrig över nätverk) - vid deploy/avpublicera och som nattligt
#' skyddsnät. Skriver bara app-existens och status, aldrig
#' exkludering/ikon-kopplingar.
#'
#' @param target `"publik"` eller `"intern"`.
#' @param shiny_rot,rapport_rot Kataloger att skanna.
#' @param pdf_mapp Undermapp i `rapport_rot` vars pdf-filer räknas som egna
#'   poster (typ `"pdf"`, namn `"<pdf_mapp>/<filnamn>"`). Samma mapp som
#'   landningssidans generator läser pdf-rapporter från.
#'
#' @return Osynligt: en lista med `funna`, `nya`, `borttagna`.
#' @export
landningssida_synka_app_lista <- function(target = c("publik", "intern"),
                                          shiny_rot   = "/srv/shiny-server",
                                          rapport_rot = "/srv/rapporter",
                                          pdf_mapp    = "pdf-rapporter") {
  target <- intern_validera_target(match.arg(target))
  hittade <- list()

  if (dir.exists(shiny_rot)) {
    m <- list.dirs(shiny_rot, recursive = FALSE, full.names = FALSE)
    m <- m[!startsWith(m, ".")]
    m <- m[vapply(m, function(x) any(file.exists(file.path(shiny_rot, x, c("ui.R", "app.R")))),
                  logical(1))]
    if (length(m)) hittade[[length(hittade) + 1]] <-
      data.frame(namn = m, typ = "app", stringsAsFactors = FALSE)
  }
  if (dir.exists(rapport_rot)) {
    m <- list.dirs(rapport_rot, recursive = FALSE, full.names = FALSE)
    m <- m[!startsWith(m, ".")]
    m <- m[file.exists(file.path(rapport_rot, m, "index.html"))]
    m <- setdiff(m, pdf_mapp)
    if (length(m)) hittade[[length(hittade) + 1]] <-
      data.frame(namn = m, typ = "rapport", stringsAsFactors = FALSE)

    pdf_filer <- intern_hitta_pdf_filer(rapport_rot, pdf_mapp)
    if (length(pdf_filer)) hittade[[length(hittade) + 1]] <-
      data.frame(namn = paste0(pdf_mapp, "/", pdf_filer), typ = "pdf", stringsAsFactors = FALSE)
  }

  funna <- if (length(hittade)) do.call(rbind, hittade) else
    data.frame(namn = character(0), typ = character(0))

  con <- intern_con_skriv(); on.exit(DBI::dbDisconnect(con), add = TRUE)

  tillagda <- 0
  for (i in seq_len(nrow(funna))) {
    res <- DBI::dbGetQuery(con, "
      INSERT INTO adminshiny.landningssida_app (server, namn, typ, status, senast_sedd)
      VALUES ($1, $2, $3, 'aktiv', now())
      ON CONFLICT (server, namn) DO UPDATE
        SET status = 'aktiv', senast_sedd = now(), typ = EXCLUDED.typ
      RETURNING (xmax = 0) AS ny_rad",
      params = list(target, funna$namn[i], funna$typ[i]))
    if (isTRUE(res$ny_rad)) tillagda <- tillagda + 1
  }

  aktiva <- DBI::dbGetQuery(con, "
    SELECT namn FROM adminshiny.landningssida_app
    WHERE server = $1 AND status = 'aktiv'", params = list(target))$namn
  saknas <- setdiff(aktiva, funna$namn)
  for (n in saknas) {
    DBI::dbExecute(con, "
      UPDATE adminshiny.landningssida_app
      SET status = 'borttagen', borttagen_tid = now()
      WHERE server = $1 AND namn = $2", params = list(target, n))
  }

  message("App-synk (", target, "): ", nrow(funna), " funna (", tillagda, " nya), ",
          length(saknas), " markerade borttagna.")
  invisible(list(funna = nrow(funna), nya = tillagda, borttagna = length(saknas)))
}

#' Samlad översikt över appar/rapporter med exkluderingsstatus och ikon
#'
#' @param target `"publik"` eller `"intern"`.
#' @param visa_borttagna Ta även med `status = 'borttagen'`.
#'
#' @return En `data.frame` med `namn`, `typ`, `status`, `senast_sedd`,
#'   `exkluderad`, `ikon`, `beskrivning`, `ordning` (handpåförd ordning, `NA` om
#'   ingen satt), sorterad som på landningssidan (appar, webbrapporter, pdf).
#' @export
landningssida_app_oversikt <- function(target = c("publik", "intern"),
                                       visa_borttagna = FALSE) {
  target <- intern_validera_target(match.arg(target))
  con <- intern_con_las(); on.exit(DBI::dbDisconnect(con), add = TRUE)

  status_villkor <- if (visa_borttagna) "" else "AND a.status = 'aktiv'"

  # Handpåförd ordning (adminshiny.landningssida_ordning) - tabellen behöver inte finnas ännu.
  har_ordning <- intern_ordning_tabell_finns(con)
  ordning_kol  <- if (har_ordning) "o.ordning" else "NULL::integer AS ordning"
  ordning_join <- if (har_ordning) "LEFT JOIN adminshiny.landningssida_ordning o ON o.server = a.server AND o.namn = a.namn" else ""
  ordning_sort <- if (har_ordning) "o.ordning NULLS LAST, " else ""

  # Samma ordning som på landningssidan: appar, webbrapporter, pdf-rapporter; inom en grupp handpåförd
  # ordning först och därefter alfabetiskt.
  DBI::dbGetQuery(con, sprintf("
    SELECT a.namn, a.typ, a.status, a.senast_sedd,
           (e.namn IS NOT NULL) AS exkluderad, i.ikon, b.beskrivning, %s
    FROM adminshiny.landningssida_app a
    LEFT JOIN adminshiny.landningssida_exkludering e  ON e.server = a.server AND e.namn = a.namn
    LEFT JOIN adminshiny.landningssida_ikon i         ON i.server = a.server AND i.namn = a.namn
    LEFT JOIN adminshiny.landningssida_beskrivning b  ON b.server = a.server AND b.namn = a.namn
    %s
    WHERE a.server = $1 %s
    ORDER BY CASE a.typ WHEN 'app' THEN 1 WHEN 'rapport' THEN 2 ELSE 3 END, %sa.namn",
    ordning_kol, ordning_join, status_villkor, ordning_sort), params = list(target))
}

#' Begär en synk av app-listan nu
#'
#' `"intern"` synkar direkt; `"publik"` köar ett kommando som
#' adminportal-lyssnaren plockar upp.
#'
#' @param target `"publik"` eller `"intern"`.
#' @param andrad_av Valfri identifierare (loggas i DB, bara `"publik"`).
#' @return Osynligt `TRUE`.
#' @export
landningssida_synka_app_lista_nu <- function(target = c("publik", "intern"),
                                             andrad_av = NA_character_) {
  target <- intern_validera_target(match.arg(target))

  if (target == "intern") {
    landningssida_synka_app_lista(target = "intern")
    message("App-listan (intern) är synkad.")
  } else {
    con <- intern_con_skriv(); on.exit(DBI::dbDisconnect(con), add = TRUE)
    DBI::dbExecute(con, "
      INSERT INTO adminshiny.kommando (server, typ, payload, skapad_av)
      VALUES ('publik', 'synka_app_lista', '{}', $1)", params = list(andrad_av))
    message("Synk av app-listan (publik) är köad - klar inom några sekunder.")
  }
  invisible(TRUE)
}

# Pdf-filerna (bara filnamnen) i rapport_rot/pdf_mapp. Tom vektor om mappen saknas.
#' @noRd
intern_hitta_pdf_filer <- function(rapport_rot, pdf_mapp) {
  mapp <- file.path(rapport_rot, pdf_mapp)
  if (!dir.exists(mapp)) return(character(0))
  filer <- list.files(mapp, pattern = "\\.pdf$", ignore.case = TRUE, full.names = FALSE)
  sort(filer[!startsWith(filer, ".")])
}
