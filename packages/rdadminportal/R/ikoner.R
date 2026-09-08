# Ikonkopplingar för appar/rapporter på landningssidan (Tabler-ikoner).

#' Kurerad lista med relevanta Tabler-ikoner
#'
#' Hela biblioteket (>4500 ikoner) går att bläddra på <https://tabler.io/icons>;
#' [landningssida_ikoner_koppla()] accepterar vilken giltig `ti-*`-klass som helst.
#'
#' @return En `data.frame` med `ikon` och `beskrivning`.
#' @export
landningssida_ikoner_lista_tillgangliga <- function() {
  kurerad <- c(
    "ti-map"        = "Karta / geografi",
    "ti-users"      = "Befolkning / grupper",
    "ti-school"     = "Utbildning / skola",
    "ti-shield"     = "Brott / säkerhet",
    "ti-briefcase"  = "Näringsliv / arbete",
    "ti-building"   = "Organisation / myndighet",
    "ti-chart-bar"  = "Statistik / analys (stapeldiagram)",
    "ti-chart-line" = "Statistik / analys (linjediagram)",
    "ti-chart-pie"  = "Statistik / analys (cirkeldiagram)",
    "ti-virus"      = "Epidemiologi / hälsa",
    "ti-heart"      = "Hälsa / vård",
    "ti-home"       = "Bostad / hushåll",
    "ti-car"        = "Transport / trafik",
    "ti-bus"        = "Kollektivtrafik",
    "ti-leaf"       = "Miljö / hållbarhet",
    "ti-coin"       = "Ekonomi",
    "ti-file-text"  = "Rapport / dokument (standard för rapporter)",
    "ti-book"       = "Utredning / kunskap",
    "ti-calendar"   = "Tidsserie / prognos",
    "ti-database"   = "Data / register",
    "ti-globe"      = "Internationellt / omvärld",
    "ti-tool"       = "Verktyg / admin",
    "ti-settings"   = "Inställningar / konfiguration",
    "ti-apps"       = "Övrigt (standard för appar)"
  )
  df <- data.frame(ikon = names(kurerad), beskrivning = unname(kurerad),
                   stringsAsFactors = FALSE)
  message("Kurerad lista (", nrow(df), " st) - full bläddring: https://tabler.io/icons")
  df
}

#' Öppna Tabler-ikonbiblioteket i webbläsaren
#' @return Osynligt `NULL`.
#' @export
landningssida_ikoner_bladdra <- function() {
  utils::browseURL("https://tabler.io/icons")
  invisible(NULL)
}

# Giltiga ikonklasser hämtas en gång per session från Tablers webfont-CSS och
# cachas. Vid nätverksfel tillåts klassen optimistiskt (med varning).
.ikon_cache <- new.env(parent = emptyenv())

intern_giltiga_ikonklasser <- function() {
  if (!is.null(.ikon_cache$klasser)) return(.ikon_cache$klasser)
  css_url <- "https://cdn.jsdelivr.net/npm/@tabler/icons-webfont@latest/dist/tabler-icons.min.css"
  css <- tryCatch(paste(readLines(css_url, warn = FALSE), collapse = "\n"),
                  error = function(e) NULL)
  if (is.null(css)) return(NULL)
  traffar <- regmatches(css, gregexpr("\\.ti-[a-z0-9-]+(?=:before)", css, perl = TRUE))[[1]]
  .ikon_cache$klasser <- unique(sub("^\\.", "", traffar))
  .ikon_cache$klasser
}

intern_ikon_giltig <- function(ikon) {
  giltiga <- intern_giltiga_ikonklasser()
  if (is.null(giltiga)) {
    warning("Kunde inte hämta Tablers ikonlista (nätverksfel) - tillåter '", ikon,
            "' optimistiskt.", call. = FALSE)
    return(TRUE)
  }
  ikon %in% giltiga
}

#' Lista manuella ikonkopplingar (inte standardgissningar)
#'
#' @param target `"publik"` eller `"intern"`.
#' @return En `data.frame` med `namn` och `ikon`.
#' @export
landningssida_ikoner_lista <- function(target = c("publik", "intern")) {
  target <- intern_validera_target(match.arg(target))

  df <- if (target == "publik") {
    con <- intern_con_las(); on.exit(DBI::dbDisconnect(con), add = TRUE)
    DBI::dbGetQuery(con, "
      SELECT namn, ikon FROM adminshiny.landningssida_ikon
      WHERE server = 'publik' ORDER BY namn")
  } else {
    rader <- intern_lokalt_anrop("/usr/local/bin/hantera_ikon.sh", "lista_overrides")
    rader <- rader[grepl("=", rader)]
    if (length(rader) == 0) {
      data.frame(namn = character(0), ikon = character(0))
    } else {
      delar <- strsplit(rader, "=")
      data.frame(namn = vapply(delar, `[`, character(1), 1),
                 ikon = vapply(delar, `[`, character(1), 2),
                 stringsAsFactors = FALSE)
    }
  }

  if (nrow(df) == 0) message("Inga manuella ikonkopplingar för ", target, ".")
  df
}

#' Koppla en ikon till en app eller rapport
#'
#' @param target `"publik"` eller `"intern"`.
#' @param namn Mappnamnet.
#' @param ikon En giltig Tabler-ikonklass, t.ex. `"ti-map"`.
#' @param andrad_av Valfri identifierare (loggas i DB, bara `"publik"`).
#' @return Osynligt `TRUE`.
#' @export
landningssida_ikoner_koppla <- function(target = c("publik", "intern"), namn, ikon,
                                        andrad_av = NA_character_) {
  target <- intern_validera_target(match.arg(target))
  stopifnot(is.character(namn), length(namn) == 1, nzchar(namn))
  stopifnot(is.character(ikon), length(ikon) == 1, nzchar(ikon))
  if (!grepl("^ti-", ikon)) {
    stop("Ikonklassen måste börja med 'ti-' (t.ex. 'ti-map'). Fick: '", ikon, "'",
         call. = FALSE)
  }
  if (!intern_ikon_giltig(ikon)) {
    stop("Okänd ikonklass: '", ikon, "'. Kontrollera stavningen på https://tabler.io/icons.",
         call. = FALSE)
  }

  con <- intern_con_skriv(); on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "
    INSERT INTO adminshiny.landningssida_ikon (server, namn, ikon, satt_av)
    VALUES ($1, $2, $3, $4)
    ON CONFLICT (server, namn) DO UPDATE SET ikon = EXCLUDED.ikon, satt_tid = now()",
    params = list(target, namn, ikon, andrad_av))

  intern_efter_andring(target,
    paste0("Ikon kopplad (intern): ", namn, " -> ", ikon, "."),
    paste0("Ikon kopplad (publik): ", namn, " -> ", ikon, "."))
}

#' Ta bort en manuell ikonkoppling (återgår till standardgissningen)
#'
#' @param target `"publik"` eller `"intern"`.
#' @param namn Mappnamnet vars koppling ska tas bort.
#' @return Osynligt `TRUE`.
#' @export
landningssida_ikoner_ta_bort_koppling <- function(target = c("publik", "intern"), namn) {
  target <- intern_validera_target(match.arg(target))
  intern_validera_namn(namn)

  con <- intern_con_skriv(); on.exit(DBI::dbDisconnect(con), add = TRUE)
  borttagna <- 0
  for (n in namn) {
    borttagna <- borttagna + DBI::dbExecute(con, "
      DELETE FROM adminshiny.landningssida_ikon
      WHERE server = $1 AND namn = $2",
      params = list(target, n))
  }

  intern_efter_andring(target,
    paste0(borttagna, " ikonkoppling(ar) borttagna (intern)."),
    paste0(borttagna, " ikonkoppling(ar) borttagna (publik)."))
}
