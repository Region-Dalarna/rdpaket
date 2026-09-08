# ==============================================================================
# Telemetri (shiny.telemetry) - delad hjälpare för alla appar.
#
# skapa_telemetry() skapar ett Telemetry-objekt kopplat mot shiny_telemetry-
# schemat i sekretess-databasen. app_namn behöver BARA vara appens eget namn -
# servern ("_intern"/"_publik") läggs till automatiskt utifrån hostname, så att
# samma app-namn på båda servrarna aldrig blandas ihop i statistiken.
#
# Vid databasfel varnas det och NULL returneras i stället för att appen kraschar
# - en tillfällig DB-störning ska aldrig hindra en app från att starta.
# ==============================================================================

# Locale-oberoende svenska veckodagsnamn (ISO 8601: 1 = måndag ... 7 = söndag).
# weekdays() undviks medvetet - den är locale-beroende och gav engelska dagnamn
# på servern, vilket fick heatmap-mergen mot svenska dagnamn att missa helt.
intern_veckodag_namn <- c(
  "måndag", "tisdag", "onsdag", "torsdag", "fredag", "lördag", "söndag"
)

intern_sessionsstart_per_veckodag_timme <- function(rader) {
  ss <- stats::aggregate(time ~ session, data = rader, FUN = min)
  ss$veckodag <- intern_veckodag_namn[as.integer(strftime(ss$time, "%u"))]
  ss$timme <- as.integer(format(ss$time, "%H"))
  ss
}

#' Skapa ett Telemetry-objekt för en app
#'
#' Kopplar mot `shiny_telemetry`-schemat i sekretess-databasen. Server-suffix
#' (`_intern`/`_publik`) läggs till automatiskt utifrån värdnamnet.
#'
#' @param app_namn Appens eget namn, utan server-suffix (t.ex. `"brott"`).
#'
#' @return Ett `shiny.telemetry::Telemetry`-objekt, eller `NULL` om paketet
#'   saknas eller databasen inte går att nå.
#' @export
skapa_telemetry <- function(app_namn) {
  if (!requireNamespace("shiny.telemetry", quietly = TRUE)) {
    warning("Paketet shiny.telemetry är inte installerat - statistik loggas inte.")
    return(NULL)
  }

  vardnamn <- Sys.info()[["nodename"]]
  server_suffix <- if (grepl("^RP0003", vardnamn)) {
    "_intern"
  } else if (grepl("^wfalmitvs978", vardnamn)) {
    "_publik"
  } else {
    warning("Okänt värdnamn '", vardnamn, "' - kan inte avgöra server. ",
            "Använder app-namnet utan server-suffix.")
    ""
  }

  losenord <- tryCatch(shiny_get_password("shiny_skriv_telemetry"),
                       error = function(e) NA_character_)
  if (is.na(losenord)) {
    warning("Kunde inte hämta lösenord för shiny_skriv_telemetry - statistik loggas inte.")
    return(NULL)
  }

  data_storage <- tryCatch({
    shiny.telemetry::DataStoragePostgreSQL$new(
      username = "shiny_skriv_telemetry",
      password = losenord,
      hostname = "WFALMITVS526.ltdalarna.se",
      port     = 5432,
      dbname   = "sekretess",
      driver   = "RPostgres"
    )
  }, error = function(e) {
    warning("Kunde inte ansluta telemetri-databasen: ", conditionMessage(e))
    NULL
  })

  if (is.null(data_storage)) return(NULL)

  shiny.telemetry::Telemetry$new(
    app_name     = paste0(app_namn, server_suffix),
    data_storage = data_storage
  )
}

#' Wrapper för `use_telemetry()` med NULL-skydd
#'
#' Så att `ui.R` slipper `if`-satsen.
#'
#' @param telemetry Ett Telemetry-objekt från [skapa_telemetry()], eller `NULL`.
#'
#' @return UI-taggar från `shiny.telemetry::use_telemetry()`, eller `NULL`.
#' @export
telemetri_ui <- function(telemetry) {
  if (is.null(telemetry)) return(NULL)
  shiny.telemetry::use_telemetry()
}

#' Starta telemetri-loggning för en session
#'
#' Hämtar exkluderingsmönster från databasen (med hårdkodad fallback om den
#' inte går att nå), så alla appar kan uppdateras centralt utan kodändring.
#'
#' @param telemetry Ett Telemetry-objekt från [skapa_telemetry()], eller `NULL`.
#' @param navigation_id Input-id för navigeringen (flikbytet).
#' @param forsta_flik Namn på fliken som visas vid start. `NULL` = logga inte
#'   någon startflik.
#'
#' @return `NULL` osynligt.
#' @export
telemetri_server <- function(telemetry, navigation_id, forsta_flik = NULL) {
  if (is.null(telemetry)) return(invisible(NULL))

  regex <- tryCatch({
    con <- shiny_uppkoppling_las(db_name = "sekretess", db_user = "shiny_las_sekretess")
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    r <- DBI::dbGetQuery(con, "
      SELECT varde FROM shiny_telemetry.installningar WHERE nyckel = 'exkludera_inputs_regex'
    ")
    if (nrow(r) > 0) r$varde[1] else NA_character_
  }, error = function(e) NA_character_)

  if (is.na(regex)) {
    regex <- "(_hovered$|_zoom$|_center$|_bounds$|_mouseover$|_mouseout$|_selected$|_set$)"
  }

  telemetry$start_session(track_inputs = FALSE, navigation_input_id = navigation_id)
  telemetry$log_all_inputs(excluded_inputs_regex = regex, excluded_inputs = navigation_id)

  if (!is.null(forsta_flik)) {
    telemetry$log_navigation_manual(navigation_id, forsta_flik)
  }
  invisible(NULL)
}

#' Hämta aggregerad telemetristatistik för en app
#'
#' @param app_namn Appens namn UTAN server-suffix (t.ex. `"brott"`).
#' @param target `"publik"` eller `"intern"` - matchar server-suffixet som
#'   [skapa_telemetry()] lägger till.
#' @param fran,till `Date` som avgränsar perioden (`NULL` = ingen gräns).
#'
#' @return En lista med `antal_sessioner`, `antal_unika_anvandare`, `flikbesok`,
#'   `per_veckodag`, `per_timme` och `per_veckodag_timme`.
#' @export
hamta_telemetri_data <- function(app_namn, target = c("publik", "intern"), fran = NULL, till = NULL) {
  target <- match.arg(target)
  full_namn <- paste0(app_namn, "_", target)

  con <- shiny_uppkoppling_las(db_name = "sekretess", db_user = "shiny_las_sekretess")
  if (is.null(con)) stop("Kunde inte ansluta till databasen.", call. = FALSE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  villkor <- "app_name = $1"
  params <- list(full_namn)
  if (!is.null(fran)) {
    villkor <- paste(villkor, "AND time >= $2")
    params <- c(params, list(fran))
  }
  if (!is.null(till)) {
    villkor <- paste(villkor, paste0("AND time <= $", length(params) + 1))
    params <- c(params, list(till))
  }

  rader <- DBI::dbGetQuery(con, paste0("
    SELECT time, session, type, details
    FROM shiny_telemetry.event_log
    WHERE ", villkor, "
    ORDER BY time
  "), params = params)

  tom_tabell <- function(kol_namn) {
    df <- data.frame(matrix(ncol = length(kol_namn), nrow = 0))
    names(df) <- kol_namn
    df
  }

  if (nrow(rader) == 0) {
    return(list(
      antal_sessioner       = 0,
      antal_unika_anvandare = 0,
      flikbesok             = tom_tabell(c("flik", "antal")),
      per_veckodag          = tom_tabell(c("veckodag", "antal")),
      per_timme             = tom_tabell(c("timme", "antal")),
      per_veckodag_timme    = tom_tabell(c("veckodag", "timme", "antal"))
    ))
  }

  # ---- Antal sessioner ----
  antal_sessioner <- length(unique(rader$session))

  # ---- Unika användare (via login-eventens username) ----
  login_rader <- rader[rader$type == "login", ]
  anvandar_id <- if (nrow(login_rader) > 0) {
    vapply(login_rader$details, function(d) {
      parsed <- jsonlite::fromJSON(d)
      if (!is.null(parsed$username)) parsed$username[1] else NA_character_
    }, character(1))
  } else character(0)
  antal_unika_anvandare <- length(unique(anvandar_id[!is.na(anvandar_id)]))

  # ---- Flikbesök (navigation-events) ----
  nav_rader <- rader[rader$type == "navigation", ]
  flikar_valid <- character(0)
  if (nrow(nav_rader) > 0) {
    flikar <- vapply(nav_rader$details, function(d) {
      parsed <- jsonlite::fromJSON(d)
      if (!is.null(parsed$value) && length(parsed$value) > 0) parsed$value[1] else NA_character_
    }, character(1))
    flikar_valid <- flikar[!is.na(flikar)]
  }
  if (length(flikar_valid) == 0) {
    flikbesok <- tom_tabell(c("flik", "antal"))
  } else {
    tab <- table(flikar_valid)
    flikbesok <- data.frame(flik = names(tab), antal = as.integer(tab), stringsAsFactors = FALSE)
    flikbesok <- flikbesok[order(-flikbesok$antal), ]
  }

  # ---- Per veckodag / timme (baserat på unika sessionsstarter) ----
  sessionsstart <- intern_sessionsstart_per_veckodag_timme(rader)

  tab_v <- table(sessionsstart$veckodag)
  per_veckodag <- data.frame(veckodag = names(tab_v), antal = as.integer(tab_v), stringsAsFactors = FALSE)

  tab_t <- table(sessionsstart$timme)
  per_timme <- data.frame(timme = as.integer(names(tab_t)), antal = as.integer(tab_t), stringsAsFactors = FALSE)

  # ---- Kombinerad veckodag x timme (för per-app-heatmap) ----
  tab_vt <- as.data.frame(table(veckodag = sessionsstart$veckodag, timme = sessionsstart$timme))
  names(tab_vt) <- c("veckodag", "timme", "antal")
  tab_vt$timme <- as.integer(as.character(tab_vt$timme))
  per_veckodag_timme <- tab_vt[tab_vt$antal > 0, ]

  list(
    antal_sessioner       = antal_sessioner,
    antal_unika_anvandare = antal_unika_anvandare,
    flikbesok             = flikbesok,
    per_veckodag          = per_veckodag,
    per_timme             = per_timme,
    per_veckodag_timme    = per_veckodag_timme
  )
}

#' Lista alla app- och server-kombinationer som loggat telemetri
#'
#' Med grundläggande måttal - används för översiktstabellen i adminportalen.
#'
#' @return En `data.frame` med `app`, `server`, `antal_sessioner`,
#'   `antal_unika_anvandare`, `forsta_aktivitet`, `senaste_aktivitet`.
#' @export
hamta_telemetri_appar <- function() {
  con <- shiny_uppkoppling_las(db_name = "sekretess", db_user = "shiny_las_sekretess")
  if (is.null(con)) stop("Kunde inte ansluta till databasen.", call. = FALSE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  df <- DBI::dbGetQuery(con, "
    SELECT app_name,
           count(DISTINCT session) AS antal_sessioner,
           min(time) AS forsta_aktivitet,
           max(time) AS senaste_aktivitet
    FROM shiny_telemetry.event_log
    GROUP BY app_name
    ORDER BY app_name
  ")

  if (nrow(df) == 0) return(df)

  # app_name slutar alltid på _intern eller _publik (satt av skapa_telemetry())
  df$server <- ifelse(grepl("_intern$", df$app_name), "intern",
                      ifelse(grepl("_publik$", df$app_name), "publik", NA_character_))
  df$app <- sub("_(intern|publik)$", "", df$app_name)

  # Unika användare per app måste räknas separat (kräver JSON-parsning av login-events)
  unika <- vapply(df$app_name, function(namn) {
    login_rader <- DBI::dbGetQuery(con, "
      SELECT details FROM shiny_telemetry.event_log
      WHERE app_name = $1 AND type = 'login'
    ", params = list(namn))
    if (nrow(login_rader) == 0) return(0L)
    id <- vapply(login_rader$details, function(d) {
      p <- jsonlite::fromJSON(d)
      if (!is.null(p$username)) p$username[1] else NA_character_
    }, character(1))
    length(unique(id[!is.na(id)]))
  }, integer(1))
  df$antal_unika_anvandare <- unika

  df[, c("app", "server", "antal_sessioner", "antal_unika_anvandare",
         "forsta_aktivitet", "senaste_aktivitet")]
}

#' Aggregera sessionsstarter per veckodag och timme över alla appar
#'
#' För den samlade heatmapen på Statistik-flikens förstasida.
#'
#' @return En `data.frame` med `veckodag`, `timme`, `antal`.
#' @export
hamta_telemetri_heatmap_alla <- function() {
  con <- shiny_uppkoppling_las(db_name = "sekretess", db_user = "shiny_las_sekretess")
  if (is.null(con)) stop("Kunde inte ansluta till databasen.", call. = FALSE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  rader <- DBI::dbGetQuery(con, "SELECT time, session FROM shiny_telemetry.event_log")
  if (nrow(rader) == 0) return(data.frame(veckodag = character(0), timme = integer(0), antal = integer(0)))

  sessionsstart <- intern_sessionsstart_per_veckodag_timme(rader)

  agg <- as.data.frame(table(veckodag = sessionsstart$veckodag, timme = sessionsstart$timme))
  names(agg) <- c("veckodag", "timme", "antal")
  agg$timme <- as.integer(as.character(agg$timme))
  agg
}

#' Hämta de senaste rååhändelserna för en app
#'
#' Med tolkad, läsbar beskrivning i stället för rå JSON.
#'
#' @param app_namn Appens namn UTAN server-suffix.
#' @param target `"publik"` eller `"intern"`.
#' @param antal Antal händelser (senaste först).
#'
#' @return En `data.frame` med `time`, `session`, `type`, `beskrivning`.
#' @export
hamta_telemetri_handelser <- function(app_namn, target = c("publik", "intern"), antal = 200) {
  target <- match.arg(target)
  full_namn <- paste0(app_namn, "_", target)

  con <- shiny_uppkoppling_las(db_name = "sekretess", db_user = "shiny_las_sekretess")
  if (is.null(con)) stop("Kunde inte ansluta till databasen.", call. = FALSE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  rader <- DBI::dbGetQuery(con, "
    SELECT time, session, type, details
    FROM shiny_telemetry.event_log
    WHERE app_name = $1
    ORDER BY time DESC
    LIMIT $2
  ", params = list(full_namn, antal))

  if (nrow(rader) == 0) return(rader)

  rader$beskrivning <- vapply(seq_len(nrow(rader)), function(i) {
    typ <- rader$type[i]
    parsed <- tryCatch(jsonlite::fromJSON(rader$details[i]), error = function(e) NULL)
    if (is.null(parsed)) return(rader$details[i])

    switch(typ,
           "navigation" = paste("Flik:", parsed$value[1]),
           "input"      = paste("Ändrade:", parsed$id[1]),
           "login"      = "Ny session (anonym)",
           "browser"    = paste("Webbläsare:", parsed$value[1]),
           rader$details[i]
    )
  }, character(1))

  rader[, c("time", "session", "type", "beskrivning")]
}
