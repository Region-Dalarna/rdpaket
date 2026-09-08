# Anslutningar mot Region Dalarnas PostgreSQL-servrar.
#
# Standardvärden läses via getOption("rdpostgres.*") så att varje dator kan
# sätta egna i .Rprofile.

intern_opt <- function(namn, default) getOption(paste0("rdpostgres.", namn), default = default)

intern_db_host    <- function() intern_opt("db_host", "WFALMITVS526.ltdalarna.se")
intern_db_port    <- function() intern_opt("db_port", 5432)
intern_db_options <- function() intern_opt("db_options", "-c search_path=public")

# Dölj ett specifikt (ofarligt) varningsmeddelande.
intern_tyst_varning <- function(expr, monster) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (grepl(monster, conditionMessage(w), fixed = TRUE)) invokeRestart("muffleWarning")
    }
  )
}

#' Koppla upp mot en PostgreSQL-databas
#'
#' Använd med standardvärden eller egna parametrar. Om `service_name` anges
#' hämtas användarnamn och lösenord ur keyring, annars används `geodata_las`.
#' Körs funktionen på databasservern själv byts värdnamnet mot `localhost`.
#'
#' @param db_name Databasnamn.
#' @param service_name keyring-service för användarnamn/lösenord, eller `NA`.
#' @param db_host,db_port,db_options Anslutningsparametrar. Standard via
#'   `getOption("rdpostgres.db_host")` m.fl.
#' @param db_user,db_password Sätts normalt via `service_name`; kan anges direkt.
#'
#' @return Ett `DBIConnection`-objekt, eller `NULL` om anslutningen misslyckas.
#' @export
uppkoppling_db <- function(db_name      = "geodata",
                           service_name = NA,
                           db_host      = intern_db_host(),
                           db_port      = intern_db_port(),
                           db_options   = intern_db_options(),
                           db_user      = NA,
                           db_password  = NA) {

  if (!is.na(service_name)) {
    if (!requireNamespace("keyring", quietly = TRUE)) {
      stop("Paketet 'keyring' krävs när service_name anges.", call. = FALSE)
    }
    anv <- keyring::key_list(service = service_name)$username
    if (is.na(db_user))     db_user     <- anv
    if (is.na(db_password)) db_password <- keyring::key_get(service_name, anv)
  } else {
    if (is.na(db_user))     db_user     <- "geodata_las"
    if (is.na(db_password)) db_password <- "geodata_las"
  }

  current_hostname <- Sys.info()[["nodename"]]
  if (!is.na(db_host) && grepl(toupper(current_hostname), toupper(db_host), fixed = TRUE)) {
    db_host <- "localhost"
  } else if (is.na(db_host)) {
    db_host <- intern_db_host()
  }

  tryCatch(
    intern_tyst_varning(
      DBI::dbConnect(
        RPostgres::Postgres(),
        bigint   = "integer",
        user     = db_user,
        password = db_password,
        host     = db_host,
        port     = db_port,
        dbname   = db_name,
        options  = db_options
      ),
      "Invalid time zone 'UTC', falling back to local time."
    ),
    error = function(e) {
      message("Ett fel inträffade vid anslutning till databasen: ", conditionMessage(e))
      NULL
    }
  )
}

#' Administrativ uppkoppling mot en PostgreSQL-databas
#'
#' Som [uppkoppling_db()] men med keyring-service `databas_adm`.
#'
#' @param databas Databasnamn.
#' @param db_host,db_port,db_options Anslutningsparametrar.
#' @param db_user,db_password Normalt via keyring; kan anges direkt.
#'
#' @return Ett `DBIConnection`-objekt, eller `NULL`.
#' @export
uppkoppling_adm <- function(databas    = "geodata",
                            db_host    = intern_db_host(),
                            db_port    = intern_db_port(),
                            db_options = intern_db_options(),
                            db_user    = NA,
                            db_password = NA) {
  uppkoppling_db(db_name = databas, service_name = "databas_adm",
                 db_host = db_host, db_port = db_port, db_options = db_options,
                 db_user = db_user, db_password = db_password)
}

# Löser upp con-argumentet: "default" -> ny anslutning (som ska stängas).
# Returnerar list(con, egen) där egen = TRUE om anslutningen skapades här.
intern_con <- function(con, adm = FALSE) {
  if (is.character(con) && length(con) == 1 && con == "default") {
    ny <- if (adm) uppkoppling_adm() else uppkoppling_db()
    if (is.null(ny)) stop("Kunde inte skapa uppkoppling.", call. = FALSE)
    list(con = ny, egen = TRUE)
  } else {
    list(con = con, egen = FALSE)
  }
}

intern_stang <- function(c) {
  if (isTRUE(c$egen) && DBI::dbIsValid(c$con)) DBI::dbDisconnect(c$con)
}
