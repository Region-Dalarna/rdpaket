# Exekvering av ETT schemalagt datauttag (cron-jobb).

intern_cron_metadata_db      <- "sekretess"
intern_cron_metadata_db_user <- "shiny_skriv_sekretess"

# uppkoppling i cron_jobb -> db_user för källdatabasen
intern_cron_db_users <- c(
  "standard"  = "shiny_las",
  "sekretess" = "shiny_las_sekretess"
)

#' Kör ett cron-jobb
#'
#' Hämtar data enligt jobbets definition i `adminshiny.cron_jobb`, skriver
#' resultatfilen till appens `www/nedladdning/`-mapp och uppdaterar
#' `senast_kord`/`senast_status`.
#'
#' Format: `"csv"`, `"xlsx"` (kräver `writexl`), `"csv_zip"`, `"gpkg"` (kräver
#' `sf`).
#'
#' @param jobb_id Id i `adminshiny.cron_jobb`.
#'
#' @return Osynligt: statussträngen (`"OK"` eller `"FEL: ..."`).
#' @export
kor_cron_jobb <- function(jobb_id) {
  logga <- function(...) cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-", ..., "\n")

  con_meta <- rdshinyappar::shiny_uppkoppling_skriv(
    db_name = intern_cron_metadata_db, db_user = intern_cron_metadata_db_user)
  if (is.null(con_meta)) {
    stop("Kunde inte ansluta till metadatadatabasen '", intern_cron_metadata_db, "'.",
         call. = FALSE)
  }
  on.exit(try(DBI::dbDisconnect(con_meta), silent = TRUE), add = TRUE)

  satt_status <- function(status) {
    DBI::dbExecute(con_meta,
      "UPDATE adminshiny.cron_jobb SET senast_kord = now(), senast_status = $1 WHERE id = $2",
      params = list(status, jobb_id))
  }

  resultat <- tryCatch({
    jobb <- DBI::dbGetQuery(con_meta,
      "SELECT * FROM adminshiny.cron_jobb WHERE id = $1", params = list(jobb_id))
    if (nrow(jobb) == 0) stop("Hittar inget jobb med id = ", jobb_id, call. = FALSE)
    jobb <- jobb[1, ]

    if (!jobb$uppkoppling %in% names(intern_cron_db_users)) {
      stop("Okänd eller ej tillåten uppkoppling: ", jobb$uppkoppling, call. = FALSE)
    }
    db_user_val <- intern_cron_db_users[[jobb$uppkoppling]]

    con_data <- rdshinyappar::shiny_uppkoppling_las(
      db_name = jobb$kalla_databas, db_user = db_user_val)
    if (is.null(con_data)) {
      stop("Kunde inte ansluta till '", jobb$kalla_databas, "' som '", db_user_val, "'.",
           call. = FALSE)
    }
    on.exit(try(DBI::dbDisconnect(con_data), silent = TRUE), add = TRUE)

    sql <- if (!is.na(jobb$egen_sql) && nzchar(trimws(jobb$egen_sql))) {
      jobb$egen_sql
    } else {
      sprintf("SELECT * FROM %s.%s",
              DBI::dbQuoteIdentifier(con_data, jobb$kalla_schema),
              DBI::dbQuoteIdentifier(con_data, jobb$kalla_tabell))
    }

    data <- if (jobb$format == "gpkg") {
      if (!requireNamespace("sf", quietly = TRUE)) stop("Paketet 'sf' krävs för gpkg.", call. = FALSE)
      sf::st_read(con_data, query = sql, quiet = TRUE)
    } else {
      DBI::dbGetQuery(con_data, sql)
    }

    malmapp <- file.path("/srv/shiny-server", jobb$app, "www", "nedladdning")
    if (!dir.exists(malmapp)) {
      if (!dir.create(malmapp, recursive = TRUE) || !dir.exists(malmapp)) {
        stop("Kunde inte skapa katalogen '", malmapp, "' - saknar appen '", jobb$app,
             "' skrivrättighet, eller finns den inte?", call. = FALSE)
      }
      logga("Skapade katalog:", malmapp)
    }

    bas <- tools::file_path_sans_ext(jobb$malfil)
    malfil_path <- switch(jobb$format,
      "csv" = {
        p <- file.path(malmapp, paste0(bas, ".csv"))
        utils::write.csv2(data, p, row.names = FALSE, fileEncoding = "UTF-8"); p
      },
      "xlsx" = {
        if (!requireNamespace("writexl", quietly = TRUE)) stop("Paketet 'writexl' krävs för xlsx.", call. = FALSE)
        p <- file.path(malmapp, paste0(bas, ".xlsx"))
        writexl::write_xlsx(data, p); p
      },
      "csv_zip" = {
        tmp_csv <- file.path(tempdir(), paste0(bas, ".csv"))
        utils::write.csv2(data, tmp_csv, row.names = FALSE, fileEncoding = "UTF-8")
        p <- file.path(malmapp, paste0(bas, ".zip"))
        if (file.exists(p)) file.remove(p)
        utils::zip(zipfile = p, files = tmp_csv, flags = "-j")
        file.remove(tmp_csv); p
      },
      "gpkg" = {
        p <- file.path(malmapp, paste0(bas, ".gpkg"))
        if (file.exists(p)) file.remove(p)
        sf::st_write(data, dsn = p, layer = bas, driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)
        p
      },
      stop("Okänt format: ", jobb$format, call. = FALSE)
    )

    logga("OK - skrev", nrow(data), "rader till", malfil_path)
    "OK"
  }, error = function(e) {
    logga("FEL:", conditionMessage(e))
    paste0("FEL: ", conditionMessage(e))
  })

  satt_status(resultat)
  invisible(resultat)
}
