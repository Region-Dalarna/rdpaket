#' Diagnostik för databaskonfiguration
#'
#' Listar vilka av de keyring-services som `uppkoppling_adm()` och
#' `rdgeorouting`s funktioner letar efter som faktiskt finns, och varnar om
#' standarduppkopplingen (`uppkoppling_db()` utan `service_name`) skulle falla
#' tillbaka på den inbyggda `geodata_las`/`geodata_las`-kopplingen. Listar även
#' `.Renviron`-lösenord via `rdshinyappar` om det paketet är installerat.
#'
#' @param services Keyring-services att leta efter. Standard: de som används
#'   av `rdpostgres` (`"databas_adm"`) och `rdgeorouting` (`"rd_geodata"`).
#'
#' @return Osynligt: en `data.frame` med `service` och `finns`.
#' @export
rdpostgres_auth_check <- function(services = c("databas_adm", "rd_geodata")) {
  cat("Databaskonfiguration\n")
  cat(strrep("-", nchar("Databaskonfiguration")), "\n")

  har_keyring <- requireNamespace("keyring", quietly = TRUE)
  if (!har_keyring) {
    message("Paketet 'keyring' är inte installerat - kan inte kolla keyring-services.")
  }

  finns <- stats::setNames(rep(NA, length(services)), services)
  if (har_keyring) {
    finns <- vapply(services, function(s) {
      tryCatch(nrow(keyring::key_list(service = s)) > 0, error = function(e) FALSE)
    }, logical(1))
  }

  for (i in seq_along(services)) {
    status <- if (is.na(finns[i])) "okänd (keyring saknas)" else if (finns[i]) "hittad" else "SAKNAS"
    cat(sprintf("  %-15s %s\n", services[i], status))
  }

  if (!har_keyring || !any(finns, na.rm = TRUE)) {
    message(
      "\nIngen keyring-service hittad. uppkoppling_db() utan service_name faller ",
      "tillbaka på den inbyggda kopplingen geodata_las/geodata_las - spara egna ",
      "uppgifter med keyring::key_set(\"<service>\", <användarnamn>)."
    )
  }

  if (requireNamespace("rdshinyappar", quietly = TRUE)) {
    tjanster <- tryCatch(rdshinyappar::shiny_list_passwords(), error = function(e) character(0))
    if (length(tjanster) > 0) {
      cat("\n.Renviron-lösenord (rdshinyappar::shiny_list_passwords()):\n")
      cat(paste0("  - ", tjanster, collapse = "\n"), "\n")
    }
  }

  invisible(data.frame(service = services, finns = as.vector(finns), stringsAsFactors = FALSE))
}
