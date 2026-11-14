# En logg över det som gått fel när presentationen byggts (ett diagram som hoppats över och varför). Loggen
# ligger i presentationsobjektet (ppt$logg) och skrivs också fortlöpande till en textfil om en sådan angetts
# (ppt_ny(loggfil = ) eller options(rdppt.loggfil)), så att den finns kvar även om bygget avbryts.

intern_tom_logg <- function() {
  data.frame(tid = character(0), steg = character(0), region = character(0),
             status = character(0), meddelande = character(0), stringsAsFactors = FALSE)
}

# status: "fel" (diagrammet hoppades över), "inga diagram" eller "hoppad" (inget att köra för - inte ett fel)
intern_logga <- function(ppt, steg, region, status, meddelande) {
  rad <- data.frame(
    tid = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    steg = steg,
    region = if (length(region) == 0 || all(is.na(region))) "" else paste(region, collapse = ","),
    status = status,
    meddelande = gsub("\\s+", " ", meddelande),
    stringsAsFactors = FALSE
  )
  ppt$logg <- rbind(ppt$logg, rad)
  if (!is.null(ppt$loggfil) && status != "hoppad") {
    try(cat(paste0(rad$tid, " - ", steg, if (nzchar(rad$region)) paste0(" (", rad$region, ")"), " ", status, ": ", rad$meddelande, "\n"),
            file = ppt$loggfil, append = TRUE), silent = TRUE)
  }
  ppt
}

# Anteckningar kan vara en text eller en funktion av regionen (anteckningar = \(region) ...)
intern_anteckningar_for <- function(anteckningar, region) {
  if (!is.function(anteckningar)) return(anteckningar)
  if (length(formals(anteckningar)) == 0) anteckningar() else anteckningar(region)
}

# Berättar vid sparandet vad som hoppats över
intern_logg_sammanfatta <- function(ppt) {
  problem <- ppt$logg[ppt$logg$status %in% c("fel", "inga diagram"), , drop = FALSE]
  if (nrow(problem) == 0) return(invisible(NULL))
  rader <- paste0("  - ", problem$steg, if_else_region(problem$region), ": ", problem$meddelande)
  message("OBS: ", nrow(problem), " ", if (nrow(problem) == 1) "diagram saknas" else "diagram saknas",
          " i presentationen:\n", paste(rader, collapse = "\n"),
          if (!is.null(ppt$loggfil)) paste0("\nLogg: ", ppt$loggfil))
  invisible(problem)
}

if_else_region <- function(region) ifelse(nzchar(region), paste0(" (", region, ")"), "")
