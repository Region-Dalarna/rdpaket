# Områdesnycklar: exportera tabellen till mikrodatabasformat.

#' Exportera en områdesnyckeltabell till csv (uppdelad i delar vid behov)
#'
#' @param con En aktiv `DBIConnection`.
#' @param omradesnyckel_schema,omradesnyckel_tabell Tabellen som exporteras.
#' @param max_chunk_stlk_mb Max filstorlek per del.
#' @param filnamn Basnamn (`NULL` = tabellnamnet).
#' @param output_mapp Målmapp (`NULL` = `rdverktyg::utskriftsmapp()`).
#' @param kolumner_namn Namngiven vektor `nytt = "gammalt"` för att döpa om
#'   kolumner.
#' @param kolumner_lista `TRUE` = lista bara kolumnerna (och lägg en
#'   `kolumner_namn`-mall i urklipp), gör ingen export.
#'
#' @return Osynligt `NULL` (eller kolumnvektorn om `kolumner_lista = TRUE`).
#' @export
postgis_omradesnyckel_exportera_till_mikrodb <- function(con,
                                                         omradesnyckel_schema = "omradesnycklar",
                                                         omradesnyckel_tabell,
                                                         max_chunk_stlk_mb = 10,
                                                         filnamn = NULL,
                                                         output_mapp = NULL,
                                                         kolumner_namn = NULL,
                                                         kolumner_lista = FALSE) {
  intern_krav("data.table")

  if (kolumner_lista) {
    kolumner <- DBI::dbGetQuery(con, glue::glue_sql(
      "SELECT column_name FROM information_schema.columns
       WHERE table_schema = {omradesnyckel_schema} AND table_name = {omradesnyckel_tabell}
       ORDER BY ordinal_position", .con = con))$column_name
    mall <- paste0('kolumner_namn = c(', paste0(kolumner, ' = "', kolumner, '"', collapse = ", "), ')')
    message("Kolumner i ", omradesnyckel_schema, ".", omradesnyckel_tabell, ":\n  ",
            paste(kolumner, collapse = "\n  "))
    message("\nMall (nytt = \"gammalt\"):\n", mall)
    if (requireNamespace("clipr", quietly = TRUE) && clipr::clipr_available()) clipr::write_clip(mall)
    return(invisible(stats::setNames(kolumner, kolumner)))
  }

  if (is.null(output_mapp)) {
    output_mapp <- if (requireNamespace("rdverktyg", quietly = TRUE)) rdverktyg::utskriftsmapp() else getwd()
  }
  output_mapp <- sub("/?$", "/", output_mapp)
  if (is.null(filnamn)) filnamn <- omradesnyckel_tabell
  filnamn <- sub("\\.csv$", "", filnamn)

  message("Hämtar data från ", omradesnyckel_schema, ".", omradesnyckel_tabell, " ...")
  df <- DBI::dbGetQuery(con, glue::glue_sql(
    "SELECT * FROM {`omradesnyckel_schema`}.{`omradesnyckel_tabell`}", .con = con))
  if (!is.null(kolumner_namn)) df <- dplyr::rename(df, dplyr::any_of(kolumner_namn))

  tmp <- file.path(output_mapp, "._tempfil.csv")
  data.table::fwrite(df, file = tmp)
  total_bytes <- file.size(tmp)
  unlink(tmp)

  rader_per_chunk <- floor((max_chunk_stlk_mb * 1e6 * 0.9) / (total_bytes / nrow(df)))
  antal_chunks <- ceiling(nrow(df) / rader_per_chunk)

  if (antal_chunks < 2) {
    data.table::fwrite(df, file = file.path(output_mapp, paste0(filnamn, ".csv")))
  } else {
    message("Datasetet är ", format(total_bytes / 1e6, digits = 2), " MB - delas upp i ", antal_chunks, " delar.")
    df$.chunk <- ceiling(seq_len(nrow(df)) / rader_per_chunk)
    for (k in seq_len(antal_chunks)) {
      del <- df[df$.chunk == k, setdiff(names(df), ".chunk")]
      data.table::fwrite(del, file = file.path(output_mapp, sprintf("%s_%02d.csv", filnamn, k)))
    }
  }
  message("Klart! ", antal_chunks, " fil(er) sparade i ", output_mapp)
  invisible(NULL)
}
