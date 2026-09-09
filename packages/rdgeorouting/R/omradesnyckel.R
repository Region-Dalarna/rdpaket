# Områdesnycklar: koppla statistikrutor till områdespolygoner (störst
# överlappning) och exportera till mikrodatabas.

#' Skapa en tabell/vy som kopplar rutor till ett områdeslager
#'
#' Varje ruta kopplas till det område den överlappar mest (`DISTINCT ON` +
#' `ST_Area(ST_Intersection(...))`).
#'
#' @param con En aktiv `DBIConnection`.
#' @param omrades_schema,omrades_tabell Områdeslagret.
#' @param omrade_urval_kolumner Kolumner att ta med från områdeslagret (`NULL` =
#'   alla).
#' @param rut_schema,rut_tabell,rut_id_kol Rutlagret.
#' @param rut_urval_kolumner Kolumner att ta med från rutlagret (`NULL` = alla).
#' @param utdata_schema,utdata_vy Målschema och -namn (`NULL` = samma som
#'   `omrades_tabell`).
#' @param fran_rutid_till_x_y_kol Namngiven lista med två element `namn = c(start,
#'   slut)` för att skapa x/y-kolumner ur rut-id:t. `NULL` = behåll bara rutid.
#' @param databas_typ `"tabell"`, `"materialiserad_vy"` eller `"vy"`.
#' @param skriv_over_befintlig_tabell_vy Ta bort befintlig tabell/vy först.
#'
#' @return Osynligt: namnet på den skapade tabellen/vyn.
#' @export
postgis_skapa_omradesnyckel_tabell_vy <- function(con, omrades_schema, omrades_tabell,
                                                  omrade_urval_kolumner = NULL,
                                                  rut_schema = "rutor",
                                                  rut_tabell = "bo_syss_100m_alla_ar",
                                                  rut_id_kol = "rutid",
                                                  rut_urval_kolumner = NULL,
                                                  utdata_schema = "omradesnycklar",
                                                  utdata_vy = NULL,
                                                  fran_rutid_till_x_y_kol = list("Ruta100swX" = c(7, 13), "Ruta100swY" = c(1, 6)),
                                                  databas_typ = "vy",
                                                  skriv_over_befintlig_tabell_vy = TRUE) {
  if (is.null(utdata_vy)) utdata_vy <- omrades_tabell

  if (!DBI::dbExistsTable(con, DBI::Id(schema = rut_schema, table = rut_tabell))) {
    stop("Rutlagret '", rut_schema, ".", rut_tabell, "' hittades inte.", call. = FALSE)
  }
  if (!DBI::dbExistsTable(con, DBI::Id(schema = omrades_schema, table = omrades_tabell))) {
    stop("Områdeslagret '", omrades_schema, ".", omrades_tabell, "' hittades inte.", call. = FALSE)
  }

  vy_typ <- switch(databas_typ,
    "tabell" = "TABLE", "materialiserad_vy" = "MATERIALIZED VIEW", "vy" = "VIEW",
    stop("databas_typ måste vara 'tabell', 'materialiserad_vy' eller 'vy'.", call. = FALSE))
  or_replace <- if (databas_typ == "vy") "OR REPLACE " else ""

  rdpostgres::postgres_schema_skapa_om_inte_finns(con = con, schema_namn = utdata_schema)

  geom_kol_for <- function(s, t) {
    DBI::dbGetQuery(con, glue::glue_sql(
      "SELECT f_geometry_column FROM geometry_columns
       WHERE f_table_schema = {s} AND f_table_name = {t}", .con = con))$f_geometry_column
  }
  rut_geom_kol <- geom_kol_for(rut_schema, rut_tabell)
  omrades_geom_kol <- geom_kol_for(omrades_schema, omrades_tabell)

  rut_kolumner <- DBI::dbGetQuery(con, glue::glue_sql(
    "SELECT column_name FROM information_schema.columns
     WHERE table_schema = {rut_schema} AND table_name = {rut_tabell}
       AND column_name != {rut_geom_kol}", .con = con))$column_name
  if (!is.null(rut_urval_kolumner)) rut_kolumner <- intersect(rut_kolumner, rut_urval_kolumner)
  if (length(rut_kolumner) == 0) stop("Inga giltiga kolumner valda för rutlagret.", call. = FALSE)

  omrades_kolumner <- DBI::dbGetQuery(con, glue::glue_sql(
    "SELECT column_name FROM information_schema.columns
     WHERE table_schema = {omrades_schema} AND table_name = {omrades_tabell}
       AND column_name != {omrades_geom_kol}", .con = con))$column_name
  if (!is.null(omrade_urval_kolumner)) omrades_kolumner <- intersect(omrades_kolumner, omrade_urval_kolumner)
  if (length(omrades_kolumner) == 0) stop("Inga giltiga kolumner valda för områdeslagret.", call. = FALSE)

  # Validera fran_rutid_till_x_y_kol
  giltig_split <- !is.null(fran_rutid_till_x_y_kol) &&
    length(fran_rutid_till_x_y_kol) == 2 &&
    !is.null(names(fran_rutid_till_x_y_kol)) && all(nzchar(names(fran_rutid_till_x_y_kol))) &&
    all(vapply(fran_rutid_till_x_y_kol, function(x) is.numeric(x) && length(x) == 2, logical(1)))
  if (!is.null(fran_rutid_till_x_y_kol) && !giltig_split) {
    message("fran_rutid_till_x_y_kol är ogiltig - kör utan uppdelning av rutid.")
  }

  rut_select <- paste(paste0("r.", rut_kolumner), collapse = ",\n      ")
  omrades_select <- paste(paste0("o.", omrades_kolumner), collapse = ",\n      ")
  rutid_split <- if (giltig_split) {
    paste(mapply(function(namn, pos) glue::glue(
      ",\n      substring(r.{rut_id_kol}, {pos[1]}, {pos[2] - pos[1] + 1})::numeric AS {namn}"),
      names(fran_rutid_till_x_y_kol), fran_rutid_till_x_y_kol), collapse = "")
  } else ""

  select_sql <- glue::glue(
    'SELECT DISTINCT ON (r.{rut_id_kol})
      {rut_select},
      {omrades_select}{rutid_split}
      FROM {rut_schema}."{rut_tabell}" r
      JOIN {omrades_schema}."{omrades_tabell}" o
        ON ST_Intersects(r.{rut_geom_kol}, o.{omrades_geom_kol})
      ORDER BY r.{rut_id_kol},
        ST_Area(ST_Intersection(r.{rut_geom_kol}, o.{omrades_geom_kol})) DESC')

  vy_sql <- if (databas_typ == "tabell") {
    glue::glue('CREATE TABLE {utdata_schema}."{utdata_vy}" AS {select_sql};')
  } else {
    glue::glue('CREATE {or_replace}{vy_typ} {utdata_schema}."{utdata_vy}" AS {select_sql};')
  }

  if (skriv_over_befintlig_tabell_vy) {
    befintlig <- DBI::dbGetQuery(con, glue::glue_sql(
      "SELECT CASE
         WHEN EXISTS (SELECT 1 FROM pg_matviews WHERE schemaname = {utdata_schema} AND matviewname = {utdata_vy}) THEN 'materialiserad_vy'
         WHEN EXISTS (SELECT 1 FROM pg_views WHERE schemaname = {utdata_schema} AND viewname = {utdata_vy}) THEN 'vy'
         WHEN EXISTS (SELECT 1 FROM pg_tables WHERE schemaname = {utdata_schema} AND tablename = {utdata_vy}) THEN 'tabell'
         ELSE NULL END AS typ", .con = con))$typ
    if (!is.na(befintlig)) {
      drop_sql <- switch(befintlig,
        "tabell" = glue::glue('DROP TABLE {utdata_schema}."{utdata_vy}";'),
        "materialiserad_vy" = glue::glue('DROP MATERIALIZED VIEW {utdata_schema}."{utdata_vy}";'),
        "vy" = glue::glue('DROP VIEW {utdata_schema}."{utdata_vy}";'))
      message("Tar bort befintlig ", befintlig, ": ", utdata_schema, ".", utdata_vy)
      DBI::dbExecute(con, drop_sql)
    }
  }

  message("Skapar ", databas_typ, ": ", utdata_schema, ".", utdata_vy)
  DBI::dbExecute(con, vy_sql)
  message("Klart: ", utdata_schema, ".", utdata_vy)
  invisible(utdata_vy)
}

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
