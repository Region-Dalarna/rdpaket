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
  rdgis::postgis_skapa_omradesnyckel_tabell_vy(
    con = con, omrades_schema = omrades_schema, omrades_tabell = omrades_tabell,
    omrade_urval_kolumner = omrade_urval_kolumner,
    rut_schema = rut_schema, rut_tabell = rut_tabell, rut_id_kol = rut_id_kol,
    rut_urval_kolumner = rut_urval_kolumner,
    utdata_schema = utdata_schema, utdata_vy = utdata_vy,
    fran_rutid_till_x_y_kol = fran_rutid_till_x_y_kol,
    databas_typ = databas_typ,
    skriv_over_befintlig_tabell_vy = skriv_over_befintlig_tabell_vy)
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
#' @seealso [rdgis::postgis_omradesnyckel_exportera_till_mikrodb()]
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
  rdgis::postgis_omradesnyckel_exportera_till_mikrodb(
    con = con, omradesnyckel_schema = omradesnyckel_schema,
    omradesnyckel_tabell = omradesnyckel_tabell,
    max_chunk_stlk_mb = max_chunk_stlk_mb, filnamn = filnamn,
    output_mapp = output_mapp, kolumner_namn = kolumner_namn,
    kolumner_lista = kolumner_lista)
}
