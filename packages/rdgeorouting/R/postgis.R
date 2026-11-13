# PostGIS: aktivera tillägget, skriva/läsa sf-tabeller, kopiera/flytta tabeller.

#' Referera till en tabell i en PostGIS-databas
#'
#' Skapar en lättviktig referens till en tabell i databasen, för funktioner
#' (t.ex. [pendling_ruta()]) som kan ta emot antingen ett redan inläst
#' `sf`-objekt eller en tabell i databasen och själva avgöra vilket. Öppnar
#' ingen anslutning och läser ingen data - det sker först när referensen
#' faktiskt används.
#'
#' @param con En `DBIConnection`, eller `NA` (standard) för Region Dalarnas
#'   databas.
#' @param schema,tabell Schema och tabellnamn för tabellen i databasen.
#'   Tabellen förutsätts ha en geometrikolumn med polygon-/
#'   multipolygongeometri - inte t.ex. rasterdata.
#'
#' @return Ett objekt av klass `postgis_tabell`.
#' @export
postgis_tabell <- function(con = NA, schema, tabell) {
  rdgis::postgis_tabell(con = con, schema = schema, tabell = tabell)
}

#' @export
print.postgis_tabell <- function(x, ...) {
  rdgis:::print.postgis_tabell(x, ...)
}

#' Aktivera PostGIS-tillägget i en databas
#'
#' @param con En aktiv `DBIConnection`.
#' @return Osynligt `NULL`.
#' @export
postgis_aktivera_i_postgres_db <- function(con) {
  rdpostgres::postgis_aktivera_i_postgres_db(con)
}

#' @rdname postgis_aktivera_i_postgres_db
#' @export
postgis_installera_i_postgres_db <- postgis_aktivera_i_postgres_db

#' Skriv ett sf-objekt till en PostGIS-tabell
#'
#' Skapar schemat om det saknas, hanterar spatialt index och primärnyckel.
#'
#' @param con En `DBIConnection` eller `"default"` (geodata).
#' @param inlas_sf sf-objektet som skrivs.
#' @param schema,tabell Målschema och måltabell.
#' @param postgistabell_id_kol Kolumn som ska bli primärnyckel (`NA` = ingen).
#' @param postgistabell_geo_kol Geometrikolumnens namn (krävs för spatialt index).
#' @param skapa_spatialt_index Skapa GIST-index på geometrikolumnen.
#' @param addera_data `TRUE` = lägg till rader i stället för att skriva över.
#' @param skriv_over_tabell_om_finns `TRUE` = ta bort och skapa om (t.ex. vid
#'   kolumnändringar); `FALSE` behåller strukturen.
#' @param tabellnamn_till_gemener,kolumnnamn_till_gemener Gör namn till gemener.
#'
#' @return Osynligt `NULL`.
#' @export
postgis_sf_till_postgistabell <- function(con = "default", inlas_sf,
                                          schema = "karta", tabell,
                                          postgistabell_id_kol = NA,
                                          postgistabell_geo_kol = NA,
                                          skapa_spatialt_index = TRUE,
                                          addera_data = FALSE,
                                          skriv_over_tabell_om_finns = FALSE,
                                          tabellnamn_till_gemener = TRUE,
                                          kolumnnamn_till_gemener = TRUE) {
  rdgis::postgis_sf_till_postgistabell(
    con = con, inlas_sf = inlas_sf, schema = schema, tabell = tabell,
    postgistabell_id_kol = postgistabell_id_kol,
    postgistabell_geo_kol = postgistabell_geo_kol,
    skapa_spatialt_index = skapa_spatialt_index, addera_data = addera_data,
    skriv_over_tabell_om_finns = skriv_over_tabell_om_finns,
    tabellnamn_till_gemener = tabellnamn_till_gemener,
    kolumnnamn_till_gemener = kolumnnamn_till_gemener)
}

#' Kopiera en tabell inom en PostGIS-databas
#'
#' @param con En `DBIConnection` eller `"default"`.
#' @param schema_fran,tabell_fran Källa.
#' @param schema_till,tabell_till Mål.
#' @param skriv_over Skriv över måltabellen om den finns.
#'
#' @return Osynligt `NULL`.
#' @export
postgis_kopiera_tabell <- function(con = "default", schema_fran, tabell_fran,
                                   schema_till, tabell_till, skriv_over = FALSE) {
  rdpostgres::postgis_kopiera_tabell(
    con = con, schema_fran = schema_fran, tabell_fran = tabell_fran,
    schema_till = schema_till, tabell_till = tabell_till, skriv_over = skriv_over)
}

#' Kopiera en tabell mellan två PostGIS-databaser
#'
#' Läser via `sf`, skriver till målet, uppdaterar SRID och loggar i
#' `metadata.uppdateringar` (tar versionsdatum/-tid från källans metadata om det
#' finns).
#'
#' @param con_fran_databas,con_till_databas `DBIConnection`-objekt (eller
#'   `"default"` för källan).
#' @param schema_fran,tabell_fran Källa.
#' @param schema_till,tabell_till Mål.
#'
#' @return Osynligt `NULL`.
#' @export
postgis_kopiera_tabell_mellan_databaser <- function(con_fran_databas,
                                                    con_till_databas,
                                                    schema_fran, tabell_fran,
                                                    schema_till, tabell_till) {
  rdgis::postgis_kopiera_tabell_mellan_databaser(
    con_fran_databas = con_fran_databas, con_till_databas = con_till_databas,
    schema_fran = schema_fran, tabell_fran = tabell_fran,
    schema_till = schema_till, tabell_till = tabell_till)
}

#' Flytta en tabell från ett schema till ett annat
#'
#' @param con En `DBIConnection` eller `"default"`.
#' @param schema_fran,tabell_fran Källa.
#' @param schema_till Målschema.
#'
#' @return Osynligt `NULL`.
#' @export
postgis_flytta_tabell <- function(con = "default", schema_fran, tabell_fran, schema_till) {
  rdpostgres::postgis_flytta_tabell(
    con = con, schema_fran = schema_fran, tabell_fran = tabell_fran,
    schema_till = schema_till)
}

#' Skriv ett sf-objekt (eller data.frame) till PostGIS och logga i metadata
#'
#' Wrapar [postgis_sf_till_postgistabell()] i `tryCatch` och skriver alltid en
#' rad till `metadata.uppdateringar`.
#'
#' @param con En aktiv `DBIConnection`.
#' @param inlas_sf sf-objektet (eller data.frame).
#' @param schema,tabell Målschema och måltabell.
#' @param postgistabell_geo_kol Geometrikolumn (`NA` = ingen geometri/index).
#' @param postgistabell_id_kol Primärnyckelkolumn (`NA` = ingen).
#' @param postgis_addera_data `TRUE` = lägg till rader.
#' @param skriv_over_tabell_om_finns Se [postgis_sf_till_postgistabell()].
#' @param felmeddelande_medskickat Om datahämtningen redan misslyckats: loggas,
#'   ingen skrivning görs.
#' @param kommentar_metadata Kommentar vid lyckad skrivning.
#' @param tabellnamn_till_gemener,kolumnnamn_till_gemener Gör namn till gemener.
#' @param version_datum,version_tid Versionens datum/tid.
#'
#' @return Osynligt `NULL`.
#' @export
postgis_databas_skriv_med_metadata <- function(con, inlas_sf, schema, tabell,
                                               postgistabell_geo_kol = "geometry",
                                               postgistabell_id_kol = NA,
                                               postgis_addera_data = FALSE,
                                               skriv_over_tabell_om_finns = TRUE,
                                               felmeddelande_medskickat = NA,
                                               kommentar_metadata = NA,
                                               tabellnamn_till_gemener = TRUE,
                                               kolumnnamn_till_gemener = TRUE,
                                               version_datum = NA, version_tid = NA) {
  rdgis::postgis_databas_skriv_med_metadata(
    con = con, inlas_sf = inlas_sf, schema = schema, tabell = tabell,
    postgistabell_geo_kol = postgistabell_geo_kol,
    postgistabell_id_kol = postgistabell_id_kol,
    postgis_addera_data = postgis_addera_data,
    skriv_over_tabell_om_finns = skriv_over_tabell_om_finns,
    felmeddelande_medskickat = felmeddelande_medskickat,
    kommentar_metadata = kommentar_metadata,
    tabellnamn_till_gemener = tabellnamn_till_gemener,
    kolumnnamn_till_gemener = kolumnnamn_till_gemener,
    version_datum = version_datum, version_tid = version_tid)
}
