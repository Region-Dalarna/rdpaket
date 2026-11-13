# rdgis 0.0.0.9002

## PostGIS-funktioner

Från `rdgeorouting` flyttas `postgis_tabell()` med print-metod,
`postgis_sf_till_postgistabell()`, `postgis_databas_skriv_med_metadata()`,
`postgis_kopiera_tabell_mellan_databaser()`,
`postgis_skapa_omradesnyckel_tabell_vy()` och
`postgis_omradesnyckel_exportera_till_mikrodb()`. Gamla publika anrop finns
kvar som kompatibilitetswrappers i `rdgeorouting`.

Kräver `rdpostgres >= 0.0.0.9005` för den samordnade funktionsflytten.

---

# rdgis 0.0.0.9001

Versionsbump utan innehållsändring (för att `remotes`/`pak`
tillförlitligt ska upptäcka uppdateringar i det här monorepot).

---

# rdgis 0.0.0.9000

Första versionen. Utbrutet ur geometri-avsnitten i `func_GIS.R` i
`Region-Dalarna/funktioner`. `sf` ligger i `Suggests` med
`requireNamespace()`-vakt i varje funktion.

## Kartdata

* `hamta_karttabell()` – tabellen över hämtbara kartlager.
* `hamta_karta()`, `kartifiera()` – hämtar ur geodatabasens `karta`-schema
  via `rdpostgres::uppkoppling_db()`. `hamta_karta()` bygger nu WHERE-
  villkoret enklare och tabellen med sökord är städad (ASCII-sökord).
* `postgis_postgistabell_till_sf()` – flyttad hit (producerar sf).

## Rutor

* `rutstorlek_estimera()`, `sf_fran_df_med_x_y_kol()`,
  `berakna_mittpunkter()`.
* `sf_fran_rutid_fil()` – **ny**, slår ihop
  `geopackage_skapa_fran_rutor_csv_xlsx_supercross()` och
  `skapa_sf_fran_csv_eller_excel_supercross()`. Läser en csv/xlsx med en
  rutid-kolumn, härleder mittpunkt, returnerar punkter eller cellpolygoner.
  Supercross-inramningen (gpkg-export, enhetsdetektering) borttagen –
  funktionen är generell.

## Geometri

* `st_largest_ring()`, `st_centroid_within_geo()`,
  `skapa_linje_langs_med_punkter()`, `spatial_join_med_ovrkat()`.
  NSE moderniserad (`.data[[...]]`), `data.table::rbindlist` →
  `do.call(rbind, ...)`.

## Filer

* `las_gisfil_fran_zipfil_via_url()`, `las_gisfil_fran_zipfil_via_sokvag()`,
  `unzip_zipfil_med_zipfiler()` (httr → curl).

## Övrigt

* `skapa_punkt_sf_av_koordinatpar()`, `adresser_inv_reg_folke_bearbeta()`
  (NSE → base), `gdb_extrahera_kolumnnamn_per_gislager()`,
  `raster_till_vektor()` (buggen med odefinierat `rutor_rast` fixad,
  `terra::` kvalificerad), `sf_to_poly()` (`sink()` → `file()`-connection).

## Inte portade

* `skapa_supercross_recode_fran_rutlager()` – öppen fråga om Mona/Supercross
  fortfarande används. `korrigera_kolnamn_supercross()` (låg i func_API.R)
  utgår.
