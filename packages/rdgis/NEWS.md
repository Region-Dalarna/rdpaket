# rdgis 0.0.0.9007

* `skapa_bakgrundskarta_topografi(kalla_schema = "topografi_1m")` bygger bakgrundskartan för hela Sverige ur Lantmäteriets Topografi 1M, som standard i schemat
  `bakgrundskarta_sverige`. Lager: `utland`, `vatten` (sjöar och hav), `skog`, `fjall`, `glaciar`, `bebyggelse`, `vattendrag`, `vagar_stora`, `vagar_mellan`, `vagar_sma` och `jarnvag`.
  Namnlager: `ortnamn`, `vattennamn` och `terrangnamn` (ur `textpunkt`; delade namn slås ihop till en punkt).
* `skapa_bakgrundskarta_topografi()` stoppar om `schema` och `kalla_schema` hör till olika källor (t.ex. `topografi_1m` till `bakgrundskarta_dalarna`), så att ett schema inte skrivs över av fel källa.
* `schema` i `skapa_bakgrundskarta_topografi()` är `NULL` som standard och väljs utifrån källan. `lager` i `bakgrundskarta_finns()` och `hamta_bakgrundskarta()` är `NULL` som standard
  och betyder alla lager för schemat (`schema = "bakgrundskarta_sverige"` för hela Sverige).

# rdgis 0.0.0.9005

* Lagret `sjoar` i bakgrundskartan: sjöpolygoner med samma namn slås ihop innan etikettpunkten beräknas, så att en stor
  sjö som består av flera polygoner (t.ex. Siljan) får en etikett i mitten av den största sammanhängande vattenytan i
  stället för en per polygon. Bygg om lagret med `skapa_bakgrundskarta_topografi(lager = "sjoar")`.

# rdgis 0.0.0.9004

* Bakgrundskartan får ett nytt lager `sjoar`: en etikettpunkt per sjö, placerad i mitten av sjöns största öppna
  vattenyta (PostGIS `ST_MaximumInscribedCircle`, annars en punkt inuti sjön) med namn, textrang och area i km2.
  Bygg det med `skapa_bakgrundskarta_topografi(lager = "sjoar")`.

# rdgis 0.0.0.9003

* Ny bakgrundskarta ur geodatabasen: `skapa_bakgrundskarta_topografi()` bygger schemat
  `bakgrundskarta_dalarna` (generaliserade och filtrerade topografilager i länsskala med rumsliga index)
  ur `topografi_10`, `hamta_bakgrundskarta()` läser lagren för ett utsnitt som `sf`-objekt och
  `bakgrundskarta_finns()` kontrollerar databasåtkomst och att lagren finns.

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
