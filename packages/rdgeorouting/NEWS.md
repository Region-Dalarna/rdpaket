# rdgeorouting 0.0.0.9008

* `pendling_kraftfalt()` och `pendling_natverk()` döljer även S4-notisen "Note:
  method with signature 'DBIObject#sf' chosen for function 'dbDataType'".

# rdgeorouting 0.0.0.9007

* `pendling_kraftfalt()` och `pendling_natverk()` döljer nu databasens NOTICE-/NOTIS-
  meddelanden i R ("tabell ... finns inte, hoppar över", "Ger rättigheter på
  table ...") i stället för att bara be servern tiga, vilket inte räckte.
  Övriga meddelanden och varningar visas som förut.

# rdgeorouting 0.0.0.9006

## PostGIS-funktioner flyttade

Funktioner för geometri, spatial skrivning, metadata och områdesnycklar finns nu
i `rdgis`; generell tabellkopiering/flytt och aktivering av PostGIS finns i
`rdpostgres`. De gamla `postgis_*()`-namnen i rdgeorouting är **borttagna** (inga
kompatibilitetswrappers - paketet är så nytt att de inte behövs). Anropa i
stället:

* `rdgis::postgis_tabell()`, `rdgis::postgis_sf_till_postgistabell()`,
  `rdgis::postgis_databas_skriv_med_metadata()`,
  `rdgis::postgis_kopiera_tabell_mellan_databaser()`,
  `rdgis::postgis_skapa_omradesnyckel_tabell_vy()`,
  `rdgis::postgis_omradesnyckel_exportera_till_mikrodb()`
* `rdpostgres::postgis_aktivera_i_postgres_db()` (alias
  `postgis_installera_i_postgres_db()`), `rdpostgres::postgis_kopiera_tabell()`,
  `rdpostgres::postgis_flytta_tabell()`

`postgis_isokroner_dela_upp_polygoner()` stannar i rdgeorouting eftersom den hör
till isokronbearbetning. Kräver `rdgis >= 0.0.0.9002` och
`rdpostgres >= 0.0.0.9005`.

# rdgeorouting 0.0.0.9005

* `pendling_kraftfalt()` och `pendling_natverk()` skriver inte längre ut
  databasens NOTICE-meddelanden ("tabell ... finns inte, hoppar över" och
  "Ger rättigheter på table ...").

# rdgeorouting 0.0.0.9004

* `pendling_kraftfalt()` och `pendling_natverk()`: tätortstabellen
  (`grafer.pendling_tatort`) får uttryckligen ett registrerat koordinatsystem
  (SRID) efter att den skrivits, så att lagren som bygger på den går att läsa
  in utan "crs not found: EPSG:0".

# rdgeorouting 0.0.0.9003

* `pendling_kraftfalt()` och `pendling_natverk()`: koordinatsystemet läses från
  en rad i vägnätet om geometrikolumnen saknar registrerat SRID (gav
  "crs not found: EPSG:0"); faller annars tillbaka på EPSG:3006.

# rdgeorouting 0.0.0.9002

* `pendling_kraftfalt()` och `pendling_natverk()` körs nu mot `ruttanalyser`
  (före: geodata, där vägnätet inte finns) och mot vägnätet
  `grafer.nvdb_bil_adresser` i stället för det gamla `nvdb_noded`. Rutterna
  söks med `pgr_dijkstra()` på `kostnad_meter`, eftersom nätet saknar de
  koordinatkolumner `pgr_aStar()` kräver. Tätorterna hämtas ur
  `karta.tatortspunkter` i geodata (matchar på SCB:s tätortskod) och skrivs till
  `grafer.pendling_tatort`; pendlingstabellen skrivs uttryckligen till
  `grafer.data`. Nya parametrar: `schema_graf`, `kant_tabell`, `nod_tabell`,
  `kant_id_kol`, `kost_kol` och `tatorter`.

# rdgeorouting 0.0.0.9001

Versionsbump utan innehållsändring (för att `remotes`/`pak`
tillförlitligt ska upptäcka uppdateringar i det här monorepot).

---

# rdgeorouting 0.0.0.9000

Första versionen. Utbrutet ur postgis-, pgrouting- och pendlingsavsnitten
i `func_GIS.R` i `Region-Dalarna/funktioner`. Kräver en PostGIS-databas
med pgRouting (Region Dalarnas `ruttanalyser`).

SQL:n är bevarad ordagrant. Det som ändrats:

* **con-hantering** – den 20-radiga keyring/`uppkoppling_adm()`-boilerplaten
  i varje funktion ersatt av `intern_rutt_con()` (tar `DBIConnection` eller
  databasnamn).
* **metadata-loggning genom tryCatch** – `<<-` mot global miljö (som inte
  fungerade som avsett) ersatt av ett internt tillståndsobjekt
  (`intern_meta_state()`).
* Tidtagningsboilerplaten borttagen.
* `library()`-anrop inuti funktioner borttagna; `sf`/`DBI`/`glue` m.fl.
  kvalificerade. `sf` i `Suggests` med vakt.

## Rättade buggar från originalet

* `postgis_flytta_tabell()` använde odefinierad `con_flytt` i stället för `con`.
* `postgis_kopiera_punkttabell_koppla_till_pgr_graf()` använde odefinierad
  `con_rutt`.
* `postgis_omradesnyckel_exportera_till_mikrodb()` anropade `fread()` (läser)
  där `fwrite()` (skriver) avsågs.
* `pgrouting_punkttabell_koppla_till_pgr_graf()` hade inverterad logik i
  fallback-grenen (satte `id_kol_finns <- FALSE` när kolumnen fanns).
* Kod efter `stop()` i error-hanterare (aldrig nåbar) borttagen.

## PostGIS

* `postgis_aktivera_i_postgres_db()` (alias `postgis_installera_i_postgres_db`),
  `postgis_sf_till_postgistabell()`, `postgis_kopiera_tabell()`,
  `postgis_kopiera_tabell_mellan_databaser()`, `postgis_flytta_tabell()`,
  `postgis_databas_skriv_med_metadata()` (sf-varianten),
  `postgis_skapa_omradesnyckel_tabell_vy()`,
  `postgis_omradesnyckel_exportera_till_mikrodb()`.

## Isokroner

* `postgis_isokroner_skapa()` + `postgis_isokroner_dela_upp_polygoner()`,
  och wrappers `postgis_isokroner_bil/meter/gang/cykel/elcykel()`.

## pgRouting

* `pgrouting_installera_i_postgis_db()`, `pgrouting_hastighet_gang/cykel/elcykel()`,
  `pgrouting_klipp_natverk_skapa_tabell()`,
  `pgrouting_hitta_narmaste_punkt_pa_natverk()`,
  `pgrouting_tabell_till_pgrgraf()`,
  `pgrouting_punkttabell_koppla_till_pgr_graf()`,
  `pgrouting_kostnadskolumner_transporttyp_graf()`,
  `pgrouting_skapa_geotabell_rutt_fran_till()`,
  `pgrouting_skapa_ny_graf_nvdb_koppla_till_punkter()`,
  `postgis_kopiera_punkttabell_koppla_till_pgr_graf()`.

## Pendling

* `pendling_natverk()`, `pendling_kraftfalt()`, `pendling_ruta()`.

## Inte portade

* De gamla funktionerna under kommentaren "äldre pgrouting-funktioner,
  används inte längre" (`las_in_rutor_xlsx_...`, `las_in_fil_skapa_punkter_...`,
  `las_in_geosf_...`, `koppla_punkter_postgis_tabell_...`,
  `skapa_n_narmaste_malpunkter_tabell()`,
  `berakna_pgr_dijkstracost_n_narmaste_tab()`,
  `join_narmaste_malpunkt_fran_n_narmaste()`,
  `koppla_kommun_till_geokol_i_tabell()`) – hårdkodade `Sys.getenv`-
  credentials, egna buggar (`rut_toponode_id` odefinierad).

**Obs:** SQL-pipelinerna kan inte köras utan en riktig pgRouting-databas
med NVDB-data. Verifiera mot `ruttanalyser` innan skarp användning.
