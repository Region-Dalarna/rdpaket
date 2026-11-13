# rddiagramkatalog 0.0.0.9060

* `karta_lf_kraftfalt_scb(skriv_gpkg_fil = TRUE)` skriver en GeoPackage (EPSG:3006)
  med lagren `tatorter` (alla tätorter med kolumnen `typ`), `primla_areas`,
  `secla_areas`, `commonla_areas`, `routes`, de enskilda lagren från beräkningen
  och `lansgrans`, redo att öppnas i QGIS. Meddelandet efter skrivningen listar
  lagren. `skriv_leaflet_fil = FALSE` ger bara GeoPackage-filen.

