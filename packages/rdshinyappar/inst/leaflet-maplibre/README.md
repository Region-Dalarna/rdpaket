# Medföljande JavaScript för `lagg_till_bakgrundskarta()`

Filerna här läses av `lagg_till_bakgrundskarta()` och bäddas in i
leaflet-kartor, så att de fungerar utan att hämta något från ett CDN:

| Fil | Paket | Version | Licens |
|---|---|---|---|
| `maplibre-gl.js`, `maplibre-gl.css` | [maplibre-gl](https://github.com/maplibre/maplibre-gl-js) | 4.7.1 | BSD-3-Clause (`LICENSE-maplibre-gl.txt`) |
| `leaflet-maplibre-gl.js` | [@maplibre/maplibre-gl-leaflet](https://github.com/maplibre/maplibre-gl-leaflet) | 0.0.22 | ISC (`LICENSE-maplibre-gl-leaflet.txt`) |

Hämtade oförändrade från `https://unpkg.com/<paket>@<version>/`. Vid
uppdatering: hämta nya filer, ändra versionsnumren i
`R/bakgrundskarta.R` och i tabellen ovan, och testa en karta både som html
och som png (`skriv_leaflet_till_fil()`).
