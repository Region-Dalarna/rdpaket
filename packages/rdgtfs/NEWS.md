# rdgtfs 0.0.0.9001

* Ny funktion `lagg_till_alternativa_id_dalatrafik()`: lägger till kolumnerna `dt_hpl_id` och `dt_stop_id`
  i `stops` för ett Sweden 3-flöde filtrerat till Dalarna, genom koordinatmatchning mot det regionala
  Dalatrafik-flödet. Endast hållplatser som trafikeras av Dalatrafik får ID; övriga får `NA`.
  Funktionen kopplas inte in automatiskt i något befintligt arbetsflöde.
* Hållplatsvyerna från `skapa_vyer_hallplats()` och `skapa_vyer_historisk_hallplats()` innehåller nu
  `dt_hpl_id` och `dt_stop_id`. Kolumnerna måste finnas i `stops` (och i historiska `stops`) innan
  vyerna skapas.

# rdgtfs 0.0.0.9000

Första versionen med dokumenterat API för GTFS-hämtning, kalenderhantering,
geografisk filtrering, PostGIS-lagring och r5r-förberedelse.
