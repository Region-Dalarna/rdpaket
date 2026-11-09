# rdppt 0.0.0.9000

* Nytt paket. Utbrutet ur `G:/skript/ppt/skapa_ppt_grund.R` och
  `G:/skript/ppt/func_ppt.R`. Första innehåll: `skapa_ppt_fran_lista()`,
  `initiera_ppt_df()`, `ppt_lista_fyll_pa()`, `rapporter_rd_webb_hamta()`.

* `ppt_lista_fyll_pa()` moderniserad: tar nu antingen `funktion` (en
  riktig R-funktion, t.ex. `rddiagramkatalog::diag_x`) eller `source_url`
  (det tidigare, enda sättet - en URL/sökväg som `source()`:as). `funktion`
  är att föredra när diagrammet redan finns som en riktig paketfunktion;
  `source_url` finns kvar som alternativ för det som inte är migrerat än.
  `felmeddelande_loggfil_mapp` har inte längre ett hårdkodat G:-standardvärde
  (var tidigare `"G:/skript/schemalagda_skript/loggfiler/"`) - `NULL`
  (ingen loggfil) som standard i stället, man anger mappen explicit om man
  vill ha en logg.

* Tre buggar hittade och fixade under portningen:
  - `is_ggplot()` (i det som nu är `ppt_lista_fyll_pa()`) fanns inte -
    varken i källskriptet eller i `ggplot2`, som bara har `is.ggplot()`.
    Hade kraschat så fort grenen faktiskt körts mot ett ggplot-objekt.
  - `bild_position_stlk()`/`intern_bild_position_stlk()` läste samma
    png-fil två gånger (en gång för bilddata, en gång bara för
    dpi-attributet) - läser nu filen en gång och återanvänder attributet.
  - `get_placeholder_dims()`/`intern_get_placeholder_dims()`: `unit =
    "px"`/`"cm"`-konverteringen beräknades men kastades alltid bort (ett
    ovillkorligt `return(retur_df)` efter if/else-blocket returnerade
    alltid originalvärdet i tum) - och hade dessutom kraschat om den
    användes (`list * 96` fungerar inte i R). Aldrig upptäckt eftersom
    `unit` alltid lämnats på standardvärdet `"in"` i praktiken. Returnerar
    nu faktiskt rätt konverterat värde.

* `getFromNamespace("print.rpptx", "officer")`-omvägen i
  `skapa_ppt_fran_lista()` är borttagen - källskriptet anropade aldrig
  `library(officer)` (bara `officer::`-prefix), vilket gjorde att vanlig
  `print()`-dispatch inte alltid hittade S3-metoden. Med `officer` som ett
  riktigt `Imports`-beroende här registreras alla dess S3-metoder
  automatiskt när paketet laddas - vanlig `print(pres, target = ...)`
  räcker.
