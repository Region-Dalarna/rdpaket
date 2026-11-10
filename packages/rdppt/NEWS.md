# rdppt 0.0.0.9003

* Nytt: `ppt_befintlig_visa()` listar sliderna i en pptx-fil med färdiga
  slides - nummer, rubrik, första texten, vad sliden innehåller (bilder,
  tabeller, diagram) och layout - så att man ser vilket nummer man ska
  ange i `ppt_befintlig(slide = )`. Nytt beroende: `xml2` (som `officer`
  redan kräver).

* Nytt: `ppt_forhandsvisa()` gör en bild av varje slide i en pptx-fil och
  visar dem i en översikt med nummer, rubrik och layout (Viewer-fliken i
  RStudio eller webbläsaren). Utan `fil` visas hur mallens layouter ser
  ut. Bilderna görs med PowerPoint på Windows, annars med LibreOffice (och
  `pdftools` eller `pdftoppm`). `ppt_befintlig_visa(fil, bilder = TRUE)`
  visar listan och bilderna tillsammans.

# rdppt 0.0.0.9002

Paketet är omgjort för att det ska vara enkelt att skapa en presentation,
och för att inget ska vara låst till en viss mall. Befintliga skript som
använder `initiera_ppt_df()`, `ppt_lista_fyll_pa()` och
`skapa_ppt_fran_lista()` fungerar som tidigare.

* Nytt sätt att bygga en presentation, steg för steg med `|>`:
  `ppt_ny()` följt av `ppt_diagram()`, `ppt_bild()`, `ppt_text()`,
  `ppt_avsnitt()`, `ppt_slutsatser()`, `ppt_rapporter()`, `ppt_avslut()`
  och till sist `ppt_spara()`. `ppt_diagram()` kör en diagramfunktion (per
  region om man vill) och håller själv reda på var bilderna sparas - man
  behöver inte ange någon bildmapp.

* Nytt: `ppt_befintlig()` lägger in färdiga, handgjorda slides från en
  pptx-fil på valfritt ställe i presentationen. Ersätter räknandet med
  `slides_flytta = list(list(fran = 1, till = 5), ...)`. I en ppt-lista
  görs samma sak med `mall = "befintlig:<nr>"`.

* Mallen är inte längre hårdkodad. Välj mall en gång med
  `ppt_mall_ange()` (eller per presentation). Utan vald mall används
  Region Dalarnas standardmall (`G:/skript/ppt/mall_rus_utan_bakgrund.pptx`)
  om den går att nå, annars en kopia av samma mall som nu följer med
  paketet (`inst/mallar/`) - det går alltså att bygga presentationer utan
  G:. `"regiondalarna"` väljer alltid kopian och `"inbyggd"` den vita
  Office-mall som är inbyggd i `officer`. `mall_fil` i
  `skapa_ppt_fran_lista()` är därför inte längre obligatorisk.

* Alla layouter i mallen går att använda - skriv layoutens namn i
  `mall`-kolumnen (eller `layout =` i `ppt_bild()` m.fl.). Rubrik, text och
  bild läggs i de platshållare layouten har, oavsett vad de heter.
  Tidigare fungerade bara elva layoutnamn, med hårdkodade
  platshållarnamn. Lämnas `mall` tom väljs layout efter innehållet.

* Nytt: `ppt_mall_visa()` visar vilka layouter och platshållare en mall
  har, och `ppt_mall_exempel()` skapar en pptx med en slide per layout.

* Nytt: `ppt_mallprofil()` för mallar med egna layoutnamn - pekar ut
  vilken layout som används till vad (titelsida, diagram, punktlista osv)
  och hur rubriker formateras.

* `skapa_ppt_fran_lista()` kontrollerar hela listan innan något byggs.
  Felstavade layoutnamn och saknade bildfiler redovisas samlat, med
  radnummer och en lista över de layouter som finns - i stället för ett
  fel från `officer` mitt i bygget.

* Ändrat beteende i `skapa_ppt_fran_lista()`:
  - `farg_huvudrubrik`, `titelmaster`, `titel_text_stlk`,
    `undertitel_text_stlk` och `rubrik_namn_stlk` har `NULL` som standard
    och hämtas då från mallprofilen (samma värden som tidigare för Region
    Dalarnas mall).
  - Rader med äldre layoutnamn som inte finns i mallen (t.ex. `"Bild och
    text till höger"`, som saknas i `mall_rus_utan_bakgrund.pptx`) gav
    tidigare fel och översätts nu till mallens motsvarande layout.
  - Bilder skalas så att de alltid ryms i sin yta med bibehållna
    proportioner.
  - Returnerar sökvägen till den sparade filen, och skriver ut den.
  - En mallfil som saknas ger ett vanligt felmeddelande, inte längre
    `rdverktyg::sakerstall_indatafil()`:s text om manuellt underhållna
    datakällor.

* Buggar fixade:
  - `gg_obj` (ett ggplot-objekt i stället för en bildfil) fungerade inte
    alls - varje sådan rad gav felet "the condition has length > 1".
  - Anteckningar kom bara med på vissa slides, och aldrig på slides med
    `gg_obj`.
  - `output_mapp` utan avslutande snedstreck gav en fil med fel namn i
    fel mapp. Mappen kontrolleras nu också innan något byggs.
  - Om namn/e-post inte gick att slå upp (t.ex. utanför Windows) stoppades
    hela bygget, även när uppgifterna inte behövdes. Nu utelämnas de med
    ett meddelande. `anv_uppgifter` tar även
    `list(namn = "...", epost = "...")`.
  - `ppt_lista_fyll_pa()` gav en varning om `is.ggplot()` med ggplot2 4.0.

* Beroendet till paketet `png` är borttaget. Paketet har nu tester
  (`tests/testthat/`), som körs mot `officer`:s inbyggda mall och mot den
  medföljande kopian av Region Dalarnas mall.

* `KOM-IGANG.qmd` är omskriven för det nya arbetssättet. Den tidigare
  versionen beskrev `gg_obj` som fungerande, visade ett `anv_uppgifter`-
  format som inte fungerade och hänvisade till en mallfil och layouter
  som inte finns.

# rdppt 0.0.0.9001

* De interna hjälpfunktionerna `intern_sakerstall_indatafil()` och
  `intern_meddela_manuell_datakalla()` (i `R/intern_indata.R`) är borttagna
  - samma två funktioner fanns duplicerade i `rddiagramkatalog`, och finns
  nu i stället delade som `rdverktyg::sakerstall_indatafil()`/
  `rdverktyg::meddela_manuell_datakalla()`. Ingen ändrad funktionalitet.

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
