# rdppt 0.0.0.9022

* `ppt_forhandsvisa()`: felmeddelanden från cscript/PowerShell med å/ä/ö gav "input string 1 is invalid UTF-8" i stället för det riktiga felet. Texten omkodas nu.

# rdppt 0.0.0.9021

* `ppt_forhandsvisa()`: bilder av sliderna görs först med ett VBScript (cscript) och sedan med PowerShell. På datorer där PowerShell är låst (signerade skript, constrained language mode) går det inte att styra PowerPoint därifrån.

# rdppt 0.0.0.9020

* `ppt_spara()`: utan angiven person får filnamnet den inloggades förnamn (`namn_peter.pptx`), om det går att slå upp.
* `ppt_forhandsvisa()` / `ppt_befintlig_visa(bilder = TRUE)`: PowerPoint styrs nu utan en .ps1-fil, så det fungerar även på datorer där körpolicyn kräver signerade skript.

# rdppt 0.0.0.9019

* Ny `ppt_regioner(region_vekt)`: länskoder, kommunkoder, regionnamn och färdiga texter (uppräkning med Dalarna sist, genitiv, filnamn).
* `ppt_ny(region_vekt = )`: `<region>` och `<regions>` (genitiv) i huvudrubrik, underrubrik och i rubrik, text och anteckningar på slidesen byts mot regionernas namn.
* `ppt_ny(loggfil = TRUE)`: loggfil i utskriftsmappen med automatiskt namn (presentation, användare, `_auto` om skriptet inte körs för hand). Mappen skapas om den saknas.

# rdppt 0.0.0.9018

* Rapportsidan: länkblocken står i ordningen Samhällsanalys webbsida, Här finns alla våra rapporter, Lyssna på vår podd.

# rdppt 0.0.0.9017

* `ppt_forhandsvisa()` och `ppt_befintlig_visa(bilder = TRUE)`: när PowerPoint inte gick att styra visas nu det riktiga felet från PowerPoint i stället för att det döljs.

# rdppt 0.0.0.9016

* Rapportsidan: länken under "Samhällsanalys webbsida" visar nu samhallsanalys.regiondalarna.se och går dit.

# rdppt 0.0.0.9015

* Rapportsidan: länkarna längst ned är tre block i samma kolumner som listorna ("Här finns alla våra rapporter" med
  shiny.regiondalarna.se, "Samhällsanalys webbsida" och "Lyssna på vår podd") i stället för tre centrerade rader.
  Det ger listorna mer plats.

# rdppt 0.0.0.9014

* `ppt_ny(visa_manuell_datakalla = FALSE)` döljer meddelandet "Data hämtas från en manuellt underhållen källa ... INTE en live-hämtning" medan presentationens diagram skapas. Standard är att det visas. Kräver rdverktyg >= 0.0.0.9010 för att ha effekt.

# rdppt 0.0.0.9013

* Rapportsidan: listorna är punktlistor, och varje spalt fylls med så många rapporter som ryms (bredden mäts i
  teckensnittet). `antal_rapporter` är numera `NULL` som standard (= fyll spalten) och anger annars ett tak. Spaltrubrikerna
  har rubrikfärgen, och texten är märkt som svenska så att PowerPoint inte understryker den som felstavad.

# rdppt 0.0.0.9012

* `ppt_spara()` sparar under ett annat namn (`namn_2.pptx`, `namn_3.pptx` ...) om filen är öppen i PowerPoint eller
  låst, och säger till om det (`nytt_namn_om_last = FALSE` ger felmeddelande som förut).

* Meddelanden i konsolen när en presentation påbörjas (`ppt_ny()`) och när rapportsidan läggs till och byggs
  (vilka spalter och hur många rapporter i varje).

* "1 diagram tillagt." i stället för "1 diagram tillagda." (flera: "tillagda").

# rdppt 0.0.0.9011

* `ppt_lagg_till_slide("rapporter", antal_rapporter = 6)` styr hur många rapporter som visas i varje spalt.
  Under spalterna står t.ex. "Urval – 6 av 12".

* Rapportsidan: spalterna har samma höjd och texten börjar överst (och är inte fet) även i PowerPoint. Längst ned
  finns länkar till landningssidan (shiny.regiondalarna.se), Samhällsanalys webbsida och podden.

# rdppt 0.0.0.9010

* Rapportsidan (`ppt_lagg_till_slide("rapporter")`) har tre spalter: interaktiva rapporter (de sex första på
  shiny.regiondalarna.se), analysrapporter och publicerade rapporter (pdf), högst sex per spalt. Under varje spalt
  står att det är ett urval, hur många som finns och en länk till alla. Längst ned finns en rad om podden
  Läget i Dalarna. Spalter som inte går att hämta lämnas ut. Nya `rapporter_rd_interaktiva_hamta()` läser
  landningssidan. Sidan använder layouten "Endast rubrik" (rollen `bild_rubrik`) i stället för "Jämförelse".

* Rättelse till 9009: teckenstorleken på rapportsidan och i `txt_size`-text med flera rader kom ändå inte med, eftersom
  officer 0.7.x inte skriver någon teckenformatering för `fp_text_lite()` utan färg. Formateringen görs nu av
  `intern_text_stil()`, som alltid anger färg (svart om inget annat anges).

# rdppt 0.0.0.9009

* `ppt_spara()` skriver ut hur lång tid det tog att skapa presentationen (från `ppt_ny()`).

* Ett enda namn i `anv_uppgifter` (`"peter"`) läggs också till i filnamnet (`utmaningar_peter.pptx`),
  som när flera namn anges.

* Rapportsidan och text med `txt_size` i flera rader fick mallens teckenstorlek i stället för den angivna:
  `officer::unordered_list()` tappar `fp_text_lite()`-stilen. Texten blir nu ett stycke per rad.

# rdppt 0.0.0.9008

* `anteckningar` kan för diagram vara en namngiven vektor eller lista med en post per region
  (`c("20" = "Text för länet", "2080" = "Text för Falun")`). Texten beräknas då i skriptet,
  före sliden, i stället för i en funktion. Funktionsvarianten finns kvar.

* Dokumentationen av `ppt_lagg_till_slide()` angav felaktigt att `"rapporter"` inte kan ha
  `anteckningar`.

# rdppt 0.0.0.9007

* `ppt_lagg_till_slide(funktionsparametrar = list(...))`: parametrar till diagramfunktionen
  som har samma namn som parametrar till `ppt_lagg_till_slide()` själv, framför allt när
  funktionens egen `region_vekt` ska ha ett fast värde medan `regionvariabel` styr vad som
  körs per region.

# rdppt 0.0.0.9006

* `ppt_spara(anv_uppgifter = )` tar flera namn (`c("peter", "jon")`) och sparar en fil per
  person (`utmaningar_peter.pptx`, ...) utan att diagrammen körs om. Namn och e-post kan
  alltså anges vid sparandet i stället för i `ppt_ny()`.

* `anteckningar` kan för diagram vara en funktion av regionen
  (`anteckningar = \(region) ...`), som körs en gång per region.

* En tom `region_vekt` (`character(0)`) hoppar över diagrammet. `NULL` är som förut: körs
  en gång utan region.

* Diagram som hoppas över (fel, eller inga diagram) loggas i presentationen, i en loggfil om
  `ppt_ny(loggfil = )` eller `options(rdppt.loggfil)` anges, och sammanfattas när
  presentationen sparas.

* Rubrikformateringen (titelsida, avsnitt) kan ändras för en presentation med
  `ppt_ny(stil = list(titel_stlk = 54))` och för alla med `options(rdppt.stil = list(...))`.
  `ppt_installningar()` visar också `stil` och `loggfil`.

* Rapportsidan har rubrikerna "Analysrapporter" och "Publicerade rapporter (pdf)" (förut
  "Webbrapporter" och "PDF-rapporter").

* Borttaget: `initiera_ppt_df()`, `ppt_lista_fyll_pa()`, `skapa_ppt_fran_lista()`,
  `ppt_diagram()` och `ppt_befintlig()`. Presentationer byggs med `ppt_ny()`,
  `ppt_lagg_till_slide()` och `ppt_spara()`; `ppt_lagg_till_slide("diagram", funktion = , ...)`
  och `ppt_lagg_till_slide("befintlig", fil = , slide = )` ersätter de två sista.
  Beroendet till `glue` är borttaget.

# rdppt 0.0.0.9005

* Nytt: `ppt_lagg_till_slide(ppt, layout, ...)` lägger till en slide av den sort `layout`
  anger: `"diagram"`, `"bild"`, `"text"`, `"slutsatser"`, `"avsnitt"`, `"rapporter"`,
  `"avslut"` eller `"befintlig"`. Namnet på en layout i mallen, t.ex. `"Två
  innehållsdelar"`, fungerar också - sliden får då den layouten och det innehåll som
  anges (`funktion`, `bild` eller `rubrik` och `text`). Vilka parametrar som gäller
  beror på sliden; en parameter som inte gäller ger ett fel som listar dem som gör det.
  `anteckningar` (slidens anteckningsfält) kan anges för alla utom `"rapporter"` och
  `"befintlig"`. `ppt_slidetyper()` skriver ut sorterna och deras parametrar.

* Borttaget (ersatta av `ppt_lagg_till_slide()`): `ppt_bild()`, `ppt_text()`,
  `ppt_avsnitt()`, `ppt_slutsatser()`, `ppt_rapporter()` och `ppt_avslut()`.
  Byt t.ex. `ppt_slutsatser(c("a", "b"))` mot `ppt_lagg_till_slide("slutsatser", text = c("a", "b"))`.
  `ppt_diagram()` och `ppt_befintlig()` finns kvar.

# rdppt 0.0.0.9004

* Nytt: `ppt_installningar()` visar vilka inställningar som gäller (mall, slides-bank,
  spara-mapp) och var varje värde kommer ifrån. Inställningarna kan sättas med
  `options()` (t.ex. i `.Rprofile`): `rdppt.mall` (sökväg, `"regiondalarna"`, `"inbyggd"`
  eller mallprofil) och `rdppt.slidebank` (pptx-fil med färdiga slides, som
  `ppt_befintlig()` hämtar från när ingen fil anges). En parameter går före en
  option, och en option före standardvärdet.

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
