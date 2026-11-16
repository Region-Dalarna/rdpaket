# rddiagramkatalog 0.0.0.9078

* `karta_gg_bakgrund_topografi()`: länsgränsen förenklas (50 m) före ritning, vilket går snabbare och inte syns i kartan.

# rddiagramkatalog 0.0.0.9077

* `karta_gg_bakgrund_topografi()`: länsgränsen och kartans utsnitt kommer från Lantmäteriets länsgränser (`lan_lm`)
  i stället för SCB:s, och den streckade gränsen kan stängas av med `lansgrans = FALSE` (standard `TRUE`).

# rddiagramkatalog 0.0.0.9076

* `karta_gg_bakgrund_topografi()`: när ett sjönamn har flera ytor väljs den största för etiketten.

# rddiagramkatalog 0.0.0.9075

* `karta_gg_bakgrund_topografi()`: nya etikettpaket `"storre_tatorter"` (största tätorten per kommun) och
  `"storre_sjoar"` (sjöarna med högsta textrang). Sjöetiketterna (`"sjoar"`, `"storre_sjoar"`) placeras i mitten av
  sjön, en per sjö. Kräver att lagret `sjoar` byggs med `rdgis::skapa_bakgrundskarta_topografi(lager = "sjoar")`.

# rddiagramkatalog 0.0.0.9074

* `karta_gg_bakgrund_topografi()`: parametern `namn` heter nu `etiketter`.

# rddiagramkatalog 0.0.0.9073

* `karta_gg_bakgrund_topografi()`: etiketter är avstängda som standard. Ny parameter `namn` väljer paket av etiketter
  (`"tatorter"`, `"orter"`, `"sjoar"`, `"vattendrag"`) och `namn_storlek` skalar texten; parametrarna `visa_namn` och
  `ortnamn` utgår. Etiketterna är mindre och ljusare. Länsgränsen ritas utan hål (ingen streckad linje runt Siljan).

# rddiagramkatalog 0.0.0.9072

* `karta_gg_bakgrund_topografi()`: färre etiketter (ny parameter `ortnamn`: bara tätorter som standard; vattennamn
  bara för större sjöar/vattendrag och ett namn per vatten, parametern `minsta_namnstorlek` utgår), gråare färger,
  en enda databasuppkoppling och tidsutskrifter för hämtning, ritning och filskrivning.

# rddiagramkatalog 0.0.0.9071

* Ny `karta_gg_bakgrund_topografi()`: diskret grå bakgrundskarta (som CartoDB Positron) över Dalarna i hög
  upplösning, ritad ur de förberedda topografilagren i geodatabasen (`rdgis::hamta_bakgrundskarta()`).
  Kontrollerar att databasen går att nå och att lagren finns innan något ritas.

# rddiagramkatalog 0.0.0.9070

* `karta_lf_kraftfalt_scb()`: standardvärdet för `totalt_troskelvarde` är nu 45 (tidigare 35), så att
  t.ex. Leksand, Rättvik och Hedemora får kraftfält. Funktionen `rdgeorouting::pendling_kraftfalt()` behåller 35.

# rddiagramkatalog 0.0.0.9069

* `karta_lf_kraftfalt_scb(skriv_gpkg_fil = TRUE)`: kontrollerar att den gamla geopackage-filen verkligen
  gick att ta bort. Tidigare ignorerades ett misslyckat `file.remove()` (på Windows när filen är öppen i
  QGIS), och lagren lades då till ovanpå de gamla, så varje tätort dubblerades (dubbla prickar och
  etiketter). Nu stoppar funktionen med ett tydligt fel som säger att filen kan vara öppen i QGIS.

# rddiagramkatalog 0.0.0.9068

* Regionparametern heter nu `region_vekt` i alla funktioner där den bara hette något annat,
  enligt konventionen i KONVENTIONER.md: `karta_gg_nettopendling_kommun_scb()`
  (`vald_lan_kod`), `karta_lf_kraftfalt_scb()` (`vald_lan`) och
  `karta_lf_pendlingsfloden_scb()` (`vald_kommun_kod`). Anrop med de gamla namnen ger felet
  "unused argument" (i `karta_lf_kraftfalt_scb()` ett eget felmeddelande, eftersom `...` annars
  hade svalt det gamla namnet och ritat standardlänet) - byt namn på argumentet. Parametrar som pekar ut en region att markera
  (`fokus_lan`, `fokus_region`, `fokus_regionkod`, `region_fokus`), ordning
  (`region_lagg_forst`) eller en separat uppsättning regioner (`region_vekt_linje`,
  `lan_kommun`) behåller sina namn.

* `diag_forsorjningskvot_per_region_scb()`: standard för `region_vekt` är nu alla län (utan
  Riket), `rdverktyg::hamtaAllaLan(tamedriket = FALSE)`, i stället för Dalarnas kommuner.

* `diag_forsorjningskvot_per_region_scb()`: skalan på kvotaxeln gick till 2 när någon kvot
  var strax över 1 (den äldre stödlinjeberäkningen avrundar värden mellan 1 och 10 uppåt till
  en jämn tvåa). Funktionen använder nu `stodlinjer_avrunda_fem = TRUE`, så axeln slutar vid
  nästa jämna steg över största värdet (t.ex. 1,2).

# rddiagramkatalog 0.0.0.9067

* `diag_forsorjningskvot_per_region_scb()`: ny parameter `overlappande_staplar` (standard
  `TRUE`) som lägger de två åren ovanpå varandra med genomskinliga staplar i stället för
  bredvid varandra. Den del som sticker ut är förändringen: grön där kvoten ökat, grå där den
  minskat. `genomskinlighet` (standard 0.6) styr genomskinligheten. `overlappande_staplar =
  FALSE` ger tidigare utseende med två staplar per region. Standardfärgerna med överlappning
  är grått (tidigare år) och grönt (senare år) ur Region Dalarnas profil
  (`rd_gra` och `rd_gron`).

* Samma funktion sorterar nu med högsta kvoten överst (tidigare lägst överst).

* Kräver `rddiagram` 0.0.0.9039 (`SkapaStapelDiagram(stapel_overlapp = )`).

# rddiagramkatalog 0.0.0.9066

* Ny funktion `diag_forsorjningskvot_per_region_scb()`: förändring av den demografiska
  försörjningskvoten (yngre + äldre i förhållande till personer i arbetsför ålder) per region,
  en grupp staplar för startåret och ett jämförelseår. Jämförelseåret ligger `jmfrtid` år
  framåt (prognos) eller bakåt (faktisk befolkning). Migrerad från `func_forsorjningskvot.R`
  (`SkapaForsorjningskvot()`) i repot `funktioner`.

* Befolkningen hämtas nu från TAB638 (till och med 2024) och TAB5557 (CKM, från 2025) i
  stället för den gamla v1-tabellen BefolkningNy, med CKM-notering i källtexten när
  CKM-år ingår. Prognosåret kommer från `rdhamtadata::hamta_befprognos_data()`.

* Arbetsför ålder är valbar med `arbetsfor_alder`: `"20-64 år"` (standard, SCB:s
  definition av demografisk försörjningskvot), `"20-65 år"` eller `"20-66 år"`. Originalet
  hade 20-65 år hårdkodat, så kvoterna blir något annorlunda med standardvärdet. Välj
  `"20-65 år"` för att få samma avgränsning som förut.

* Originalets parameter `fokus` (markera kommuner i ett län) och sortering på `diff`
  (förändringen mellan åren) är inte migrerade. `sortera_efter` sorterar på `"slut"` eller
  `"start"`.

* Bugg rättad före första versionen: hundraåringarna fick koden `"100+1"` i
  åldersfrågan, men den koden finns bara i TAB5557. I TAB638 heter de `"100+"`, så de föll
  bort för alla år till och med 2024 (för Riket 2015 saknades 1 896 personer, för Dalarna 63).
  Båda koderna skickas nu med, och summan per region och år stämmer med SCB:s total.

# rddiagramkatalog 0.0.0.9065

* `karta_lf_kraftfalt_scb()`: ny parameter `tryck_kvalitet` (standard `FALSE`). Med
  `bakgrundskarta = "osm"` hämtas bildrutorna från en zoomnivå djupare, så att bakgrunden
  blir dubbelt så skarp för utskrift (fler rutor hämtas från OSM:s server).

# rddiagramkatalog 0.0.0.9064

* `karta_lf_kraftfalt_scb()`: `bakgrundskarta = "osm"` ger OpenStreetMaps vanliga bildrutor
  (ingen WebGL). Används lämpligen med `bakgrund_dimning`.

# rddiagramkatalog 0.0.0.9063

* `karta_lf_kraftfalt_scb()`: ny parameter `bakgrund_dimning` (0-1, standard 0 = av) som
  lägger en ljusgrå, delvis genomskinlig yta över bakgrundskartan (under kraftfälten,
  prickarna och texten) så att bakgrunden blir blekare och mer grå. `bakgrund_dimfarg`
  väljer färgen.

# rddiagramkatalog 0.0.0.9062

* `karta_lf_kraftfalt_scb()` kontrollerar `bakgrundskarta` innan något hämtas.

# rddiagramkatalog 0.0.0.9061

* `karta_lf_kraftfalt_scb()`: ny parameter `bakgrundskarta`. `"openfreemap"`
  (standard) är som tidigare; `"esri_gra"` är Esris gråa bakgrundskarta som
  rastertiles. Den ritas utan WebGL och ger därför bakgrund även i png-filer på
  datorer där headless Edge/Chrome saknar WebGL. Kontrollera Esris
  användarvillkor för ditt användningsfall.

