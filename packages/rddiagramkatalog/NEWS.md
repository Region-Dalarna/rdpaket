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

