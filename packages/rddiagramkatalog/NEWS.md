# rddiagramkatalog 0.0.0.9092

* Ny `karta_gg_bakgrund_sverige()`: bakgrundskarta över hela Sverige eller flera län ur Topografi 1M (schemat `bakgrundskarta_sverige`, byggt med
  `rdgis::skapa_bakgrundskarta_topografi(kalla_schema = "topografi_1m")`). Vatten, skog, fjäll, glaciärer, bebyggelse, stora vägar, järnväg, vattendrag och länsgränser, med
  etikettpaket för städer, tätorter, största tätort per län och större vatten. `utland` styr hur grannländerna ritas. Kräver rdgis 0.0.0.9008.

# rddiagramkatalog 0.0.0.9091

* `tid_koder` hanteras nu rätt när tidsserien delas mellan flera SCB-tabeller, eller när "9999" kombineras med andra år, i `diag_flytt_inrikes_aldersgrupper_scb()`,
  `diag_flytt_inrikes_utrikes_netto_scb()`, `diag_inflyttlan_utflyttlan_scb()`, `diag_fruktsamhet_scb()` (`"9999"` gav två år), `diag_storsta_fodelseland_scb()` (`"2024"` kraschade,
  `c("2020","9999")` tappade 2020), `diag_syss_rams_bas_tidsserie_fran_ar1993_scb()` (`"9999"` kraschade), `diag_syss_vistelsetid_inr_utr_fodda_ett_per_ar_scb()`,
  `diag_storsta_yrke_per_geografi_scb()` och `diag_utbniva_inr_utr_fodda_kon_lan_scb()` (`"9999"` tillsammans med andra år tappades eller kraschade).
  Tidskoder som inte finns i någon tabell ger ett tydligt felmeddelande i stället för "Can't rename columns that don't exist".

# rddiagramkatalog 0.0.0.9090

* Felmeddelandet när inga av de begärda tidskoderna finns i SCB:s tabeller säger nu "perioderna" i stället för "åren", eftersom det även gäller månadsskripten.

# rddiagramkatalog 0.0.0.9089

* Tidsserier som delas mellan en historiktabell och en CKM-tabell hämtar nu rätt år ur respektive tabell (`intern_tid_for_tabell()`) i `diag_befolkningsforandring_region_kon_ar_scb()`,
  `diag_befolkningsforandring_scb()`, `diag_befolkningsforandring_manad_scb()`, `diag_befutv_per_komponent_ar_scb()`, `diag_bef_inr_utr_en_aldersgrupp_scb()`,
  `diag_fodda_manad_scb()`, `diag_fodelsenetto_region_scb()`, `diag_inr_flytt_in_ut_netto_per_alder_scb()`, `diag_inr_flytt_in_ut_netto_tidsserie_per_alder_scb()` och
  `diag_utrikes_fodda_tidsserie_scb()`. Förut gav `tid_koder = "9999"` sista året i *varje* tabell (t.ex. både 2024 och 2025), och i `diag_befutv_per_komponent_ar_scb()`
  tappades 2025 eftersom CKM-tabellens nyaste period (2026) saknar värden. Nu ger `"9999"` senaste året med data. Ett år som inte finns i någon tabell ger ett tydligt felmeddelande
  i stället för "Can't rename columns that don't exist".
* Nya interna hjälpfunktioner `intern_stoppa_om_ingen_tid()` och `intern_behall_senaste_tid()`; `intern_tid_for_tabell()` skickar de senaste åren i stället för `"9999"` till den nyaste tabellen.
  `diag_etablering_utb_kon_scb()` och `diag_arbloshet_syss_utbildning_facet_ar_scb()` använder `intern_behall_senaste_tid()` så att `"9999"` ger senaste året med data.
* `diag_utrikes_fodda_tidsserie_scb()` kraschade med "missing value where TRUE/FALSE needed" när `tid_koder` gav ett enda år eller för få år i prognosen. Förändringsdiagrammen
  (kommuner, län, prognos) hoppas nu över med en varning i stället.

# rddiagramkatalog 0.0.0.9088

* Ny intern hjälpfunktion `intern_tid_for_tabell()` som väljer vilka `tid_koder` som ska skickas till varje tabell när en tidsserie delas mellan flera SCB-tabeller
  (enskilda år skärs mot tabellens år, `"9999"` hämtas bara ur den nyaste tabellen). Regeln finns i KONVENTIONER.md (avsnittet CKM).
* `diag_etablering_utb_kon_scb()` gav fel för enskilda år (t.ex. `tid_koder = "2024"`) och fel år för `"9999"`, eftersom samma koder skickades till både
  historiktabellerna (till och med 2021) och de nya (från 2022). Nu hoppas tabeller utan de begärda åren över, och inga år alls ger ett tydligt felmeddelande.
* `diag_arbloshet_syss_utbildning_facet_ar_scb()` använder den nya hjälpfunktionen i stället för en lokal kopia.

# rddiagramkatalog 0.0.0.9087

* `diag_arbloshet_syss_utbildning_facet_ar_scb()` (0.0.0.9086) heter nu `diag_arbloshet_syss_utbildning_facet_ar_scb()`. Det ursprungliga namnet
  `diag_arbloshet_procent_utbildning_facet_ar_scb()` finns kvar som föråldrat alias.
* Nytt mått `matt = "antal_sysselsatta"` (antal förvärvsarbetande 20-64 år) vid sidan av `"arbetsloshet"` (standard) och `"sysselsattningsgrad"`.
* `tid_koder` med enskilda år (t.ex. `"2024"` eller `c("2010", "2020")`) ger inte längre fel när året saknas i en av SCB:s två tabeller (TAB6367 2019-2024,
  TAB5400 2006-2018): de år som saknas i en tabell hoppas över, `"9999"` hämtas bara ur den nyare tabellen och inga år alls ger ett tydligt felmeddelande.

# rddiagramkatalog 0.0.0.9086

* `diag_arbloshet_procent_utbildning_facet_ar_scb()` heter nu `diag_arbloshet_syss_utbildning_facet_ar_scb()` och har en ny parameter
  `matt`: `"arbetsloshet"` (standard, som tidigare) eller `"sysselsattningsgrad"` (förvärvsarbetande i procent av alla 20-64 år, ur SCB:s TAB6367/TAB5400).
  Titel, källtext och filnamn följer måttet. Det gamla namnet finns kvar som föråldrat alias som ger samma diagram som `matt = "arbetsloshet"`.

# rddiagramkatalog 0.0.0.9085

* `karta_gg_kraftfalt_scb()`: textrutan och teckenförklaringen sluter tätare kring innehållet: rutans höjd räknas med uppmätt radavstånd
  och teckenförklaringens bredd följer längsta texten i stället för en fast bredd.

# rddiagramkatalog 0.0.0.9084

* `karta_gg_kraftfalt_scb()`: textrutan är lägre och ligger inte längre över länsgränsen: den är bredare, texten något mindre och höjden räknas
  rätt (blankraderna mellan styckena räknades dubbelt och radavståndet var för stort, vilket gav tom yta längst ner).

# rddiagramkatalog 0.0.0.9083

* `karta_gg_kraftfalt_scb()`: tätortsnamn vid lokala arbetsmarknadscentrum och solitära (`tatortsnamn = TRUE`, storlek med
  `namn_storlek`). Namnen är små och mörkgrå med vit kontur och placeras med ggrepel så att de inte hamnar ovanpå punkter eller varandra.

# rddiagramkatalog 0.0.0.9082

* `karta_gg_kraftfalt_scb()`: mindre punkter för lokala arbetsmarknadscentrum (och solitära). Textrutan och teckenförklaringen ligger nu helt i
  hörnen: kartans utsnitt sätts om efter att kraftfältslagren lagts på (geom_sf ersatte det tidigare och gav extra luft runt kartan).
* Bakgrundskartan har ingen marginal runt bilden (`plot.margin` noll).


# rddiagramkatalog 0.0.0.9081

* Ny `karta_gg_kraftfalt_scb()`: statisk (ggplot) version av kraftfältskartan i hög upplösning med den grå topografiska
  bakgrundskartan (med namn på de största sjöarna), fält och tätortspunkter stilsatta som i QGIS-kartan, textruta uppe till
  höger, teckenförklaring nere till vänster och källhänvisning nere till höger. Standardtexten har tröskelvärdena inskrivna.
* Bakgrundskartans hämtning och ritning ligger i en gemensam intern funktion som även andra kartor kan bygga på.

# rddiagramkatalog 0.0.0.9080

* `karta_gg_bakgrund_topografi()`: ny parameter `marginal` (standard 3 %) som lägger en marginal runt länet, så att ytor utanför
  länsgränsen ritas ut och källhänvisningen inte skärs av. Källhänvisningen är två rader och anger bakgrundskartans källa
  (Lantmäteriet, Topografi 10).

# rddiagramkatalog 0.0.0.9079

* `karta_gg_bakgrund_topografi()`: källhänvisning nere till höger i kartan (parametern `kalla_text`, kan stängas av med `NULL`).
  Dokumentationen anger inte längre någon licens för topografin.

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

