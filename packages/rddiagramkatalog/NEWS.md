# rddiagramkatalog 0.0.0.9056

* Kräver rdgeorouting 0.0.0.9007, som döljer databasens NOTIS-meddelanden i `karta_lf_kraftfalt_scb()`.

# rddiagramkatalog 0.0.0.9055

* Kräver rddiagram 0.0.0.9035, där html-kartorna får Region Dalarnas ikon som favicon.

# rddiagramkatalog 0.0.0.9054

* `karta_lf_kraftfalt_scb()`: nya parametrar `karta_bredd`, `karta_hojd`, `zoom`
  (skarp png, `zoom = 3`) och `favicon`. png fungerar nu även när webshot2 inte
  går att köra (Chrome/Edge tar bilden direkt). Kräver rddiagram 0.0.0.9034 och
  rdgeorouting 0.0.0.9005 (tystare databasnotiser).

# rddiagramkatalog 0.0.0.9053

* Kräver rdgeorouting 0.0.0.9004.

# rddiagramkatalog 0.0.0.9052

* Kräver rdgeorouting 0.0.0.9003 (rättar "crs not found" i kraftfältsberäkningen).

# rddiagramkatalog 0.0.0.9051

* `karta_lf_kraftfalt_scb()`: tar `...` som skickas till
  `rdgeorouting::pendling_kraftfalt()` (t.ex. `kant_tabell`). Kräver
  rdgeorouting 0.0.0.9002, som räknar mot ruttanalyser och rätt vägnät.

# rddiagramkatalog 0.0.0.9050

* `karta_lf_kraftfalt_scb()` kopplar upp mot sekretessdatabasen med
  `rdpostgres::uppkoppling_adm("sekretess")`.

# rddiagramkatalog 0.0.0.9049

* Ny funktion `karta_lf_kraftfalt_scb()`: leaflet-karta över tätorters
  kraftfält (lokala arbetsmarknadscentrum, satelliter och kraftfältens lila
  fält), beräknade med `rdgeorouting::pendling_kraftfalt()` på
  `rutor.pend_relation_tatorter` i sekretessdatabasen. Neutral bakgrundskarta
  utan logga, länsgräns, förklaringsruta och textruta. Året pendlingsdata
  gäller (standard 2022) anges med `pendlingsar` eftersom tabellen saknar
  årskolumn. Kräver paketet rdgeorouting.

# rddiagramkatalog 0.0.0.9048

* "Skriv till fil" i `diagramkatalog_utforska()` gav varningar som
  "cannot resolve owner of file ..." (Windows, filer på G: vars ägarkonto
  inte går att slå upp). Funktionen kollar nu bara filernas ändringstid
  (`file.mtime()`) i stället för att hämta all filinformation inklusive ägare
  (`file.info()`). Samma sak i den interna hjälpfunktionen som hittar senaste
  indatafil.

# rddiagramkatalog 0.0.0.9047

* `diagramkatalog_utforska()`: länken till den interaktiva exempelkartan
  (html) visas även när ingen exempelbild (png) finns för en `karta_lf_*()`,
  i stället för att bara visa "Inget exempel sparat".

# rddiagramkatalog 0.0.0.9046

* `karta_gg_befolkningsprognos_kommun_scb()` och
  `karta_lf_befolkningsprognos_kommun_scb()` har samma stil som
  nettopendlingskartan: profilens färger (`diagramfarger("rd_karta_rod_gron")`,
  grönt för ökning och rött för minskning), klasser symmetriska kring noll med
  ett smalt vitt mittenintervall (minst ±0,1 procent) och upp till 11 klasser.
  Titeln är "Prognosticerad befolkningsutveckling i <region> år <från>–<till>",
  källtexten två rader (källa och bearbetning på första) och ggplot-kartan har
  teckenförklaringen i ett fritt hörn (`legend_position = "auto"`), något
  mindre text och mer luft under titeln. Ny parameter `karta_fargvektor`.
  Leaflet-kartan använder samma klasser och färger som ggplot-kartan. Kräver
  rddiagram 0.0.0.9029.

# rddiagramkatalog 0.0.0.9045

* `karta_gg_nettopendling_kommun_scb()`: ny parameter `legend_position`.
  Standard `"auto"` lägger teckenförklaringen i ett hörn där den inte hamnar
  ovanpå någon kommun (nere till vänster om det är fritt) och annars utanför
  kartan. Egen placering anges med t.ex. `"bottom-left"` eller `"right"`.
  Teckenförklaringen är också något mindre. Kräver rddiagram 0.0.0.9028.
* Alla fyra `karta_*()`-funktioner stoppar nu direkt, utan att hämta data eller
  rita, om kartan varken ska skrivas till fil eller returneras
  (`skriv_diagramfil`/`skriv_leaflet_fil` och `returnera_figur` är alla
  `FALSE`), med ett meddelande om vad som ska sättas. Det går att ha bara fil,
  bara returnerad karta eller båda.

# rddiagramkatalog 0.0.0.9044

* `karta_gg_nettopendling_kommun_scb()`: teckenförklaringen ligger uppe till
  höger i kartan (där det är tomt), titeln har lite mer luft mot kartan och
  källtexten är något mindre (storlek 7). Kräver rddiagram 0.0.0.9027.

# rddiagramkatalog 0.0.0.9043

* `karta_gg_nettopendling_kommun_scb()`: nya färger och klasser. Färgerna
  kommer från Region Dalarnas profil (`diagramfarger("rd_karta_rod_gron")`):
  grönt för kommuner med nettoinpendling, rött för nettoutpendling, vitt
  kring noll. Klassgränserna är symmetriska kring noll med ett mycket smalt vitt
  mittenintervall (cirka 1 % av största värdet, t.ex. -50 till 50 när
  största värdet är 5 000) och upp till 11 klasser, anpassade efter hur stor
  nettopendlingen är. Tidigare var alla värden mellan 0 och 2000 vita. Titeln är nu "Nettopendling i <län> år <år>" och källtexten två
  rader. `karta_fargvektor` har `NULL` som standard. Kräver rddiagram
  0.0.0.9026.
* `KONVENTIONER.md`: nya avsnitt om färger (i första hand
  `diagramfarger()`, helst profilens) och om kartors titel och källtext.

# rddiagramkatalog 0.0.0.9042

## Kartfunktionerna delade i ggplot och leaflet

Prefixet säger nu vilken sorts output en funktion ger (se
`KONVENTIONER.md`): `diag_` och `karta_gg_` ger ggplot (png/svg),
`karta_lf_` ger interaktiva leaflet-kartor (html).

* `karta_pendling_scb()` är ersatt av
  `karta_gg_nettopendling_kommun_scb()` (koropletkarta över nettopendling per
  kommun i ett län) och `karta_lf_pendlingsfloden_scb()` (flödeslinjer från en
  kommun). De delade bara data. `valt_ar` kan vara vilket år som helst från
  1993. Flödeskartan hämtar bara data för de valda kommunerna i stället för
  för hela länet.
* `karta_befolkningsprognos_scb()` är ersatt av
  `karta_gg_befolkningsprognos_kommun_scb()` och
  `karta_lf_befolkningsprognos_kommun_scb()`: samma karta i varsitt format.
* De gamla namnen är borttagna utan övergång.
* Alla `karta_*()` returnerar en namngiven lista av `ggplot`- respektive
  `leaflet`-objekt, osynligt. Det ger inte längre en lång utskrift av alla
  kartor i konsolen; det som skrivs är "Karta skapad: <sökväg>" för varje
  fil. Kräver rddiagram 0.0.0.9025.
* Filnamnen för flödeskartorna börjar nu på `pendlingsfloden_`.
* `diag_arbetspendling_over_tid_scb()` hämtar sin data med
  `rdhamtadata::hamta_pendling_rams_bas_scb()` i stället för med en egen kopia
  av samma kod.
* `diagramkatalog_utforska()`: leaflet-kartor öppnas i webbläsaren vid
  "Skapa diagram", "Skriv till fil" fungerar även för `karta_lf_*()`, och
  exempelkartor har en länk till den interaktiva kartan.
* `data-raw/skapa_exempeldiagram.R` skriver för `karta_lf_*()` både en png
  och en html-fil (med delat `lib/`).

# rddiagramkatalog 0.0.0.9041

* `karta_pendling_scb()` och `karta_befolkningsprognos_scb()` använder inte
  längre CartoDB Positron som bakgrundskarta. CartoDB kräver sedan
  25 september 2026 en API-nyckel, och utan nyckel visar leaflet-kartorna
  bara en "API KEY REQUIRED"-bild i stället för bakgrunden. De använder nu
  `rddiagram::lagg_till_bakgrundskarta()` (OpenFreeMap Positron: samma
  ljusgrå stil, gratis utan nyckel). Pendlingskartan, som lägger egna
  kommunnamn ovanpå, använder varianten utan text. Kräver rddiagram
  0.0.0.9023.

# rddiagramkatalog 0.0.0.9040

* `karta_pendling_scb()` har nu `vald_kommun_kod = "2080"` (Falun) som
  standard och går därmed att köra utan argument. Det var den enda av
  katalogens 92 `diag_*()`/`karta_*()`-funktioner som hade en parameter
  utan standardvärde. Regeln står nu i `KONVENTIONER.md`.

# rddiagramkatalog 0.0.0.9039

* `diag_bef_utfall_prognos_per_aldersgrupp_scb()` och
  `diag_bef_utfall_prognos_per_region_totalt_scb()` stannar nu med ett
  tydligt fel ("Kunde inte hämta befolkningsprognosen ...") om
  prognoshämtningen misslyckats. Tidigare fortsatte de med `NULL` och
  kraschade tre steg senare med `object 'ålder' not found`, vilket dolde
  den verkliga orsaken. Orsaken till det fel som upptäcktes så (`object
  '.SD' not found` i `rdhamtadata::hamta_befprognos_data()`) är fixad i
  rdhamtadata 0.0.0.9004, och rdverktyg 0.0.0.9008 visar nu alltid det
  sista felet när `funktion_upprepa_forsok_om_fel()` ger upp.

# rddiagramkatalog 0.0.0.9038

## `diagramkatalog_utforska()`: ny knapp "Skriv till fil"

* Ny knapp i gadgetens titelrad, mellan "Skapa diagram" och "Infoga kod".
  Kör samma funktionsanrop som formuläret bygger upp, men tvingar
  `skriv_diagramfil = TRUE` oavsett kryssrutans läge, så att bilden
  faktiskt skrivs till den `utmapp` som står i formuläret.
* Kräver att funktionen har både `utmapp`- och `skriv_diagramfil`-
  parametrar (alla 92 funktioner i katalogen har det i dag) och att
  `utmapp`-fältet är ifyllt - annars visas ett tydligt felmeddelande i
  stället för att försöka köra anropet.
* En tillfällig notis (7 sekunder) bekräftar efteråt att diagrammet har
  skapats, med full sökväg + filnamn för de fil(er) som faktiskt skrevs/
  uppdaterades i `utmapp` under anropet (funktionerna själva returnerar
  bara ggplot-/leaflet-objekt, inte vilket filnamn de skrev till, så
  filen identifieras genom att jämföra mappens innehåll före/efter
  anropet).

# rddiagramkatalog 0.0.0.9037

## Städning

* De interna hjälpfunktionerna `intern_sakerstall_indatafil()` och
  `intern_meddela_manuell_datakalla()` är flyttade till
  `rdverktyg::sakerstall_indatafil()`/`rdverktyg::meddela_manuell_datakalla()`
  - samma två funktioner fanns duplicerade i `rdppt`. Ingen ändrad
  funktionalitet, bara en delad implementation.

# rddiagramkatalog 0.0.0.9036

## Ny funktion: `karta_pendling_scb()` - andra kartan, första med flera-kartor-stöd

Migrerad ur `karta_pendling_leaflet.R` (`karta_arbetspendling()`). Tar emot
en eller flera kommuner och en eller flera pendlingstyper
(`vald_pendlingstyp = "alla"` ger samtliga fyra) i ETT anrop, precis som
källskriptets batch-/loop-mönster - returnerar en lista med en
`list(ggplot=, leaflet=)` per kombination (eller bara `list(ggplot=,
leaflet=)` rakt av om det blir exakt en kombination), enligt den nya
konventionen i 0.0.0.9035.

**Medvetet designval (beslutat med användaren):** ggplot-versionen är INTE
samma flödeslinjer som leaflet-versionen (ingen vettig statisk motsvarighet
till flera överlappande flödeslinjer) - i stället en enklare, regional
choroplettkarta över NETTOPENDLING per kommun (inpendlare minus
utpendlare, oavsett vald `vald_pendlingstyp`) i det län den valda kommunen
ligger i, med samma mönster (`rddiagram::skapa_koropletkarta_ggplot()`,
diskreta nollcentrerade klasser) som `karta_befolkningsprognos_scb()`.
Beräknas en gång per län och återanvänds för alla kombinationer i samma
län, i stället för att räknas om i onödan. Datahämtningen breddades
därför till hela länet/länen (inte bara de uttryckligen valda
kommunerna) - choropletten behöver nettopendling för samtliga kommuner i
länet.

leaflet-versionen behåller källskriptets detaljerade flödeslinjer
oförändrade i sak. gpkg-export (GeoPackage + valfria `.qml`/`.lyrx`-
stilfiler för QGIS/ArcGIS) behålls som en egen parameter
(`skriv_gpkg_fil`/`skriv_stilfiler`), oberoende av `skriv_leaflet_fil`.

**Borttaget:** `mapview`/`webshot` (gamla, inte `webshot2`) som
beroenden - leaflet-filen skrivs nu med `rddiagram::skriv_leaflet_till_fil()`
i stället för `mapview::mapshot()`, vilket även löser en bugg som redan
var dokumenterad i källskriptets egen kommentar (mapview laddades aldrig
någonstans, fungerade bara i sessioner där det råkade vara laddat av
andra skäl). Ett icke-fungerande försök att bifoga ett eget CSS-stilblad
från en lokal G:-sökväg (källskriptets egen kommentar: "har inte fått
detta att fungera") togs också bort - gjorde ingenting, och sökvägen
fanns bara på en specifik dators G:-enhet.

**Inte testat mot en riktig databas** - samma begränsning som
`karta_befolkningsprognos_scb()` (`rdgis::hamta_karta()` kräver en live
PostGIS-koppling). Datahämtningen (`rdhamtadata::hamta_pendling_rams_bas_scb()`)
och list-/namnuppbyggnadslogiken för flera kombinationer är testade.



Stöd för `karta_*()`-funktioner som kan producera FLERA kartor i ett
anrop (t.ex. en per kommun/pendlingstyp, som källrepots batch-/
loop-baserade kartskript gör) - returnerar en namngiven lista där varje
element själv är `list(ggplot=, leaflet=)`, i stället för att listan är
en enda karta. Sökindexet, `data-raw/skapa_exempeldiagram.R` och
`diagramkatalog_utforska()`s förhandsvisning känner nu av tre fall:
en enkel-karta-funktion (`$ggplot` finns direkt), en flera-kartor-
funktion (`$ggplot` finns på första elementet), eller ett vanligt
`diag_*()`-diagram (första elementet är redan ett `ggplot`-objekt).
Se KONVENTIONER.md. Första användningen: den kommande
`karta_pendling_scb()`.



`diag_inr_flyttnetto_bakgrund_scb()`: två buggar hittade (av användaren,
som granskade en testbild) och fixade i `facet_aldersgrupp`-läget från
0.0.0.9032:

- Sista åldersgruppen visade "66-100 år" i stället för "66+ år". Orsak:
  koden som bygger åldersgränsvektorn för `rdverktyg::skapa_aldersgrupper()`
  jämförde mot strängen `"100+"` för att avgöra om den sista gruppen hade
  en öppen övre gräns, trots att det faktiska sentinelvärdet (dokumenterat
  i `alder_grp`, och i originalskriptets egen kommentar) är `"999"`.
  Jämförelsen utlöstes därför aldrig, vilket satte en konkret gräns i
  stället för en öppen - fanns redan i originalet.
- Teckenförklaringen (född i Sverige/utrikes född) saknades i
  facet-diagram. `SkapaStapelDiagram()` döljer legenden som standard vid
  facettering om inte `facet_legend_bottom = TRUE` anges explicit - satt
  nu för båda facet-lägena (`facet_diagram` och `facet_aldersgrupp`).



## Ny funktionsfamilj: `karta_*()`

Första kartan migrerad: `karta_befolkningsprognos_scb()` (ur
`karta_befolkningsprognos.R`), choroplettkarta över prognosticerad
befolkningsförändring per kommun. Till skillnad från `diag_*()`-funktionerna
returnerar `karta_*()`-funktioner alltid BÅDE en ggplot- och en
leaflet-version: `list(ggplot = <ggplot-objekt>, leaflet = <leaflet-objekt>)`.
ggplot-versionen byggs med den redan befintliga delade byggfunktionen
`rddiagram::skapa_koropletkarta_ggplot()` (samma mall/profil som används av
andra kartor i paketet, i stället för att återskapa ggplot-koden för hand
som källskriptet gjorde) - skillnad mot originalet: använder diskreta,
nollcentrerade klasser i stället för en kontinuerlig färgskala, eftersom
den delade byggfunktionens kontinuerliga läge centrerar mittfärgen på
MITTEN AV DATAINTERVALLET snarare än på 0, vilket hade varit missvisande
för ett ökning/minskning-mått om intervallet inte råkar vara symmetriskt.

Leaflet-versionen skrivs till fil med den nya `rddiagram::skriv_leaflet_till_fil()`
(se rddiagram NEWS.md) - html (standard, interaktiv) eller png (statisk,
via webshot2) via den nya parametern `leaflet_filformat`.

Sökindexet (`sok_diagram()`/`diagramkatalog_utforska()`) och
`skapa_exempeldiagram.R` känner nu av `karta_*()`-funktioner på samma sätt
som `diag_*()` - förhandsvisningen i gadgeten och de automatiskt
genererade exempelbilderna använder ggplot-versionen av returlistan.

**Inte testat mot en riktig databas** - `rdgis::hamta_karta()` kräver en
live PostGIS-koppling (`rdpostgres::uppkoppling_db()`), inte tillgänglig i
den här miljön. Datahämtningen (pxweb2r) och hela fil-skriv-pipelinen
(inkl. webshot2) är däremot testade fullt ut mot SCB:s live-API respektive
en egen testkarta.

# rddiagramkatalog 0.0.0.9032

`diag_inr_flyttnetto_bakgrund_scb()`: ny parameter `facet_aldersgrupp` -
visar åldersgrupperna (`alder_grp`) som facet i diagrammet i stället för
att bara summera ihop dem (vilket tidigare var enda alternativet - datan
var rätt men syntes aldrig uppdelad i själva diagrammet, en känd
begränsning från 0.0.0.9031). Kan inte kombineras med `facet_diagram`
(bara en facet-variabel i taget). Stänger av `visa_totalvarden`, som
annars skulle räkna fel per åldersgrupp.

# rddiagramkatalog 0.0.0.9031

Slog ihop `diag_inr_flyttnetto_bakgrund_scb()` med sitt tidigare omigrerade
syskonskript (`diag_inr_flyttnetto_inr_utr_fodda_scb.R` i källrepot - de
två källskripten råkade dela funktionsnamn, se migreringsstatus.md).
Funktionen har nu samma funktionalitet som båda ursprungsskripten
tillsammans: åldersgruppsuppdelning (`alder_grp`) och relativt flyttnetto
(`relativt_flyttnetto`), utöver det som redan fanns.

Tre buggar hittades och fixades under sammanslagningen (fanns redan i ett
eller båda ursprungsskripten, inga nya):

- **`gruppera_namn` kraschade** ("object 'regionkod' not found") - kolumnen
  sattes aldrig efter gruppering. Fanns redan i det tidigare migrerade
  skriptet, bekräftat genom test. Satt till `"grp"` nu, samma konvention
  som `rdhamtadata::hamta_befprognos_diff_data()` använder för
  motsvarande gruppering.
- **Totalstrecket i teckenförklaringen ritades som en missvisande
  sicksack-linje** genom de råa dataraderna i stället för att vara en ren
  förklaringssymbol (upptäckt genom visuell kontroll av testbilder).
  Fixat med `alpha = 0` på det laget - samma lösning som redan fanns i
  det tidigare omigrerade syskonskriptet, nu återanvänd.
- **Filnamnskrock**: en åldersuppdelad bild fick exakt samma filnamn som
  standardbilden (ingen av ursprungsskripten skilde på dem i filnamnet),
  så den ena skulle tyst skriva över den andra om båda genererades till
  samma mapp. Lade till ett `_aldersgrupper`-suffix.

**Känd begränsning, inte fixad:** `alder_grp` ger rätt data (och fungerar
för `returnera_data`/Excel-export), men själva diagrammet skiljer INTE
visuellt på åldersgrupperna - samma begränsning fanns redan i det
omigrerade syskonskriptet (ingen facet/färg kopplad till `aldersgrupp` i
`SkapaStapelDiagram()`-anropet). Inte löst här, eftersom det är en
designfråga (hur ska åldersgrupperna visas?) snarare än en bugg.

# rddiagramkatalog 0.0.0.9030

Migrerade 10 ytterligare diagram från Region-Dalarna/diagram:

**Raka PxWeb-diagram, med G:-härdning där det behövdes:**

- `diag_asylsokande_tidsserie_scb` (ex. diag_antal_asylsokande_1984_IntRap.R)
  - kombinerar en manuellt underhållen G:-Excel-fil (äldre år) med
    live SCB-data - härdad med `intern_hamta_senaste_fil()`.
- `diag_fek_foradlingsvarde_bransch_lan_scb` (ex.
  diag_fek_foradlingsvarde_bransch_lan_scb.R) - läser en delad
  branschnyckel från G:, härdad med `intern_sakerstall_indatafil()`.
- `diag_fek_lve_tidsserie` (ex. diag_fek_lve_tidsserie.R) - samma
  branschnyckel, läst en gång i stället för två (originalet läste
  samma fil två gånger).
- `diag_inr_flyttnetto_bakgrund_scb` (ex.
  diagram_flytt_inrikes_bakgrund_SCB.R, funktionen
  `diag_inr_flyttnetto_inr_utr_fodda`) - döpt om vid migrering: källrepot
  har ett ANNAT skript (diag_inr_flyttnetto_inr_utr_fodda_scb.R, inte
  migrerat än) med exakt samma funktionsnamn men mer funktionalitet
  (åldersgrupper, relativt flyttnetto) - ett av de dubbla
  funktionsnamn-par som flaggats tidigare i projektet. Bugfix (confirmed):
  en `write.xlsx()`-rad refererade en odefinierad variabel
  (`arblosa_bakgr`, kvarglömd från ett annat skript) - oanvänd rad borttagen.

**NMS/G:-baserat, samma härdningsmönster som tidigare:**

- `diag_bransch_utb_alder_nms` (ex. diagram_bransch_utb_alder_NMS.R).
  Bugfix (confirmed): Excel-export skedde efter `return()` och kördes
  därmed aldrig vid `returnera_figur = TRUE` (standardvärdet) - samma
  mönster som hittats upprepade gånger tidigare i projektet, flyttad
  före returneringen.

**Andel offentligt anställda - bara den aktiva varianten migrerad:**

- `diag_andel_offentligt_bas_scb` (ex. diagram_andel_offentligt_BAS.R).
  Den RAMS-baserade varianten (diagram_andel_offentligt.R) migrerades
  INTE - dess egen källkod flaggar uttryckligen att den är ersatt av
  BAS-varianten och bygger på frusen data (2019-2021). Detta korrigerar
  en tidigare bedömning i migreringsstatus.md som felaktigt klassade de
  två som "inte dubbletter, båda aktiva".

**Aktiv ersättare till avvecklat skript:**

- `diag_arb_stallen_mm_foretagarna` (ex.
  diagram_arb_stallen_mm_foretagarna.R, ersätter det redan tidigare
  avvecklade diagram_arb_stallen_mm_ejAPI.R). Bugfix (confirmed): en
  villkorssats för att spara Excel-data kontrollerade fel variabel
  (`output_mapp_figur` i stället för `output_mapp_data`) - rättad till
  att matcha samma mönster som resten av funktionen.

**G:-källad hämtkod (inte bara data) - själva hämtlogiken ligger på G:
och behandlas som en svart låda, härdad med `intern_sakerstall_indatafil()`
på skriptfilen innan den sourcas:**

- `diag_gymnasieantagna_antal` (ex. diagram_gymnasiantagning_antal_kon.R).
  Bugfix (confirmed): samma export-efter-return-mönster som ovan, samt
  en oanvänd/aldrig implementerad parameter (`diag_lan_antal`) borttagen.
- `diag_utslapp_konsumtion_sei` (ex. diag_utslapp_konsumtion_SEI.R).
  Bugfix (confirmed): kommundiagrammets titel/filnamn använde fel
  dataframe (länsdatasetets år i stället för kommundatasetets) - gav fel
  årtal om de skiljde sig åt, och kraschade helt om `diag_lan = FALSE`.

**G:-lokal "senaste fil"-mönster, nu med stöd för rekursiv sökning:**

- `diag_kompetensbrist_lan_tvv` (ex. diagram_kompetensbrist_lan_TVV.R).
  `intern_hamta_senaste_fil()` i `intern_indata.R` fick en ny valfri
  `recursive`-parameter (standard `FALSE`, bakåtkompatibel) för att
  stödja detta skriptets rekursiva filsökning.

Alla tio verifierade: de 3 utan G:-beroende (flyttnetto, andel
offentligt, företagarna) smoke-testade live; de 7 G:-beroende
bekräftade ge tydliga, handlingsbara felmeddelanden i den här
sandlådan (där G: inte finns).



Migrerade 5 befolkningsprognosdiagram ("Profet"-gruppen) från
Region-Dalarna/diagram. Dessa bygger nu på det nya paketet
`rdhamtadata` (`hamta_befprognos_data()`,
`hamta_befprognos_diff_data()`, `hamta_befprognos_riket_inr_utr_scb()`)
i stället för `source()` mot externa GitHub-skript:

- `diag_befolkningsprognos_profet_scb` (ex. `SkapaBefPrognosDiagram()`
  i diag_befolkningsprognos_scb_api_profet.R)
- `diag_befolkningsforandring_komponenter_prognos_scb` (ex.
  `SkapaBefPrognosDiagram_befforandr()` i samma källfil)
- `diag_bef_utfall_prognos_per_aldersgrupp_scb`
- `diag_bef_utfall_prognos_per_region_totalt_scb`
- `diag_befprognos_diff_tot_per_region_scb` (ex.
  diag_befprognos_diff_tot_per_kommun.R)

En tredje funktion i diag_befolkningsprognos_scb_api_profet.R,
`SkapaBefPrognosDiagram_InrUtrFodda()`, migrerades INTE - källkoden
bekräftar uttryckligen att den redan är ersatt av pxweb2r-anrop i
`diag_utrikes_fodda_tidsserie_scb()` (dess `diag_forandring_prognos`-gren,
TAB6008), som redan fanns i paketet sedan tidigare.

Alla fem diagram jämför tidigare kunnat regionens prognos mot riket
("00"), vilket krävde en v1-pxweb-tabell (BE0401A/BefolkprognRevNb) som
källkoden själv flaggade som "inte migrerad till pxweb2r ännu". Hittade
och verifierade v2-motsvarigheten (TAB5974, "Folkmängd efter
inrikes/utrikes född, ålder och kön") via `pxweb2r::pxweb2_search_tables()`
och en live-jämförelse av variabelstruktur/etiketter - nu en vanlig
pxweb2r-baserad funktion (`rdhamtadata::hamta_befprognos_riket_inr_utr_scb()`)
i stället för legacy v1-pxweb-kod.

Bugfix (confirmed via källkoden, egen TODO-kommentar i originalet):
`diag_befprognos_diff_tot_per_region_scb` använde en odefinierad
variabel `skickad_jmfrtid` i en gren som bygger diagramtiteln vid flera
prognosår - bytt till `jmfrtid`. Grenen körs inte i praktiken idag
(hamta_befprognos_diff_data() ger alltid ett enda prognosår) men är
fixad för att inte krascha om fler prognosår stöds i framtiden.

Alla fem smoke-testade live (inklusive riket-grenen) mot SCB:s API.

# rddiagramkatalog 0.0.0.9028

Migrerade 7 diagram som läser indata från lokala/manuellt underhållna
filer (G:-enheten eller en delad GitHub-nyckel), i stället för en
live-API-hämtning. Lade samtidigt till konsekvent härdning av denna typ
av indata (se diskussion i chatten):

- `diag_bas_syss_per_bransch_manad_jmfr_1ar_tillbaka_scb` (ex.
  diag_bas_syss_bransch_jmfr_manad_1ar_tillbaka.R)
- `diag_sysselsatta_andel_scb` (ex. diagram_andel_forvarvsarbetande_bransch.R)
- `diag_sysselsatta_forandring_bransch_scb` (ex. diagram_forandring_sysselsatta_bransch.R)
- `diag_sysselsatta_forandring_bransch_kommun_scb` (ex. diagram_forandring_sysselsatta_bransch_kommun.R)
- `diag_50proc_lonesumma_nms` (ex. diagram_50_proc_lonesumma_NMS.R)
- `diag_examen_hogskolan_nms` (ex. diagram_examen_hogskolan_NMS.R)
- `diag_utb_niva_bakgr_alder_nms` (ex. diagram_utb_bakgr_alder_NMS.R)

Nya interna hjälpfunktioner (R/intern_indata.R):
- `intern_meddela_manuell_datakalla()` - skriver alltid ut varifrån data
  kommer och att den är en manuell ögonblicksbild som måste uppdateras
  manuellt, inte en live-hämtning.
- `intern_sakerstall_indatafil()` - validerar att en specifik indatafil
  (hel sökväg inkl. filnamn) finns, med ett tydligt fel om den saknas
  eller flyttats - i stället för det kryptiska "cannot open file" som
  annars dyker upp långt senare.
- `intern_hamta_senaste_fil()` - hittar senaste filen i en mapp som
  matchar ett mönster, med ett tydligt fel om ingen fil matchar - i
  stället för det obegripliga felet `which.max()` annars ger på en tom
  träfflista.
- `intern_hamta_branschnyckel()` - hämtar Region Dalarnas delade
  bransch-/färgnyckel från GitHub-repot `depot` i stället för en lokal
  G:-kopia (fyra av skripten ovan + den redan migrerade
  `diag_forandr_branscher_bubblor_scb`, som uppdaterades att använda
  samma delade hjälpfunktion).

Fixade även samma typ av "Excel-export kördes aldrig"-bugg som i
tidigare omgångar, den här gången i diag_sysselsatta_andel_scb och
diag_examen_hogskolan_nms.

`diagram_arb_stallen_mm_ejAPI.R` migrerades INTE - skriptet är enligt
sin egen kommentar och en print()-varning i originalet inte längre
underhållet och ersatt av `diagram_arb_stallen_mm_foretagarna.R`, som
hämtar data live från foretagsklimat.se i stället för G:. Den är
kandidat för en separat migrering (inget G:-beroende, så den faller
utanför denna omgångs tema).

# rddiagramkatalog 0.0.0.9027

Gjorde returvärdet konsekvent för de tre diagramfunktioner som
avvek från paketets mönster (och från originalskripten) genom att
returnera ett naket `ggplot`-objekt direkt i stället för en namngiven
lista:

- `diag_forandr_branscher_bubblor_scb`
- `diag_inr_flytt_in_ut_netto_per_alder_scb`
- `diag_inr_flytt_in_ut_netto_tidsserie_per_alder_scb`

Samtliga returnerar nu en namngiven lista med ett `ggplot`-objekt, som
alla andra `diag_*()`-funktioner i paketet. Detta är en beteendeändring
för befintlig anropskod som förväntar sig ett rent ggplot-objekt
(måste nu plocka ut det med `[[1]]`).

Förenklade samtidigt tillbaka skapa_exempeldiagram.R:s `diagram_lista[[1]]`-
hantering (specialfallet för naket ggplot-objekt behövs inte längre).

# rddiagramkatalog 0.0.0.9026

Sista 5 diagrammen migrerade från Region-Dalarna/diagram - samtliga 53
"oproblematiska" diagram är nu migrerade till paketet:

- `diag_syss_vistelsetid_inr_utr_fodda_ett_per_ar_scb` (ex.
  diag_syss_vistelsetid_inr_utr_fodda_scb.R; saknade helt en
  skriv_diagramfil-parameter i originalet, lade till en, standard TRUE)
- `diag_utbniva_tidserie_och_lansjmfr_scb` (ex. diag_utbniva_flera_diagram_scb.R)
- `diag_utbniva_inr_utr_fodda_kon_lan_scb` (ex. diag_utbniva_inr_utr_fodda_lan_scb.R)
- `diag_uvas_bakgrund_vistelsetid_scb` (ex. diag_uvas_vistelsetid_kon_tid_IntRap.R,
  funktionen hette internt diag_UVAS_bakgrund_vistelsetid)
- `diag_storsta_yrke_per_geografi_scb` (ex. diag_yrke_ssyk3_storsta_per_geografi.R,
  funktionen hette internt diag_storsta_yrke_per_geografi)

Kvarstående arbete efter denna migrering (inte del av de 53): de 3
par av dubblerade funktionsnamn, de 8 filer med source()-anrop mot
externa/avvecklade repon, samt karta_*-filer, behöver fortfarande en
egen genomgång innan Region-Dalarna/diagram kan fasas ut helt.

# rddiagramkatalog 0.0.0.9025

Ytterligare 4 diagram migrerade från Region-Dalarna/diagram (48 av 53
nu klara):

- `diag_sjalvskattad_halsa_kon_lan_fohm` (ex. diag_sjalvskattad_halsa_kon_lan_fohm.R,
  funktionen hette internt diag_sjalvskattad_halsa_kon_lan; saknade
  helt en skriv_diagramfil-parameter i originalet - diagrammet skrevs
  alltid till fil. Lade till parametern, standard TRUE, för att följa
  paketets konvention och göra funktionen testbar utan sidoeffekter)
- `diag_sjalvskattad_halsa_kon_lan_kommun_fohm` (samma fix som ovan)
- `diag_sociala_rel_sjalvskattad_halsa_fohm` (ex.
  diag_sociala_rel_sjalvskattad_halsa_inrikes_utrikes_int_rap.R,
  funktionen hette internt diag_fohm)
- `diag_syss_rams_bas_tidsserie_fran_ar1993_scb` (ex.
  diag_syss_rams_bas_tidsserie_fran_ar1993.R, funktionen hette internt
  diag_syss_rams_bas_tidssserie_fran_ar1993 - stavfel rättat)

Nytt beroende: pxweb (Folkhälsomyndighetens v1-PXWeb-API, inget
v2/pxweb2r-stöd finns där).

# rddiagramkatalog 0.0.0.9024

Ytterligare 4 diagram migrerade från Region-Dalarna/diagram (44 av 53
nu klara):

- `diag_demografi_kolada` (ex. diagram_medelald_demfors.R, funktionen
  hette redan diag_demografi internt; data kommer via RKA/Kolada)
- `diag_ohalsotal_sjukpenningtal_forsakringskassan` (ex.
  diagram_ohalsotal_sjukpenningtal_kv_man.R; nytt beroende: rdpostgres,
  med fallback till Försäkringskassans öppna Excel-filer)
- `diag_forandr_branscher_bubblor_scb` (ex.
  diag_rams_bas_syss_forandr_sni_branscher_bubblor_scb.R, funktionen
  hette redan diag_forandr_branscher_bubblor internt; nytt beroende:
  ggrepel)
- `diag_sfi_bakgrund_scb` (ex. diag_SFI_godkänd_vistelsetid_kon_tid_IntRap.R,
  funktionen hette internt diag_SFI_bakgrund)

# rddiagramkatalog 0.0.0.9023

Ytterligare 4 diagram migrerade från Region-Dalarna/diagram (40 av 53
nu klara):

- `diag_inflyttlan_utflyttlan_scb` (ex. diagram_inflyttlan_utflyttlan_SCB.R)
- `diag_inkomst_scb` (ex. diagram_inkomst_region_aldersgrupper_kv_man.R)
- `diag_kvalifikationskrav_scb` (ex. diagram_kvalifikationskrav_SCB.R)
- `diag_lediga_jobb_tid_sektor_scb` (ex. diagram_lediga_jobb_E1.R)

Fixade även samma typ av "Excel-export kördes aldrig"-bugg som i
0.0.0.9021/0.0.0.9022, den här gången i diag_kvalifikationskrav_scb.

# rddiagramkatalog 0.0.0.9022

Ytterligare 4 diagram migrerade från Region-Dalarna/diagram (36 av 53
nu klara):

- `diag_fodelsenetto_region_scb` (ex. diagram_fodelsenetto_region_SCB.R)
- `diag_foraldrapenning_vab_forsakringskassan` (ex. diagram_foraldrapenning_vab_kv_man.R,
  datan kommer från Försäkringskassans öppna Excel-filer, inte SCB)
- `diag_forvarvsarbetande_90_scb` (ex. diagram_forvarvsarbetande_90_senastear_SCB.R)
- `diag_fruktsamhet_scb` (ex. diagram_fruktsamhet_SCB.R)

Fixade även samma typ av "Excel-export kördes aldrig"-bugg som i
0.0.0.9021, den här gången i diag_forvarvsarbetande_90_scb.

# rddiagramkatalog 0.0.0.9021

Ytterligare 4 diagram migrerade från Region-Dalarna/diagram (32 av 53
nu klara):

- `diag_befolkningsforandring_scb` (ex. diagram_befolkningsforandring.R)
- `diag_befolkningsforandring_region_kon_ar_scb` (ex. diagram_befolkningsforandring_region_kon_ar_SCB.R)
- `diag_flytt_inrikes_aldersgrupper_scb` (ex. diagram_flytt_inrikes_aldersgrupper_SCB.R)
- `diag_flytt_inrikes_utrikes_netto_scb` (ex. diagram_flytt_inrikes_utrikes_netto_SCB.R)

Fixade även två buggar som grävdes fram under migreringen:
- `diag_befolkningsforandring_scb`: Excel-exporten (output_mapp_data/
  filnamn_data) kördes aldrig i praktiken eftersom funktionen returnerade
  före den koden när returnera_figur = TRUE (standardvärdet). Exporten
  görs nu alltid innan returneringen.
- `diag_flytt_inrikes_utrikes_netto_scb`: flyttnetto-diagrammet skrevs
  alltid till fil via ett ovillkorat ggsave()-anrop, oavsett
  skriv_diagramfil. Villkorat nu så att parametern faktiskt styr.

# rddiagramkatalog 0.0.0.9020

Ytterligare 4 diagram migrerade från Region-Dalarna/diagram (28 av 53
nu klara):

- `diag_pendling_over_kommungrans_scb`
- `diag_pendling_over_lans_fa_grans_scb`
- `diag_arbetsmarknadsstatus_senastear_scb` (ex. diagram_arbetsmarknadsstatus_senastear.R)
- `diag_arbetsmarknadsstatus_tidsserie_scb` (ex. diagram_arbetsmarknadsstatus_tidsserie_SCB.R)

Dessutom: fetstilta rubrikraderna i `diagramkatalog_utforska()`s hjälpruta.

# rddiagramkatalog 0.0.0.9019

Ytterligare 4 diagram migrerade från Region-Dalarna/diagram (24 av 53
nu klara):

- `diag_huv_ink_kalla_bakgrund_scb`
- `diag_inr_flytt_in_ut_netto_per_alder_scb`
- `diag_inr_flytt_in_ut_netto_tidsserie_per_alder_scb`
- `diag_nettoinkomst_kon_aldersgrupp_scb`

# rddiagramkatalog 0.0.0.9018

Ytterligare 4 diagram migrerade från Region-Dalarna/diagram (20 av 53
nu klara):

- `diag_gymn_genomstromning_4ar_prg_skolverket` (hämtar från Skolverket, inte SCB)
- `diag_helarsekvivalenter_scb` (ex. diag_helarsekvivalenter_kon_scb.R)
- `diag_huvink_alla_kat_ar_scb`
- `diag_huvink_alla_kat_prel_manad_scb`

Nya paketberoenden: `stringi`, `tibble`.

# rddiagramkatalog 0.0.0.9017

Ytterligare 8 diagram migrerade från Region-Dalarna/diagram (16 av 53
nu klara):

- `diag_brp_per_inv_scb`
- `diag_ekonomiska_prognoser_olika_progn_institut_ki` (ex. diag_ek_prognoser_olika_prognosinstitut_ki.R - hämtar från Konjunkturinstitutet, inte SCB)
- `diag_diverse_vistelsetid_scb` (ex. diag_ek_standard_boende_valdeltagande_vistelsetid_IntRap.R)
- `diag_ek_stod_bakgrund_scb`
- `diag_etablering_utb_kon_scb` (ex. diag_etableringstid_kon_lan_tidsserie_KvMa_IntRap.R)
- `diag_etablering_diverse_scb` (ex. diag_etableringstid_kon_lan_tidsserie_RUS.R)
- `diag_gymnasiebehorighet_mm_scb` (ex. diag_gym_hogskola_behorighet_vistelsetid_IntRap.R)
- `diag_gymn_elever_kon_prg_skolverket` (hämtar från Skolverket, inte SCB)

Nya paketberoenden: `forcats`, `readr`, `rvest`, `xml2`.

# rddiagramkatalog 0.0.0.9016

Nästa migreringsomgång från Region-Dalarna/diagram (första 8 av 53
problemfria kandidater), samma namn-/parameterkonventioner som
pilotomgången:

- `diag_antal_utbniva_alder_kon_scb`
- `diag_arbetspendling_over_tid_scb`
- `diag_bas_arbstatus_region_kon_alder_fodelseregion_prel_manad_scb`
- `diag_bas_status_alla_kat_prel_manad_scb`
- `diag_fodda_manad_scb`
- `diag_utrikes_fodda_tidsserie_scb`
- `diag_bef_inr_utr_en_aldersgrupp_scb`
- `diag_befutv_per_komponent_ar_scb`

Nya paketberoenden: `RColorBrewer`, `writexl`.

# rddiagramkatalog 0.0.0.9015

- Knappen "Visa diagram" i `diagramkatalog_utforska()` heter nu "Skapa
  diagram" - matchar bättre vad som faktiskt händer (och den "Skapar
  diagram..."-notis som redan visades under tiden).

# rddiagramkatalog 0.0.0.9014

- Knappen i hjälprutan ("Visa hjälp") heter nu "Cancel" i stället för
  Shinys standardtext "Dismiss".

# rddiagramkatalog 0.0.0.9013

- Exempeldiagram tillagda: `man/figures/<funktion>.png`, ett diagram per
  funktion (14 av 15 - det Kolada-baserade diagrammet kunde inte genereras
  i den här miljön pga samma nätverksbegränsning som tidigare), genererade
  med rena standardinställningar via det nya underhållsskriptet
  `data-raw/skapa_exempeldiagram.R` (körs manuellt vid behov, inte i CI,
  eftersom det kräver riktiga API-anrop).
  - Visas nu i `?funktionsnamn` (den riktiga HTML-hjälpen) via en
    `\figure{}`-referens i varje funktions dokumentation.
  - Visas även i `diagramkatalog_utforska()` när man väljer en funktion i
    listan, tydligt märkt "Exempel (standardinställningar - kan vara
    inaktuellt)" så det inte förväxlas med en live-förhandsvisning av
    aktuella parametrar. Funktioner utan sparat exempel visar ett
    neutralt meddelande i stället.

# rddiagramkatalog 0.0.0.9012

- `diagramkatalog_utforska()` scrollar nu automatiskt ner till diagrammet
  när "Visa diagram" är klar, så det inte hamnar utom synhåll under ett
  långt parameterformulär.

# rddiagramkatalog 0.0.0.9011

- "Visa hjälp" i `diagramkatalog_utforska()` visar nu hjälptexten i en egen
  ruta ovanpå gadgeten, i stället för att öppna RStudios Hjälp-ruta - den
  senare ligger i RStudios huvudfönster och hamnar bakom gadgetens eget
  dialogfönster (`dialogViewer()`), så den syntes inte.

# rddiagramkatalog 0.0.0.9010

- "Visa diagram" i `diagramkatalog_utforska()` visar nu en "Skapar
  diagram..."-notis medan funktionen körs (vilket kan ta några sekunder mot
  SCB:s API), så man ser att något händer i stället för att gränssnittet
  bara verkar stå still. Notisen försvinner automatiskt när diagrammet är
  klart (eller om det blev fel).

# rddiagramkatalog 0.0.0.9009

- Fix: ordet "pull-right" syntes som text i den övre listen i
  `diagramkatalog_utforska()`. Orsak: `miniUI::gadgetTitleBar()`s `right`
  måste vara EN tagg, inte en `tagList()` med flera knappar - annars
  misslyckas den interna `tagAppendAttributes(right, class = "pull-right")`.
  Knapparna ligger nu i en gemensam `div`.
- "Visa hjälp" har fått en egen färg (`btn-info`, varken grå som "Cancel"
  eller blå som de andra två), och ligger separat från "Visa diagram" och
  "Infoga kod", som nu ligger intill varandra.
- Alla tre knapparna är något mindre (mindre padding/typsnittsstorlek).

# rddiagramkatalog 0.0.0.9008

- Knapparna "Visa diagram", "Visa hjälp" och "Infoga kod" i
  `diagramkatalog_utforska()` sitter nu i den övre listen bredvid "Cancel",
  som blåa knappar (samma stil som gadgetens "Done"-knapp normalt har) för
  att synas tydligt mot den grå Cancel-knappen - i stället för att ligga i
  en rad under parameterformuläret.

# rddiagramkatalog 0.0.0.9007

- `diagramkatalog_bladdra()` har döpts om till `diagramkatalog_utforska()`
  - "bläddra" fångade bara en del av vad funktionen faktiskt gör (söka,
  läsa hjälp, fylla i parametrar, inte bara skumma en lista). Addinet i
  RStudios Addins-meny heter nu "Utforska diagramkatalog".

# rddiagramkatalog 0.0.0.9006

- `diagramkatalog_bladdra()` har fått en knapp "Visa hjälp" som öppnar den
  valda funktionens vanliga hjälpsida (samma som `?funktionsnamn`) i
  RStudios Hjälp-ruta.

# rddiagramkatalog 0.0.0.9005

- Fix: `sok_diagram()` och `diagramkatalog_bladdra()` listade tidigare sig
  själva (och varandra) bland diagrammen, eftersom sökindexet byggdes av
  alla exporterade funktioner i paketet, inte bara `diag_*()`-funktionerna.
  Filtrerar nu till funktionsnamn som börjar med `diag_`.
- `diagramkatalog_bladdra()` öppnas nu i ett eget, större dialogfönster
  ovanpå RStudio (`shiny::dialogViewer()`, 1100x750) i stället för det
  mindre, fasta Viewer-fönstret.

# rddiagramkatalog 0.0.0.9004

- `sok_diagram()` lägger nu, som standard, de matchande funktionsnamnen i
  urklippshanteraren (en per rad), redo att klistras in. Styrs av den nya
  parametern `funktionsnamn_lagg_i_urklippshanteraren` (standard `TRUE`),
  vars eget standardvärde läses från
  `options(rddiagramkatalog.kopiera_funktionsnamn = ...)` - så det går att
  stänga av permanent (t.ex. i `.Rprofile`) utan att ange parametern vid
  varje anrop. Kräver paketet `clipr`; hoppar annars bara tyst över
  kopieringen.
- `sok_diagram()` ger nu ett begripligt, handlingsbart felmeddelande om
  paketets hjälpdatabas inte kan läsas (t.ex. "lazy-load database is
  corrupt"/"internal error in R_decompress1"), i stället för R:s kryptiska
  originalfel. Det här är ofta en tillfällig låsning av hjälpfilerna (t.ex.
  OneDrive-synk eller ett antivirusprogram) som löser sig vid en omstart av
  R/RStudio.

# rddiagramkatalog 0.0.0.9003

Sök- och bläddringsfunktionalitet för katalogen:

- `sok_diagram(sokord)` söker igenom titel, beskrivning och parameter-
  dokumentation (samma text som `?funktionsnamn` visar) för alla
  diagramfunktioner och returnerar de som matchar. Bygger sökindexet från
  paketets installerade `.Rd`-filer (`tools::Rd_db()`), inte källkoden, så
  det fungerar oavsett hur paketet installerats och kräver inget separat
  register att hålla uppdaterat när fler diagram läggs till.
- `diagramkatalog_bladdra()` öppnar ett grafiskt gränssnitt (RStudio-addin,
  "Bläddra diagramkatalog" i Addins-menyn) där man söker, väljer en
  funktion, fyller i parametrar i ett formulär som byggs generiskt från
  funktionens `formals()` (fungerar automatiskt för alla diagram, även
  framtida tillskott), och antingen förhandsvisar diagrammet eller infogar
  den färdiga koden i det aktiva skriptet. Kräver `shiny` och `miniUI`
  (valfria beroenden, bara nödvändiga för denna funktion); "Infoga kod"
  kräver dessutom `rstudioapi` och att man kör RStudio.

# rddiagramkatalog 0.0.0.9002

Konsekvent namngivning över alla 15 pilotfunktioner:

- Alla funktionsnamn börjar med `diag_`, är enbart gemener, och slutar med
  källan - utom de som jämför bestämda, utvalda perioder (inte en
  tidsserie), där det inte är lika meningsfullt:
  `diag_chefer` -> `diag_chefer_scb`, `diag_gini_SCB` -> `diag_gini_scb`,
  `diag_matchning_lan` -> `diag_matchning_lan_scb`,
  `diag_narstaendepenning` -> `diag_narstaendepenning_forsakringskassan`,
  `diag_fodelsetal_summerad_fruktsamhet_jmfr_riket_lan_kommuner` -> `..._scb`,
  `diag_trangboddhet_inrikes_utrikes` -> `..._scb`,
  `diagram_langtidsarbetslohet_tidsserie` -> `diag_langtidsarbetslohet_tidsserie_kolada`,
  `diagram_storsta_fodelseland` -> `diag_storsta_fodelseland_scb`.
- Outputmappen heter nu `utmapp` i alla funktioner (var tidigare `output_mapp`,
  `output_mapp_figur` eller `output_fold`), med `rdverktyg::utskriftsmapp()`
  som standardvärde.
- Skriv-till-fil-parametern heter nu `skriv_diagramfil` i alla funktioner
  (var tidigare bl.a. `spara_figur`, `skriv_bildfil`, `spara_diagrambildfil`).
- Regionparametern heter nu `region_vekt` (var `region`/`vald_region` i
  några funktioner), färgvektorn `diag_fargvekt` (var `valda_farger`/
  `diag_fargvektor`), och data-till-global-environment-parametern
  `returnera_data` (var bl.a. `returnera_data_rmarkdown`).
- Loggan i diagrammet styrs nu alltid av två separata parametrar,
  `visa_logga_i_diagram` (TRUE/FALSE) och `logga_sokvag` (sökväg eller NA),
  i stället för en enda kombinerad parameter i vissa funktioner.
- Om `utmapp` inte finns (t.ex. om `rdverktyg::utskriftsmapp()` inte är
  konfigurerad för datorn) och `skriv_diagramfil = TRUE`: funktionen
  stoppar nu med ett tydligt felmeddelande, i stället för att misslyckas
  längre in i anropet till rddiagram.
- Flera funktioner som hämtar en tidsserie har fått en `tid_koder`-parameter
  (om de inte redan hade en): `"*"` ger allt, `"9999"` ger senaste perioden,
  och ett nytt mönster, `"2015:9999"`, ger "från 2015 till och med senaste
  tillgängliga period" (år eller månad beroende på tabell).

# rddiagramkatalog 0.0.0.9001

Första versionen - pilotomgång med 15 diagramfunktioner flyttade från
Region-Dalarna/diagram:

- `diag_arbloshet_procent_utbildning_facet_ar_scb`
- `diag_aterstaende_medellivslangd_utbniva_lan_scb`
- `diag_bas_arbloshet_manad_jmfr_1ar_tillbaka_scb`
- `diag_bas_syss_arbloshet_aldersgrupp_kon_scb`
- `diag_befolkningsforandring_manad_scb`
- `diag_chefer`
- `diag_fodelsetal_globalt_varldsbanken`
- `diag_fodelsetal_summerad_fruktsamhet_jmfr_riket_lan_kommuner`
- `diag_gini_SCB`
- `diag_inkomst_bakgrund_scb`
- `diag_matchning_lan`
- `diag_narstaendepenning`
- `diagram_langtidsarbetslohet_tidsserie`
- `diagram_storsta_fodelseland`
- `diag_trangboddhet_inrikes_utrikes`

De fem ursprungligt tänkta funktionerna `diag_antal_asylsokande_1984_IntRap`,
`diagram_50proc_lonesumma`, `diagram_examen_hogskolan_NMS`,
`diagram_kompetensbrist` och `diagram_utb_niva_bakgr_alder` visade sig ha
hårdkodade sökvägar till indata på G:-enheten och är bytta mot de fem
verifierat rena alternativ som listas ovan.

Källfilernas inline `requireNamespace()`/`remotes::install_github()`-kontroller
är ersatta med vanliga `Imports:`/`Remotes:` i `DESCRIPTION`. Övrig kod är
oförändrad. Resten av diagrammen i Region-Dalarna/diagram flyttas hit i
senare omgångar.
