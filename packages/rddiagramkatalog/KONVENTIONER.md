# Konventioner för diagram- och kartfunktioner i rddiagramkatalog

Det här dokumentet samlar de konventioner som vuxit fram under migreringen
av diagram från `Region-Dalarna/diagram` till det här paketet. Syftet är
att alla `diag_*()`/`karta_*()`-funktioner ska se och bete sig lika,
oavsett vem som skrivit eller migrerat dem - så att man kan gissa sig till
hur en funktion fungerar innan man ens läst dess hjälptext.

Gäller både nya diagram och migrering av gamla skript från källrepot. Se
`data-raw/migreringsstatus.md` för vilka källskript som redan är
migrerade.

## Funktionsnamn

- `diag_*()` för diagram (returnerar `ggplot`-objekt), `karta_*()` för
  kartor (returnerar både `ggplot` och `leaflet`, se nedan).
- Helt gemener, understreck mellan ord.
- Avslutas med källsuffix där det är relevant och inte redan uppenbart av
  resten av namnet: `_scb`, `_fohm`, `_kolada`, `_forsakringskassan`,
  `_nms`, `_ki`, `_skolverket`.
- Namnet ska beskriva VAD diagrammet visar, inte varifrån koden kom -
  byt gärna namn vid migrering om källskriptets namn är missvisande eller
  inkonsekvent (dokumentera namnbytet i mappningstabellen i
  `migreringsstatus.md` om det gäller en migrering).
- Kolliderar ett källskripts funktionsnamn med något annat (källrepot har
  haft några såna fall) - döp om och notera varför i roxygen-dokumentationen
  (se `diag_inr_flyttnetto_bakgrund_scb()` för ett exempel).

## Parameterkonventioner

Återanvänd dessa namn när de passar, i stället för att hitta på nya:

| Parameter | Betydelse |
|---|---|
| `region_vekt` | Region(er): kommun-, läns- eller landskoder. |
| `tid_koder`/`tid_vekt` | Vilka år/perioder som ska hämtas. `"*"` = alla, `"9999"` = senaste. |
| `kon_klartext` | Könsuppdelning, klartext (`"män"`, `"kvinnor"`), `NA` = ingen uppdelning. |
| `diagram_capt` | Källtext under diagrammet. |
| `diag_fargvekt`/`karta_fargvektor` | Egen färgvektor, `NA`/`NULL` = paketets standard. |
| `utmapp` | Mapp där diagram (och ev. data) sparas. Standard: `rdverktyg::utskriftsmapp()`. |
| `skriv_diagramfil` | `TRUE` om diagrammet ska skrivas till fil. |
| `skriv_till_excelfil` | `TRUE` om bearbetad data ska skrivas till en excelfil i `utmapp`. |
| `returnera_data` | `TRUE` = skriv data till `.GlobalEnv` (för den som vill jobba vidare med den i konsolen). |
| `returnera_figur` | `TRUE` (standard) = returnera figuren/figurerna. |
| `visa_logga_i_diagram`, `logga_sokvag` | Styr Region Dalarna-loggan på diagrammet. |
| `filformat` | Bildformat, t.ex. `"png"` (standard) eller `"svg"`. |

`karta_*()`-funktioner använder samma mönster, plus:

| Parameter | Betydelse |
|---|---|
| `skriv_leaflet_fil` | `TRUE` om leaflet-versionen ska skrivas till fil. |
| `leaflet_filformat` | `"html"` (standard, interaktiv) eller `"png"` (statisk, via webshot2). |

## Returvärde

- `diag_*()` returnerar **alltid en namngiven lista av `ggplot`-objekt**,
  även när det bara blir ett diagram. Namnen ska motsvara det filnamn
  (utan ändelse) diagrammet skulle sparas som - `sok_diagram()`,
  `diagramkatalog_utforska()` och `data-raw/skapa_exempeldiagram.R`
  förlitar sig alla på att första elementet (`[[1]]`) går att skicka
  rakt in i `ggplot2::ggsave()`/`shiny::renderPlot()`.
- `karta_*()` returnerar i stället `list(ggplot = <ggplot-objekt>,
  leaflet = <leaflet-objekt>)` - en statisk och en interaktiv version av
  samma karta. Katalogens delade infrastruktur (sökindex, exempelbilder,
  gadgetens förhandsvisning) plockar automatiskt ut `$ggplot`-elementet
  när det finns, så nya `karta_*()`-funktioner fungerar utan vidare
  ändringar där - bara följ samma returstruktur.
- En `karta_*()`-funktion som kan producera FLERA kartor i ett och samma
  anrop (t.ex. en per kommun/typ, likt originalskriptens
  batch-/loop-mönster) returnerar i stället en namngiven lista där varje
  element SJÄLVT är `list(ggplot = ..., leaflet = ...)` - namnen ska
  motsvara filnamnet (utan ändelse) respektive karta skulle sparas som,
  precis som för `diag_*()`. Katalogens delade infrastruktur känner av
  det här automatiskt (kollar om `$ggplot` finns direkt på toppnivån
  först, annars på det första elementet) - ingen ytterligare ändring
  behövs där så länge strukturen följs. Första exemplet:
  `karta_pendling_scb()` (flera kommuner/pendlingstyper i ett anrop).

## Paketberoenden - inga lösa skript

- **Aldrig** `library()`, `pacman::p_load()`, eller `source()` mot externa
  repon (`funktioner`, `hamta_data`) i en `diag_*()`/`karta_*()`-funktion.
  Anropa alltid med fullt namespace (`dplyr::filter()`, `pxweb2r::...`
  osv.) och lägg beroendet i paketets `DESCRIPTION` (`Imports`, plus
  `Remotes` om det är ett annat paket i monorepot eller `pxweb2r`).
- Om källskriptet har ett `if (!requireNamespace(...)) install.packages(...)`-
  block - ta bort det vid migrering. Det behövs inte i ett riktigt
  R-paket, `DESCRIPTION` sköter installationen.
- `requireNamespace()`-koll är OK kvar för genuint valfria beroenden
  (`Suggests`), t.ex. `webshot2` för `leaflet_filformat = "png"` - med
  ett tydligt felmeddelande om paketet saknas när det faktiskt behövs.

## Datahämtning

- Föredra `pxweb2r` framför det gamla `pxweb`-paketet för SCB-data.
  `source()` mot `func_API.R`/legacy-pxweb-kod ska INTE portas - leta
  reda på v2-motsvarigheten till v1-tabellen i stället (jämför
  variabelstruktur/etiketter, verifiera via en live-fråga mot SCB innan
  du litar på en gissad TAB-kod).
- Funktioner som hämtar rådata och som används av **fler än ett**
  diagram (eller som i sig är omfattande nog att förtjäna ett eget
  namnrum) hör hemma i paketet **`rdhamtadata`**, inte som en intern
  hjälpfunktion i `rddiagramkatalog`. Exempel: `hamta_befprognos_data()`,
  `hamta_pendling_rams_bas_scb()`. Bygg ut `rdhamtadata` inkrementellt -
  porta bara det som faktiskt behövs just nu, inte hela källrepot i
  förväg.
- En funktion som bara EN diagramfunktion använder kan vara en intern,
  oexporterad hjälpfunktion direkt i den filen eller i
  `R/intern_*.R`.

## Manuell/lokal indata (t.ex. G:-filer)

Diagram som läser en fil från en delad nätverksenhet (G:) i stället för
ett API ska härdas med hjälpfunktionerna i `R/intern_indata.R`:

- `intern_sakerstall_indatafil(sokvag)` - validerar att en specifik fil
  (hel sökväg) finns, med ett tydligt, handlingsbart felmeddelande om
  den saknas. Skriver också ut en konsolvarning (se nedan) om den finns.
- `intern_hamta_senaste_fil(mapp, monster)` - hittar senast ändrade fil
  som matchar ett mönster i en mapp, med tydligt fel om mappen eller
  filen saknas.
- `intern_meddela_manuell_datakalla(kalla)` - skriver ut en konsolvarning
  om att datan är en manuellt underhållen ögonblicksbild, inte en
  live-hämtning. Anropas automatiskt av de två funktionerna ovan - du
  ska normalt inte behöva anropa den själv.

Samma princip gäller om det är ett **skript** (kod), inte en datafil, som
ligger på G: och som behöver `source()`:as (t.ex. en webbskrapningsfunktion
som inte går att porta hit) - validera att skriptfilen finns med
`intern_sakerstall_indatafil()` innan `source()`, och lägg till ett eget
`intern_meddela_manuell_datakalla()`-anrop som beskriver källan, eftersom
den förstnämnda bara känner till filsökvägen, inte vad den faktiskt
innehåller.

Om funktionen hämtar data både via G: och via ett API som fallback
(samma mönster finns i `rdhamtadata::hamta_befprognos_data()`) - varna
bara när G:-grenen faktiskt används, inte när API-grenen används i
stället.

## Delade byggfunktioner (rddiagram)

Återanvänd alltid dessa i stället för att återskapa logiken för hand:

- `rddiagram::SkapaStapelDiagram()` / `SkapaLinjeDiagram()` - stapel-
  respektive linjediagram med Region Dalarnas profil, färgskalor och
  stödlinjelogik.
- `rddiagram::skapa_koropletkarta_ggplot()` - choroplettkartor. Vid ett
  divergerande mått (ökning/minskning runt noll) - använd diskreta,
  nollcentrerade klasser (`klassindelning = pretty(c(-max_abs, max_abs),
  n = 6)`) i stället för det kontinuerliga läget, som centrerar
  mittfärgen på mitten av DATAINTERVALLET snarare än på noll (missvisande
  om intervallet inte råkar vara symmetriskt).
- `rddiagram::skriv_till_diagramfil()` - skriv ett `ggplot`-objekt till
  fil (med logga).
- `rddiagram::skriv_leaflet_till_fil()` - motsvarigheten för
  `leaflet`-objekt. `"html"` (standard) eller `"png"` (via `webshot2`).
- `rddiagram::diagramfarger()` - standardfärgpaletter.
- `rddiagram::lagg_till_ckm_notering()` - se nedan.
- `rdverktyg::hamtaregtab()`, `hamtakommuner()`, `hamtaregion_kod_namn()`,
  `skapa_kortnamn_lan()`, `skapa_aldersgrupper()`, `utskriftsmapp()` m.fl.
  - regionhantering och allmänna verktyg.

Hittar du ett behov som inte täcks av något befintligt - fundera på om
det hör hemma som en ny delad funktion i `rddiagram`/`rdverktyg` snarare
än en engångslösning i ett enskilt diagram, särskilt om fler diagram
troligen behöver samma sak.

## CKM (röjandekontroll)

SCB bytte metod för flera tabeller 2025 ("CKM" - en ny tabell för nya
årgångar, den gamla historiktabellen fryst). Återkommande mönster när ett
diagram spänner över brytpunkten:

- Hämta från BÅDA tabellerna (historik + CKM) och slå ihop med
  `dplyr::bind_rows()`.
- CKM-tabeller saknar ofta en egen "totalt"-kod för en eller flera
  variabler (ålder, kön) som historiktabellen HAR - kontrollera det
  specifikt i stället för att anta att elimination (utelämna variabeln
  ur `query`) fungerar likadant i båda. Om CKM saknar en totalkod:
  hämta alla individuella värden explicit och summera ihop dem själv.
- CKM-tabeller dubblerar ofta etiketter för öppna/aggregerade
  ålderskategorier ("100+ år" kan finnas under flera koder) - filtrera
  bort dubbletter innan du använder etiketterna för matchning.
- Lägg till en källnotering med `rddiagram::lagg_till_ckm_notering()` när
  CKM-data faktiskt ingår i diagrammet, så läsaren vet att den senaste
  tidsperioden bygger på en något annorlunda metod.

## Dokumentation (roxygen)

- Varje exporterad funktion ska ha en fullständig roxygen-header: en
  rads sammanfattning, en längre beskrivning vid behov, `@param` för
  varje parameter, `@return` som beskriver exakt vad som returneras
  (se formuleringarna ovan under "Returvärde" för standardfraser), och
  `@export`.
- `@return En namngiven lista med \`ggplot\`-objekt.` för `diag_*()`.
- `@return En namngiven lista: \`ggplot\` (ett \`ggplot\`-objekt) och
  \`leaflet\` (ett \`leaflet\`-objekt).` för `karta_*()`.
- Kör `roxygen2::roxygenise()` efter varje ändring - kör den EN GÅNG TILL
  om du får en varning om att en länk inte kunde slås upp (`@description
  Could not resolve link to topic`) - det är en transient ordningsfråga,
  inte ett riktigt fel, och försvinner vid andra körningen.

## Arbetsflöde: migrera eller lägga till ett diagram

1. Läs HELA källskriptet (om det är en migrering) - kontrollera filens
   radantal (`wc -l`) mot vad du faktiskt läst in, särskilt för långa
   filer. Ett skript kan innehålla fler än en funktion.
2. Skriv/migrera funktionen enligt konventionerna ovan.
3. Bumpa versionen i `DESCRIPTION` (sista siffran, t.ex.
   `0.0.0.9033` → `0.0.0.9034`).
4. Lägg till en ny sektion överst i `NEWS.md` som beskriver VAD som
   ändrats och VARFÖR - inte bara en lista av filnamn. Dokumentera
   explicit varje bugg du hittar och fixar under arbetet (se nästa
   avsnitt).
5. `roxygen2::roxygenise("packages/rddiagramkatalog")` (kör två gånger
   vid transienta varningar).
6. `R CMD INSTALL --no-byte-compile --no-test-load packages/rddiagramkatalog`.
7. Rök-testa funktionen live, i en isolerad temp-mapp (för att upptäcka
   oavsiktliga filskrivningar utanför avsedd mapp) - mot skarpa API:er
   där det går. Om en del av funktionen kräver något som inte finns
   tillgängligt i din miljö (G:-fil, databaskoppling) - bekräfta i
   stället att felet som uppstår är tydligt och handlingsbart, inte ett
   kryptiskt R-fel.
8. Om funktionen genererar ett diagram: kör (eller be om att få köra)
   `data-raw/skapa_exempeldiagram.R` så att katalogen får en exempelbild -
   eller låt det ske automatiskt, se nästa stycke.

Exempelbilder behöver inte genereras för hand varje gång. `data-raw/
schemalagd_bildgenerering.R` är tänkt att köras regelbundet (t.ex.
veckovis) via en schemaläggare på en maskin med åtkomst till G:-enheten
och databasen - den hämtar senaste koden, installerar paketen, kör
`skapa_exempeldiagram.R`, och committar/pushar nya bilder automatiskt
om något saknades. Körs INTE som vanlig GitHub Actions-körning (molnet
når varken G: eller databasen).
9. Commit med ett beskrivande meddelande (se nästa avsnitt) och push.
10. Uppdatera `data-raw/migreringsstatus.md` om det gäller en migrering
    från källrepot.

## Hitta och fixa buggar under migrering

Källrepots skript är inte alltid buggfria. Ett återkommande mönster:
kod (ofta en Excel-export) placerad EFTER en `return()`-sats som nästan
alltid triggas, så koden aldrig körs i praktiken. Andra exempel som
hittats: felvända villkor, odefinierade variabler, fel kolumnreferenser,
jämförelser mot fel sentinelvärde.

Hitta du en sådan bugg under migrering: fixa den, men **dokumentera det
alltid explicit** - i en kodkommentar på plats, i NEWS.md, och i
commit-meddelandet. Skriv aldrig tyst om logik "för att det känns
rimligare" - läsaren (kollega eller framtida du) ska kunna se exakt vad
som ändrades jämfört med originalet och varför, särskilt om det påverkar
vilka tal som kommer ut ur funktionen.

## Versionshantering och commits

- En version per logisk ändring (inte en per fil). Flera diagram som
  migreras i samma arbetspass kan dela en version/commit om de hör ihop.
- Commit-meddelanden: förklara VAD som ändrats och VARFÖR, inte bara en
  radad lista av filnamn - en rad sammanfattning, sedan stycken för
  eventuella bakomliggande beslut, hittade buggar, eller avvägningar.
- `git fetch`/`git pull --rebase` innan push - flera kan jobba i samma
  repo samtidigt.

## Osäkra ändringar - fråga innan du gissar

Stöter du på en genuin designfråga under migrering (inte en detalj du
kan göra ett rimligt, dokumenterat val om) - en arkitekturfråga, en
oklar avvägning mellan att vara trogen originalet eller förbättra det,
eller ett källskript som inte passar de etablerade mönstren - fråga
innan du bygger vidare, i stället för att gissa och behöva göra om
arbetet. Exempel från det här projektet: hur `karta_*()`-funktioner ska
returnera BÅDE en ggplot- och en leafletversion, och om `func_API.R`:s
legacy-kod skulle portas in eller ersättas med riktiga v2-anrop.
