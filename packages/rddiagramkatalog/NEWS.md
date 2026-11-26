# rddiagramkatalog 0.0.0.9132

* `diag_gymnasieantagna_antal()`: nya `max_antal` (högst så många staplar; 25 som standard på inriktningsnivå utan valt program, eftersom det finns över hundra inriktningar) och `exkludera_introduktion`. Med `niva = "inriktning"` kan `program` vara ett helt program, vilket ger dess inriktningar. Filnamnen får suffix för inriktning och valt program så att diagram inte skriver över varandra.

# rddiagramkatalog 0.0.0.9131

* `diag_gymnasieantagna_antal()` läser nu `dkf.gymnasieantagna` i `oppna_data` (samma källa som utbildningsappen) i stället för den lokala skriptfilen på G:. Ny `matt` (antagna, förstahandssökande, platser, outnyttjade; kön bara för de två första), `diagramtyp` ("program" för ett år eller "tidsserie"), `ar`, `program`, `niva` (program eller inriktning) och `kommun`. Parametrarna `diag_antal`, `diag_antal_fleraar`, `valda_ar` och `diagram_capt_fleraar` är borttagna.

# rddiagramkatalog 0.0.0.9130

* `diag_befprognos_diff_tot_per_region_scb()`: diagrammet summerade förändringen över alla åldersgrupper (en rad per åldersgrupp från `hamta_befprognos_diff_data()`) och blev därför kraftigt överdrivet (t.ex. Orsa +50 %). Filtrerar nu på totalen (`aldergrp == "totalt"`).

# rddiagramkatalog 0.0.0.9129

* `diag_matchning_e2_jobbfloden_naringsgren_scb(visa_flode = "jobbytare")` visar bara jobbytare från respektive till annan näringsgren.

# rddiagramkatalog 0.0.0.9128

* `diag_matchning_e2_jobbfloden_naringsgren_scb()`: ny parameter `visa_flode`: `"uppdelat"` (standard, som tidigare), `"totalt"` (inflöden och utflöden totalt) eller `"bada"` (facetdiagram med båda vyerna, och med urval i `kon_bakgr_klartext` som egna rader). Nettoförändringen markeras nu med ett svart streck i alla paneler, och finns med i teckenförklaringen.

# rddiagramkatalog 0.0.0.9127

* `diag_bas_syss_per_bransch_manad_jmfr_1ar_tillbaka_scb()`: ny parameter `dela_upp_pa` för att visa förändringen uppdelad på `"kön och bakgrund"` (standard, som tidigare), bara `"kön"`, bara `"bakgrund"` (inrikes/utrikes födda) eller `"ingen"` (bara totalen per bransch). Filnamnet får ändelsen `_kon`, `_bakgrund` respektive `_totalt` när uppdelningen avviker från standarden.

# rddiagramkatalog 0.0.0.9126

* `diag_bas_syss_per_bransch_manad_jmfr_1ar_tillbaka_scb()`: skillnaden var vänd (äldre månad minus senaste, alltså minus där sysselsättningen ökat) och titeln visade åren i fel ordning ("aug år 2025 jämfört med aug år 2026"). Raderna sorteras nu kronologiskt, så plus betyder ökning och titeln lyder "aug år 2026 jämfört med aug år 2025".

# rddiagramkatalog 0.0.0.9125

* Ny funktion `diag_matchning_e2_jobbfloden_naringsgren_scb()` (E2, TAB6927): jobbflöden till och från näringsgrenar (nya förvärvsarbetande, jobbytare från/till annan näringsgren, lämnat sysselsättning) med nettoförändringen markerad. Flera näringsgrenar eller flera urval (t.ex. kvinnor och män) blir facetdiagram, med fast y-axel som standard (`facet_skala`).

# rddiagramkatalog 0.0.0.9124

* `diag_matchning_u3_utbildningsfloden_scb()`: ny parameter `facet_skala`. Standard `"fixed"` ger samma y-axel i alla paneler (tidigare fria axlar); `"free"` ger var sin.

# rddiagramkatalog 0.0.0.9123

* `diag_matchning_u3_utbildningsfloden_scb()`: `kon_bakgr_klartext` kan ha flera värden, t.ex. `c("kvinnor", "män")`, som blir paneler i ett facetdiagram (`diag_facet = TRUE`) eller egna diagram (`FALSE`).

# rddiagramkatalog 0.0.0.9122

* `diag_inr_flyttnetto_bakgrund_scb()`: totalstrecket har nu ett synligt streck i teckenförklaringen.

# rddiagramkatalog 0.0.0.9121

* `diag_bef_inr_utr_en_aldersgrupp_scb(forandring = TRUE)`: totalstrecket har nu ett synligt streck i teckenförklaringen.

# rddiagramkatalog 0.0.0.9120

* `diag_fodelsetal_globalt_varldsbanken()`: Mikronesien och Saint-Martin saknade svenskt namn och hamnade båda i en kategori "NA" överst i diagrammet (två staplar ovanpå varandra). De har nu egna namn, och länder utan svenskt namn får det engelska.
* `diag_bef_inr_utr_en_aldersgrupp_scb(forandring = TRUE)` visar förändringen från året innan, uppdelat på inrikes och utrikes födda, med ett svart totalstreck i varje stapel (`visa_totalvarden = FALSE` döljer det).

# rddiagramkatalog 0.0.0.9119

* `karta_gg_kraftfalt_scb(liggande = TRUE)` ger en liggande bild (3:2) i stället för den stående. Bakgrundskartan vidgas åt sidorna så att länet hamnar i mitten, med textrutan uppe till höger och teckenförklaringen nere till vänster i utrymmet bredvid länet. `liggande = FALSE` är standard och ger samma bild som förut. `bredd_cm` är numera `NULL` som standard (20 cm stående, 27 cm liggande), och filnamnet får ändelsen `_liggande`.

# rddiagramkatalog 0.0.0.9115

* `diagramkatalog_utforska()`: knapparna är omarbetade.
  * "Skapa diagram" visar bara diagrammet och skriver aldrig någon fil (`skriv_diagramfil = FALSE` tvingas i förhandsvisningen, `skriv_leaflet_fil = FALSE` för kartorna).
  * "Skriv till fil" skriver det nyss skapade diagrammet direkt till fil, utan att köra om skriptet, om anropet är oförändrat sedan "Skapa diagram" och diagrammet bär med sig sina filinställningar (gäller funktioner byggda på `SkapaStapelDiagram()` och `SkapaLinjeDiagram()`). Annars körs anropet om med `skriv_diagramfil = TRUE` som förut.
  * "Infoga kod" är ersatt av "Kopiera kod", som lägger anropet i urklipp (`clipr`) och lämnar gadgeten öppen. Går urklippet inte att nå infogas koden i skriptet som förut.
* Kräver rddiagram >= 0.0.0.9046.

# rddiagramkatalog 0.0.0.9111

* `diagramkatalog_utforska()` har fått sökbara listrutor (som också tillåter eget värde) där de valbara värdena är kända, i stället för bara textrutor:
  * parametrar som valideras med `match.arg()` (t.ex. `diagramtyp`, `matt`) får en listruta med de giltiga valen,
  * `region_vekt` (flerval) och `jmfr_region` får en lista med riket, län och kommuner (namnet visas, koden skickas),
  * parametrar som en funktion skickar direkt till ett SCB-uttag (`pxweb2r::pxweb2_get_data()`), t.ex. `kon_klartext`, `alder_klartext`, `cont_klartext` och `tid_koder`, får SCB:s giltiga värden för just den tabellen och variabeln. Kopplingen mellan parameter och tabellvariabel läses automatiskt ur funktionens kod (inklusive enkla mellansteg som `tid_vekt <- tid_koder`) och godkänns bara om parameterns standardvärde finns bland SCB:s värden. Värdena slås upp när funktionen väljs och cachas under sessionen, så ingenting sparas i någon databas och listorna är alltid aktuella.
  Parametrar där värdena inte är kända är textrutor som tidigare, och en tom listruta betyder funktionens standardvärde. I katalogen får 97 av 111 funktioner minst en listruta (197 listrutor).

# rddiagramkatalog 0.0.0.9110

* `diag_storsta_yrke_per_geografi_scb()`: diagrammet anger nu att statistiken är dagbefolkningen (anställda med arbetsplats i regionen, inte bosatta). Undertiteln lyder "Anställda 16–64 år med arbetsplats i regionen (dagbefolkning)" (16–69 år från 2023, då SCB breddade åldersintervallet), och y-axeln heter "Antal anställda (dagbefolkning)" i stället för "Antal sysselsatta", som var missvisande. Gäller alla tre tabellerna (TAB4396, TAB3119 och TAB4434), som alla redovisar dagbefolkning.
* Rättat: de `antal_yrken` största yrkena valdes över alla regioner och år samtidigt, så med flera regioner eller år fick regionerna med färre anställda för få staplar. De väljs nu för varje region, år och kön för sig. (Felet fanns redan i originalskriptet.)

# rddiagramkatalog 0.0.0.9109

* Nytt diagram `diag_befpyramid_scb()`: befolkningspyramider per region och år (utfall eller prognos), konverterat från det gamla skriptet `diag_befpyramid()`. Hämtar med `pxweb2r` och bygger vidare på de nya CKM-tabellerna:
  * Utfall: TAB638 (till och med 2024) och TAB5557 (2025, CKM) för totalbefolkningen, TAB4823 och TAB6645 (2025, CKM) för inrikes och utrikes födda.
  * Prognos: SCB:s regionala befolkningsprognos 2024 (TAB698, till 2070), SCB:s befolkningsprognos för riket (TAB5974, till 2120) och, bara för inrikes och utrikes födda per region, den föråldrade prognosen 2022 (TAB6302). Källtexten anger alltid vilka år som är prognos och vilken prognos.
  Allt i originalet finns kvar: flera regioner och år, jämförelse med annan region (`jmfr_linje = "region"`), mellan inrikes och utrikes födda (`"utr_inr"`) eller med ett annat år (`"ar"`), åldersgrupper eller ettårsklasser, sammanslagning av regioner (`grupp_namn`), logga och animerad gif (`skapa_gif`, kräver paketet `gifski`, som lagts till i Suggests).
  Rättat jämfört med originalet: åldersgruppen 45–54 år innehöll 44-åringar, begränsningen av gifens bildlängd (`del_langd == 1`) var en jämförelse och gjorde ingenting, och 100 år och äldre i tabeller med ettårsklasser över 99 slås nu ihop till 100+ år. Parametrarna följer konventionerna (`region_vekt`, `utmapp`, `skriv_diagramfil`, `returnera_data`, `returnera_figur`).
  Gifen är inte provkörd (gifski saknas i utvecklingsmiljön).

# rddiagramkatalog 0.0.0.9108

* `diag_matchning_u1a_arbmarknadsstallning_utbgrp_scb()` och `diag_matchning_u2_pendling_utbgrp_scb()` sätter nu ihop de nya tabellerna (TAB6367/TAB6369, 2019–) med SCB:s äldre U1a (TAB5400) och U2 (TAB5279), 2006–2018, till tidsserier 2006–2024 (`inkludera_aldre_tabell = TRUE`, standard). Tabellerna har samma regioner, och 117 av de 119 äldre utbildningsgrupperna finns med samma kod och namn i de nya; grupper som bara finns i en av tabellerna får bara den delen. De äldre tabellerna är inte CKM-behandlade, och serien har tidsseriebrott 2019 och 2022. Ett enskilt år till och med 2018 i `ar` hämtas ur den äldre tabellen.
* Fördelningsdiagrammet i U1a klippte bort en stapeldel när andelarna summerade till 100,00000000000001 (flyttalsfel); andelarna avrundas nu nedåt. Årsetiketterna i tidsserier med många år ritas lodrätt.

# rddiagramkatalog 0.0.0.9107

* Två nya diagram ur SCB:s regionala matchningsindikatorer, för riket, län och FA-regioner (inte kommuner), 2019–2024, med utbildningsgrupp som uppdelning:
  * `diag_matchning_u1a_arbmarknadsstallning_utbgrp_scb()` (U1a, TAB6367): förvärvsfrekvens, arbetslöshet, arbetskraftsdeltagande eller andel studerande per utbildningsgrupp. Diagramtyper: `"utbildningsgrupper"` (ett år, med markering av valda grupper), `"tidsserie"`, `"fordelning"` (förvärvsarbetande, arbetslösa och utanför arbetskraften) och `"jamforelse"` (regioner jämförda för en utbildningsgrupp).
  * `diag_matchning_u2_pendling_utbgrp_scb()` (U2, TAB6369): andel utpendlare, andel inpendlare eller nettopendling per utbildningsgrupp, totalt eller uppdelat på kvinnor och män. Diagramtyper: `"utbildningsgrupper"`, `"tidsserie"` och `"jamforelse"`.
  Utbildningsgrupper med färre än `minsta_antal` (30) personer tas inte med i jämförelserna. Tidsseriebrott 2019 och 2022 (SCB:s noter) är beskrivna i hjälpsidorna. Gemensam ritning i `intern_matchning_stapel()`.

# rddiagramkatalog 0.0.0.9106

* Nya diagram ur SCB:s regionala matchningsindikatorer, migrerade från det lokala skriptet `diag_matchningsindikatorerna.R` och delade i tre funktioner (en per tabell), hämtade direkt med `pxweb2r`:
  * `diag_matchning_e3_utbgrp_scb()` (E3, TAB6929): andel i matchade yrken per utbildningsgrupp, ett år (`diagramtyp = "utbildningsgrupper"`) eller över tid (`"tidsserie"`).
  * `diag_matchning_u3_utbildningsfloden_scb()` (U3, TAB6928): inflöden och utflöden per utbildningsgrupp, med nettoförändringen markerad.
  * `diag_matchning_e2_jobbfloden_utbgrp_scb()` (E2, TAB6927): nya förvärvsarbetande och jobbytare, som andel eller antal.
  Alla använder de nya tabellerna (med CKM, 20–69 respektive 18–69 år). SCB:s äldre tabeller (utan CKM) uppdateras inte längre och har samma tidsperiod som de nya (E2: en period mer, men inte jämförbar), så de används inte. FA-regioner finns inte i de nya tabellerna.
  Ändrat jämfört med originalet: `Andel i matchade yrken` räknas nu som A/(A+B+C), alltså helt matchade av de förvärvsarbetande med tillräckliga uppgifter. Originalet räknade 100 minus skillnaden mellan förvärvsgrad och matchad förvärvsgrad, vilket också räknar de icke förvärvsarbetande som matchade. SCB:s eget mått A/E finns som `matt = "matchad förvärvsgrad"`. Tabellernas näringsgrensvariabel sätts till samtliga (A-U). Funktionen returnerar nu alla diagram (originalets `diag_jobbinfloden_utbgrp()` returnerade bara det sista), och inga proxy-inställningar eller `library()`/`source()` behövs.
* Källtexten i `diag_hogst_andel_utr_fodda_inv_sen_10_aren_ssyk3_mikrodata_scb()` är "Källa: Regionala utvecklingsdatabasen (SCB)" utan vy tills vyn är bestämd.

# rddiagramkatalog 0.0.0.9105

* Nytt diagram `diag_hogst_andel_utr_fodda_inv_sen_10_aren_ssyk3_mikrodata_scb()`: yrkena (SSYK3) med högst andel utrikes födda som invandrat de senaste tio åren, för en eller flera regioner (facetdiagram eller ett diagram per region). Migrerat från det lokala skriptet `diag_hogst_andel_utr_fodda_som_inv_sen_10_aren_per_SSYK3()`.
  Data är en Excel-fil på G: ur mikrodatabasen (kontrolleras med `rdverktyg::sakerstall_indatafil()`). Källtexten följer mikrodatakonventionen: "Regionala utvecklingsdatabasen (SCB), NMS".
  Ändrat: inga `library()`/`source()`, `output_mapp`/`skapa_fil` heter `utmapp`/`skriv_diagramfil`, `skapa_facetdiagram` heter `diag_facet`, `lagg_till_logga` heter `visa_logga_i_diagram`, nya `returnera_data`/`returnera_figur`, kolumnerna kontrolleras med tydligt fel, och `antal_storsta_yrken` används nu (originalet visade alltid 15 oavsett värde). Stavfel i källtexten rättat (förvärvsarbetande).

# rddiagramkatalog 0.0.0.9104

* `diag_etablering_vistelsetid_kon_ar_scb()`: rubriken följer det lokala originalet ("Sysselsatta utrikes respektive inrikes födda i arbetsför ålder i Dalarna år 1997-2024", utan kolon, med måttets ord för övriga mått), `startar = NA` (standard) tar alla år från 1997, färgskalorna börjar på originalets ljusa nyanser, och källtexten är nu "Registerdata för integration" (SCB:s statistik AA0003, som tabellerna TAB389/TAB4881/TAB6383/TAB6384 tillhör) i stället för BAS.

# rddiagramkatalog 0.0.0.9103

* Nytt diagram `diag_etablering_vistelsetid_kon_ar_scb()`: etablering på arbetsmarknaden för utrikes födda efter vistelsetid (och inrikes födda som jämförelse), en panel per kön och en stapel per år. Migrerat från det lokala skriptet `diag_etablering_vistelsetid_lan_kommun()`.
  Hämtar direkt med `pxweb2r` (TAB389/TAB6384 för län, TAB4881/TAB6383 för kommun), utan hämta-skript. Kvinnor är gula och män gröna (`diagramfarger("kon")`), med ljusast nyans för äldsta året. Startår och slutår står i rubriken (`startar`, `slutar`), och teckenförklaringen är avstängd som standard (`visa_teckenforklaring = TRUE` slår på den).
  Nytt: `cont_klartext` väljer mått, standard `"sysselsättningsgrad"`; även företagare, arbetslöshetsmått, kvar efter ett år, chefsposition och eftergymnasialt med kvalificerat arbete. Före 2022 är måttet "förvärvsarbetande" (ny definition från 2019) i SCB:s äldre tabeller.
  Kräver `rddiagram` ≥ 0.0.0.9044.

# rddiagramkatalog 0.0.0.9102

* Nytt diagram `diag_bas_arblos_syss_inr_utr_fodda_manad_scb()`: arbetslöshet (eller annat BAS-mått) per månad för inrikes och utrikes födda. Migrerat från `diag_bas_arblosa_inr_utr_fodda_manad()`.
  Hämtar direkt med `pxweb2r` (TAB6260), utan hämta-skript. Ett diagram per anrop: `diagramtyp = "jamforelse"` (regioner jämförda senaste månaden med data) eller `"tidsserie"` (med flera regioner en panel per region, vilket ersätter originalets facetdiagram).
  `region_vekt` tar valfritt antal regioner (standard `rdverktyg::hamtakommuner("20")`: länet, kommunerna och riket), så man kan jämföra t.ex. Dalarna med en kommun. `dataetiketter` är `TRUE`/`FALSE` i stället för en vektor som gav två filer.
  Ändrat jämfört med originalet: `vald_region` → `region_vekt` (flera tillåtna), `output_mapp` → `utmapp`, `skriv_diagram` → `skriv_diagramfil`, `returnera_dataframe_global_environment` → `returnera_data`, månader utan värde (senaste perioden i SCB:s tabell) hoppas över via `intern_behall_senaste_tid()`, och månadsnamnen är svenska oavsett språkinställning.

# rddiagramkatalog 0.0.0.9101

* Källtexternas vy för mikrodatabasen ändrad till RTB (Registret för totalbefolkningen) i de två mikrodatadiagrammen.

# rddiagramkatalog 0.0.0.9100

* Källtext för mikrodatabasen enhetlig: "Källa: Regionala utvecklingsdatabasen (SCB), <vy>". Ändrat i `diag_uppfoljning_gymnavg_kvarboende_flytt_mikrodata_scb()` (vy: Gymnasiet – avgångna och RTB) och `diag_inflyttare_kvarboende_5_ar_kommun_mikrodata_scb()` (vy: RTB, Registret för totalbefolkningen). Regeln finns i KONVENTIONER.md.

# rddiagramkatalog 0.0.0.9099

* Nytt diagram `diag_uppfoljning_gymnavg_kvarboende_flytt_mikrodata_scb()`: var gymnasiets avgångselever bor tio år senare (kvarboende, återflyttad, utflyttad, utvandrad, avliden), utan uppdelning på kön och bakgrund, över kommungräns, länsgräns eller båda.
  Migrerat från `diag_uppfoljning_kvarboende_flytt_gymnavg_ej_kon_bakgr()`; namnet följer namnkonventionen och anger att datan kommer från mikrodatabasen (manuell Excel-fil på G:, kontrolleras med `rdverktyg::sakerstall_indatafil()`).
  Ändrat: inga `library()`/`pacman`/`source()`, `output_mapp`/`skriv_bildfil` heter `utmapp`/`skriv_diagramfil`, nya `returnera_data`/`returnera_figur`, och flikarna kontrolleras med tydligt fel. Originalets filter `!flytt_kat %in% "Andel återflyttare"` och `andel - 0.00` gjorde ingenting och är borttagna (inga tal ändras).
  Originalets enkla diagram (kommun- respektive länsgräns) skrev aldrig bildfil eftersom `skriv_till_diagramfil` inte skickades med; nu styr `skriv_diagramfil` alla tre.

# rddiagramkatalog 0.0.0.9098

* Nytt diagram `diag_inflyttare_kvarboende_5_ar_kommun_mikrodata_scb()`: andel som bor kvar i kommunen fem år efter inflyttning, ett stapeldiagram per kommun i Dalarna. Migrerat från `diag_inflyttare_kommuner_dalarna_kvar_efter_5ar()`.
  Läser en manuellt underhållen Excel-fil på G: (Dalarnas mikrodatabas, därav `mikrodata` i namnet) med `rdverktyg::sakerstall_indatafil()`, som ger tydligt fel om filen saknas, och kontrollerar att kolumnerna `Ar`, `andel` och `Kommun_namn` finns.
  Ändrat jämfört med originalet: inga `library()`/`source()` (paketberoenden i stället), `output_mapp`/`skriv_bildfil` heter `utmapp`/`skriv_diagramfil`, nya `returnera_data` och `returnera_figur`, och funktionen har nytt namn enligt namnkonventionen.

# rddiagramkatalog 0.0.0.9097

* `karta_gg_kraftfalt_scb()`: meddelandet "Coordinate system already present" skrivs inte längre i konsolen.

# rddiagramkatalog 0.0.0.9096

* `karta_gg_kraftfalt_scb()`: texten i textrutan görs så stor som får plats i rutan, som behåller sin storlek. Raderna bryts efter textens verkliga bredd (via `systemfonts`), så att utrymmet till höger om texten används.

# rddiagramkatalog 0.0.0.9095

* `karta_gg_kraftfalt_scb()` använder nu bakgrundskartan över Sverige (Topografi 1M, med grannländer) i stället för Dalarna-kartan ur Topografi 10, med sjö- och havsnamn (`etiketter = "vatten"`).
  Kräver att `rdgis::uppdatera_bakgrundskartor()` (rdgis 0.0.0.9011) har byggt `bakgrundskarta_sverige`. Källtexten anger Topografi 1M.

# rddiagramkatalog 0.0.0.9094

* `karta_gg_bakgrund_sverige()`: har lagret `grannlander` byggts (`rdgis::skapa_grannlander()`, kräver rdgis 0.0.0.9010) ritas grannländerna grå och havet utanför Sverige i vattenfärg. Utan lagret ritas utlandet som förut.

# rddiagramkatalog 0.0.0.9093

* `karta_gg_bakgrund_sverige()`: länsgränserna klipps nu mot kusten och ritas bara över land och sjöar. `lansgrans_i_havet = TRUE` ritar dem även i territorialvattnet. Kräver lagret `hav`
  (bygg med `rdgis::skapa_bakgrundskarta_topografi(kalla_schema = "topografi_1m", lager = "hav")`) och rdgis 0.0.0.9009.
* Ny parameter `max_orter_per_lan` begränsar antalet ortsnamn per län (största först, efter textklass och därefter folkmängd). Ortsnamnen är dessutom ca 20 % mindre.

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

