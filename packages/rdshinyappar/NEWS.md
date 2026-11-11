# rdshinyappar 0.0.0.9007

* `lagg_till_bakgrundskarta()`: namnen på kartan visas nu på svenska som standard
  (ny parameter `sprak = "sv"`), t.ex. "Sverige" och "Norge" i stället för
  "Sweden" och "Norway". Där svenskt namn saknas används engelska och därefter
  ortens eget namn. `sprak = "en"` ger engelska namn, `sprak = NULL` stilens
  originaltext.

---

# rdshinyappar 0.0.0.9006

* `lagg_till_bakgrundskarta()`: ny parameter `landsnamn`. Med `landsnamn = FALSE`
  döljs landsnamn (t.ex. "Sweden") medan övrig text finns kvar.

---

# rdshinyappar 0.0.0.9005

* `lagg_till_bakgrundskarta()`: `etiketter = "orter"` ger bara namn på orter,
  städer, län och länder (inga vägnamn, vattennamn eller flygplatser), och den
  nya parametern `etikett_opacitet` (0-1) tonar ner texten utan att ta bort
  den, t.ex. `lagg_till_bakgrundskarta(etiketter = "orter", etikett_opacitet = 0.5)`.

---

# rdshinyappar 0.0.0.9004

* Ny funktion `lagg_till_bakgrundskarta()`: en ljus, neutral bakgrundskarta
  (OpenFreeMap Positron, gratis, utan API-nyckel) för leaflet-kartor, att
  använda i `renderLeaflet()` i stället för `addProviderTiles()`. MapLibre GL
  JS som kartan behöver följer med paketet (`inst/leaflet-maplibre`), så
  inget hämtas från något CDN. Samma funktion som i rddiagram, så att
  apparna inte behöver rddiagram och dess beroenden. Paketet importerar nu
  `htmltools` och `htmlwidgets` (följer redan med `leaflet`).

---

# rdshinyappar 0.0.0.9002

* `df_till_sf()` fungerar nu med sf 1.1-3. Geometri som RPostgres levererar
  med klassen `pq_geometry` görs om till WKB innan tolkningen, eftersom sf:s
  metod för `pq_geometry` har ett stavfel som från sf 1.1-3 ger felet
  "object(s) should be of class 'sfg'" (r-spatial/sf#2625).

---

# rdshinyappar 0.0.0.9001

Versionsbump utan innehållsändring (för att `remotes`/`pak`
tillförlitligt ska upptäcka uppdateringar i det här monorepot).

---

# rdshinyappar 0.0.0.9000

Första versionen. Utbrutet ur `func_shinyappar.R` i
`Region-Dalarna/funktioner` – paketet innehåller exakt de funktioner som
låg i den filen.

## Lösenord (`~/.Renviron`)

* `shiny_set_password()`, `shiny_get_password()`, `shiny_delete_password()`,
  `shiny_list_passwords()`.
* `shiny_delete_password()` kör nu även `Sys.unsetenv()` så att det gamla
  värdet inte ligger kvar i den aktiva sessionen efter borttagning.
* Gemensam namnvalidering bruten ut till `intern_kontrollera_service()`.

## Databas

* `shiny_uppkoppling_skriv()`, `shiny_uppkoppling_las()` – delar nu intern
  hjälpare `intern_shiny_uppkoppling()`; anslutningsfel går till `message()`
  i stället för `print()`.
* `shiny_db_list()` – oförändrad.
* `df_till_sf()` – `st_crs()` kvalificeras nu som `sf::st_crs()`; `sf` ligger
  i `Suggests` med `requireNamespace()`-vakt.

## Telemetri (`shiny.telemetry`)

* `skapa_telemetry()`, `telemetri_ui()`, `telemetri_server()`,
  `hamta_telemetri_data()`, `hamta_telemetri_appar()`,
  `hamta_telemetri_heatmap_alla()`, `hamta_telemetri_handelser()`.
* Sessionsstart-aggregeringen bruten ut till
  `intern_sessionsstart_per_veckodag_timme()` (delades av tre funktioner).
* `hamta_telemetri_data()`: `tom_tabell()` byggde alltid en tvåkolumnsram –
  gav fel för `per_veckodag_timme` (tre kolumner) när perioden var tom.
  Använder nu `length(kol_namn)`.
