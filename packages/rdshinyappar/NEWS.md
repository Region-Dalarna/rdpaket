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
