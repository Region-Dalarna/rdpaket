# rdpostgres 0.0.0.9001

## Nytt

* `rdpostgres_auth_check()` - listar vilka av keyring-services
  `"databas_adm"`/`"rd_geodata"` som finns, varnar om `uppkoppling_db()`
  utan `service_name` skulle falla tillbaka på den inbyggda
  `geodata_las`/`geodata_las`-kopplingen, och listar `.Renviron`-lösenord
  via `rdshinyappar::shiny_list_passwords()` om det paketet finns.
* `uppkoppling_db()` skriver nu ett meddelande första gången den faller
  tillbaka på `geodata_las`/`geodata_las` utan att `service_name`/
  `db_user`/`db_password` angetts - beteendet är oförändrat, bara synligt.

---

# rdpostgres 0.0.0.9000

Första versionen. Utbrutet ur postgres-avsnittet i `func_GIS.R` i
`Region-Dalarna/funktioner`.

## Anslutningar

* `uppkoppling_db()`, `uppkoppling_adm()` – värdnamn/port/options via
  `getOption("rdpostgres.*")`. Kör man på databasservern byts värdnamnet
  mot `localhost`.
* Alla funktioner tar `con` som en `DBIConnection` eller `"default"`;
  den gemensamma logiken ligger nu i `intern_con()`/`intern_stang()`.

## Lista

* `postgres_lista_databaser()`, `postgres_lista_scheman_tabeller()`,
  `postgres_lista_roller_anvandare()`,
  `postgres_lista_behorighet_till_scheman()`,
  `postgres_lista_kolumnnamn_i_schema()`,
  `postgres_lista_rollmedlemskap()` (hette tidigare `postgres_test`).

## Databaser, scheman, tabeller

* `postgres_databas_skapa()/ta_bort()`, `postgres_schema_finns()/
  skapa_om_inte_finns()/ta_bort()`, `postgres_tabell_finns()/ta_bort()`,
  `postgres_finns_schema_tabell_kolumner()`.

## Roller och rättigheter

* `postgres_anvandare_lagg_till()/ta_bort()`, `postgres_roll_*`,
  `postgres_rattigheter_anvandare_lagg_till()/ta_bort()`,
  `postgres_losenord_byt_for_anvandare()`,
  `postgres_lista_giltiga_rattigheter()`.
* `postgres_alla_rattigheter()` – den dubblettdefinierade funktionen städad;
  den rekursiva `role_closure`-varianten är den som behålls.
* `postgres_alla_rattigheter_server()`.

## Data

* `postgres_tabell_till_df()`, `postgres_df_till_postgrestabell()`,
  `oppnadata_hamta()` / `postgres_hamta_oppnadata()`.

## Metadata

* `postgres_meta()`, `postgres_meta_skapa_vy_aktuell_version()`,
  `postgres_metadata_uppdatera()`, `postgres_databas_skriv_med_metadata()`
  (data.frame-varianten; sf-varianten hör till kommande rdgeorouting),
  `postgres_tabell_uppdaterades()`, `fil_dataset_uppdaterades()`.
* `uppdaterad_till_text_datum_tid()` – slår ihop de tidigare
  `pxweb2_uppdaterad_till_text_datum_tid()` och
  `tidformat_uppdaterad_till_text_datum_tid()`.
* `postgres_pxweb2_uppdatera_tabell()` – använder nu `pxweb2r`
  (`pxweb2_table_needs_update()`, `pxweb2_table_updated()`), Suggests.
* `postgres_pxweb2_uppdatera_tabell_skapa_skript()` – **inte portad**
  (skör sträng-generering knuten till gamla pxweb2-interna funktioner;
  `pxweb2r::pxweb2_data_script_template()` täcker liknande behov).

## Grants

* `postgres_grants_auto_skapa()`, `postgres_grants_pa_befintliga_objekt()`,
  `postgres_grants_auto_visa()`, `postgres_grants_auto_testa()`.

## Keyring

* `get_password_tk()` (maskerat tcltk-fönster), `keyring_lagg_till_inloggning()`
  – flyttade hit från func_GIS.R eftersom de sätter upp de
  keyring-services `uppkoppling_db()`/`uppkoppling_adm()` läser.

## Övrigt

* `postgres_felmeddelande()`, `logga_event()`.
* `meddelande_tid`-parametrarna och tidtagningsboilerplaten borttagna
  genomgående (var avstängt som standard i alla funktioner).
* Namespace kvalificerad (`DBI::`, `glue::`, `dplyr::` m.fl.);
  `%>%` → `|>`.
