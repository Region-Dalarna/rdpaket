# rddeploy 0.0.0.9012

## Fix: `anv_fornamn_efternamn_hamta()` - hittade den riktiga roten till `"input string 1 is invalid UTF-8"`

Samma felmeddelande som i 0.0.0.9009 (`sub(re, "", x, perl = TRUE)` i
`trimws()`) kom tillbaka även efter att 0.0.0.9011 (`ConvertTo-Json`,
bekräftat CLM-säkert och korrekt avkodat med `jsonlite::fromJSON()`)
garanterat gav en giltig UTF-8-sträng tillbaka från PowerShell. Det
visade att felet aldrig satt i PowerShell-anropet eller teckenkodningen
på vägen dit - det satt i R-kodens egen städpipeline.

Rotorsaken: `sub("[^a-zA-ZåäöÅÄÖ ].*", ...)`
använde standardregex-motorn (TRE), vars tolkning av en UTF-8-märkt
sträng beror på R-sessionens **aktiva lokal** - och R på Windows kör som
standard i en icke-UTF-8-lokal (t.ex. svensk `cp1252`), till skillnad
från de flesta Linux/Mac-sessioner. Verifierat här genom att köra samma
kod under en icke-UTF-8-lokal (`LC_ALL=C`): TRE kraschade direkt på en
helt giltig UTF-8-sträng (`"unable to translate ... to a wide string"`) -
och i mindre tydliga fall kan den producera en sträng som ser ut att gå
igenom men faktiskt blivit trasig, vilket är precis vad som kraschade i
`trimws()` ett steg senare på användarens dator.

Löst genom att använda `perl = TRUE` (PCRE) genomgående i hela
städpipelinen, som alltid tolkar en UTF-8-märkt sträng som UTF-8 oavsett
systemlokal. `\p{L}` (vilken Unicode-bokstav som helst) ersätter samtidigt
den hårdkodade å/ä/ö-listan - fungerar nu för namn med vilka bokstäver
som helst, inte bara svenska. Testsviten kördes grönt både under en
UTF-8-lokal och under `LC_ALL=C` för att bekräfta fixen.



## Fix: `anv_fornamn_efternamn_hamta()` - `[Convert]::ToBase64String()` blockeras av Constrained Language Mode

0.0.0.9010:s felmeddelande-ytning (ny i den versionen) avslöjade den
riktiga orsaken till `status 1`: PowerShell på användarens dator kör i
**Constrained Language Mode** - en säkerhetspolicy som är vanlig på hårt
styrda företags-/regiondatorer (t.ex. via WDAC/AppLocker). I det läget är
direkta anrop på godtyckliga .NET-typer som `[Convert]::ToBase64String()`
och `[System.Text.Encoding]::UTF8.GetBytes()` blockerade
(`"Method invocation is supported only on core types in this language
mode"`), medan `Get-CimInstance`/`Select-Object` fungerade fint - de är
cmdlets, inte `[Typ]::Metod()`-anrop.

Base64-omvägen (0.0.0.9009) byts nu ut mot `ConvertTo-Json` - också en
cmdlet, så den fungerar i Constrained Language Mode. Den löser
ursprungsproblemet på köpet: `ConvertTo-Json` escapar icke-ASCII-tecken
som `\uXXXX` som standard, så utdatan är ren ASCII och kan fortfarande
inte förvanskas av någon kodsida. `jsonlite::fromJSON()` (samma paket som
redan användes för Base64-avkodningen) läser tillbaka den i R.

Den interna, testbara hjälpfunktionen döptes om till
`intern_namn_fran_json()` och testerna uppdaterades till det nya formatet.



## Fix: `anv_fornamn_efternamn_hamta()` gav `status 1` på riktig Windows-dator (0.0.0.9009 löste fel orsak)

0.0.0.9009 löste ett riktigt UTF-8-krasch-problem (och den fixen behövs
fortfarande), men en användare testade på sin egen dator direkt efteråt
och fick ett ANNAT fel: `Get-CimInstance`-anropet självt avslutades med
`status 1` (PowerShell-kommandot kraschade innan det ens hann skriva ut
något). Exakt varför `Get-CimInstance ... -Filter "Name=... AND
Domain=..."` kastar ett fel på just den datorn (behörigheter, CIM-tjänst,
nätverk till domänkontrollant - går inte att veta utan att kunna köra
PowerShell själv här) spelar numera mindre roll:

- Hela CIM-delen körs nu i ett `try { ... } catch { $n = $null }` i
  PowerShell-skriptet, i stället för `-ErrorAction SilentlyContinue` per
  cmdlet (som bara dämpar icke-avbrytande fel, inte alla feltyper). Ett
  fel ger nu bara ett tomt namn (→ `NA_character_` i R), aldrig en
  krasch av hela kommandot.
- Provar först `Name` + `Domain` (snabbare), och faller tillbaka till bara
  `Name` om det första försöket inte gav något.
- `system2()` fångar nu även stderr och kommandots exit-status. Misslyckas
  PowerShell-anropet ändå (t.ex. om PowerShell inte går att starta alls)
  visas en varning med den riktiga feltexten i stället för tyst `NA`, så
  nästa fel (om det finns ett) går att diagnostisera utan gissningar.



## Fix: `anv_fornamn_efternamn_hamta()` kraschade fortfarande på riktig Windows-dator (0.0.0.9008 räckte inte)

0.0.0.9008 satte `[Console]::OutputEncoding = UTF8` i PowerShell-skriptet
för att få rätt bytes tillbaka, men det testades aldrig på en riktig
Windows-dator - och räckte inte: `system2(stdout = TRUE)` läser PowerShells
pipe via Windows egen kodsida (t.ex. CP1252), inte de bytes PowerShell
faktiskt skrev. Å/ä/ö blev fortfarande ogiltig UTF-8, som nu kraschade i
`trimws()`/`sub()` med `"input string 1 is invalid UTF-8"` istället för i
den gamla byte-tabellen.

Löst genom att låta PowerShell Base64-koda `FullName` (`[Convert]::ToBase64String`
på UTF-8-bytes) innan det skrivs ut, och avkoda Base64 + UTF-8 i R
(`jsonlite::base64_dec()`, redan ett transitivt beroende via `gh`). Base64
är ren ASCII och kan inte förvanskas av någon kodsida någonstans i kedjan -
eliminerar hela den här felklassen i stället för att lappa den. Lade också
till en `validUTF8()`-koll som extra säkerhetsnät: om avkodningen någonsin
ger ogiltig UTF-8 (t.ex. trasig indata) returneras `NA_character_` i
stället för att krascha.

Avkodnings-/städlogiken bröts ut till en intern, testbar funktion
(`intern_namn_fran_base64()`) och fick egna tester - själva PowerShell-
anropet går fortfarande bara att testa på Windows, men logiken som
faktiskt orsakade kraschen gör det nu på alla OS.



## Fix: `anv_fornamn_efternamn_hamta()` kunde ge fel/krascha på vissa Windows-datorer

Felet rapporterades i en annan session och hade flera orsaker:

- Byte-till-tecken-listan för att rätta till WMI:s teckenkodning
  (`\x94`, `\x93` m.fl.) var felaktig - matchade ingen riktig kodsida (CP850
  har t.ex. Ö på `\x99`, inte `\x93`), och på R 4.2+/Windows (som kör UTF-8)
  är de ensamma bytena inte ens giltig UTF-8, så `gsub()` kunde underkänna
  mönstret helt.
- `Get-WmiObject` är borttaget i PowerShell 7+ (ersatt av `Get-CimInstance`).
- Kommandosträngen byggdes med manuellt eskapade citattecken
  (`\\\"`) i stället för `system2()` + `shQuote(type = "cmd")`.
- `Win32_UserAccount` filtrerades bara på `Name`, vilket kan bli mycket
  långsamt på domänanslutna datorer (räknar upp domänkonton). Filtreras nu
  även på `Domain`.
- Å, ä och ö skrevs ut som vanliga tecken i källkoden (regex och
  varningstext) i stället för `\u`-escapes, vilket ger en WARNING vid
  `R CMD check` och kan förvanskas beroende på hur paketet byggs.

Löst genom att låta PowerShell sätta `[Console]::OutputEncoding` till UTF-8
själv och skriva ut `FullName` direkt - då behövs ingen efterhandsrättning
av enskilda bytes alls.



## fluidPage() i shinyapp-mallens ui.R sätter nu title så webbläsarfliken får rätt text

Ändring som innebär att webbläsarfliken visar rätt titel när man kör en shinyapp skapad med
rddeploy.

---
=======
# rddeploy 0.0.0.9006

## Shinyapp-mallen laddar `rdshinyappar` via `library()` i stället för `source()`

`global.R`-mallen hämtade tidigare `skapa_telemetry()` genom att
`source()`a `func_shinyappar.R` direkt från GitHub (`raw.githubusercontent.com`)
vid varje appstart - ett nätverksberoende vid körning, och paketet
`rdshinyappar` (som redan används av `telemetri_ui()`/`telemetri_server()`
i mallens `ui.R`/`server.R`) har länge haft samma funktion. Nya
Shiny-app-projekt får nu `library(rdshinyappar)` i `global.R` i stället.

# rddeploy 0.0.0.9005

## `.Rproj`-filen som saknades i nya Shiny-app-projekt är tillbaka

`shinyapp_skapa_med_github_repo()` och
`shinyapp_skapa_med_github_repo_forka_befintligt()` skapade ingen
`.Rproj`-fil, trots att det var tanken. Anropet till
`usethis::create_project()` föll bort när funktionerna refaktorerades från
den ursprungliga koden i `Region-Dalarna/funktioner` (`func_API.R`) till
`rddeploy`s stegfunktioner. Ny intern `intern_scaffold_rproj()` anropas nu
tidigt i båda flödena och skapar `.Rproj`-filen (`rstudio = TRUE` sätts
explicit, så den skapas oavsett om man kör från RStudio eller ej).

# rddeploy 0.0.0.9004

## Fixar krasch vid kloning när `grundsokvag` slutar med `/`

`github_lagg_till_repo_fran_github()` och
`webbsida_med_portal_skapa_med_github_repo()` byggde mål-sökvägen till
`gert::git_clone()` med `file.path(grundsokvag, repo_namn)`. Eftersom
`intern_gh_mapp()`/`intern_gh_mapp_an()` alltid lägger till ett avslutande
`/` gav det ett dubbelt snedstreck (`.../gh//repo`), vilket fick
`git_clone()` att krascha med det kryptiska felet
`Error in libgit2::git_clone : invalid argument: 'repo'`. Strippar nu bort
ev. avslutande snedstreck innan `file.path()` byggs, samma mönster som
redan användes i `shinyapp_config.R`.

# rddeploy 0.0.0.9003

## `github_commit_push()` visar åter vilka filer som ändrats

`github_commit_push()`/`github_commit_push_analytikernatverket()` skrev bara
ut fillistan (nya/ändrade/borttagna) när `commit_txt = NULL`, eftersom den
byggdes som en del av det auto-genererade commit-meddelandet. Angav man ett
eget `commit_txt` syntes den inte alls. Skriver nu alltid ut fillistan innan
commit/push, oavsett `commit_txt`.

## Döljer keyrings ofarliga backend-varning

`rddeploy_pat()`s reservväg via keyring (och `intern_foraldrade_keyring_services()`)
kunde skriva ut `"Selecting 'env' backend. Secrets are stored in environment
variables"` från `keyring` självt - ofarligt, bara keyrings engångsval av
lagringsbackend i miljöer utan ett riktigt OS-backend (Linux utan
secret_service, huvudlösa sessioner). Döljs nu specifikt, andra varningar
från keyring syns fortfarande.

---

# rddeploy 0.0.0.9002

## Tydligare urklippshantering

* `github_lista_repo_filer()`/`gh_dia()`/`gh_ppt()`/`ppt_lista_rader()` m.fl.
  kopierade tidigare tyst till urklipp - misslyckades kopieringen (t.ex.
  Linux utan `xclip`/`xsel`/`wl-copy`, eller en huvudlös session utan
  skrivbordsmiljö) hände ingenting alls, utan minsta antydan om varför.
  Ny intern `intern_kopiera_urklipp()` ger nu alltid besked: `"Kopierat
  till urklipp."` när det gick, annars en tydlig förklaring och en
  påminnelse om att kopiera raden/raderna för hand. Samma kod, samma
  `clipr`-paket, oavsett OS - skillnaden är bara att felet syns.

---

# rddeploy 0.0.0.9001

## `rddeploy_auth_check()` utökad

* Flaggar föråldrade keyring-poster `"github"` (användarnamn+lösenord -
  GitHub har inte stött det för git-operationer sedan 2021) och `"git2r"`
  (gav bara git-identitet, ersatt av `git_kontrollera_id_uppgifter()`).
  Rör dem inte automatiskt - bara en varning med kommando för att radera.
* Skriver ut en sammanfattande rad: allt konfigurerat eller något saknas.
* Databas-/keyring-autentisering (`databas_adm`, `rd_geodata`, `.Renviron`)
  ligger medvetet utanför den här funktionen - se
  `rdpostgres::rdpostgres_auth_check()`.

---

# rddeploy 0.0.0.9000

Första versionen. Utbrutet ur `func_API.R` och
`func_landningssida_adminportal.R` i `Region-Dalarna/funktioner`.
Git- och GitHub-anropen är omskrivna att gå via **gert**, **gh** och
**gitcreds** i stället för handrullade `system("git ...")`- och
`git2r`-anrop.

## Konfiguration

* Alla personberoende sökvägar och organisationsnamn läses via
  `getOption("rddeploy.*")` – se `?"rddeploy-config"`.
* `rddeploy_config_status()` visar aktuella värden och om sökvägarna finns.

## Autentisering

* `rddeploy_pat()` – hämtar GitHub-token (GITHUB_PAT → gitcreds → keyring)
  och sätter `GITHUB_PAT` så att gh/gert/credentials hittar den. Ersätter
  `.gh_pat()`.
* `rddeploy_auth_check()` – kort diagnostik (token, konto, scopes,
  git-identitet), i `usethis::git_sitrep()`-anda.
* `git_kontrollera_id_uppgifter()` – sätter lokal git-identitet från den
  globala configen via gert; tar nu emot `repo`.

## Användaruppgifter

* `anv_anvandarkonto_hamta()`, `anv_fornamn_efternamn_hamta()` (Windows;
  returnerar `NA` på annat OS), `anv_epostadress_hamta()` (via `gh`,
  konfigurerbar domän), `anv_hamta_namn_epost_fran_lista()` (personlistan
  konfigurerbar via `options(rddeploy.namn_epost_lista=)`).

## GitHub

* `github_lista_repos()` (+ `_analytikernatverket`) – via `gh`, med
  paginering; returnerar tibble med `privat`/`arkiverad`.
* `github_lista_repo_filer()` (+ `_analytikernatverket`) – hämtar hela
  filträdet i ett anrop (git trees-API). Flaggorna `url_vekt_enbart`/
  `skriv_source_konsol`/`icke_source_repo`/`skriv_ppt_lista` ersatta av
  `retur = c("source", "url", "df", "ppt")`. Sök-DSL:en (OR/`&`/`!`)
  bevarad, utbruten till `intern_filtrera_filnamn()`.
* `gh_dia()`, `gh_ppt()`, `gh_hamta_analytikernatverket()`,
  `ppt_lista_rader()` – urklipp via `clipr`.
* `github_status_filer_lokalt_repo()`, `github_commit_push()`,
  `github_pull_lokalt_repo_fran_github()` (+ `_analytikernatverket`) – via
  `gert::git_status/git_add/git_commit/git_pull` + `intern_gh_push()`.
  HOME/USERPROFILE-dansen borttagen (behövs inte med libgit2).
* `github_lagg_till_repo_fran_github()` (+ `_analytikernatverket`) – via
  `gert::git_clone`; 404-koll via `gh`; `.Rproj` via `usethis` (Suggests).

## Depot

* `depot_hamta_fran()`, `depot_hamta_mapp_fran()`, `depot_skriv_mall_fran()`
  – textfiler via Contents-API med raw-accept (förbi CDN-cache), binärfiler
  via `download_url`.

## GitHub Actions

* `intern_trigga_workflow()`, `intern_vanta_pa_workflow()`,
  `intern_ge_team_behorighet()` – via `gh` (ersätter handrullade
  `httr::POST/PUT` mot Actions- och teams-API:t).

## Webbrapport och portal

* `skapa_webbrapport_github()` – ~400 rader inbäddad text/RMarkdown flyttad
  till `inst/templates/webbrapport/`. Det stora, inaktuella exempel-`.Rmd`:t
  ersatt av ett rent skelett som använder `rddiagram`. git/GitHub via
  `gert` + `usethis`.
* `webbrapport_publicera()`, `webbrapport_avpublicera()` – redan `gert`/`gh`,
  städade; avpublicera triggar workflow via `gh`.
* `webbsida_med_portal_skapa_med_github_repo()`, `dokumentation_sida_skapa()`
  – konfigurerbara sökvägar, team-behörighet via delad hjälpare.

## Shiny-appar

* Mallfiler (`deploy.yml`, `avpublicera.yml`, `global.R`, `ui.R`, `server.R`,
  `_dependencies.R`, `app.css`, `README.md`, ...) i
  `inst/templates/shinyapp/` med `<<platshållare>>` (så att GitHub Actions
  `${{ ... }}` står orört).
* `shinyapp_config()` – validerat config-objekt i stället för ~12 argument.
* Monoliterna (`shinyapp_skapa_med_github_repo` ~730 rader,
  `..._forka_befintligt` ~510) uppdelade i stegfunktioner:
  `intern_scaffold_struktur/appfiler/www/workflows/meta/renv`,
  `intern_init_git_och_github`, `intern_preflight` (skriver ut vad som
  skapas, kräver att föräldermappen finns, ber om bekräftelse om inte
  `force = TRUE`). renv-bootstrappen isolerad i `intern_scaffold_renv()`.
* `shinyapp_publicera()` – `gert` i stället för `system2("git")`; pushar
  default-branchens topp till `publicera-<target>` (som `webbrapport_publicera`).
  `tvinga_omdeploy` för omdeploy av samma commit.
* `shinyapp_avpublicera()`, `shinyapp_flytta()` – `gh`-triggade workflows,
  server-URL:er konfigurerbara (`rddeploy.shiny_host_*`).
* `.gh_pat`/`.gh_push` -> `rddeploy_pat()` / `intern_gh_push()`.

## Utanför rddeploy

* Landningssidefunktionerna (`landningssida_*`) och `kor_cron_jobb()` ligger
  i det egna paketet **`rdadminportal`** (adminportal + serverdrift), skilt
  från rddeploy som är analytikernas arbetsflöden.
* `skapa_hamta_data_skript_pxweb()` - ska inte portas
  (`pxweb2r::pxweb2_data_script_template()` täcker behovet).
