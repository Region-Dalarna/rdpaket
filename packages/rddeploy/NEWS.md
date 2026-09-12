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
