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
