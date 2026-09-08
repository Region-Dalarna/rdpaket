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
