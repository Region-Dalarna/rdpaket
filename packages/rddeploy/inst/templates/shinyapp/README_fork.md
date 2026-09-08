# <<rapport_titel>>

Detta repository innehåller en Shinyapplikation (`<<github_repo>>`) för
Samhällsanalys, Region Dalarna.

Appkoden under `app/` är importerad från ett befintligt repo via `git subtree`:

- **Källa:** <<kalla_repo_url>>
- **Branch vid import:** <<kalla_branch>>

## Struktur

- All appkod ligger i katalogen `app/` (importerad via git subtree)
- `_publicering_till_server.yml` i root styr vilken Shiny-server som är default för `shinyapp_publicera()`
- Deploy via GitHub Actions (`.github/workflows/deploy.yml`, `avpublicera.yml`)

Appmapp på servern: `/srv/shiny-server/<<github_repo>>`.

## Hämta uppdateringar från källrepot

Appkoden under `app/` är kopplad till källrepot via `git subtree`. Working tree
måste vara **clean** innan du kör (committa eller stasha först).

```
cd <<gitprojekt_sokvag>>
git status
git subtree pull --prefix=app <<kalla_repo_url>> <<kalla_branch>> --squash
git push
```
