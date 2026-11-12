# Migrering av analysrapporterna från GitHub Pages till Shinyservern

Att-göra-lista. Bocka av allteftersom. Tills allt är klart och DNS är ompekad
ska det gamla systemet (`publicera_rapporter`, `docs/`, skripten som kopierar
dit) **fortsätta fungera orört**.

## Principer
- Varje analysrapport publiceras från sitt eget repo med
  `rddeploy::analysrapport_publicera()`. `main` = publicerad rapport,
  utvecklingsbranch granskas internt (`target = "intern"`).
- Beskrivning för landningssidan = `<meta name="description">`, satt med
  `analysrapport_beskrivning_satt()` (i `hero_image.html`, följer med
  omknittning) och `analysrapport_html_beskrivning_satt()` (direkt i en
  färdig html).
- Underrapporter (t.ex. kommunsidor) ligger i `underrapporter/` i repot och
  hamnar under `<rapportadress>/underrapporter/`, utan eget kort.
- `.rapport_deploy_namn` styr servernamn/adress när html-filen inte heter som repot.

## Rapporter
| Rapport | Status |
|---|---|
| laget_i_Dalarna (`laget_i_dalarna`) | **Klar**, publicerad publikt och beskrivning verifierad på landningssidan |
| befolkningsutvecklingen_i_Dalarna (`befolkningsutveckling`) | Förberedd på branchen `shiny-migrering` (underrapporter = Pages-versionerna, beskrivning satt). Återstår: pusha, publicera internt, granska, merga till main, publicera publikt |
| integrationen_i_Dalarna | Ej påbörjad |
| kompetensforsorjning_i_Dalarna (3 htmler i repot) | Ej påbörjad |
| kvinnor_man_i_Dalarna | Ej påbörjad |
| sarbarhetsanalys | Ej påbörjad |
| uppfoljning_dalastrategin | Ej påbörjad |
| uppfoljning_strategier_planer | Ej påbörjad |

För varje rapport: `analysrapport_gor_publicerbar()` -> beskrivning (förslag
godkänns av Peter) -> committa/pusha -> `analysrapport_publicera(target = "intern")`
-> kontrollera kort och sidor -> publik.

## Att kontrollera under pilotarna
- [ ] Att serverskriptet `rapport_deploy_repo.sh` tar med undermappen `underrapporter/`
      (testas i pilot 2: öppna en kommunsida under rapportens adress).
- [ ] Att `analysrapport_publicera()` inte stoppar på "html äldre än Rmd" efter en pull
      (annars `aven_om_html_aldre = TRUE`).

## Kommunsidorna i befolkningsutvecklingen
- [ ] De nyare kommunsidorna i repots rot (uppdaterade 2026-05) är inte
      publicerade. Huvudrapporten säger att kommunfördjupningarna fördröjs på
      grund av SCB:s CKM. Besluta när de ska ersätta Pages-versionerna i
      `underrapporter/`, och ta då bort texten i avsnittet "Kommunfördjupningar".
- [ ] Ev. länka till kommunsidorna från huvudrapporten (idag länkas de ingenstans ifrån).

## När alla rapporter är migrerade och fungerar
- [ ] Skript 2/3/4 i varje rapport: byt kopiera-till-docs/`publicera_rapporter`
      mot `analysrapport_publicera()` (kommunsidor skrivs till `underrapporter/`
      med ascii-gemena namn).
- [ ] Ta bort `docs/` ur rapportrepona.
- [ ] Omdirigeringar för gamla adresser (`samhallsanalys.regiondalarna.se/<fil>.html`)
      så att befintliga länkar fungerar: sidor i `publicera_rapporter/docs` som
      skickar vidare till nya adresser. Gäller även kommunsidorna.
- [ ] Hantera `publicera_rapporter` rot/`index.html` (landningssida för gamla domänen).
- [ ] Peka om DNS för `samhallsanalys.regiondalarna.se` (idag GitHub Pages, CNAME)
      till Shinyservern, och ta bort CNAME ur Pages.
- [ ] Arkivera `samhallsanalys-rapporter` (rör inte dess `publicera`-branch innan dess).
- [ ] Arkivera `publicera_rapporter` när inget längre publiceras därifrån.
- [ ] Skriv om dokumentationen: löptext "webbrapporter" -> "analysrapporter",
      beskriv det nya flödet (`publicering.qmd`, `shiny.qmd`, `rdpaket.qmd`) och
      `landningssida`-README.
- [ ] `knitta_webbrapport()` i rapporternas skript (och dokumentationen): byt namn
      till analysrapport om funktionen ska följa namnbytet.
- [ ] Rapporternas Rmd `source(".../funktioner/...")` -> paket (rdpaket), på sikt.
- [ ] Rensa bort onödig gammal kod/filer (t.ex. `befolkningsutveckling_2021.html`,
      `Old/`) när det gamla flödet är borta.
