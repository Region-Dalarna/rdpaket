# rdhamtadata 0.0.0.9000

* Nytt paket. Första innehåll: `hamta_befprognos_data()` och
  `hamta_befprognos_diff_data()`, utbrutna ur
  `Region-Dalarna/hamta_data` (befolkningsprognoser från SCB:s API
  via `pxweb2r`, eller från lokala Profet-/Halland-prognosfiler).
* `hamta_befprognos_diff_data()` migrerades utan den tidigare
  `spara_excelfil`-parametern - den var redan oanvänd i källskriptet
  (ingen `writexl`-export skedde någonstans i funktionskroppen), så
  den och det tillhörande `writexl`-beroendet togs bort i stället för
  att portas som död kod.
