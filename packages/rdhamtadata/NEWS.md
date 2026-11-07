# rdhamtadata 0.0.0.9001

* Ny funktion `hamta_befprognos_riket_inr_utr_scb()` - befolkningsprognos
  för riket uppdelat på ålder, kön och inrikes/utrikes född. Motsvarar
  v1-tabellen BE0401A/BefolkprognRevNb, som källrepot (Region-Dalarna/
  hamta_data) flaggade som "inte migrerad till pxweb2r ännu" och därför
  fortfarande körde via source() mot func_API.R. v2-motsvarigheten
  (TAB5974) hittades och verifierades via `pxweb2r::pxweb2_search_tables()`
  och en live-jämförelse av variabelstruktur/etiketter - ingen
  legacy-pxweb-kod behövde portas.

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
