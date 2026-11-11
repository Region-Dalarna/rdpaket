# Migreringsstatus: Region-Dalarna/diagram → rddiagramkatalog

Dokumentation av vilka diagram som är migrerade från källrepot `Region-Dalarna/diagram`
till paketet `rddiagramkatalog`, och vilka som återstår. Byggd genom innehållsbaserad
matchning (datakälla, PxWeb-tabellkoder/URL:er, `diagram_capt`-text, kommentarer) mellan
källfilerna och paketets `diag_*()`-funktioner - inte bara namnlikhet, eftersom namnen
ofta ändrades rejält vid migrering.

Skapad: 2026-11-07. Senast uppdaterad: 2026-11-09 (efter migrering av
karta_pendling_leaflet.R - båda kartorna är nu migrerade). Uppdatera vid behov när fler
diagram/kartor migreras - filen är ingen automatiskt genererad rapport.

**Status:** av 94 källskript i `~/gh/diagram` är ~91 migrerade (eller avsiktligt inte
migrerade - se "Avsiktligt inte migrerade" nedan; två av dessa källskript blev en enda
sammanslagen funktion, se mappningstabellen), och **ca 1 fil återstår** att utreda - se
avsnitt B.

Rader märkta **medel konfidens** eller **låg konfidens** är inte 100 % verifierade - dubbelkolla
innan du litar blint på dem.

## A) Mappningstabell - migrerade i detta projekt (60 filer, via 12 commits)

| Källfil (~/gh/diagram) | Funktion (rddiagramkatalog) | Kommentar |
|---|---|---|
| diagram_50_proc_lonesumma_NMS.R | diag_50proc_lonesumma_nms.R | |
| diag_antal_utbniva_alder_kon_scb.R | diag_antal_utbniva_alder_kon_scb.R | Identiskt namn |
| diagram_arbetsmarknadsstatus_senastear.R | diag_arbetsmarknadsstatus_senastear_scb.R | |
| diagram_arbetsmarknadsstatus_tidsserie_SCB.R | diag_arbetsmarknadsstatus_tidsserie_scb.R | |
| diag_rams_bas_pendling_pendlingsrelationer_over_tid.R | diag_arbetspendling_over_tid_scb.R | Namnet bytt helt - bekräftat via identisk `diagram_capt` |
| diag_bas_arbmstatus_forandr_senmanad_arsvis_scb.R | diag_bas_arbstatus_region_kon_alder_fodelseregion_prel_manad_scb.R | Namnet bytt helt - bekräftat via identisk exempelbilds-URL |
| diag_bas_status_alla_kategorier_prel_manad_scb.R | diag_bas_status_alla_kat_prel_manad_scb.R | |
| diag_bas_syss_bransch_jmfr_manad_1ar_tillbaka.R | diag_bas_syss_per_bransch_manad_jmfr_1ar_tillbaka_scb.R | |
| diag_bef_inr_utr_en_aldersgrupp_scb.R | diag_bef_inr_utr_en_aldersgrupp_scb.R | Identiskt namn |
| diagram_befolkningsforandring_region_kon_ar_SCB.R | diag_befolkningsforandring_region_kon_ar_scb.R | |
| diagram_befolkningsforandring.R | diag_befolkningsforandring_scb.R | Bekräftat via kommentartext om CKM-tabeller |
| diag_befutv_per_komponent_ar_scb.R | diag_befutv_per_komponent_ar_scb.R | Identiskt namn |
| diag_brp_per_inv_scb.R | diag_brp_per_inv_scb.R | Identiskt namn |
| diagram_medelald_demfors.R | diag_demografi_kolada.R | Namnet bytt helt - Kolada-källa + rKolada bekräftar |
| diag_ek_prognoser_olika_prognosinstitut_ki.R | diag_ek_prognoser_olika_prognosinstitut_ki.R | Identiskt namn |
| diag_ek_stod_bakgrund.R | diag_ek_stod_bakgrund_scb.R | Identisk SCB-url i kommentar |
| diagram_examen_hogskolan_NMS.R | diag_examen_hogskolan_nms.R | |
| diagram_flytt_inrikes_aldersgrupper_SCB.R | diag_flytt_inrikes_aldersgrupper_scb.R | |
| diagram_flytt_inrikes_utrikes_netto_SCB.R | diag_flytt_inrikes_utrikes_netto_scb.R | |
| diagram_foraldrapenning_vab_kv_man.R | diag_foraldrapenning_vab_forsakringskassan.R | Identiska FK-API-URL:er |
| diag_rams_bas_syss_forandr_sni_branscher_bubblor_scb.R | diag_forandr_branscher_bubblor_scb.R | Branschnyckel retrofittad till delad GitHub-fil |
| diagram_forvarvsarbetande_90_senastear_SCB.R | diag_forvarvsarbetande_90_scb.R | |
| diagram_fruktsamhet_SCB.R | diag_fruktsamhet_scb.R | |
| diag_gym_hogskola_behorighet_vistelsetid_IntRap.R | diag_gymnasiebehorighet_mm_scb.R | **Medel konfidens** |
| diag_gymn_elever_kon_prg_alla_arskurser_skolverket.R | diag_gymn_elever_kon_prg_skolverket.R | |
| diag_gymn_genomstromning_4ar_prg_skolverket.R | diag_gymn_genomstromning_4ar_prg_skolverket.R | Identiskt namn |
| diag_helarsekvivalenter_kon_scb.R | diag_helarsekvivalenter_scb.R | |
| diag_huvink_alla_kat_ar_scb.R | diag_huvink_alla_kat_ar_scb.R | Identiskt namn (TAB3202) |
| diag_huvink_alla_kat_prel_manad_scb.R | diag_huvink_alla_kat_prel_manad_scb.R | Identiskt namn (TAB1784) |
| diag_huv_ink_bakgrund_kon_IntRap.R | diag_huv_ink_kalla_bakgrund_scb.R | **Medel konfidens** |
| diagram_inflyttlan_utflyttlan_SCB.R | diag_inflyttlan_utflyttlan_scb.R | |
| diagram_inkomst_region_aldersgrupper_kv_man.R | diag_inkomst_scb.R | Bekräftat via TAB5278 |
| diag_inr_flytt_in_ut_netto_per_alder.R | diag_inr_flytt_in_ut_netto_per_alder_scb.R | |
| diag_inr_flytt_in_ut_netto_tidsserie_per_alder.R | diag_inr_flytt_in_ut_netto_tidsserie_per_alder_scb.R | |
| diagram_kvalifikationskrav_SCB.R | diag_kvalifikationskrav_scb.R | Bekräftat via TAB4436 |
| diagram_lediga_jobb_E1.R | diag_lediga_jobb_tid_sektor_scb.R | **Medel konfidens** |
| diag_nettoinkomst_konsuppdelad_per_aldersgrupp_scb.R | diag_nettoinkomst_kon_aldersgrupp_scb.R | Identisk exempelbilds-URL |
| diagram_ohalsotal_sjukpenningtal_kv_man.R | diag_ohalsotal_sjukpenningtal_forsakringskassan.R | Identiska FK-URL:er + capt-text |
| diag_pendling_over_kommungrans.R | diag_pendling_over_kommungrans_scb.R | |
| diag_pendling_over_lans_fa_grans.R | diag_pendling_over_lans_fa_grans_scb.R | |
| diag_SFI_godkänd_vistelsetid_kon_tid_IntRap.R | diag_sfi_bakgrund_scb.R | **Medel konfidens** |
| diag_sjalvskattad_halsa_kon_lan_fohm.R | diag_sjalvskattad_halsa_kon_lan_fohm.R | Identiskt namn |
| diag_sjalvskattad_halsa_kon_lan_kommun_fohm.R | diag_sjalvskattad_halsa_kon_lan_kommun_fohm.R | Identiskt namn |
| diag_sociala_rel_sjalvskattad_halsa_inrikes_utrikes_int_rap.R | diag_sociala_rel_sjalvskattad_halsa_fohm.R | Identisk hämta-funktionsreferens |
| diag_yrke_ssyk3_storsta_per_geografi.R | diag_storsta_yrke_per_geografi_scb.R | Identisk hämta-funktionsreferens |
| diagram_andel_forvarvsarbetande_bransch.R | diag_sysselsatta_andel_scb.R | Identisk `diagram_capt` ordagrant |
| diagram_forandring_sysselsatta_bransch_kommun.R | diag_sysselsatta_forandring_bransch_kommun_scb.R | TAB3784 |
| diagram_forandring_sysselsatta_bransch.R | diag_sysselsatta_forandring_bransch_scb.R | TAB3784 |
| diag_syss_rams_bas_tidsserie_fran_ar1993.R | diag_syss_rams_bas_tidsserie_fran_ar1993_scb.R | |
| diag_syss_vistelsetid_inr_utr_fodda_scb.R | diag_syss_vistelsetid_inr_utr_fodda_ett_per_ar_scb.R | Identisk hämta-funktionsreferens |
| diag_trangboddhet_inrikes_utrikes_IntRap.R | diag_trangboddhet_inrikes_utrikes_scb.R | |
| diagram_utb_bakgr_alder_NMS.R | diag_utb_niva_bakgr_alder_nms.R | |
| diag_utbniva_inr_utr_fodda_lan_scb.R | diag_utbniva_inr_utr_fodda_kon_lan_scb.R | |
| diag_utbniva_flera_diagram_scb.R | diag_utbniva_tidserie_och_lansjmfr_scb.R | **Låg-medel konfidens** - källan beskriver fyra diagram totalt, oklart om allt är täckt |
| diag_uvas_vistelsetid_kon_tid_IntRap.R | diag_uvas_bakgrund_vistelsetid_scb.R | Identisk TAB4931/hämta-referens |
| diag_etableringstid_kon_lan_tidsserie_KvMa_IntRap.R | diag_etablering_diverse_scb.R | **Låg-medel konfidens**, osäkert vilken av paret som är vilken |
| diag_etableringstid_kon_lan_tidsserie_RUS.R | diag_etablering_utb_kon_scb.R | **Låg-medel konfidens**, se ovan |
| diag_ek_standard_boende_valdeltagande_vistelsetid_IntRap.R | diag_diverse_vistelsetid_scb.R | **Låg konfidens** |
| ej säkert identifierad | diag_fodda_manad_scb.R | Källa ej verifierad - trolig kandidat: diag_bef_forandring_per_manad.R |

### Tillagda 2026-11-07 (Profet-gruppen + 10 enklare diagram)

| Källfil (~/gh/diagram) | Funktion (rddiagramkatalog) | Kommentar |
|---|---|---|
| diag_befolkningsprognos_scb_api_profet.R (`SkapaBefPrognosDiagram()`) | diag_befolkningsprognos_profet_scb.R | Filen innehöll tre funktioner - se nedan och pilotomgångstabellen |
| diag_befolkningsprognos_scb_api_profet.R (`SkapaBefPrognosDiagram_befforandr()`) | diag_befolkningsforandring_komponenter_prognos_scb.R | |
| diag_befolkningsprognos_scb_api_profet.R (`SkapaBefPrognosDiagram_InrUtrFodda()`) | *(ingen - se pilotomgångstabellen)* | Tredje funktionen i samma fil - INTE migrerad separat, källkoden bekräftar att den redan är ersatt av `diag_utrikes_fodda_tidsserie_scb` |
| diag_befprognos_diff_tot_per_kommun.R | diag_befprognos_diff_tot_per_region_scb.R | Funktionsnamnet i källan matchade redan (filnamnet gjorde inte det) |
| diag_bef_utfall_prognos_per_aldersgrupp_scb.R | diag_bef_utfall_prognos_per_aldersgrupp_scb.R | Identiskt namn |
| diag_bef_utfall_prognos_per_region_totalt_scb.R | diag_bef_utfall_prognos_per_region_totalt_scb.R | Identiskt namn |
| diag_antal_asylsokande_1984_IntRap.R | diag_asylsokande_tidsserie_scb.R | |
| diag_fek_foradlingsvarde_bransch_lan_scb.R | diag_fek_foradlingsvarde_bransch_lan_scb.R | Identiskt namn |
| diag_fek_lve_tidsserie.R | diag_fek_lve_tidsserie.R | Identiskt namn |
| diagram_flytt_inrikes_bakgrund_SCB.R + diag_inr_flyttnetto_inr_utr_fodda_scb.R | diag_inr_flyttnetto_bakgrund_scb.R | **Uppdaterat 2026-11-09:** de två källskripten (som råkade dela funktionsnamn i källan) är nu slagna ihop till en funktion med `alder_grp`/`relativt_flyttnetto` från den rikare varianten. Se NEWS.md 0.0.0.9031 för detaljer och tre buggar som hittades/fixades på vägen. |
| diagram_bransch_utb_alder_NMS.R | diag_bransch_utb_alder_nms.R | |
| diagram_andel_offentligt_BAS.R | diag_andel_offentligt_bas_scb.R | RAMS-varianten (diagram_andel_offentligt.R) migrerades INTE - se "Avsiktligt inte migrerade" |
| diagram_arb_stallen_mm_foretagarna.R | diag_arb_stallen_mm_foretagarna.R | Identiskt namn |
| diagram_gymnasiantagning_antal_kon.R | diag_gymnasieantagna_antal.R | |
| diag_utslapp_konsumtion_SEI.R | diag_utslapp_konsumtion_sei.R | |
| diagram_kompetensbrist_lan_TVV.R | diag_kompetensbrist_lan_tvv.R | |

## Pilotomgång - migrerades innan detta projekt (15 filer)

Fanns redan i paketet innan migreringsprojektet startade. Tas med för fullständighetens skull.

| Källfil | Funktion (rddiagramkatalog) |
|---|---|
| diag_arbloshet_procent_utbildning_facet_ar_scb.R | diag_arbloshet_procent_utbildning_facet_ar_scb.R |
| diag_aterstaende_medellivslangd_utbniva_lan_scb.R | diag_aterstaende_medellivslangd_utbniva_lan_scb.R |
| diag_bas_arbloshet_jmfr_manad_1ar_tillbaka_region_scb.R | diag_bas_arbloshet_manad_jmfr_1ar_tillbaka_scb.R |
| diag_bas_syss_arbloshet_aldersgrupp_kon_scb.R | diag_bas_syss_arbloshet_aldersgrupp_kon_scb.R |
| diagram_andel_chefer_utb_kv_man.R | diag_chefer_scb.R |
| diag_fodelsetal_globalt_varldsbanken.R | diag_fodelsetal_globalt_varldsbanken.R |
| diag_fodelsetal_summerad_fruktsamhet_jmfr_riket_lan_kommuner.R | diag_fodelsetal_summerad_fruktsamhet_jmfr_riket_lan_kommuner_scb.R |
| diag_gini_tidsserie_jmf_SCB.R | diag_gini_scb.R |
| diag_forvarvsinkomst_bakgrund_vistelsetid_IntRap.R | diag_inkomst_bakgrund_scb.R (**medel konfidens**) |
| diag_langtidsarbetsloshet_kolada_kon.R | diag_langtidsarbetslohet_tidsserie_kolada.R *(obs: stavfel "lohet" i funktionsnamnet)* |
| diagram_matchning_lan_bakgrund.R | diag_matchning_lan_scb.R |
| diagram_narstaendepenning_forsakringskassan_kv_man.R | diag_narstaendepenning_forsakringskassan.R |
| diag_storsta_fodelseland_IntRap.R | diag_storsta_fodelseland_scb.R |
| diag_befolkningsforandring_region_kon_manad_scb.R | diag_befolkningsforandring_manad_scb.R (**medel konfidens**) |
| diag_bef_inrikes_utrikes_antal_forandring_prognos_IntRap.R (`diagram_utrikes_fodda_tidsserie()`) | diag_utrikes_fodda_tidsserie_scb.R | Bekräftat 2026-11-07: dess `diag_forandring_prognos`-gren (TAB6008) ersätter explicit `SkapaBefPrognosDiagram_InrUtrFodda()` i diag_befolkningsprognos_scb_api_profet.R - källkodens egen kommentar där bekräftar det. Rättar en tidigare "oklar status"-notering. |
| karta_befolkningsprognos.R | karta_gg_befolkningsprognos_kommun_scb.R + karta_lf_befolkningsprognos_kommun_scb.R | **Delad 2026-11-11** i en ggplot- och en leaflet-funktion (NEWS.md 0.0.0.9042). Tidigare en funktion, `karta_befolkningsprognos_scb()`. Datahämtningen delas via `intern_befprognos_kartdata()`. |
| karta_pendling_leaflet.R (`karta_arbetspendling()`) | karta_lf_pendlingsfloden_scb.R + karta_gg_nettopendling_kommun_scb.R | **Delad 2026-11-11** (NEWS.md 0.0.0.9042). Tidigare en funktion, `karta_pendling_scb()`, med en ggplot-karta över nettopendling i länet och en leaflet-karta med flödeslinjer som bara delade data. Datahämtningen delas via `rdhamtadata::hamta_pendling_rams_bas_scb()`. |

## B) Ej migrerade källskript (ca 1 fil, exklusive avsiktligt skippade)

### Kartorna är klara
Både `karta_befolkningsprognos.R` och `karta_pendling_leaflet.R` är migrerade (se mappningstabellen
ovan), uppdelade i `karta_gg_*()` (ggplot) och `karta_lf_*()` (leaflet) - se KONVENTIONER.md.
ggplot-kartorna byggs med `rddiagram::skapa_koropletkarta_ggplot()`, leaflet-kartorna skrivs till
fil med `rddiagram::skriv_leaflet_till_fil()` (html eller png via webshot2).

### Oklar status - gick inte att verifiera med säkerhet
Kan redan vara täckt av en migrerad funktion (se tabell A, låg/medel konfidens), men skulle
behöva en manuell dubbelkoll innan den migreras på nytt (för att undvika dubbelarbete).

- `diag_bef_forandring_per_manad.R`

## Avsiktligt inte migrerade (avvecklade eller ersatta källskript)

- `diagram_arb_stallen_mm_ejAPI.R` - avvecklad enligt egen källkod (print-varning).
  Ersättaren `diagram_arb_stallen_mm_foretagarna.R` är migrerad
  (`diag_arb_stallen_mm_foretagarna`).
- `diagram_andel_offentligt.R` (RAMS, tabell AM0207Z/v2 TAB5838) - källkoden flaggar
  UTTRYCKLIGEN ("OBS!! ... Använd istället diagram_andel_offentligt_BAS.R OBS!!") att den är
  ersatt av BAS-varianten och bygger på frusen data (2019-2021). Ersättaren
  `diagram_andel_offentligt_BAS.R` är migrerad (`diag_andel_offentligt_bas_scb`). **Rättar en
  tidigare slutsats** (2026-11-06) som felaktigt klassade de två som "inte dubbletter, båda
  avsiktligt separata" - den bedömningen missade RAMS-filens egen avvecklingsvarning.
