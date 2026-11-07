# Migreringsstatus: Region-Dalarna/diagram → rddiagramkatalog

Dokumentation av vilka diagram som är migrerade från källrepot `Region-Dalarna/diagram`
till paketet `rddiagramkatalog`, och vilka som återstår. Byggd genom innehållsbaserad
matchning (datakälla, PxWeb-tabellkoder/URL:er, `diagram_capt`-text, kommentarer) mellan
källfilerna och paketets `diag_*()`-funktioner - inte bara namnlikhet, eftersom namnen
ofta ändrades rejält vid migrering.

Skapad: 2026-11-07. Uppdatera vid behov när fler diagram migreras - filen är ingen
automatiskt genererad rapport.

**Status vid skapandet:** av 94 källskript i `~/gh/diagram` är ~73 migrerade (58 med hög/
identifierad konfidens + ett antal med medel/låg konfidens, se markeringar nedan), 2 är
kartor (annan kategori, se grupp 3 nedan), 1 är avvecklad, och **~18-21 filer återstår**
att migrera eller utreda.

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
| diag_inr_flyttnetto_inr_utr_fodda_scb.R | diag_inr_flyttnetto_inr_utr_fodda_scb.R | Identiskt namn |
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
| ej säkert identifierad | diag_utrikes_fodda_tidsserie_scb.R | Källa ej verifierad |

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

## B) Ej migrerade källskript - grupperade efter utmaning (~18-21 filer)

### 1. Befolkningsprognos (Profet) - läser lokala filer från G:
Samma mönster som de 7 redan hårdade NMS-diagrammen (se `R/intern_indata.R`-hjälpfunktionerna).
Pekar mot `G:/Samhällsanalys/Statistik/Befolkningsprognoser/Profet/datafiler/` eller liknande.
Behöver samma typ av filvalidering, plus att en "auto-caption"-logik (källtext väljs beroende
på vilken tabell som faktiskt användes) behöver portas.

- `diag_befolkningsprognos_scb_api_profet.R`
- `diag_befprognos_diff_tot_per_kommun.R`
- `diag_bef_utfall_prognos_per_aldersgrupp_scb.R`
- `diag_bef_utfall_prognos_per_region_totalt_scb.R`
- `diag_bef_inrikes_utrikes_antal_forandring_prognos_IntRap.R`

### 2. `source()` mot externa GitHub-skript
Hämtar inte bara data från fil, utan kör `source("https://raw.githubusercontent.com/...")`
mot skript i ett annat repo (Region-Dalarna/hamta_data). Hämtlogiken måste skrivas om till
paketfunktioner, inte bara wrappas.

- `diag_befprognos_diff_tot_per_kommun.R` (även grupp 1)
- `karta_pendling_leaflet.R`

### 3. Kartor (Leaflet/GeoPackage) - annan diagramtyp
Producerar interaktiva kartor, inte `ggplot`. Passar inte `diag_*()`-mönstret (som alltid
returnerar en namngiven lista av ggplot-objekt) - kräver eget grepp, kanske en separat
funktionsfamilj (`karta_*()`?) snarare än att tvingas in i samma mall.

- `karta_befolkningsprognos.R`
- `karta_pendling_leaflet.R`

### 4. Namnlika men INTE dubbletter - två olika datakällor
Undersöktes specifikt eftersom de flaggats tidigare som möjlig dubblett. Svar: **inte en
dubblett** - samma mått (andel offentligt anställda) men från två olika statistikprodukter
(RAMS vs. BAS), alltså avsiktligt separata diagram. Föreslår tydligare suffix vid migrering
(`_rams_scb` / `_bas_scb`) för att undvika framtida förväxling.

- `diagram_andel_offentligt.R` (RAMS, tabell AM0207Z / v2 TAB5838)
- `diagram_andel_offentligt_BAS.R` (BAS, tabell AM0210B/SektorSyssM)

Inga andra tydliga dubblettpar hittades i denna genomgång - de "3 paren" som nämndes någon
gång tidigare i projektet gick inte att återskapa utan mer kontext om vilka de syftade på.

### 5. Avvecklat källskript
- `diagram_arb_stallen_mm_ejAPI.R` - avvecklad enligt egen källkod (print-varning), ignorera.
- `diagram_arb_stallen_mm_foretagarna.R` - den aktiva ersättaren. Väntar fortfarande på migrering.

### 6. NMS/G:-baserat, samma mönster som redan hårdade diagram
- `diagram_bransch_utb_alder_NMS.R`

### 7. Unika externa datakällor (inte SCB PxWeb) - kräver egen hämtlogik per källa
- `diagram_gymnasiantagning_antal_kon.R` (Dalarnas kommunförbund)
- `diagram_kompetensbrist_lan_TVV.R` (Tillväxtverket, tillvaxtdata.tillvaxtverket.se)
- `diag_utslapp_konsumtion_SEI.R` (konsumtionskompassen.se + Kolada)

### 8. Raka PxWeb-diagram utan särskild utmaning
Redo att migreras med det etablerade mönstret, inga kända komplikationer.

- `diag_antal_asylsokande_1984_IntRap.R`
- `diag_fek_foradlingsvarde_bransch_lan_scb.R`
- `diag_fek_lve_tidsserie.R`
- `diagram_flytt_inrikes_bakgrund_SCB.R`

### 9. Oklar status - gick inte att verifiera med säkerhet
Kan redan vara täckta av en migrerad funktion (se tabell A, låg/medel konfidens), men
skulle behöva en manuell dubbelkoll innan de migreras på nytt (för att undvika dubbelarbete).

- `diag_bef_forandring_per_manad.R`
