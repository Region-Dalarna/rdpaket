#' Diagram: inrikes flyttnetto uppdelat på bakgrund
#'
#' Inrikes flyttnetto (flyttningsnetto till/från andra kommuner/regioner),
#' uppdelat på född i Sverige/utrikes född. Ett diagram per region, eller
#' ett gemensamt facet-diagram. Går även att dela upp på åldersgrupper och/
#' eller visa flyttnettot relativt befolkningen i samma grupper.
#'
#' Slår ihop vad som tidigare var två separata, delvis överlappande
#' källskript (`diagram_flytt_inrikes_bakgrund_SCB.R` och
#' `diag_inr_flyttnetto_inr_utr_fodda_scb.R`, som i källrepot råkade dela
#' funktionsnamn) till en enda funktion.
#'
#' @param region_vekt Region(er). `"00"` (Riket) stöds inte - riket saknar
#'   inrikes flyttnetto.
#' @param alder_grp Åldersgrupper. `"*"` (standard) = ingen åldersuppdelning.
#'   För en specifik grupp: `list(c(20, 66))` ger 20-65 år. För flera:
#'   `list(c(0, 19), c(20, 65), c(66, 79), c(80, 999))` (`999` = högsta
#'   ålder, dvs. öppen övre gräns).
#' @param gruppera_namn Namn att gruppera ihop de medskickade regionerna
#'   till en gemensam rad under, eller `NA` (standard) för ingen gruppering.
#' @param relativt_flyttnetto Om `TRUE` visas flyttnettot relativt
#'   befolkningen i samma grupper (föregående års folkmängd) i stället för
#'   som absoluta tal. Stänger av `visa_totalvarden`.
#' @param facet_diagram Om `TRUE` skapas ett gemensamt facet-diagram för
#'   alla regioner, annars (standard) ett diagram per region.
#' @param facet_aldersgrupp Om `TRUE` facetteras varje diagram på
#'   åldersgrupperna (kräver att `alder_grp` är satt - inte `"*"`), i
#'   stället för att summera ihop dem. Kan inte kombineras med
#'   `facet_diagram = TRUE` (facet stöder bara en variabel åt gången).
#'   Stänger av `visa_totalvarden` (totalstrecket räknas annars fel per
#'   åldersgrupp).
#' @param diag_fargvekt Färgvektor för diagrammet.
#' @param diagram_capt Diagrammets källtext.
#' @param utmapp Mapp där diagram (och ev. excelfil) sparas.
#' @param skriv_diagramfil TRUE om diagram ska skrivas till fil.
#' @param skriv_till_excelfil TRUE om data ska skrivas till en excelfil i
#'   `utmapp`.
#' @param returnera_data Skall data returneras till global environment.
#' @param filformat Bildformat på diagrammet, t.ex. `"png"` (standard)
#'   eller `"svg"`.
#' @param visa_totalvarden Visa ett streck för totalt netto (inrikes +
#'   utrikes födda tillsammans). Gäller inte när `relativt_flyttnetto = TRUE`.
#' @param fixa_y_axel_varden_jamna_tal TRUE för jämnare värden på y-axeln.
#'   Fungerar inte alltid - stäng av vid behov.
#' @param demo TRUE om man bara vill se ett exempel på diagrammet i
#'   webbläsaren och inget annat.
#' @param visa_totalvarden_dataetiketter Visa dataetiketter för
#'   totalvärdena.
#' @param totalvarden_dataetiketter_farg Färg på totalstrecken.
#' @param totalvarden_dataetiketter_hjust Justerar dataetiketter för
#'   totalvärden i höjdled.
#' @param totalvarden_dataetiketter_textstorlek Textstorlek för
#'   dataetiketter för totalvärden.
#' @param totalvarden_linjetjocklek Tjocklek på totalstrecken, i tiondels
#'   procent av hela differensen i datasetet.
#'
#' @return En namngiven lista med `ggplot`-objekt.
#' @export
diag_inr_flyttnetto_bakgrund_scb <- function(
    region_vekt = "20",
    alder_grp = "*",
    gruppera_namn = NA,
    relativt_flyttnetto = FALSE,
    facet_diagram = FALSE,
    facet_aldersgrupp = FALSE,
    diag_fargvekt = rddiagram::diagramfarger("rd_gron")[c(1, 4)],
    diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nInrikes flyttnetto är skillnaden mellan de som flyttat in till och de som flyttat ut från en kommun/region, från och till andra kommuner/regioner",
    utmapp = rdverktyg::utskriftsmapp(),
    skriv_diagramfil = TRUE,
    skriv_till_excelfil = FALSE,
    returnera_data = FALSE,
    filformat = "png",
    visa_totalvarden = TRUE,
    fixa_y_axel_varden_jamna_tal = TRUE,
    demo = FALSE,
    visa_totalvarden_dataetiketter = FALSE,
    totalvarden_dataetiketter_farg = "black",
    totalvarden_dataetiketter_hjust = 20,
    totalvarden_dataetiketter_textstorlek = 2,
    totalvarden_linjetjocklek = 4
) {

  # ========================================== Allmän info ============================================
  # Diagram för inrikes flyttnetto uppdelat på bakgrund (inrikes och utrikes födda), med valfri
  # uppdelning på åldersgrupper och/eller relativt flyttnetto (mot befolkningen i samma grupper).
  # ========================================== Inställningar ============================================

  if (demo) {
    demo_url <- "https://region-dalarna.github.io/utskrivna_diagram/Flyttnetto_bakgrund_Dalarna.png"
    browseURL(demo_url)
    rdverktyg::stop_tyst()
  }

  if ("00" %in% region_vekt) {
    stop("Region 00 (Riket) saknar inrikes flyttnetto och kan inte användas.\nÄndra region_vekt")
  }

  if (facet_aldersgrupp && identical(alder_grp, "*")) {
    stop("facet_aldersgrupp = TRUE kräver att alder_grp är satt till faktiska åldersgrupper (inte \"*\").")
  }
  if (facet_aldersgrupp && facet_diagram) {
    stop("facet_aldersgrupp och facet_diagram kan inte kombineras - facet stöder bara en variabel åt gången (antingen region eller åldersgrupp).")
  }
  if (facet_aldersgrupp) visa_totalvarden <- FALSE   # totalstrecket räknas annars fel per åldersgrupp

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil || skriv_till_excelfil)

  if (is.na(diag_fargvekt[1])) diag_fargvekt <- rddiagram::diagramfarger("rd_gron")

  # Hjälpfunktioner: välj individuella ettårsåldrar respektive 100+-koden för en given tabell.
  # CKM-tabellerna dubblerar annars etiketter över flera aggregeringshierarkier (5-/10-årsklasser) -
  # samma problem/lösning som i tidigare migrerade skript (t.ex. diag_utrikes_fodda_tidsserie_scb.R).
  hamta_individuella_aldrar <- function(table_id) {
    v <- pxweb2r::pxweb2_get_values(table_id, "Alder", quiet = TRUE)
    v <- v[grepl("^[0-9]+\\+? år$", v$label), ]
    v <- v[!duplicated(v$label), ]
    v$code
  }
  hamta_100plus_kod <- function(table_id) {
    v <- pxweb2r::pxweb2_get_values(table_id, "Alder", quiet = TRUE)
    v$code[v$label == "100+ år" & v$type == "Variable"][1]
  }

  # ========================================== Åldershantering ============================================

  if (!identical(alder_grp, "*")) {
    alder_grp <- purrr::map(alder_grp, as.character)
    alder_vekt <- alder_grp

    # 999 (= högsta ålder) byts ut till 99 för att kunna skapa en sekvensvektor
    if (any(purrr::map_lgl(alder_grp, ~ "999" %in% .x))) {
      alder_vekt <- purrr::map(alder_vekt, ~ replace(.x, .x == "999", "99"))
    }
    alder_vekt <- as.character(unlist(purrr::map(alder_vekt, ~ seq(.x[1], .x[2]))))
    if (any(purrr::map_lgl(alder_grp, ~ "999" %in% .x))) alder_vekt <- c(alder_vekt, "100+")

    # Bugfix (bekräftad genom test - gav felaktigt "66-100 år" i stället för "66+ år" i facet-
    # etiketterna): jämförelsen avsåg uppenbarligen att fånga upp sentinelvärdet för "öppen övre
    # gräns", men jämförde mot strängen "100+" trots att det faktiska sentinelvärdet (dokumenterat
    # här och i originalskriptets egen parameterkommentar) är "999". Med "100+" i jämförelsen
    # utlöstes den här grenen ALLTID när 999 användes, vilket satte en konkret (inte oändlig)
    # övre gräns - skapa_aldersgrupper() i rdverktyg ger då bara en "+"-etikett om den riktiga
    # maxåldern i datan överstiger gränsen, vilket den aldrig gjorde mot en artificiellt hög
    # gräns på 1000. Fanns redan i originalet (samma jämförelse där), inte något nytt här.
    alder_grp_vekt <- purrr::map_int(alder_grp, ~ readr::parse_number(.[1]))
    if (alder_grp[[length(alder_grp)]][2] != "999") {
      alder_grp_vekt <- append(alder_grp_vekt, readr::parse_number(alder_grp[[length(alder_grp)]][2]) + 1)
    }

    alder_hamta_historik <- alder_vekt[alder_vekt != "100+"]
    alder_hamta_ckm <- alder_hamta_historik
    if ("100+" %in% alder_vekt) {
      alder_hamta_historik <- c(alder_hamta_historik, "100+")
      alder_hamta_ckm <- c(alder_hamta_ckm, hamta_100plus_kod("TAB6657"))
    }
  } else {
    alder_vekt <- alder_grp
    alder_hamta_historik <- NA   # elimineras - TAB4693 har en egen totalkod för Alder
    alder_hamta_ckm <- "TotSA"
  }

  # =====================================================================================================
  # Hämtas via SCB:s PxWeb-API v2 med pxweb2r, från två tabeller (SCB bytte metod - CKM,
  # röjandekontroll - för nya årgångar, med en ny tabell från och med 2025):
  # - TAB4693 = gamla "FlyttFodReg", 2002-2024. Alder/Kon kan elimineras (utelämnas ur query_list),
  #   ger då totalen automatiskt från SCB:s API i stället för att behöva summeras ihop manuellt här.
  # - TAB6657 = nya CKM-tabellen, från 2025. Alder är INTE elimineringsbar där längre - måste anges
  #   explicit som totalkoden "TotSA" (eller individuella åldrar vid åldersuppdelning).
  # Kön elimineras i båda tabellerna (Kon = NA) - en egen summering av köns-delarna kan annars avvika
  # från den riktiga totalen för CKM-tabellen (röjandekontrollen avrundar varje delvärde för sig).
  cont_hamta <- c("Flyttningsnetto, eget län", "Flyttningsnetto, övriga län")

  flytt_df_historik <- pxweb2r::pxweb2_get_data(
    table = "TAB4693",
    query = list(
      Region = region_vekt,
      Fodelseregion = c("född i Sverige", "utrikes född"),
      Alder = alder_hamta_historik,
      Kon = NA,
      ContentsCode = cont_hamta,
      Tid = "*"
    ),
    on_all_values_invalid = "null", quiet = TRUE)

  flytt_df_ckm <- pxweb2r::pxweb2_get_data(
    table = "TAB6657",
    query = list(
      Region = region_vekt,
      Fodelseregion = c("född i Sverige", "utrikes född"),
      Alder = alder_hamta_ckm,
      Kon = NA,
      ContentsCode = cont_hamta,
      Tid = "*"
    ),
    on_all_values_invalid = "null", quiet = TRUE)

  # När ingen åldersuppdelning efterfrågas har historik-grenen ingen ålderskolumn alls (eliminerad),
  # medan CKM-grenen har en ålderskolumn med enda värdet "totalt ålder" (TotSA) - tas bort här så att
  # bind_rows() nedan inte skapar en extra, inkonsekvent kolumn.
  if (identical(alder_grp, "*") && !is.null(flytt_df_ckm)) {
    flytt_df_ckm <- dplyr::select(flytt_df_ckm, -dplyr::any_of("ålder"))
  }

  har_ckm_data <- !is.null(flytt_df_ckm) && nrow(flytt_df_ckm) > 0
  ckm_fran_ar <- if (har_ckm_data) min(as.integer(flytt_df_ckm$år)) else NULL
  diagram_capt <- rddiagram::lagg_till_ckm_notering(diagram_capt, har_ckm_data, fran_ar = ckm_fran_ar)

  px_df <- dplyr::bind_rows(flytt_df_historik, flytt_df_ckm) |>
    dplyr::rename(regionkod = region_kod, varde = value) |>
    dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region, byt_ut_riket_mot_sverige = TRUE))

  # Slår ihop de två delkomponenterna (eget län + övriga län) till inrikes flyttningsnetto - grupperar
  # på allt utom tabellinnehåll/varde, så en ev. ålderskolumn bevaras om åldersuppdelning är aktiv.
  grp_utan_contvar <- setdiff(names(px_df), c("tabellinnehåll", "varde"))
  px_df <- px_df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grp_utan_contvar))) |>
    dplyr::summarise(Inrikes_flyttnetto = sum(varde, na.rm = TRUE), .groups = "drop")

  # Vid åldersuppdelning: gör om enskilda åldrar till åldersgrupper och summera ihop.
  if (!identical(alder_grp, "*")) {
    px_df <- px_df |>
      dplyr::mutate(alder_num = readr::parse_number(ålder),
             aldersgrupp = rdverktyg::skapa_aldersgrupper(alder_num, alder_grp_vekt)) |>
      dplyr::select(-c(alder_num, ålder)) |>
      dplyr::group_by(dplyr::across(dplyr::where(~ is.character(.x) || is.factor(.x)))) |>
      dplyr::summarise(Inrikes_flyttnetto = sum(Inrikes_flyttnetto, na.rm = TRUE), .groups = "drop")
  }

  if (!is.na(gruppera_namn)) {
    # Bugfix (bekräftad genom test): regionkod sattes aldrig till något efter gruppering (saknades
    # helt i originalet, redan innan sammanslagningen med syskonskriptet) - skapa_diagram() nedan
    # filtrerar på regionkod, vilket kraschade ("object 'regionkod' not found") så fort
    # gruppera_namn faktiskt användes. Sätts nu till "grp", samma konvention som används i
    # rdhamtadata::hamta_befprognos_diff_data() för motsvarande gruppering.
    px_df <- px_df |>
      dplyr::select(-c(regionkod, region)) |>
      dplyr::group_by(dplyr::across(dplyr::where(~ is.character(.x) || is.factor(.x)))) |>
      dplyr::summarise(Inrikes_flyttnetto = sum(Inrikes_flyttnetto, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(regionkod = "grp", region = gruppera_namn)
  } else {
    px_df <- px_df |>
      dplyr::group_by(dplyr::across(dplyr::where(~ is.character(.x) || is.factor(.x)))) |>
      dplyr::summarise(Inrikes_flyttnetto = sum(Inrikes_flyttnetto, na.rm = TRUE), .groups = "drop")
  }

  # ========================================== Relativt flyttnetto ============================================

  if (relativt_flyttnetto) {

    bef_ar_vekt <- as.character(as.numeric(unique(px_df$år)) - 1)

    # TAB4823 (historik) saknar en egen totalkod för Alder (till skillnad från TAB4693 ovan) - alla
    # individuella åldrar hämtas därför alltid och summeras ihop här, oavsett alder_grp. TAB6645 (CKM)
    # saknar den helt på samma sätt och hanteras likadant. Ingen av tabellerna har heller en totalkod
    # för Kon - båda könen hämtas explicit och summeras bort, i stället för att elimineras (samma
    # mönster som i diag_utrikes_fodda_tidsserie_scb.R, som använder samma tabellpar).
    if (!identical(alder_grp, "*")) {
      alder_bef_hamta_historik <- alder_vekt[alder_vekt != "100+"]
      alder_bef_hamta_ckm <- alder_bef_hamta_historik
      if ("100+" %in% alder_vekt) {
        alder_bef_hamta_historik <- c(alder_bef_hamta_historik, "100+")
        alder_bef_hamta_ckm <- c(alder_bef_hamta_ckm, hamta_100plus_kod("TAB6645"))
      }
    } else {
      alder_bef_hamta_historik <- "*"
      alder_bef_hamta_ckm <- hamta_individuella_aldrar("TAB6645")
    }

    kon_hamta <- c("män", "kvinnor")

    bef_df_historik <- pxweb2r::pxweb2_get_data(
      table = "TAB4823",
      query = list(
        Region = region_vekt,
        Alder = alder_bef_hamta_historik,
        Kon = kon_hamta,
        Fodelseregion = c("född i Sverige", "utrikes född"),
        ContentsCode = "Antal",
        Tid = bef_ar_vekt
      ),
      on_all_values_invalid = "null", quiet = TRUE)

    bef_df_ckm <- pxweb2r::pxweb2_get_data(
      table = "TAB6645",
      query = list(
        Region = region_vekt,
        Alder = alder_bef_hamta_ckm,
        Kon = kon_hamta,
        Fodelseregion = c("född i Sverige", "utrikes född"),
        ContentsCode = "Antal",
        Tid = bef_ar_vekt
      ),
      on_all_values_invalid = "null", quiet = TRUE)

    bef_df <- dplyr::bind_rows(bef_df_historik, bef_df_ckm) |>
      dplyr::rename(regionkod = region_kod, Antal = value) |>
      dplyr::select(-tabellinnehåll) |>
      dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region)) |>
      # summera bort kön (ingen av tabellerna har en egen totalkod för det)
      dplyr::group_by(dplyr::across(-c(kön, Antal))) |>
      dplyr::summarise(Antal = sum(Antal, na.rm = TRUE), .groups = "drop")

    if (!identical(alder_grp, "*")) {
      bef_df <- bef_df |>
        dplyr::mutate(alder_num = readr::parse_number(ålder),
               aldersgrupp = rdverktyg::skapa_aldersgrupper(alder_num, alder_grp_vekt)) |>
        dplyr::select(-c(alder_num, ålder))
    } else {
      bef_df <- dplyr::select(bef_df, -ålder)
    }

    bef_df <- bef_df |>
      dplyr::group_by(dplyr::across(dplyr::where(~ is.character(.x) || is.factor(.x)))) |>
      dplyr::summarise(dplyr::across(dplyr::where(is.numeric), \(x) sum(x, na.rm = TRUE)), .groups = "drop") |>
      dplyr::rename(bef = Antal) |>
      dplyr::mutate(år = as.character(as.numeric(år) + 1))           # för att koppla till rätt år

    kol_nycklar <- intersect(
      names(dplyr::select(bef_df, dplyr::where(~ is.character(.x) || is.factor(.x)))),
      names(dplyr::select(px_df, dplyr::where(~ is.character(.x) || is.factor(.x))))
    )

    px_df <- dplyr::left_join(px_df, bef_df, by = kol_nycklar) |>
      dplyr::mutate(rel_flyttnetto = Inrikes_flyttnetto / bef * 100) |>
      dplyr::rename(abs_flyttnetto = Inrikes_flyttnetto,
             Inrikes_flyttnetto = rel_flyttnetto)

    visa_totalvarden <- FALSE          # vid relativt flyttnetto tas totalvärdena bort
    relativt_txt <- "relativt "
  } else relativt_txt <- ""

  # Returnerar data till R globala miljö
  if (returnera_data == TRUE) {
    assign("flytt_bakgrund_df", px_df, envir = .GlobalEnv)
  }

  if (skriv_till_excelfil) {
    reg_namn <- ifelse(!is.na(gruppera_namn), gruppera_namn, paste0(region_vekt, collapse = "_"))
    openxlsx::write.xlsx(px_df, paste0(utmapp, "Flyttnetto_", reg_namn, "_ar", min(px_df$år), "_", max(px_df$år), ".xlsx"), overwrite = TRUE)
  }

  # ============================================= Skapa diagram ==============================================

  skapa_diagram <- function(df, vald_regionkod) {  # skapa en funktion som skapar diagram för varje region

    df <- dplyr::filter(df, regionkod %in% vald_regionkod)

    reg_txt <- (rdverktyg::skapa_kortnamn_lan(unique(df$region), byt_ut_riket_mot_sverige = TRUE))[1]
    unika_ar <- unique(df$år)
    unika_reg <- unique(vald_regionkod)

    if (length(unique(df$region)) > 1) {
      reg_txt <- paste0(reg_txt, "_facet")
      diagtitel_txt <- ""
    } else {
      diagtitel_txt <- paste0(" i ", reg_txt)
    }
    diagram_titel <- paste0("Inrikes ", relativt_txt, "flyttnetto", diagtitel_txt)

    # =================================== visa totalvärden ========================================
    if (visa_totalvarden == TRUE && facet_diagram == FALSE) {

      diff <- max(df$Inrikes_flyttnetto) - min(df$Inrikes_flyttnetto) # ta reda på skillnaden mellan det högsta och lägsta värdet i datasetet
      totalvarden_linjebredd <- diff * (totalvarden_linjetjocklek / 1000)      # gör en linjetjocklek på totallinjerna som är 0,2 % av diff (på raden ovan)
      total_list <- list()

      for (reg in 1:length(unika_reg)) {
        for (ar in 1:length(unika_ar)) {
          arsvarde <- dplyr::pull(
            dplyr::select(
              dplyr::filter(df, år == unika_ar[ar], regionkod == unika_reg[reg]),
              Inrikes_flyttnetto
            )
          )
          arsvarde <- sum(arsvarde, na.rm = TRUE)
          total_list <- c(total_list, list(list(geom = "rect", ymin = arsvarde - totalvarden_linjebredd, ymax = arsvarde + totalvarden_linjebredd, xmin = ar - 0.45, xmax = ar + 0.45, alpha = 1, fill = "black")))
          if (visa_totalvarden_dataetiketter) {
            total_list <- c(total_list, list(list(geom = "text", y = arsvarde + totalvarden_dataetiketter_hjust, x = ar, size = totalvarden_dataetiketter_textstorlek, angle = 0, fontface = "plain", label = arsvarde, color = totalvarden_dataetiketter_farg)))
          } # slut if-sats om man vill vissa dataetiketter
        } # slut for-loop unika_ar
      } # slut for_loop unika_reg
    } else total_list <- NA # slut if-sats visa_totalvärden

    # ======================= skapa ggplot-objekt =================

    # aldersgrupper-suffix förhindrar att en åldersuppdelad bild skriver över standardbilden (eller
    # tvärtom) när bägge genereras till samma utmapp - annars identiskt filnamn i båda fallen.
    diagramfil <- stringr::str_replace_all(
      paste0("Flyttnetto_", stringr::str_remove(relativt_txt, " "), if (nzchar(relativt_txt)) "_" else "",
             reg_txt, if (!identical(alder_grp, "*")) "_aldersgrupper" else "",
             if (facet_aldersgrupp) "_facet" else "", ".", filformat),
      "__", "_"
    )

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = df,
      skickad_x_var = "år",
      skickad_y_var = "Inrikes_flyttnetto",
      skickad_x_grupp = "födelseregion",
      diagram_titel = diagram_titel,
      diagram_capt = diagram_capt,
      stodlinjer_avrunda_fem = fixa_y_axel_varden_jamna_tal,
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      manual_y_axis_title = if (relativt_flyttnetto) "procent" else "",
      geom_position_stack = if (relativt_flyttnetto) FALSE else TRUE,
      fokusera_varden = total_list,
      facet_grp = if (facet_aldersgrupp) "aldersgrupp" else if (facet_diagram) "region" else NULL,
      # SkapaStapelDiagram() döljer legenden som standard när ett diagram facetteras (legend_pos
      # defaultar till "none" om inte facet_legend_bottom = TRUE anges explicit) - utan den syns
      # inte vilken färg som är "född i Sverige" respektive "utrikes född" i ett facet-diagram.
      facet_legend_bottom = TRUE,
      skriv_till_diagramfil = skriv_diagramfil && !visa_totalvarden,
      manual_color = diag_fargvekt,
      diagram_bildformat = filformat,
      output_mapp = utmapp,
      filnamn_diagram = diagramfil
    )

    if (visa_totalvarden) {
      # Den faktiska totalindikatorn ritas redan av fokusera_varden/total_list ovan (de korta svarta
      # strecken i varje stapel) - det här geom_line-lagret finns bara för att få en förklaringsruta
      # ("inrikes flyttnetto totalt") i teckenförklaringen. alpha = 0 gör linjen osynlig i själva
      # diagrammet (annars ritas den synligt genom de råa dataraderna, en per år och födelseregion -
      # upptäckt genom visuell kontroll: utan alpha = 0 blev det en missvisande sicksack-linje mellan
      # de två kategorierna i stället för en ren förklaringssymbol), och group = 1 tar bort varningen
      # "Each group consists of only one observation" (samma lösning som redan fanns i syskonskriptet
      # - diag_inr_flyttnetto_inr_utr_fodda_scb.R - innan sammanslagningen, av samma anledning).
      gg_obj <- gg_obj +
        ggplot2::geom_line(ggplot2::aes(group = 1, color = "line"), alpha = 0) +
        ggplot2::scale_color_manual(name = "", values = c("line" = "black"), labels = "inrikes flyttnetto totalt") +
        ggplot2::theme(legend.key = ggplot2::element_rect(fill = "white"),
              legend.box.just = "bottom")
    } # slut if-sats visa_totalvarden

    gg_list <- list(gg_obj)

    # Diagrammet ovan skriver aldrig till fil på egen hand när visa_totalvarden = TRUE (eftersom
    # totalstrecket läggs på EFTER SkapaStapelDiagram(), så det måste sparas separat här för att
    # komma med på bilden).
    if (skriv_diagramfil && visa_totalvarden) {
      rddiagram::skriv_till_diagramfil(gg_obj,
                            output_mapp = utmapp,
                            filnamn_diagram = diagramfil)
    } # slut if-sats
    names(gg_list) <- stringr::str_remove(diagramfil, "\\.[^.]+$")
    return(gg_list)
  } # slut skapa diagram-funktion för varje region

  # Efter gruppera_namn finns bara en "region" kvar (regionkod "grp") - de ursprungliga koderna i
  # region_vekt finns inte längre i px_df och kan inte användas för att välja ut rader.
  diagram_regionkoder <- if (!is.na(gruppera_namn)) "grp" else region_vekt

  if (length(diagram_regionkoder) == 1) facet_diagram <- FALSE

  if (facet_diagram) {
    diag <- skapa_diagram(px_df, diagram_regionkoder)
  } else {
    diag <- purrr::flatten(purrr::map(unique(diagram_regionkoder), ~ skapa_diagram(px_df, .x)))
  }

  return(diag)
}
