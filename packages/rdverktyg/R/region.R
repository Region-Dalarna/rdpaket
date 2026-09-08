# Region- och kommunkoder - utbrutna ur func_API.R i Region-Dalarna/funktioner.
# Bygger på SCB:s codelist-endpoints via pxweb2r::pxweb2_get_codelist().

.rdverktyg_cache <- new.env(parent = emptyenv())

#' Regiontabell med kod och namn för riket, län och kommuner
#'
#' Hämtar SCB:s standard-valuesets för riket, län och kommuner och sätter ihop
#' dem till en tabell. Resultatet cachas per R-session.
#'
#' @param uppdatera Om `TRUE` hämtas tabellen om även om den finns i cachen.
#'
#' @return En `tibble` med kolumnerna `regionkod` och `region`.
#' @export
hamtaregtab <- function(uppdatera = FALSE) {
  if (isTRUE(uppdatera) || is.null(.rdverktyg_cache$regiontabell)) {
    .rdverktyg_cache$regiontabell <- dplyr::bind_rows(
      pxweb2r::pxweb2_get_codelist("vs_RegionRiket99"),
      pxweb2r::pxweb2_get_codelist("vs_RegionLän07"),
      pxweb2r::pxweb2_get_codelist("vs_RegionKommun07")
    ) |>
      dplyr::transmute(regionkod = .data$code, region = .data$label)
  }
  .rdverktyg_cache$regiontabell
}

#' Kommunkoder för ett eller flera län
#'
#' @param lan Länskod(er), t.ex. `"20"`. Bara de två första tecknen används.
#' @param tamedlan Ta med länets egen kod i resultatet.
#' @param tamedriket Ta med rikskoden `"00"`.
#' @param allakommuner Om `TRUE` returneras alla kommuner i landet (`lan`
#'   ignoreras för urvalet av kommuner).
#'
#' @return En teckenvektor med regionkoder.
#' @export
hamtakommuner <- function(lan = "20", tamedlan = TRUE, tamedriket = TRUE, allakommuner = FALSE) {
  regdf <- hamtaregtab()
  lan <- substr(lan, 1, 2)
  nydf <- regdf

  if (!allakommuner) {
    nydf <- dplyr::filter(nydf, .data$regionkod == "00" | substr(.data$regionkod, 1, 2) %in% lan)
  }
  if (!tamedlan)   nydf <- dplyr::filter(nydf, !.data$regionkod %in% lan)
  if (!tamedriket) nydf <- dplyr::filter(nydf, .data$regionkod != "00")

  if (allakommuner) {
    if (tamedlan) {
      nydf <- nydf[nchar(nydf$regionkod) %in% c(2L, 4L), ]
      if (!tamedriket) nydf <- nydf[nydf$regionkod != "00", ]
    } else {
      nydf <- nydf[nchar(nydf$regionkod) == 4L, ]
    }
  }

  as.character(nydf$regionkod)
}

#' Alla länskoder
#'
#' @param tamedriket Ta med rikskoden `"00"`.
#'
#' @return En teckenvektor med länskoder (och ev. `"00"`).
#' @export
hamtaAllaLan <- function(tamedriket = TRUE) {
  koder <- hamtaregtab()$regionkod
  retur <- koder[nchar(koder) == 2L]
  if (!tamedriket) retur <- retur[retur != "00"]
  retur
}

#' Slå upp region-namn för en eller flera regionkoder
#'
#' @param regionkod En eller flera regionkoder.
#' @param kolada Om `TRUE` nollutfylls koderna till 4 tecken (Kolada-format).
#'
#' @return En `tibble` med `regionkod` och `region`. Koder som inte hittas får
#'   sig själva som `region`.
#' @export
hamtaregion_kod_namn <- function(regionkod, kolada = FALSE) {
  regdf <- hamtaregtab()
  retur_df <- regdf[regdf$regionkod %in% regionkod, ]

  if (nrow(retur_df) == 0) {
    retur_df <- tibble::tibble(regionkod = regionkod, region = regionkod)
  }
  if (isTRUE(kolada)) {
    retur_df <- dplyr::mutate(
      retur_df,
      regionkod = stringr::str_pad(.data$regionkod, width = 4, side = "left", pad = "0")
    )
  }
  retur_df
}

#' Kommunkoder med namn för ett län
#'
#' @param lanskod Länskod, t.ex. `"20"`.
#' @param kolada Om `TRUE` nollutfylls koderna till 4 tecken.
#'
#' @return En `tibble` med `regionkod` och `region`.
#' @export
hamta_kommunkoder <- function(lanskod = "20", kolada = FALSE) {
  hamtaregion_kod_namn(
    regionkod = hamtakommuner(lanskod, tamedlan = FALSE, tamedriket = FALSE),
    kolada = kolada
  )
}

#' Kortnamn för län
#'
#' `skapa_kortnamn_lan("Dalarnas län")` ger `"Dalarna"`. Tar bort avslutande
#' `" län"` och ett ev. foge-s.
#'
#' @param lansnamn Ett eller flera länsnamn.
#' @param byt_ut_riket_mot_sverige Om `TRUE` blir `"Riket"` -> `"Sverige"`.
#'
#' @return En teckenvektor med kortnamn.
#' @export
skapa_kortnamn_lan <- function(lansnamn, byt_ut_riket_mot_sverige = FALSE) {
  ut <- stringr::str_remove(lansnamn, "s? län$")
  if (isTRUE(byt_ut_riket_mot_sverige)) {
    ut[!is.na(lansnamn) & lansnamn == "Riket"] <- "Sverige"
  }
  ut
}

#' Dela en kolumn med "kod klartext" i två kolumner
#'
#' Används på en kolumn (län, kommun eller annat) där kod och klartext ligger
#' ihop, t.ex. `"2080 Falun"`. Delas i `regionkod` och `region` vid första
#' förekomsten av `separator`.
#'
#' @param skickad_df En data.frame.
#' @param regionkolumn Namnet på kolumnen som ska delas.
#' @param tabortgammalkolumn Om `TRUE` tas ursprungskolumnen bort.
#' @param separator Tecken att dela vid. Standard är mellanslag.
#'
#' @return `skickad_df` med kolumnerna `regionkod` och `region` tillagda.
#' @export
region_kolumn_splitta_kod_klartext <- function(skickad_df, regionkolumn,
                                               tabortgammalkolumn = TRUE, separator = " ") {
  tidyr::separate_wider_delim(
    skickad_df,
    cols = dplyr::all_of(regionkolumn),
    delim = separator,
    names = c("regionkod", "region"),
    too_many = "merge",
    too_few = "align_start",
    cols_remove = tabortgammalkolumn
  )
}

#' Är regionkoderna alla kommuner i ett och samma län?
#'
#' @param reg_koder Regionkoderna som testas.
#' @param tillat_lanskod Tillåt att länets egen kod finns med.
#' @param tillat_rikskod Tillåt att rikskoden `"00"` finns med.
#' @param returnera_text Returnera `"<Län>s kommuner"` i stället för `TRUE`.
#' @param returtext Text att returnera i stället för `NA`/`FALSE` när villkoret
#'   inte är uppfyllt och `returnera_text = TRUE`.
#'
#' @return `TRUE`/`FALSE`, eller en textsträng om `returnera_text = TRUE`.
#' @export
ar_alla_kommuner_i_ett_lan <- function(reg_koder, tillat_lanskod = TRUE, tillat_rikskod = TRUE,
                                       returnera_text = FALSE, returtext = NA) {

  retur_varde <- TRUE
  returtext_na <- is.na(returtext)

  if (length(unique(stringr::str_sub(reg_koder[reg_koder != "00"], 1, 2))) > 1) {
    retur_varde <- FALSE
  } else {
    ej_kommun <- unique(c("00", stringr::str_sub(reg_koder, 1, 2)))
    if (any(nchar(reg_koder) < 4 & !reg_koder %in% ej_kommun)) retur_varde <- FALSE

    kommuner_akt_lan <- hamtakommuner(
      unique(stringr::str_sub(reg_koder[reg_koder != "00"], 1, 2)),
      tamedlan = FALSE, tamedriket = FALSE
    )
    reg_koder_bara_komm <- reg_koder[!reg_koder %in% ej_kommun]
    if (length(reg_koder_bara_komm) < 1) retur_varde <- FALSE
    if (!isTRUE(all(reg_koder_bara_komm == kommuner_akt_lan))) retur_varde <- FALSE
    if (any(reg_koder == stringr::str_sub(reg_koder, 1, 2)) & !tillat_lanskod) retur_varde <- FALSE
    if (any(reg_koder == "00") & !tillat_rikskod) retur_varde <- FALSE
  }

  if (!returnera_text) return(retur_varde)

  if (retur_varde) {
    lanskod <- unique(stringr::str_sub(reg_koder[reg_koder != "00"], 1, 2))
    paste0(skapa_kortnamn_lan(hamtaregion_kod_namn(lanskod)$region), "s kommuner")
  } else {
    if (returtext_na) FALSE else returtext
  }
}

#' Innehåller regionkoderna alla 21 län i Sverige?
#'
#' @param reg_koder Regionkoderna som testas.
#' @param tillat_rikskod Tillåt att rikskoden `"00"` finns med.
#' @param returnera_text Returnera `"Sveriges län"` i stället för `TRUE`.
#' @param returtext Text att returnera i stället för `FALSE` när villkoret inte
#'   är uppfyllt och `returnera_text = TRUE`.
#'
#' @return `TRUE`/`FALSE`, eller en textsträng om `returnera_text = TRUE`.
#' @export
ar_alla_lan_i_sverige <- function(reg_koder, tillat_rikskod = TRUE,
                                  returnera_text = FALSE, returtext = NA) {

  retur_varde <- TRUE
  returtext_na <- all(is.na(returtext))

  if (any(nchar(reg_koder) > 2)) retur_varde <- FALSE
  reg_koder_utan <- unique(reg_koder[reg_koder != "00"])
  alla_lan <- hamtaAllaLan(tamedriket = FALSE)
  if (!(all(reg_koder_utan %in% alla_lan) && all(alla_lan %in% reg_koder_utan) &&
        length(reg_koder_utan) == 21)) {
    retur_varde <- FALSE
  }
  if (any(reg_koder == "00") & !tillat_rikskod) retur_varde <- FALSE

  if (!returnera_text) return(retur_varde)

  if (retur_varde) "Sveriges län" else if (returtext_na) FALSE else returtext
}


# --- Utökning av läns-/kommunkoder till mindre områden -----------------------
# Bygger på pxweb2r::pxweb2_get_values() - signaturen har bytt api_url -> tabell_id
# jämfört med func_API.R. Behöver verifieras mot riktiga tabeller.

intern_giltiga_regionkoder <- function(tabell_id, region_var, base_url) {
  vals <- pxweb2r::pxweb2_get_values(tabell_id, variables = region_var, base_url = base_url)
  if ("type" %in% names(vals)) vals <- dplyr::filter(vals, .data$type == "Variable")
  vals$code
}

#' Utöka läns-/kommunkoder till tätortskoder
#'
#' Skicka läns-, kommun- eller tätortskoder och få tillbaka alla giltiga
#' tätortskoder inom dem (från den angivna tabellen).
#'
#' @param tabell_id PxWeb-tabell-id.
#' @param koder Läns- (2 tecken), kommun- (4 tecken) eller tätortskoder
#'   (>4 tecken). `"*"` returneras oförändrat.
#' @param region_var Regionvariabelns namn i tabellen.
#' @param base_url Bas-URL till PxWeb API v2.
#'
#' @return En teckenvektor med tätortskoder.
#' @export
tatortskoder_bearbeta <- function(tabell_id, koder, region_var = "Region",
                                  base_url = "https://statistikdatabasen.scb.se/api/v2/tables/") {
  if (all(koder == "*")) return(koder)

  giltiga <- intern_giltiga_regionkoder(tabell_id, region_var, base_url)

  kommun_koder <- koder[nchar(koder) == 4]
  lan_koder    <- koder[nchar(koder) == 2]
  omr_koder    <- koder[nchar(koder) > 4]

  unique(c(
    giltiga[substr(giltiga, 1, 4) %in% kommun_koder],
    giltiga[substr(giltiga, 1, 2) %in% lan_koder],
    giltiga[giltiga %in% omr_koder]
  ))
}

#' Utöka läns-/kommunkoder till RegSO-koder
#'
#' @inheritParams tatortskoder_bearbeta
#' @param behall_bara_regsokoder Om `TRUE` behålls bara koder längre än 4 tecken.
#'
#' @return En teckenvektor med RegSO-koder.
#' @export
regsokoder_bearbeta <- function(tabell_id, koder, region_var = "Region",
                                behall_bara_regsokoder = TRUE,
                                base_url = "https://statistikdatabasen.scb.se/api/v2/tables/") {
  ut <- tatortskoder_bearbeta(tabell_id, koder, region_var, base_url)
  if (isTRUE(behall_bara_regsokoder)) ut <- ut[nchar(ut) > 4]
  ut
}

#' Utöka läns-/kommunkoder till DeSO-koder
#'
#' @inheritParams tatortskoder_bearbeta
#' @param behall_bara_desokoder Om `TRUE` behålls bara koder längre än 4 tecken.
#'
#' @return En teckenvektor med DeSO-koder.
#' @export
desokoder_bearbeta <- function(tabell_id, koder, region_var = "Region",
                               behall_bara_desokoder = TRUE,
                               base_url = "https://statistikdatabasen.scb.se/api/v2/tables/") {
  ut <- tatortskoder_bearbeta(tabell_id, koder, region_var, base_url)
  if (isTRUE(behall_bara_desokoder)) ut <- ut[nchar(ut) > 4]
  ut
}

#' Slå upp riktiga regionkoder i tabeller med påhittade koder
#'
#' Vissa myndigheter använder egna löpnummer som regionkoder men lägger de
#' riktiga koderna först i klartexten, t.ex. `"20 Dalarnas län"`. Funktionen
#' plockar ut den riktiga koden och namnet ur klartexten.
#'
#' @param tabell_id PxWeb-tabell-id.
#' @param koder Riktiga regionkoder man vill ha de påhittade koderna för.
#'   `"*"` = alla.
#' @param variabel Regionvariabelns namn i tabellen.
#' @param returnera_nyckeltabell Om `TRUE` returneras en tabell med både
#'   påhittad kod (`felaktig_kod`), riktig `regionkod` och `region`.
#' @param base_url Bas-URL till PxWeb API v2.
#'
#' @return En teckenvektor med de påhittade koderna, eller en `tibble` om
#'   `returnera_nyckeltabell = TRUE`.
#' @export
hamta_regionkod_med_knas_regionkod <- function(tabell_id, koder, variabel,
                                               returnera_nyckeltabell = FALSE,
                                               base_url = "https://statistikdatabasen.scb.se/api/v2/tables/") {
  vals <- pxweb2r::pxweb2_get_values(tabell_id, variables = variabel, base_url = base_url)

  nyckel <- vals |>
    dplyr::transmute(
      felaktig_kod = .data$code,
      regionkod = stringr::str_extract(.data$label, "^\\S+"),
      region = stringr::str_trim(stringr::str_remove(.data$label, "^\\S+"))
    )

  if (!all(koder == "*")) {
    nyckel <- dplyr::filter(nyckel, .data$regionkod %in% koder)
  }

  if (isTRUE(returnera_nyckeltabell)) nyckel else nyckel$felaktig_kod
}
