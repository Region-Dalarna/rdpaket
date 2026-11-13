#' Leaflet-karta: kraftfält kring lokala arbetsmarknadscentrum
#'
#' Karta över tätorters kraftfält, byggd på pendlingen mellan tätorter. Tätorter
#' där färre än `enskilt_troskelvarde` procent av de sysselsatta pendlar till en
#' annan tätort är lokala arbetsmarknadscentrum (LA). Tätorter där minst så
#' många pendlar till en annan tätort är lokala arbetsmarknadssatelliter och
#' tillhör kraftfältet för den tätort som pendlingen går till. Kraftfälten
#' ritas som lila fält runt pendlingsvägarna mellan satelliterna och deras LA.
#' Beräkningen görs av [rdgeorouting::pendling_kraftfalt()] mot
#' vägnätet i geodatabasen.
#'
#' Pendlingsunderlaget är tabellen `rutor.pend_relation_tatorter` i
#' sekretessdatabasen (kolumnerna `from_id`, `to_id` och `n`). Tabellen har
#' ingen årskolumn - ange vilket år den gäller i `pendlingsar` så att året
#' kommer med i kartans källtext och filnamn.
#'
#' Bakgrunden är den neutrala bakgrundskartan i
#' [rddiagram::lagg_till_bakgrundskarta()], utan logga.
#'
#' @param tabell_pend_relation Data.frame med pendlingsrelationer
#'   (`from_id`, `to_id`, `n`), eller `NULL` (standard) för att läsa
#'   `rutor.pend_relation_tatorter` ur sekretessdatabasen.
#' @param kraftfalt Ett färdigt resultat från
#'   [rdgeorouting::pendling_kraftfalt()]. Anges det hoppas
#'   beräkningen över, vilket är praktiskt om du vill ändra utseendet utan att
#'   räkna om kraftfälten. `NULL` (standard) = beräkna.
#' @param pendlingsar Vilket år pendlingsdata gäller. Visas i kartans källtext
#'   och i filnamnet. Standard är `2022`.
#' @param vald_lan Länskod, för länsgränsen och för kartans utsnitt. Standard
#'   `"20"` (Dalarna). Vilka tätorter som finns med bestäms av underlaget.
#' @param enskilt_troskelvarde,totalt_troskelvarde Tröskelvärden (procent) för
#'   klassificeringen, se [rdgeorouting::pendling_kraftfalt()].
#' @param primar_la_buffer,sekundar_la_buffer,gemensam_la_buffer Bredd (meter)
#'   på kraftfälten runt pendlingsvägarna.
#' @param con_sekretess En `DBIConnection` mot sekretessdatabasen, eller `NA`
#'   (standard) för att koppla upp med `rdpostgres::uppkoppling_adm("sekretess")`. Används bara när
#'   `tabell_pend_relation` är `NULL`.
#' @param kartrubrik Rubriken i textrutan uppe till höger. Standard
#'   `"Kraftfält"`.
#' @param forklaringstext Texten i textrutan, som ett eller flera stycken
#'   (en teckenvektor). `NULL` (standard) = en standardtext som anger
#'   tröskelvärdena.
#' @param karta_bredd,karta_hojd,zoom,karta_fordrojning Bildmått (pixlar),
#'   pixeltäthet och väntetid (sekunder, så att bakgrundskartan hinner ritas)
#'   för png, se [rddiagram::skriv_leaflet_till_fil()]. `zoom = 3` ger en skarp
#'   bild för utskrift. En png görs utan zoomknappar och utan texten om att
#'   klicka på tätorterna. Gäller bara `leaflet_filformat = "png"`.
#' @param favicon Ikon på webbläsarfliken för html: `TRUE` (standard), `FALSE`
#'   eller sökväg till en bildfil, se [rddiagram::skriv_leaflet_till_fil()].
#' @param ... Skickas vidare till [rdgeorouting::pendling_kraftfalt()], t.ex.
#'   `kant_tabell` för att räkna på ett annat vägnät.
#' @param etikettstorlek Textstorlek (px) på ortnamnen.
#' @param utmapp Mapp där kartfilen sparas. Måste finnas om
#'   `skriv_leaflet_fil` eller `skriv_gpkg_fil` är `TRUE`.
#' @param skriv_leaflet_fil TRUE om kartan ska skrivas till fil.
#' @param leaflet_filformat `"html"` (standard, interaktiv) eller `"png"`
#'   (statisk ögonblicksbild via webshot2, se
#'   [rddiagram::skriv_leaflet_till_fil()]).
#' @param skriv_gpkg_fil TRUE om kraftfältslagren dessutom ska skrivas till en
#'   GeoPackage-fil (SWEREF99 TM) för vidare arbete i t.ex. QGIS.
#' @param returnera_figur TRUE (standard) för att returnera kartan.
#'
#' @return Osynligt: en namngiven lista med ett `leaflet`-objekt. Namnet är
#'   filens namn utan ändelse (t.ex. `"kraftfalt_dalarnas_lan_ar2022"`).
#' @seealso [rdgeorouting::pendling_kraftfalt()],
#'   [karta_lf_pendlingsfloden_scb()]
#' @export
karta_lf_kraftfalt_scb <- function(
    tabell_pend_relation = NULL,
    kraftfalt = NULL,
    pendlingsar = 2022,
    vald_lan = "20",
    enskilt_troskelvarde = 20,
    totalt_troskelvarde = 35,
    primar_la_buffer = 2000,
    sekundar_la_buffer = 1000,
    gemensam_la_buffer = 5000,
    con_sekretess = NA,
    kartrubrik = "Kraftfält",
    forklaringstext = NULL,
    etikettstorlek = 13,
    karta_bredd = 1200,
    karta_hojd = 850,
    zoom = 1,
    karta_fordrojning = 8,
    favicon = TRUE,
    ...,
    utmapp = rdverktyg::utskriftsmapp(),
    skriv_leaflet_fil = TRUE,
    leaflet_filformat = "html",
    skriv_gpkg_fil = FALSE,
    returnera_figur = TRUE
) {

  # ========================================== Inställningar ============================================

  intern_kontrollera_utdata(c(skriv_leaflet_fil, skriv_gpkg_fil), returnera_figur, c("skriv_leaflet_fil", "skriv_gpkg_fil"))
  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_leaflet_fil || skriv_gpkg_fil)

  # ========================================== Hämta och beräkna ========================================

  if (is.null(kraftfalt)) {
    if (is.null(tabell_pend_relation)) {
      tabell_pend_relation <- intern_hamta_pendling_tatorter(con_sekretess)
    }
    kraftfalt <- rdgeorouting::pendling_kraftfalt(
      tabell_pend_relation,
      enskilt_troskelvarde = enskilt_troskelvarde, totalt_troskelvarde = totalt_troskelvarde,
      primar_la_buffer = primar_la_buffer, sekundar_la_buffer = sekundar_la_buffer,
      gemensam_la_buffer = gemensam_la_buffer, ...)
  }

  lan_gis <- rdgis::hamta_karta("lan", regionkoder = vald_lan)
  lansnamn <- rdverktyg::hamtaregion_kod_namn(vald_lan)$region[1]

  bygg_karta <- function(interaktiv) {
    intern_bygg_kraftfaltskarta(
      kraftfalt, lan_gis, pendlingsar = pendlingsar,
      enskilt_troskelvarde = enskilt_troskelvarde, totalt_troskelvarde = totalt_troskelvarde,
      kartrubrik = kartrubrik, forklaringstext = forklaringstext, etikettstorlek = etikettstorlek,
      interaktiv = interaktiv)
  }
  leaflet_map <- bygg_karta(interaktiv = TRUE)

  filnamn_bas <- paste0("kraftfalt_", gsub("[^a-z0-9]+", "_", tolower(stringi::stri_trans_general(lansnamn, "Latin-ASCII"))),
                        "_ar", pendlingsar)

  if (skriv_gpkg_fil) {
    gpkg <- paste0(utmapp, filnamn_bas, ".gpkg")
    if (file.exists(gpkg)) file.remove(gpkg)
    for (namn in names(kraftfalt)) {
      lager <- kraftfalt[[namn]]
      if (!is.null(lager) && nrow(lager) > 0) sf::st_write(lager, gpkg, layer = namn, append = TRUE, quiet = TRUE)
    }
    message("Lager skrivna till ", gpkg)
  }

  if (skriv_leaflet_fil) {
    # en png går inte att klicka i: utan zoomknappar och utan texten om att klicka på tätorterna
    ar_png <- tolower(sub("^\\.+", "", trimws(leaflet_filformat))) == "png"
    karta_for_fil <- if (ar_png) bygg_karta(interaktiv = FALSE) else leaflet_map
    rddiagram::skriv_leaflet_till_fil(karta_for_fil, utmapp, filnamn_bas, karta_filformat = leaflet_filformat,
                                      karta_bredd = karta_bredd, karta_hojd = karta_hojd, zoom = zoom,
                                      karta_fordrojning = karta_fordrojning, favicon = favicon)
  }

  if (returnera_figur) invisible(stats::setNames(list(leaflet_map), filnamn_bas))
}

# Läser pendlingstabellen för tätorter ur sekretessdatabasen (rutor.pend_relation_tatorter).
intern_hamta_pendling_tatorter <- function(con = NA) {
  egen <- FALSE
  if (!inherits(con, "DBIConnection")) {
    con <- rdpostgres::uppkoppling_adm("sekretess")
    if (is.null(con)) stop("Kunde inte ansluta till sekretessdatabasen.", call. = FALSE)
    egen <- TRUE
  }
  if (egen) on.exit(DBI::dbDisconnect(con), add = TRUE)

  df <- rdpostgres::postgres_tabell_till_df(con, schema = "rutor", tabell = "pend_relation_tatorter")
  if (is.null(df)) stop("Kunde inte läsa rutor.pend_relation_tatorter.", call. = FALSE)
  df[, c("from_id", "to_id", "n")]
}

# Bygger själva kartan av färdiga kraftfältslager. Utbruten från karta_lf_kraftfalt_scb() så att
# ritdelen går att köra och testa utan databas.
intern_bygg_kraftfaltskarta <- function(kraftfalt, lan_gis, pendlingsar, enskilt_troskelvarde,
                                        totalt_troskelvarde, kartrubrik, forklaringstext,
                                        etikettstorlek, interaktiv = TRUE) {
  farg_la <- "#f28c28"
  farg_sekundar <- "#2a9d8f"
  farg_satellit <- "#c62828"
  farg_falt <- "#b0309c"
  farg_lansgrans <- "#2b6cb0"

  till_wgs84 <- function(x) sf::st_transform(x, 4326)
  # tätorternas geometri kan vara polygoner - markörerna ritas i en punkt innanför
  till_punkter <- function(x) {
    if (is.null(x) || nrow(x) == 0) return(NULL)
    x <- suppressWarnings(sf::st_set_geometry(x, sf::st_point_on_surface(sf::st_geometry(x))))
    till_wgs84(x)
  }

  falt <- lapply(kraftfalt[c("primla_areas", "secla_areas", "commonla_areas")],
                 function(x) if (is.null(x) || nrow(x) == 0) NULL else till_wgs84(x))
  la <- till_punkter(rbind_sf_kolumner(kraftfalt$la, kraftfalt$solitary))
  sekundar <- till_punkter(kraftfalt$common_la)
  satelliter <- till_punkter(kraftfalt$satellites)
  lan_gis <- till_wgs84(lan_gis)

  # ===== textrutor =====
  if (is.null(forklaringstext)) {
    forklaringstext <- c(
      paste0("De tätorter där färre än ", enskilt_troskelvarde, "% av de sysselsatta pendlar till en annan ",
             "tätort kategoriseras som ett Lokalt arbetsmarknadscentrum."),
      paste0("Varje tätort där ", enskilt_troskelvarde, "% eller fler av de sysselsatta pendlar till en annan ",
             "tätort kategoriseras som en lokal arbetsmarknadssatellit och tillhör den tätorts kraftfält som ",
             "pendlingen går till."),
      paste0("Om en tätort har en pendling på ", enskilt_troskelvarde, "% eller mer till fler än en tätort ",
             "så bildas ett Sekundärt lokalt arbetsmarknadscentrum med den tätort som näst störst andel av ",
             "pendlingen går till."))
  }
  rubrikruta <- htmltools::div(
    style = paste0("background: white; border: 1px solid #333; padding: 10px 14px; width: 380px; ",
                   "font-family: Georgia, 'Times New Roman', serif; font-size: 13px; text-align: justify; line-height: 1.25;"),
    htmltools::h3(kartrubrik, style = "text-align: center; margin: 0 0 10px; font-size: 18px;"),
    lapply(forklaringstext, function(x) htmltools::p(x, style = "margin: 0 0 12px;")))

  prick <- function(farg, storlek) {
    htmltools::span(style = paste0("display: inline-block; border-radius: 50%; background: ", farg, "; width: ",
                                   storlek, "px; height: ", storlek, "px; margin-right: 8px; vertical-align: middle;"))
  }
  forklaring <- htmltools::div(
    style = "background: white; border: 1px solid #ccc; padding: 8px 12px; font-size: 14px; line-height: 1.7;",
    htmltools::div("Teckenförklaring", style = "font-weight: bold; font-size: 17px;"),
    htmltools::div(prick(farg_la, 12), "Lokalt arbetsmarknadscentrum"),
    htmltools::div(prick(farg_sekundar, 12), "Sekundärt lokalt arbetsmarknadscentrum"),
    htmltools::div(prick(farg_satellit, 8), "Lokal arbetsmarknadssatellit"),
    htmltools::div(htmltools::span(style = paste0("display: inline-block; width: 22px; border-top: 2px dashed ", farg_lansgrans,
                                                  "; margin-right: 8px; vertical-align: middle;")), "Länsgräns"))

  kalla <- htmltools::div(
    style = "background: rgba(255,255,255,0.7); padding: 2px 6px; font-size: 10px;",
    paste0("Karta skapad av: Samhällsanalys, Region Dalarna. Källa: pendlingsdata år ", pendlingsar,
           ", bearbetning av Region Dalarna.",
           if (interaktiv) " Klicka på en tätort för att se andelen som pendlar."))

  # ===== utsnitt: länets yta, med extra plats åt öster där textrutorna ligger =====
  bb <- as.numeric(sf::st_bbox(lan_gis))  # xmin, ymin, xmax, ymax (utan namn - namngivna värden blir json-objekt)
  bredd <- bb[3] - bb[1]
  hojd <- bb[4] - bb[2]

  m <- leaflet::leaflet(options = leaflet::leafletOptions(zoomSnap = 0.25, zoomControl = interaktiv)) |>
    rddiagram::lagg_till_bakgrundskarta(etiketter = "orter", etikett_opacitet = 0.35) |>
    leaflet::fitBounds(bb[1] - 0.02 * bredd, bb[2] - 0.02 * hojd, bb[3] + 0.35 * bredd, bb[4] + 0.02 * hojd)

  for (f in falt) {
    if (!is.null(f)) {
      m <- leaflet::addPolygons(m, data = f, stroke = FALSE, fillColor = farg_falt, fillOpacity = 0.35)
    }
  }

  m <- leaflet::addPolygons(m, data = lan_gis, fill = FALSE, color = farg_lansgrans, weight = 2, opacity = 1,
                            dashArray = "5, 5")

  etikett_stil <- list("font-size" = paste0(etikettstorlek, "px"), "color" = "#222",
                       "text-shadow" = "-1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff")
  lagg_punkter <- function(m, x, farg, radie, med_etikett) {
    if (is.null(x)) return(m)
    m <- leaflet::addCircleMarkers(m, data = x, radius = radie, color = farg, weight = 1, opacity = 1,
                                   fillColor = farg, fillOpacity = 0.95,
                                   popup = ~intern_kraftfalt_popup(tobeteckn, perc_total_commuters))
    if (med_etikett) {
      m <- leaflet::addLabelOnlyMarkers(m, data = x, label = ~as.character(tobeteckn),
                                        labelOptions = leaflet::labelOptions(noHide = TRUE, direction = "top", textOnly = TRUE,
                                                                             offset = c(0, -4), style = etikett_stil))
    }
    m
  }
  m <- lagg_punkter(m, satelliter, farg_satellit, 3, FALSE)
  m <- lagg_punkter(m, la, farg_la, 6, TRUE)
  m <- lagg_punkter(m, sekundar, farg_sekundar, 7, TRUE)

  m |>
    leaflet::addControl(html = as.character(rubrikruta), position = "topright", className = "kraftfalt-ruta") |>
    leaflet::addControl(html = as.character(forklaring), position = "bottomright", className = "kraftfalt-ruta") |>
    leaflet::addControl(html = as.character(kalla), position = "bottomleft", className = "kraftfalt-ruta") |>
    htmlwidgets::prependContent(htmltools::tags$style(".kraftfalt-ruta { background: transparent !important; box-shadow: none !important; }"))
}

# Lager med olika kolumner (la och solitary har samma, men skydda mot avvikelser) läggs ihop på
# gemensamma kolumner. NULL-lager hoppas över.
rbind_sf_kolumner <- function(...) {
  lager <- Filter(function(x) !is.null(x) && nrow(x) > 0, list(...))
  if (length(lager) == 0) return(NULL)
  gemensamma <- Reduce(intersect, lapply(lager, names))
  do.call(rbind, lapply(lager, function(x) x[, gemensamma]))
}

intern_kraftfalt_popup <- function(namn, andel_pendlare) {
  ifelse(is.na(andel_pendlare), as.character(namn),
         paste0("<b>", namn, "</b><br>", round(as.numeric(andel_pendlare)), "% av de sysselsatta pendlar till annan tätort"))
}
