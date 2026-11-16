#' Karta: bakgrundskarta över Dalarna (topografi)
#'
#' Diskret grå bakgrundskarta i stil med CartoDB Positron, ritad som en statisk ggplot-karta i hög
#' upplösning ur Lantmäteriets topografi (`topografi_10`) i geodatabasen: vatten, skog, bebyggelse,
#' vägar, järnväg samt ort- och vattennamn. Lagren är förberedda (filtrerade och generaliserade) i
#' schemat `bakgrundskarta_dalarna` med `rdgis::skapa_bakgrundskarta_topografi()`, så att kartan ritas
#' snabbt. Funktionen kräver åtkomst till databasen och stoppar med ett tydligt meddelande om den
#' saknas eller om bakgrundskartan inte byggts. Går kartan att rita ovanpå (som `ggplot`-objekt) kan
#' den användas som underlag för andra kartor.
#'
#' Källa: Lantmäteriet, Topografi 10 (CC BY 4.0).
#'
#' @param region_vekt Ett eller flera län, som länskod. Standard `"20"` (Dalarnas län). En
#'   kommunkod ger länet kommunen ligger i.
#' @param vagar Vilka vägnivåer som ritas: `"stora"` (motortrafikleder, landsvägar), `"mellan"`
#'   (små landsvägar, huvudgator) och `"sma"` (lokalgator, småvägar). Standard `c("stora", "mellan")`;
#'   `NULL` ritar inga vägar.
#' @param etiketter Vilka etiketter som ritas, som en teckenvektor med ett eller flera "paket". Standard
#'   `NULL` = inga etiketter, så att kartan fungerar som neutral bakgrund. Etiketterna är små och ljusgrå.
#'   * `"storre_tatorter"`: den största tätorten (efter folkmängd) i varje kommun.
#'   * `"tatorter"`: städer och större tätorter (ett hundratal namn i länet).
#'   * `"orter"`: tätorterna plus större byar och bebyggelsenamn utanför tätort, t.ex. Arvet och Dalbyn.
#'   * `"storre_sjoar"`: de största sjöarna (högsta textrang hos Lantmäteriet), t.ex. Siljan, Runn och Amungen.
#'   * `"sjoar"`: större sjöar. Etiketten sitter i mitten av sjöns största öppna vattenyta, en per sjö.
#'   * `"vattendrag"`: större vattendrag, ett namn per vattendrag.
#' @param namn_storlek Faktor för etikettstorleken. Standard 1; 1,5 ger 50 % större text.
#' @param bredd_cm Bildens bredd i centimeter när kartan skrivs till fil. Höjden följer kartans form.
#' @param dpi Upplösning när kartan skrivs till fil. Standard 300.
#' @param utmapp Mapp där kartfilen sparas. Måste finnas om `skriv_diagramfil` är `TRUE`.
#' @param skriv_diagramfil TRUE om kartan ska skrivas som bildfil.
#' @param filformat `"png"` (standard) eller `"svg"`/`"pdf"`.
#' @param returnera_figur TRUE (standard) för att returnera kartan. Minst en av `skriv_diagramfil`
#'   och `returnera_figur` måste vara `TRUE`.
#'
#' @return Osynligt: en namngiven lista med ett `ggplot`-objekt; namnet är bildfilens namn utan ändelse.
#' @seealso [rdgis::hamta_bakgrundskarta()], [rdgis::skapa_bakgrundskarta_topografi()]
#' @export
karta_gg_bakgrund_topografi <- function(
    region_vekt = "20",
    vagar = c("stora", "mellan"),
    etiketter = NULL,
    namn_storlek = 1,
    bredd_cm = 20,
    dpi = 300,
    utmapp = rdverktyg::utskriftsmapp(),
    skriv_diagramfil = TRUE,
    filformat = "png",
    returnera_figur = TRUE
) {

  # ========================================== Allmän info ============================================
  # Bakgrundskarta ur geodatabasens schema bakgrundskarta_dalarna. Lagren läses med
  # rdgis::hamta_bakgrundskarta() och ritas av intern_rita_bakgrundskarta().
  # ========================================== Inställningar ============================================

  intern_kontrollera_utdata(skriv_diagramfil, returnera_figur, "skriv_diagramfil")
  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)
  vagar <- intern_valj_vagar(vagar)
  etiketter <- intern_valj_etiketter(etiketter)
  tid <- function(text, t0) message(text, " (", round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1), " s)")

  lager_namn <- c("vatten", "skog", "bebyggelse", paste0("vagar_", vagar), "jarnvag",
                  if (any(etiketter %in% c("tatorter", "orter"))) "ortnamn",
                  if ("vattendrag" %in% etiketter) "vattennamn",
                  if (any(etiketter %in% c("sjoar", "storre_sjoar"))) "sjoar")

  con <- rdpostgres::uppkoppling_db(db_name = "geodata", service_name = "rd_geodata")
  if (!is.null(con)) on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (is.null(con) || !rdgis::bakgrundskarta_finns(lager = lager_namn, con = con)) {
    stop("Bakgrundskartan går inte att hämta. Den kräver åtkomst till geodatabasen och att lagren byggts ",
         "med rdgis::skapa_bakgrundskarta_topografi().", call. = FALSE)
  }

  t0 <- Sys.time()
  lan_gis <- sf::st_transform(rdgis::hamta_karta("lan", unique(substr(region_vekt, 1, 2))), 3006)
  bbox <- sf::st_bbox(lan_gis)
  lager <- rdgis::hamta_bakgrundskarta(lager = lager_namn, bbox = bbox, marginal = 0.02, con = con)
  if ("storre_tatorter" %in% etiketter) {
    lager$storre_tatorter <- intern_storsta_tatort_per_kommun(unique(substr(region_vekt, 1, 2)))
  }
  tid("Hämtade lagren ur databasen", t0)

  t0 <- Sys.time()
  gg <- intern_rita_bakgrundskarta(lager, bbox, vagar = vagar, etiketter = etiketter,
                                   namn_storlek = namn_storlek, lan_gis = lan_gis)
  tid("Byggde kartan", t0)

  lan_namn <- paste(unique(rdverktyg::hamtaregion_kod_namn(unique(substr(region_vekt, 1, 2)))$region), collapse = "_")
  filnamn_bas <- paste0("bakgrundskarta_", lan_namn)

  if (skriv_diagramfil) {
    hojd_cm <- bredd_cm * as.numeric(diff(bbox[c("ymin", "ymax")]) / diff(bbox[c("xmin", "xmax")]))
    t0 <- Sys.time()
    fil <- file.path(utmapp, paste0(filnamn_bas, ".", filformat))
    ggplot2::ggsave(fil, gg, width = bredd_cm, height = hojd_cm, units = "cm", dpi = dpi)
    tid(paste0("Skrev kartan till ", fil), t0)
  }

  if (returnera_figur) invisible(stats::setNames(list(gg), filnamn_bas))
}

# Kontrollerar och tolkar vagar-parametern till de lagernamn-suffix som finns.
#' @noRd
intern_valj_vagar <- function(vagar) {
  if (is.null(vagar)) return(character(0))
  okanda <- setdiff(vagar, c("stora", "mellan", "sma"))
  if (length(okanda) > 0) {
    stop("vagar ska vara \"stora\", \"mellan\" och/eller \"sma\", inte: ", paste(okanda, collapse = ", "),
         call. = FALSE)
  }
  intersect(c("stora", "mellan", "sma"), vagar)
}

# Största tätorten (efter folkmängd, bef) i varje kommun i länen, som punkter med namn och rang.
#' @noRd
intern_storsta_tatort_per_kommun <- function(lanskoder) {
  t <- rdgis::hamta_karta("tatortspunkter", lanskoder)
  if (is.null(t) || nrow(t) == 0) return(NULL)
  t <- t[t$ar == max(t$ar), ]
  t <- t[order(-t$bef), ]
  t <- t[!duplicated(t$kommun), ]
  t <- sf::st_transform(t, 3006)
  sf::st_sf(namn = t$tatort, thojd = 30, geometry = sf::st_geometry(t))
}

# Kontrollerar etikettpaketen.
#' @noRd
intern_valj_etiketter <- function(etiketter) {
  if (is.null(etiketter)) return(character(0))
  giltiga <- c("storre_tatorter", "tatorter", "orter", "storre_sjoar", "sjoar", "vattendrag")
  okanda <- setdiff(etiketter, giltiga)
  if (length(okanda) > 0) {
    stop("etiketter ska vara ", paste0("\"", giltiga, "\"", collapse = ", "), ", inte: ",
         paste(okanda, collapse = ", "), call. = FALSE)
  }
  unique(etiketter)
}

# Tar bort hål i polygoner (t.ex. en stor sjö som är utklippt ur länet), så att bara ytterkanten ritas.
#' @noRd
intern_utan_hal <- function(sf_obj) {
  g <- sf::st_geometry(sf::st_union(sf_obj))[[1]]
  g <- if (inherits(g, "MULTIPOLYGON")) {
    sf::st_multipolygon(lapply(g, function(p) list(p[[1]])))
  } else {
    sf::st_polygon(list(g[[1]]))
  }
  sf::st_sfc(g, crs = sf::st_crs(sf_obj))
}

# Ritar bakgrundskartan ur en lista med sf-lager (som rdgis::hamta_bakgrundskarta() ger). Skild från
# databashämtningen så att den går att testa och återanvända. Färger ur diagramfarger("rd_bakgrundskarta").
# Vägarna ritas med en ljusgrå kant under en vit linje, som i Positron.
#' @noRd
intern_rita_bakgrundskarta <- function(lager, bbox, vagar = c("stora", "mellan"), etiketter = character(0),
                                       namn_storlek = 1, lan_gis = NULL) {
  f <- rddiagram::diagramfarger("rd_bakgrundskarta")
  gg <- ggplot2::ggplot()

  yta <- function(gg, namn, farg) {
    if (!is.null(lager[[namn]]) && nrow(lager[[namn]]) > 0) {
      gg <- gg + ggplot2::geom_sf(data = lager[[namn]], fill = farg, colour = NA)
    }
    gg
  }
  gg <- yta(gg, "skog", f[["skog"]])
  gg <- yta(gg, "bebyggelse", f[["bebyggelse"]])
  gg <- yta(gg, "vatten", f[["vatten"]])

  linje <- function(gg, namn, farg, bredd) {
    if (!is.null(lager[[namn]]) && nrow(lager[[namn]]) > 0) {
      gg <- gg + ggplot2::geom_sf(data = lager[[namn]], colour = farg, linewidth = bredd)
    }
    gg
  }
  # minsta först, så att de större vägarna ligger överst
  bredder <- c(sma = 0.15, mellan = 0.25, stora = 0.4)
  for (v in intersect(c("sma", "mellan", "stora"), vagar)) {
    gg <- linje(gg, paste0("vagar_", v), f[["vag_kant"]], bredder[[v]] + 0.15)
  }
  for (v in intersect(c("sma", "mellan", "stora"), vagar)) {
    gg <- linje(gg, paste0("vagar_", v), f[["vag"]], bredder[[v]])
  }
  gg <- linje(gg, "jarnvag", f[["jarnvag"]], 0.3)

  if (!is.null(lan_gis)) {
    gg <- gg + ggplot2::geom_sf(data = intern_utan_hal(lan_gis), fill = NA, colour = "#9A9A9A",
                                linewidth = 0.3, linetype = "22")
  }

  # Etikettpaket. Storleken (mm) följer textens rang (thojd i topografin: 20 eller 30); ett namn per
  # vatten eftersom samma namn skrivs ut flera gånger längs älvar.
  text_lager <- function(gg, lagernamn, urval, farg, kursiv, storlek, unika) {
    d <- lager[[lagernamn]]
    if (is.null(d) || nrow(d) == 0) return(gg)
    d <- d[urval(d), ]
    # störst rang först och, för vatten som består av flera ytor, störst yta först
    d <- d[if ("area_km2" %in% names(d)) order(-d$thojd, -d$area_km2) else order(-d$thojd), ]
    if (unika) d <- d[!duplicated(d$namn), ]
    if (nrow(d) == 0) return(gg)
    xy <- sf::st_coordinates(d)
    d$x <- xy[, 1]; d$y <- xy[, 2]
    d$storlek <- namn_storlek * ifelse(d$thojd >= 30, storlek[2], storlek[1])
    gg + ggplot2::geom_text(data = sf::st_drop_geometry(d),
                            ggplot2::aes(x = x, y = y, label = namn, size = storlek),
                            colour = farg, fontface = if (kursiv) "italic" else "plain",
                            check_overlap = TRUE, show.legend = FALSE)
  }
  if ("sjoar" %in% etiketter) {
    gg <- text_lager(gg, "sjoar", function(d) d$thojd >= 20, f[["vattennamn"]], TRUE, c(1.5, 1.9), TRUE)
  } else if ("storre_sjoar" %in% etiketter) {
    gg <- text_lager(gg, "sjoar", function(d) d$thojd >= 30, f[["vattennamn"]], TRUE, c(1.5, 1.9), TRUE)
  }
  if ("vattendrag" %in% etiketter) {
    gg <- text_lager(gg, "vattennamn", function(d) d$detaljtyp == "VATTDRTX" & d$thojd >= 20,
                     f[["vattennamn"]], TRUE, c(1.4, 1.4), TRUE)
  }
  if ("orter" %in% etiketter) {
    gg <- text_lager(gg, "ortnamn", function(d) d$detaljtyp %in% c("BEBTÄTTX", "BEBTX") & d$thojd >= 20,
                     f[["ortnamn"]], FALSE, c(1.5, 2.1), FALSE)
  } else if ("tatorter" %in% etiketter) {
    gg <- text_lager(gg, "ortnamn", function(d) d$detaljtyp == "BEBTÄTTX" & d$thojd >= 20,
                     f[["ortnamn"]], FALSE, c(1.5, 2.1), FALSE)
  } else if ("storre_tatorter" %in% etiketter) {
    gg <- text_lager(gg, "storre_tatorter", function(d) rep(TRUE, nrow(d)), f[["ortnamn"]], FALSE, c(2.1, 2.1), FALSE)
  }
  if (length(etiketter) > 0) gg <- gg + ggplot2::scale_size_identity()

  gg +
    ggplot2::coord_sf(xlim = bbox[c("xmin", "xmax")], ylim = bbox[c("ymin", "ymax")], crs = 3006, expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = f[["land"]], colour = NA),
                   plot.background = ggplot2::element_rect(fill = f[["land"]], colour = NA))
}
