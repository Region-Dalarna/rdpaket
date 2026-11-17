#' Karta: bakgrundskarta över Sverige eller flera län (topografi 1M)
#'
#' Diskret grå bakgrundskarta i stil med CartoDB Positron, ritad som en statisk ggplot-karta ur
#' Lantmäteriets Topografi 1M (skala 1:1 000 000) i geodatabasen: vatten, skog, fjäll, glaciärer,
#' bebyggelse, vägar, järnväg och vattendrag samt ort- och vattennamn. Passar för hela Sverige eller
#' flera län; för ett enskilt län ger [karta_gg_bakgrund_topografi()] (Topografi 10) mer detaljer.
#' Lagren är förberedda i schemat `bakgrundskarta_sverige` med
#' `rdgis::skapa_bakgrundskarta_topografi(kalla_schema = "topografi_1m")`. Funktionen kräver åtkomst
#' till databasen och stoppar med ett tydligt meddelande om den saknas eller om bakgrundskartan inte
#' byggts. Kartan kan användas som underlag för andra kartor (returneras som `ggplot`-objekt).
#'
#' Källa: Lantmäteriet (Topografi 1M).
#'
#' @param region_vekt Ett eller flera län, som länskod. `NULL` (standard) ger hela Sverige. En kommunkod
#'   ger länet kommunen ligger i.
#' @param vagar Vilka vägnivåer som ritas: `"stora"` (motorvägar, motortrafikleder, mötesfria vägar och
#'   landsvägar), `"mellan"` (små landsvägar) och `"sma"`. Standard `"stora"`, eftersom de mindre vägarna
#'   gör en riksöversikt stökig. `NULL` ritar inga vägar.
#' @param vattendrag `TRUE` (standard) ritar vattendrag som tunna linjer.
#' @param utland Hur områden utanför Sverige ritas: `"grann"` (standard) en något mörkare grå ton, så
#'   att Sverige syns, `"vatten"` i vattenfärg (grannländerna blir då som hav) eller `"inget"` i samma
#'   färg som Sverige.
#' @param kalla_text Källhänvisning nere till höger. Standard två rader: bakgrundskartans källa
#'   (Lantmäteriet, Topografi 1M) och "Bearbetning: Samhällsanalys, Region Dalarna". `NULL`, `NA` eller
#'   `""` ger ingen text.
#' @param marginal Marginal runt området som andel av kartans bredd och höjd. Standard 0,03.
#' @param lansgrans `TRUE` (standard) ritar länsgränserna (Lantmäteriets `lan_lm`) som tunna grå linjer.
#' @param etiketter Vilka etiketter som ritas, som en teckenvektor med ett eller flera "paket". Standard
#'   `NULL` = inga etiketter.
#'   * `"storstader"`: de största städerna (21 st.).
#'   * `"stader"`: städer, ca 125 namn.
#'   * `"tatorter"`: större tätorter, ca 450 namn.
#'   * `"orter"`: alla orter, även mindre tätorter och bebyggelse.
#'   * `"storsta_per_lan"`: den största tätorten (efter folkmängd) i varje län i kartan.
#'   * `"storre_vatten"`: de största sjöarna och havsområdena (Vänern, Vättern, Mälaren, Östersjön ...).
#'   * `"vatten"`: större sjöar och havsområden.
#'   * `"terrang"`: terrängnamn (öar, fjäll).
#' @param namn_storlek Faktor för etikettstorleken. Standard 1.
#' @param bredd_cm Bildens bredd i centimeter när kartan skrivs till fil. Höjden följer kartans form.
#' @param dpi Upplösning när kartan skrivs till fil. Standard 300.
#' @param utmapp Mapp där kartfilen sparas. Måste finnas om `skriv_diagramfil` är `TRUE`.
#' @param skriv_diagramfil TRUE om kartan ska skrivas som bildfil.
#' @param filformat `"png"` (standard) eller `"svg"`/`"pdf"`.
#' @param returnera_figur TRUE (standard) för att returnera kartan. Minst en av `skriv_diagramfil`
#'   och `returnera_figur` måste vara `TRUE`.
#'
#' @return Osynligt: en namngiven lista med ett `ggplot`-objekt; namnet är bildfilens namn utan ändelse.
#' @seealso [karta_gg_bakgrund_topografi()], [rdgis::hamta_bakgrundskarta()],
#'   [rdgis::skapa_bakgrundskarta_topografi()]
#' @export
karta_gg_bakgrund_sverige <- function(
    region_vekt = NULL,
    vagar = "stora",
    vattendrag = TRUE,
    utland = "grann",
    kalla_text = "Bakgrundskarta: Lantmäteriet, Topografi 1M\nBearbetning: Samhällsanalys, Region Dalarna",
    marginal = 0.03,
    lansgrans = TRUE,
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
  # Bakgrundskarta ur geodatabasens schema bakgrundskarta_sverige (byggt ur topografi_1m). Lagren läses med
  # rdgis::hamta_bakgrundskarta() och ritas av intern_rita_bakgrund_sverige().
  # ========================================== Inställningar ============================================

  intern_kontrollera_utdata(skriv_diagramfil, returnera_figur, "skriv_diagramfil")
  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  b <- intern_bygg_bakgrund_sverige(region_vekt, vagar = vagar, vattendrag = vattendrag, utland = utland,
                                    etiketter = etiketter, namn_storlek = namn_storlek, kalla_text = kalla_text,
                                    marginal = marginal, lansgrans = lansgrans)
  gg <- b$gg
  bbox <- b$bbox

  filnamn_bas <- if (is.null(region_vekt)) {
    "bakgrundskarta_sverige"
  } else {
    paste0("bakgrundskarta_", paste(unique(rdverktyg::hamtaregion_kod_namn(unique(substr(region_vekt, 1, 2)))$region),
                                    collapse = "_"))
  }

  if (skriv_diagramfil) {
    hojd_cm <- bredd_cm * as.numeric(diff(bbox[c("ymin", "ymax")]) / diff(bbox[c("xmin", "xmax")]))
    fil <- file.path(utmapp, paste0(filnamn_bas, ".", filformat))
    ggplot2::ggsave(fil, gg, width = bredd_cm, height = hojd_cm, units = "cm", dpi = dpi)
    message("Skrev kartan till ", fil)
  }

  if (returnera_figur) invisible(stats::setNames(list(gg), filnamn_bas))
}

# Kontrollerar parametrarna, hämtar lagren ur databasen och ritar kartan. Returnerar ggplot-objektet, kartans
# utsnitt (bbox i SWEREF 99 TM) och länsgränserna (lan_lm).
#' @noRd
intern_bygg_bakgrund_sverige <- function(region_vekt, vagar, vattendrag, utland, etiketter, namn_storlek,
                                         kalla_text, marginal, lansgrans) {
  vagar <- intern_valj_vagar(vagar)
  etiketter <- intern_valj_etiketter_sverige(etiketter)
  utland <- match.arg(utland, c("grann", "vatten", "inget"))
  tid <- function(text, t0) message(text, " (", round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1), " s)")

  lager_namn <- c("utland", "vatten", "skog", "fjall", "glaciar", "bebyggelse", "jarnvag",
                  paste0("vagar_", vagar),
                  if (vattendrag) "vattendrag",
                  if (any(etiketter %in% c("storstader", "stader", "tatorter", "orter"))) "ortnamn",
                  if (any(etiketter %in% c("storre_vatten", "vatten"))) "vattennamn",
                  if ("terrang" %in% etiketter) "terrangnamn")

  con <- rdpostgres::uppkoppling_db(db_name = "geodata", service_name = "rd_geodata")
  if (!is.null(con)) on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (is.null(con) || !rdgis::bakgrundskarta_finns(schema = "bakgrundskarta_sverige", lager = lager_namn, con = con)) {
    stop("Bakgrundskartan går inte att hämta. Den kräver åtkomst till geodatabasen och att lagren byggts med ",
         "rdgis::skapa_bakgrundskarta_topografi(kalla_schema = \"topografi_1m\").", call. = FALSE)
  }

  t0 <- Sys.time()
  lan_gis <- sf::st_transform(
    rdgis::hamta_karta("lan_lm", if (is.null(region_vekt)) NA else unique(substr(region_vekt, 1, 2))), 3006)
  bbox <- sf::st_bbox(lan_gis)
  bredd <- bbox[["xmax"]] - bbox[["xmin"]]; hojd <- bbox[["ymax"]] - bbox[["ymin"]]
  bbox <- sf::st_bbox(c(xmin = bbox[["xmin"]] - marginal * bredd, ymin = bbox[["ymin"]] - marginal * hojd,
                        xmax = bbox[["xmax"]] + marginal * bredd, ymax = bbox[["ymax"]] + marginal * hojd),
                      crs = sf::st_crs(3006))
  lager <- rdgis::hamta_bakgrundskarta(lager = lager_namn, bbox = bbox, marginal = 0,
                                       schema = "bakgrundskarta_sverige", con = con)
  if ("storsta_per_lan" %in% etiketter) lager$storsta_per_lan <- intern_storsta_tatort_per_lan(lan_gis)
  tid("Hämtade lagren ur databasen", t0)

  t0 <- Sys.time()
  # länsgränserna förenklas efter kartans storlek (ca 1/2000 av bredden, 300 m för hela Sverige)
  gg <- intern_rita_bakgrund_sverige(
    lager, bbox, vagar = vagar, utland = utland, etiketter = etiketter, namn_storlek = namn_storlek,
    kalla_text = kalla_text,
    lan_gis = if (lansgrans) sf::st_simplify(lan_gis, preserveTopology = TRUE, dTolerance = bredd / 2000))
  tid("Byggde kartan", t0)
  list(gg = gg, bbox = bbox, lan_gis = lan_gis)
}

# Kontrollerar etikettpaketen för Topografi 1M.
#' @noRd
intern_valj_etiketter_sverige <- function(etiketter) {
  if (is.null(etiketter)) return(character(0))
  giltiga <- c("storstader", "stader", "tatorter", "orter", "storsta_per_lan", "storre_vatten", "vatten", "terrang")
  okanda <- setdiff(etiketter, giltiga)
  if (length(okanda) > 0) {
    stop("etiketter ska vara ", paste0("\"", giltiga, "\"", collapse = ", "), ", inte: ",
         paste(okanda, collapse = ", "), call. = FALSE)
  }
  unique(etiketter)
}

# Största tätorten (efter folkmängd, bef) i varje län i lan_gis, som punkter med namn och rang.
#' @noRd
intern_storsta_tatort_per_lan <- function(lan_gis) {
  t <- rdgis::hamta_karta("tatortspunkter", NA)
  if (is.null(t) || nrow(t) == 0) return(NULL)
  t <- sf::st_transform(t[t$ar == max(t$ar), ], 3006)
  traff <- sf::st_intersects(lan_gis, t)
  rader <- unique(unlist(lapply(traff, function(i) if (length(i) > 0) i[which.max(t$bef[i])])))
  t <- t[rader, ]
  sf::st_sf(namn = t$tatort, klass = 7L, geometry = sf::st_geometry(t))
}

# Ritar bakgrundskartan ur en lista med sf-lager (som rdgis::hamta_bakgrundskarta() ger). Skild från
# databashämtningen så att den går att testa. Basfärgerna kommer ur diagramfarger("rd_bakgrundskarta");
# utland, fjäll och glaciär har egna toner här. Landets yta är kartans bakgrund (Topografi 1M har ingen
# markyta för "öppen mark").
#' @noRd
intern_rita_bakgrund_sverige <- function(lager, bbox, vagar = "stora", utland = "grann", etiketter = character(0),
                                         namn_storlek = 1, kalla_text = NULL, lan_gis = NULL) {
  f <- rddiagram::diagramfarger("rd_bakgrundskarta")
  farg_utland <- switch(utland, grann = "#E2E2E0", vatten = f[["vatten"]], inget = f[["land"]])
  gg <- ggplot2::ggplot()

  yta <- function(gg, namn, farg) {
    if (!is.null(lager[[namn]]) && nrow(lager[[namn]]) > 0) {
      gg <- gg + ggplot2::geom_sf(data = lager[[namn]], fill = farg, colour = NA)
    }
    gg
  }
  linje <- function(gg, namn, farg, bredd) {
    if (!is.null(lager[[namn]]) && nrow(lager[[namn]]) > 0) {
      gg <- gg + ggplot2::geom_sf(data = lager[[namn]], colour = farg, linewidth = bredd)
    }
    gg
  }

  gg <- yta(gg, "utland", farg_utland)
  gg <- yta(gg, "fjall", "#F4F4F2")
  gg <- yta(gg, "skog", f[["skog"]])
  gg <- yta(gg, "glaciar", "#FFFFFF")
  gg <- yta(gg, "bebyggelse", f[["bebyggelse"]])
  gg <- yta(gg, "vatten", f[["vatten"]])
  gg <- linje(gg, "vattendrag", f[["vatten"]], 0.15)

  # minsta först, så att de större vägarna ligger överst; ljusgrå kant under vit linje, som i Positron
  bredder <- c(sma = 0.1, mellan = 0.15, stora = 0.3)
  for (v in intersect(c("sma", "mellan", "stora"), vagar)) {
    gg <- linje(gg, paste0("vagar_", v), f[["vag_kant"]], bredder[[v]] + 0.1)
  }
  for (v in intersect(c("sma", "mellan", "stora"), vagar)) {
    gg <- linje(gg, paste0("vagar_", v), f[["vag"]], bredder[[v]])
  }
  gg <- linje(gg, "jarnvag", f[["jarnvag"]], 0.2)

  if (!is.null(lan_gis)) {
    gg <- gg + ggplot2::geom_sf(data = sf::st_cast(sf::st_geometry(lan_gis), "MULTILINESTRING"), colour = "#9A9A9A",
                                linewidth = 0.2)
  }

  # Etikettpaket. Storleken (mm) följer textens klass (textstorleksklass i topografin: 2, 3, 5, 7, 10).
  storlek_klass <- function(klass) {
    ifelse(klass >= 10, 3.0, ifelse(klass >= 7, 2.6, ifelse(klass >= 5, 2.2, ifelse(klass >= 3, 1.8, 1.5))))
  }
  text_lager <- function(gg, lagernamn, urval, farg, kursiv) {
    d <- lager[[lagernamn]]
    if (is.null(d) || nrow(d) == 0) return(gg)
    d <- d[urval(d), ]
    if (nrow(d) == 0) return(gg)
    d <- d[order(-d$klass), ]   # störst först, så att de större namnen går före vid överlapp
    xy <- sf::st_coordinates(d)
    d$x <- xy[, 1]; d$y <- xy[, 2]
    d$storlek <- namn_storlek * storlek_klass(d$klass)
    gg + ggplot2::geom_text(data = sf::st_drop_geometry(d),
                            ggplot2::aes(x = x, y = y, label = namn, size = storlek),
                            colour = farg, fontface = if (kursiv) "italic" else "plain",
                            check_overlap = TRUE, show.legend = FALSE)
  }
  tatort <- function(d) d$kategori == "Tätort"
  if ("orter" %in% etiketter) {
    gg <- text_lager(gg, "ortnamn", function(d) rep(TRUE, nrow(d)), f[["ortnamn"]], FALSE)
  } else if ("tatorter" %in% etiketter) {
    gg <- text_lager(gg, "ortnamn", function(d) tatort(d) & d$klass >= 3, f[["ortnamn"]], FALSE)
  } else if ("stader" %in% etiketter) {
    gg <- text_lager(gg, "ortnamn", function(d) tatort(d) & d$klass >= 5, f[["ortnamn"]], FALSE)
  } else if ("storstader" %in% etiketter) {
    gg <- text_lager(gg, "ortnamn", function(d) tatort(d) & d$klass >= 7, f[["ortnamn"]], FALSE)
  }
  if ("storsta_per_lan" %in% etiketter) {
    gg <- text_lager(gg, "storsta_per_lan", function(d) rep(TRUE, nrow(d)), f[["ortnamn"]], FALSE)
  }
  if ("vatten" %in% etiketter) {
    gg <- text_lager(gg, "vattennamn", function(d) d$klass >= 3, f[["vattennamn"]], TRUE)
  } else if ("storre_vatten" %in% etiketter) {
    gg <- text_lager(gg, "vattennamn", function(d) d$klass >= 5, f[["vattennamn"]], TRUE)
  }
  if ("terrang" %in% etiketter) {
    gg <- text_lager(gg, "terrangnamn", function(d) rep(TRUE, nrow(d)), f[["vattennamn"]], FALSE)
  }
  if (length(etiketter) > 0) gg <- gg + ggplot2::scale_size_identity()

  # källhänvisning nere till höger, strax innanför kartkanten
  if (!is.null(kalla_text) && length(kalla_text) == 1 && !is.na(kalla_text) && nzchar(kalla_text)) {
    gg <- gg + ggplot2::annotate("text", x = bbox[["xmax"]] - 0.008 * (bbox[["xmax"]] - bbox[["xmin"]]),
                                 y = bbox[["ymin"]] + 0.006 * (bbox[["ymax"]] - bbox[["ymin"]]),
                                 label = kalla_text, hjust = 1, vjust = 0, size = 2.1, lineheight = 1.1,
                                 colour = f[["ortnamn"]])
  }

  gg +
    ggplot2::coord_sf(xlim = bbox[c("xmin", "xmax")], ylim = bbox[c("ymin", "ymax")], crs = 3006, expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0),
                   panel.background = ggplot2::element_rect(fill = f[["land"]], colour = NA),
                   plot.background = ggplot2::element_rect(fill = f[["land"]], colour = NA))
}
