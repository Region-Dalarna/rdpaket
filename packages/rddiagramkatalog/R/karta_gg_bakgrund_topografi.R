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
#' @param visa_namn `TRUE` (standard) för ort- och vattennamn.
#' @param ortnamn Vilka ortnamn som visas: `"tatorter"` (standard, bara tätorterna: städer och större
#'   samhällen), `"orter"` (även större byar) eller `"inga"`. Vattennamn visas bara för större sjöar och
#'   vattendrag, ett namn per sjö eller vattendrag.
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
    visa_namn = TRUE,
    ortnamn = "tatorter",
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
  ortnamn <- match.arg(ortnamn, c("tatorter", "orter", "inga"))
  tid <- function(text, t0) message(text, " (", round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1), " s)")

  lager_namn <- c("vatten", "skog", "bebyggelse", paste0("vagar_", vagar), "jarnvag",
                  if (visa_namn) c(if (ortnamn != "inga") "ortnamn", "vattennamn"))

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
  tid("Hämtade lagren ur databasen", t0)

  t0 <- Sys.time()
  gg <- intern_rita_bakgrundskarta(lager, bbox, vagar = vagar, visa_namn = visa_namn,
                                   ortnamn = ortnamn, lan_gis = lan_gis)
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

# Ritar bakgrundskartan ur en lista med sf-lager (som rdgis::hamta_bakgrundskarta() ger). Skild från
# databashämtningen så att den går att testa och återanvända. Färger ur diagramfarger("rd_bakgrundskarta").
# Vägarna ritas med en ljusgrå kant under en vit linje, som i Positron.
#' @noRd
intern_rita_bakgrundskarta <- function(lager, bbox, vagar = c("stora", "mellan"), visa_namn = TRUE,
                                       ortnamn = "tatorter", lan_gis = NULL) {
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
    gg <- gg + ggplot2::geom_sf(data = lan_gis, fill = NA, colour = "#9A9A9A", linewidth = 0.3, linetype = "22")
  }

  if (visa_namn) {
    # urval: en rad per vatten (samma namn skrivs ut flera gånger längs älvar), störst först
    text_lager <- function(gg, namn, farg, kursiv, filter_fn, unika) {
      d <- lager[[namn]]
      if (is.null(d) || nrow(d) == 0) return(gg)
      d <- d[filter_fn(d), ]
      d <- d[order(-d$thojd), ]
      if (unika) d <- d[!duplicated(d$namn), ]
      if (nrow(d) == 0) return(gg)
      xy <- sf::st_coordinates(d)
      d$x <- xy[, 1]; d$y <- xy[, 2]
      gg + ggplot2::geom_text(data = sf::st_drop_geometry(d),
                              ggplot2::aes(x = x, y = y, label = namn, size = thojd),
                              colour = farg, fontface = if (kursiv) "italic" else "plain",
                              check_overlap = TRUE, show.legend = FALSE)
    }
    gg <- text_lager(gg, "vattennamn", f[["vattennamn"]], TRUE, function(d) d$thojd >= 20, TRUE)
    if (ortnamn != "inga") {
      gg <- text_lager(gg, "ortnamn", f[["ortnamn"]], FALSE, unika = FALSE,
                       filter_fn = function(d) if (ortnamn == "tatorter") {
                         d$detaljtyp == "BEBTÄTTX" & d$thojd >= 20
                       } else d$thojd >= 20)
    }
    gg <- gg + ggplot2::scale_size_continuous(range = c(1.8, 3.6))
  }

  gg +
    ggplot2::coord_sf(xlim = bbox[c("xmin", "xmax")], ylim = bbox[c("ymin", "ymax")], crs = 3006, expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = f[["land"]], colour = NA),
                   plot.background = ggplot2::element_rect(fill = f[["land"]], colour = NA))
}
