#' Karta: kraftfält kring lokala arbetsmarknadscentrum (ggplot)
#'
#' Statisk karta (ggplot) i hög upplösning över tätorters kraftfält, byggd på pendlingen mellan tätorter.
#' Tätorter där ingen enskild pendling är större än `enskilt_troskelvarde` procent och där högst
#' `totalt_troskelvarde` procent pendlar ut totalt är lokala arbetsmarknadscentrum (LA). Tätorter med spridd
#' utpendling över tröskelvärdet (solitära) markeras på samma sätt men har inget kraftfält. Tätorter där mer
#' än `enskilt_troskelvarde` procent pendlar till en annan tätort är lokala arbetsmarknadssatelliter och tillhör
#' kraftfältet för det centrum de pendlar mest till; fältet ritas som en zon kring vägen dit. Beräkningen görs
#' av [rdgeorouting::pendling_kraftfalt()] mot vägnätet i geodatabasen.
#'
#' Bakgrunden är den grå topografiska bakgrundskartan i [karta_gg_bakgrund_topografi()] med namn på de största
#' sjöarna. Kartan har en textruta uppe till höger, en teckenförklaring nere till vänster och källhänvisning nere
#' till höger. Textrutan och teckenförklaringen är anpassade för Dalarnas form; för andra län kan de hamna
#' ovanpå kartan. Leaflet-versionen finns i [karta_lf_kraftfalt_scb()].
#'
#' Kräver åtkomst till geodatabasen och att bakgrundskartan byggts med
#' `rdgis::skapa_bakgrundskarta_topografi()`.
#'
#' @inheritParams karta_lf_kraftfalt_scb
#' @inheritParams karta_gg_bakgrund_topografi
#' @param pendlingsar Vilket år pendlingsdata gäller. Visas i källhänvisningen och filnamnet. Standard 2022.
#' @param kartrubrik Rubriken i textrutan. Standard `"Kraftfält"`.
#' @param forklaringstext Texten i textrutan som ett eller flera stycken (en teckenvektor). `NULL` (standard) = en
#'   standardtext som anger tröskelvärdena.
#' @param tatortsnamn `TRUE` (standard) skriver tätortens namn vid lokala arbetsmarknadscentrum och solitära, i liten
#'   mörkgrå text som undviker att hamna ovanpå punkterna och varandra. `FALSE` ger inga namn.
#' @param namn_storlek Faktor för namnens textstorlek. Standard 1; 1,3 ger 30 % större text.
#' @param kalla_text Källhänvisning nere till höger. `NULL` (standard) = bakgrundskartans källa, pendlingsdatats
#'   år och "Bearbetning: Samhällsanalys, Region Dalarna" på två rader. `NA` eller `""` ger ingen text.
#' @param bredd_cm Bildens bredd i centimeter. Textrutan och teckenförklaringen dimensioneras efter den, så ändra
#'   storlek med denna parameter och inte genom att skala bilden.
#'
#' @return Osynligt: en namngiven lista med ett `ggplot`-objekt; namnet är bildfilens namn utan ändelse.
#' @seealso [karta_lf_kraftfalt_scb()], [karta_gg_bakgrund_topografi()]
#' @export
karta_gg_kraftfalt_scb <- function(
    tabell_pend_relation = NULL,
    kraftfalt = NULL,
    pendlingsar = 2022,
    region_vekt = "20",
    enskilt_troskelvarde = 20,
    totalt_troskelvarde = 45,
    primar_la_buffer = 2000,
    sekundar_la_buffer = 1000,
    gemensam_la_buffer = 5000,
    con_sekretess = NA,
    kartrubrik = "Kraftfält",
    forklaringstext = NULL,
    tatortsnamn = TRUE,
    namn_storlek = 1,
    kalla_text = NULL,
    vagar = c("stora", "mellan"),
    marginal = 0.03,
    bredd_cm = 20,
    dpi = 300,
    ...,
    utmapp = rdverktyg::utskriftsmapp(),
    skriv_diagramfil = TRUE,
    filformat = "png",
    returnera_figur = TRUE
) {

  # ========================================== Inställningar ============================================

  # ... går vidare till rdgeorouting::pendling_kraftfalt() och skulle annars tyst svälja det gamla namnet
  if ("vald_lan" %in% names(list(...))) {
    stop("Parametern vald_lan heter numera region_vekt.", call. = FALSE)
  }
  intern_kontrollera_utdata(skriv_diagramfil, returnera_figur, "skriv_diagramfil")
  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)

  # ========================================== Hämta och beräkna ========================================

  if (is.null(kraftfalt)) {
    if (is.null(tabell_pend_relation)) tabell_pend_relation <- intern_hamta_pendling_tatorter(con_sekretess)
    kraftfalt <- rdgeorouting::pendling_kraftfalt(
      tabell_pend_relation,
      enskilt_troskelvarde = enskilt_troskelvarde, totalt_troskelvarde = totalt_troskelvarde,
      primar_la_buffer = primar_la_buffer, sekundar_la_buffer = sekundar_la_buffer,
      gemensam_la_buffer = gemensam_la_buffer, ...)
  }

  if (is.null(kalla_text)) {
    kalla_text <- paste0("Bakgrundskarta: Lantmäteriet, Topografi 10\n",
                         "Pendlingsdata: SCB ", pendlingsar, ". Bearbetning: Samhällsanalys, Region Dalarna")
  }
  b <- intern_bygg_bakgrundskarta(region_vekt, vagar = vagar, etiketter = "storre_sjoar", namn_storlek = 1,
                                  kalla_text = kalla_text, marginal = marginal, lansgrans = FALSE)

  gg <- intern_lagg_kraftfalt_pa_karta(
    b$gg, kraftfalt, b$bbox, b$lan_gis, kartrubrik = kartrubrik,
    forklaringstext = if (is.null(forklaringstext)) {
      intern_kraftfalt_forklaringstext(enskilt_troskelvarde, totalt_troskelvarde)
    } else forklaringstext,
    bredd_cm = bredd_cm, tatortsnamn = tatortsnamn, namn_storlek = namn_storlek)

  lansnamn <- rdverktyg::hamtaregion_kod_namn(region_vekt)$region[1]
  filnamn_bas <- paste0("kraftfalt_", gsub("[^a-z0-9]+", "_", tolower(stringi::stri_trans_general(lansnamn, "Latin-ASCII"))),
                        "_ar", pendlingsar)

  if (skriv_diagramfil) {
    hojd_cm <- bredd_cm * as.numeric(diff(b$bbox[c("ymin", "ymax")]) / diff(b$bbox[c("xmin", "xmax")]))
    fil <- file.path(utmapp, paste0(filnamn_bas, ".", filformat))
    ggplot2::ggsave(fil, gg, width = bredd_cm, height = hojd_cm, units = "cm", dpi = dpi)
    message("Skrev kartan till ", fil)
  }

  if (returnera_figur) invisible(stats::setNames(list(gg), filnamn_bas))
}

# Standardtexten i textrutan, med tröskelvärdena inskrivna.
#' @noRd
intern_kraftfalt_forklaringstext <- function(enskilt_troskelvarde, totalt_troskelvarde) {
  c(paste0("Tätorter där ingen enskild pendling är större än ", enskilt_troskelvarde,
           " % av de sysselsatta, och där högst ", totalt_troskelvarde,
           " % av de sysselsatta pendlar ut totalt, kategoriseras som lokala arbetsmarknadscentrum. Tätorter med spridd ",
           "utpendling över ", totalt_troskelvarde,
           " % (solitära) markeras på samma sätt men har inget kraftfält."),
    paste0("Varje tätort där mer än ", enskilt_troskelvarde,
           " % av de sysselsatta pendlar till en annan tätort är en lokal arbetsmarknadssatellit. ",
           "Den tillhör kraftfältet för det lokala arbetsmarknadscentrum som den pendlar mest till, ",
           "och fältet ritas som en zon kring vägen dit."),
    paste0("Pendlar en satellit mer än ", enskilt_troskelvarde,
           " % till två centrum bildas även ett smalare, sekundärt kraftfält kring vägen till det centrum ",
           "dit pendlingen är näst störst."))
}

# Lägger kraftfältens fält, tätortspunkter, länsgräns, textruta och teckenförklaring ovanpå bakgrundskartan.
# Skild från databashämtningen så att den går att köra och testa utan databas. Mått i mm (textstorlek,
# ruta och teckenförklaring) räknas om till kartans enheter utifrån bildens bredd (bredd_cm).
#' @noRd
intern_lagg_kraftfalt_pa_karta <- function(gg, kraftfalt, bbox, lan_gis, kartrubrik, forklaringstext, bredd_cm = 20,
                                            tatortsnamn = TRUE, namn_storlek = 1) {
  f <- rddiagram::diagramfarger("rd_kraftfaltskarta")
  till_3006 <- function(x) if (is.null(x) || nrow(x) == 0) NULL else sf::st_transform(x, 3006)
  # tätorternas geometri kan vara polygoner - punkterna ritas i en punkt innanför
  till_punkter <- function(x) {
    if (is.null(x) || nrow(x) == 0) return(NULL)
    x <- suppressWarnings(sf::st_set_geometry(x, sf::st_point_on_surface(sf::st_geometry(x))))
    sf::st_transform(x, 3006)
  }

  falt <- function(gg, lager, farg) {
    x <- till_3006(lager)
    if (is.null(x)) return(gg)
    gg + ggplot2::geom_sf(data = x, fill = farg, colour = NA, alpha = 0.7)
  }
  gg <- falt(gg, kraftfalt$primla_areas, f[["falt"]])
  gg <- falt(gg, kraftfalt$secla_areas, f[["sekundart_falt"]])
  gg <- falt(gg, kraftfalt$commonla_areas, f[["sekundart_falt"]])

  gg <- gg + ggplot2::geom_sf(data = intern_utan_hal(sf::st_simplify(lan_gis, preserveTopology = TRUE, dTolerance = 50)),
                              fill = NA, colour = f[["lansgrans"]], linewidth = 0.5)

  punkter <- function(gg, lager, fyllning, kant, storlek) {
    x <- till_punkter(lager)
    if (is.null(x)) return(gg)
    gg + ggplot2::geom_sf(data = x, shape = 21, fill = fyllning, colour = kant, size = storlek, stroke = 0.4)
  }
  gg <- punkter(gg, kraftfalt$satellites, f[["satellit"]], f[["satellit_kant"]], 1.6)
  gg <- punkter(gg, kraftfalt$solitary, f[["solitar"]], f[["solitar_kant"]], 1.9)
  gg <- punkter(gg, rbind_sf_kolumner(kraftfalt$la), f[["centrum"]], f[["centrum_kant"]], 2.4)

  # tätortsnamn vid centrum och solitärer: små, mörkgrå, med en vit kontur, och med ggrepel så att de inte
  # hamnar ovanpå punkterna eller varandra
  if (tatortsnamn) {
    nam <- till_punkter(rbind_sf_kolumner(kraftfalt$la, kraftfalt$solitary))
    if (!is.null(nam) && "tobeteckn" %in% names(nam)) {
      xy <- sf::st_coordinates(nam)
      nam <- data.frame(x = xy[, 1], y = xy[, 2], namn = as.character(nam$tobeteckn))
      gg <- gg + ggrepel::geom_text_repel(
        data = nam, ggplot2::aes(x = x, y = y, label = namn), size = 2.3 * namn_storlek, colour = "#4A4A4A",
        bg.color = "white", bg.r = 0.12, box.padding = 0.25, point.padding = 0.3, min.segment.length = Inf,
        max.overlaps = Inf, seed = 1)
    }
  }

  # geom_sf-lagren ovan ersätter bakgrundens utsnitt med ett nytt (med extra luft runt kartan), så sätt det igen
  gg <- suppressMessages(gg + ggplot2::coord_sf(xlim = bbox[c("xmin", "xmax")], ylim = bbox[c("ymin", "ymax")],
                                                crs = 3006, expand = FALSE))

  # ===== mått: 1 mm i bilden = upm kartenheter (meter) =====
  w <- bbox[["xmax"]] - bbox[["xmin"]]
  upm <- w / (bredd_cm * 10)

  # ===== textruta uppe till höger =====
  textstorlek <- 2.4
  ruta_mm <- 0.40 * bredd_cm * 10
  tecken_per_rad <- floor((ruta_mm - 10) / (textstorlek * 2.845 * 0.3528 * 0.48))
  stycken <- vapply(forklaringstext, function(x) paste(strwrap(x, width = tecken_per_rad), collapse = "\n"), "")
  brodtext <- paste(stycken, collapse = "\n\n")
  rader <- length(strsplit(brodtext, "\n", fixed = TRUE)[[1]])  # blankraderna mellan stycken räknas med
  rad_mm <- textstorlek * 1.33 * 1.1  # radavstånd (lineheight 1.1)
  rubrik_mm <- 10
  ruta_hojd <- (rubrik_mm + rader * rad_mm + 3) * upm
  x1 <- bbox[["xmax"]]; x0 <- x1 - ruta_mm * upm
  y1 <- bbox[["ymax"]]; y0 <- y1 - ruta_hojd
  gg <- gg +
    ggplot2::annotate("rect", xmin = x0, xmax = x1, ymin = y0, ymax = y1, fill = "white", colour = NA) +
    ggplot2::annotate("text", x = (x0 + x1) / 2, y = y1 - 3 * upm, label = kartrubrik, family = "serif",
                      fontface = "bold", size = 5, vjust = 1, hjust = 0.5) +
    ggplot2::annotate("text", x = x0 + 4 * upm, y = y1 - rubrik_mm * upm, label = brodtext, family = "serif",
                      size = textstorlek, lineheight = 1.1, vjust = 1, hjust = 0)

  # ===== teckenförklaring nere till vänster =====
  poster <- list(
    list("Lokalt arbetsmarknadscentrum", "punkt", f[["centrum"]], f[["centrum_kant"]], 2.4),
    list("Solitär", "punkt", f[["solitar"]], f[["solitar_kant"]], 1.9),
    list("Lokal arbetsmarknadssatellit", "punkt", f[["satellit"]], f[["satellit_kant"]], 1.6),
    list("Kraftfält", "ruta", f[["falt"]], NA, NA),
    list("Sekundärt kraftfält", "ruta", f[["sekundart_falt"]], NA, NA),
    list("Länsgräns", "linje", "white", f[["lansgrans"]], NA))
  legend_mm <- 0.34 * bredd_cm * 10
  rh <- 5.5; pad <- 3
  lx0 <- bbox[["xmin"]]; ly0 <- bbox[["ymin"]]
  gg <- gg + ggplot2::annotate("rect", xmin = lx0, xmax = lx0 + legend_mm * upm,
                               ymin = ly0, ymax = ly0 + (length(poster) * rh + 2 * pad) * upm,
                               fill = "white", colour = NA)
  for (i in seq_along(poster)) {
    p <- poster[[i]]
    yi <- ly0 + (pad + (length(poster) - i + 0.5) * rh) * upm
    xs <- lx0 + (pad + 3) * upm
    gg <- gg + ggplot2::annotate("text", x = lx0 + (pad + 9) * upm, y = yi, label = p[[1]], hjust = 0,
                                 size = 2.9, vjust = 0.5)
    if (p[[2]] == "punkt") {
      gg <- gg + ggplot2::annotate("point", x = xs, y = yi, shape = 21, fill = p[[3]], colour = p[[4]],
                                   size = p[[5]], stroke = 0.4)
    } else if (p[[2]] == "ruta") {
      gg <- gg + ggplot2::annotate("rect", xmin = xs - 3.5 * upm, xmax = xs + 3.5 * upm, ymin = yi - 1.8 * upm,
                                   ymax = yi + 1.8 * upm, fill = p[[3]], alpha = 0.7, colour = NA)
    } else {
      gg <- gg + ggplot2::annotate("rect", xmin = xs - 3.5 * upm, xmax = xs + 3.5 * upm, ymin = yi - 1.8 * upm,
                                   ymax = yi + 1.8 * upm, fill = p[[3]], colour = p[[4]], linewidth = 0.5)
    }
  }
  gg
}
