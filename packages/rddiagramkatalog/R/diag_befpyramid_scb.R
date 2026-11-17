#' Diagram: befolkningspyramid
#'
#' Befolkningspyramider (kvinnor till vänster, män till höger) för en eller flera regioner och år, med utfall eller
#' prognos ur SCB:s befolkningsstatistik, hämtat med `pxweb2r`. Funktionen ger en pyramid per region och år, som en
#' namngiven lista, och kan skriva dem till bildfiler och sätta ihop dem till en animerad gif.
#'
#' Jämförelse (`jmfr_linje`) ritas som en linje ovanpå staplarna:
#' * `"ingen"` (standard): antal invånare, utan jämförelse.
#' * `"region"`: åldersgruppernas andel av respektive kön, jämfört med regionen i `jmfr_region`.
#' * `"utr_inr"`: inrikes födda (staplar) jämfört med utrikes födda (linje), i andelar av respektive grupp.
#' * `"ar"`: antal invånare år `ar_vekt` (staplar) jämfört med `jmfr_ar` (linje).
#'
#' År till och med senaste utfallsår (2025) är utfall: TAB638 och TAB5557 (CKM, 2025) för totalbefolkningen och TAB4823
#' och TAB6645 (CKM, 2025) för inrikes och utrikes födda. Senare år är prognoser: SCB:s regionala befolkningsprognos
#' 2024 (TAB698, län och kommuner, till 2070) och SCB:s befolkningsprognos för riket (TAB5974, till 2120). För inrikes
#' och utrikes födda per region finns bara SCB:s prognos 2022 (TAB6302, till 2070), som är föråldrad. Källtexten i
#' diagrammet anger alltid när ett år är en prognos och vilken.
#'
#' @param region_vekt En eller flera regioner: kommunkod, länskod eller `"00"` för riket. Standard Dalarna.
#' @param ar_vekt År som pyramiderna ska göras för, t.ex. `c(2025, 2040)`. `NA` (standard) ger senaste utfallsår.
#' @param jmfr_linje `"ingen"` (standard), `"region"`, `"utr_inr"` eller `"ar"`, se ovan.
#' @param jmfr_region Region att jämföra med när `jmfr_linje = "region"`. Standard riket (`"00"`).
#' @param jmfr_ar Jämförelseår när `jmfr_linje = "ar"`.
#' @param aldersgrupper `TRUE` delar upp i åldersgrupper (0–4, 5–14 ... 95+ år), `FALSE` (standard) visar ettårsklasser.
#' @param grupp_namn Om flera regioner i `region_vekt` ska slås ihop till en pyramid anges här namnet på gruppen.
#' @param diag_fargvekt Färger för kvinnor och män, i den ordningen. Standard `rddiagram::diagramfarger("kon")`.
#' @param diagram_capt Diagrammets källtext. Texten kompletteras automatiskt med en rad om prognosen när ett år är
#'   en prognos.
#' @param visa_logga_i_diagram `TRUE` om logga ska visas i diagrammet.
#' @param logga_sokvag Sökväg till logga. `NA` använder standardloggan.
#' @param utmapp Mapp där diagram och gif sparas. Måste finnas om `skriv_diagramfil = TRUE`.
#' @param skriv_diagramfil `TRUE` om pyramiderna ska skrivas till fil.
#' @param skapa_gif `TRUE` skapar en animerad gif per region av alla år i `ar_vekt` (kräver minst två år, att
#'   `skriv_diagramfil = TRUE` och paketet `gifski`).
#' @param gif_langd_sek Total längd på gifen i sekunder. Ett år visas aldrig längre än en sekund.
#' @param returnera_data `TRUE` lägger data i `.GlobalEnv` som `befpyramid`.
#' @param returnera_figur `TRUE` (standard) returnerar pyramiderna.
#'
#' @return En namngiven lista med `ggplot`-objekt, en per region och år.
#' @export
diag_befpyramid_scb <- function(
    region_vekt = "20",
    ar_vekt = NA,
    jmfr_linje = c("ingen", "region", "utr_inr", "ar"),
    jmfr_region = "00",
    jmfr_ar = NA,
    aldersgrupper = FALSE,
    grupp_namn = NA,
    diag_fargvekt = rddiagram::diagramfarger("kon"),
    diagram_capt = "Källa: SCB\nBearbetning: Samhällsanalys, Region Dalarna",
    visa_logga_i_diagram = TRUE,
    logga_sokvag = NA,
    utmapp = rdverktyg::utskriftsmapp(),
    skriv_diagramfil = TRUE,
    skapa_gif = FALSE,
    gif_langd_sek = 10,
    returnera_data = FALSE,
    returnera_figur = TRUE
) {

  start <- Sys.time()

  # ========================================== Inställningar ============================================

  jmfr_linje <- match.arg(jmfr_linje)
  region_vekt <- unique(as.character(region_vekt))
  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil)
  if (jmfr_linje == "region" && (length(jmfr_region) != 1 || is.na(jmfr_region))) {
    stop("jmfr_region ska vara en regionkod när jmfr_linje = \"region\".", call. = FALSE)
  }
  if (jmfr_linje == "ar" && (length(jmfr_ar) != 1 || is.na(jmfr_ar))) {
    stop("jmfr_ar ska vara ett år när jmfr_linje = \"ar\".", call. = FALSE)
  }
  if (skapa_gif) {
    if (!skriv_diagramfil) stop("skapa_gif = TRUE kräver skriv_diagramfil = TRUE.", call. = FALSE)
    if (!requireNamespace("gifski", quietly = TRUE)) stop("Paketet 'gifski' krävs för skapa_gif = TRUE.", call. = FALSE)
  }

  senaste_utfall <- max(c(intern_befpyramid_tid("TAB638"), intern_befpyramid_tid("TAB5557")))
  if (all(is.na(ar_vekt))) ar_vekt <- senaste_utfall
  ar_vekt <- sort(unique(as.character(ar_vekt)))

  # regionerna som ritas, och eventuell jämförelseregion (ritas inte som egen pyramid)
  plottade_regioner <- if (jmfr_linje == "region") setdiff(region_vekt, jmfr_region) else region_vekt
  if (length(plottade_regioner) == 0) {
    stop("Det finns ingen region att rita, eftersom jmfr_region är samma som region_vekt.", call. = FALSE)
  }
  hamta_regioner <- unique(c(plottade_regioner, if (jmfr_linje == "region") jmfr_region))
  hamta_ar <- unique(c(ar_vekt, if (jmfr_linje == "ar") as.character(jmfr_ar)))

  # ========================================== Hämta och bearbeta data ==================================

  bef_df <- intern_befpyramid_hamta(hamta_regioner, hamta_ar, med_fodd = jmfr_linje == "utr_inr")
  saknade_ar <- setdiff(hamta_ar, unique(bef_df$ar))
  if (length(saknade_ar) > 0) {
    warning("Inga data för år ", paste(saknade_ar, collapse = ", "), " (utanför tabellernas tidsperiod för valda regioner).",
            call. = FALSE)
    ar_vekt <- setdiff(ar_vekt, saknade_ar)
    if (length(ar_vekt) == 0) stop("Inga av de valda åren finns i tabellerna.", call. = FALSE)
  }
  if (jmfr_linje == "ar" && !as.character(jmfr_ar) %in% bef_df$ar) {
    stop("Jämförelseåret ", jmfr_ar, " finns inte i tabellerna.", call. = FALSE)
  }

  bef_df <- bef_df |>
    dplyr::mutate(aldergrp = intern_befpyramid_aldergrupper(alder, aldersgrupper),
                  kon = stringr::str_to_title(kon))

  # flera regioner till en grupp
  if (!is.na(grupp_namn)) {
    gruppera <- bef_df$regionkod %in% plottade_regioner
    bef_df <- dplyr::bind_rows(
      bef_df[!gruppera, ],
      bef_df[gruppera, ] |>
        dplyr::summarise(bef = sum(bef, na.rm = TRUE), kalla = paste(sort(unique(kalla)), collapse = "; "),
                         .by = c(kon, ar, fodd, typ, aldergrp, alder)) |>
        dplyr::mutate(regionkod = "grp", region = grupp_namn))
    plottade_regioner <- "grp"
  }

  agg_df <- bef_df |>
    dplyr::summarise(bef = sum(bef, na.rm = TRUE), kalla = paste(sort(unique(kalla)), collapse = "; "),
                     .by = c(regionkod, region, kon, ar, fodd, typ, aldergrp))

  # andel av respektive kön (per region, år och eventuell födelsegrupp) används för jämförelser mellan grupper
  agg_df <- agg_df |>
    dplyr::mutate(proc = bef / sum(bef) * 100, .by = c(regionkod, region, kon, ar, fodd, typ))

  y_var <- if (jmfr_linje %in% c("region", "utr_inr")) "proc" else "bef"
  if (returnera_data) assign("befpyramid", agg_df, envir = .GlobalEnv)

  # ========================================== Ritfunktioner ============================================

  etikett_format <- function(x) {
    x <- format(abs(x), big.mark = " ", scientific = FALSE, trim = TRUE)
    if (y_var == "proc") paste0(x, " %") else x
  }
  var_annan_etikett <- function(x) x[seq(1, length(x), by = 2)]
  fargvekt <- stats::setNames(diag_fargvekt[1:2], c("Kvinnor", "Män"))
  logga <- if (!visa_logga_i_diagram) FALSE else if (is.na(logga_sokvag)) TRUE else logga_sokvag

  # källtext: grundtexten plus en rad om prognosen när något av de ritade åren är en prognos
  skapa_caption <- function(df) {
    prognos_kallor <- unique(unlist(strsplit(df$kalla[df$typ == "prognos"], "; ")))
    if (length(prognos_kallor) == 0) return(diagram_capt)
    prognos_ar <- sort(unique(df$ar[df$typ == "prognos"]))
    paste0(diagram_capt, "\nObs! År ", paste(prognos_ar, collapse = ", "), " är prognos: ", paste(prognos_kallor, collapse = "; "), ".")
  }

  rita <- function(stapel_df, linje_df, stapel_namn, linje_namn, titel, y_max, caption) {
    y_plot <- \(df) ifelse(df$kon == "Kvinnor", -df[[y_var]], df[[y_var]])
    stapel_df$y_plot <- y_plot(stapel_df)
    p <- ggplot2::ggplot(stapel_df, ggplot2::aes(x = .data$aldergrp, y = .data$y_plot, fill = .data$kon)) +
      ggplot2::geom_col()
    if (!is.null(linje_df)) {
      linje_df$y_plot <- y_plot(linje_df)
      linje_df$linje <- linje_namn
      p <- p + ggplot2::geom_line(data = linje_df, linewidth = 1.3,
                                  ggplot2::aes(x = .data$aldergrp, y = .data$y_plot, group = .data$kon, linetype = .data$linje)) +
        ggplot2::scale_linetype_manual(name = NULL, values = stats::setNames("solid", linje_namn))
    }
    p <- p +
      ggplot2::scale_y_continuous(labels = etikett_format, limits = y_max * c(-1, 1)) +
      (if (!aldersgrupper) ggplot2::scale_x_discrete(breaks = var_annan_etikett)) +
      ggplot2::scale_fill_manual(name = if (is.null(linje_df)) "" else stapel_namn, values = fargvekt) +
      ggplot2::labs(y = if (y_var == "bef") "antal invånare" else "åldersgruppernas andel av totalen per kön",
                    caption = caption, title = titel) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(size = 10),
        axis.text.y = ggplot2::element_text(size = if (aldersgrupper) 10 else 8),
        axis.title.y = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_text(size = 8),
        plot.caption = ggplot2::element_text(face = "italic", hjust = 0, vjust = 0),
        plot.caption.position = "plot",
        plot.title = ggplot2::element_text(hjust = 0.5, size = 20),
        legend.key = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = "white"),
        panel.grid.major.y = ggplot2::element_line(linewidth = 0.4, colour = "lightgrey"),
        panel.grid.minor.y = ggplot2::element_line(linewidth = 0.4, colour = "lightgrey"),
        panel.grid.major.x = ggplot2::element_line(linewidth = 0.4, colour = "lightgrey"),
        panel.grid.minor.x = ggplot2::element_line(linewidth = 0.4, colour = "lightgrey"),
        axis.ticks = ggplot2::element_blank()) +
      ggplot2::guides(fill = ggplot2::guide_legend(order = 1), linetype = ggplot2::guide_legend(order = 2)) +
      ggplot2::coord_flip()
    p
  }

  # ========================================== Pyramider ================================================

  gg_list <- list()
  for (reg in plottade_regioner) {
    gif_filer <- character(0)
    reg_df <- agg_df[agg_df$regionkod == reg, ]
    reg_txt <- if (reg == "grp") grupp_namn else rdverktyg::skapa_kortnamn_lan(unique(reg_df$region))
    if (reg_txt == "Riket") reg_txt <- "riket"   # "Befolkning i riket", inte "i Riket"

    # underlag för stapel- och jämförelsedata per jämförelsetyp
    stapel_all <- switch(jmfr_linje,
      "utr_inr" = reg_df[reg_df$fodd == "inrikes födda", ],
      "ar" = reg_df[reg_df$ar %in% ar_vekt, ],
      reg_df[reg_df$ar %in% ar_vekt, ])
    linje_all <- switch(jmfr_linje,
      "region" = agg_df[agg_df$regionkod == jmfr_region, ],
      "utr_inr" = reg_df[reg_df$fodd == "utrikes födda", ],
      "ar" = reg_df[reg_df$ar == as.character(jmfr_ar), ],
      NULL)
    if (jmfr_linje == "region" && nrow(linje_all) == 0) stop("Hittade ingen data för jmfr_region ", jmfr_region, ".", call. = FALSE)
    stapel_all <- stapel_all[stapel_all$ar %in% ar_vekt, ]
    # gemensam y-axel för alla år i regionen, så att pyramiderna går att jämföra
    y_max <- max(c(stapel_all[[y_var]], if (!is.null(linje_all)) linje_all[[y_var]]), na.rm = TRUE)

    for (ar in rev(sort(unique(stapel_all$ar)))) {
      stapel_df <- stapel_all[stapel_all$ar == ar, ]
      linje_df <- switch(jmfr_linje,
        "region" = linje_all[linje_all$ar == ar, ],
        "utr_inr" = linje_all[linje_all$ar == ar, ],
        "ar" = linje_all,
        NULL)
      jmfr_txt <- switch(jmfr_linje,
        "region" = sub("^Riket$", "riket", rdverktyg::skapa_kortnamn_lan(unique(linje_all$region))),
        "utr_inr" = "utrikes födda",
        "ar" = paste0("Bef år ", jmfr_ar),
        NULL)
      stapel_txt <- switch(jmfr_linje, "region" = reg_txt, "utr_inr" = "inrikes födda", "ar" = paste0("Bef år ", ar), "")
      titel <- switch(jmfr_linje,
        "ingen" = paste0("Befolkning i ", reg_txt, " år ", ar),
        "region" = paste0("Befolkning i ", reg_txt, " år ", ar, "\njämfört med ", jmfr_txt),
        "utr_inr" = paste0("Inrikes födda i ", reg_txt, " år ", ar, "\njämfört med utrikes födda"),
        "ar" = paste0("Befolkning i ", reg_txt, " år ", ar, "\njämfört med år ", jmfr_ar))
      jmfr_fil <- switch(jmfr_linje, "ingen" = "", "utr_inr" = "_jmfr_utr_inr",
                         "region" = paste0("_jmfr_", intern_matchning_filtext(jmfr_txt)),
                         "ar" = paste0("_jmfr_ar_", jmfr_ar))
      aldersgrupper_fil <- if (aldersgrupper) "_aldersgrupper" else ""
      filnamn <- paste0("befpyramid_", intern_matchning_filtext(reg_txt), "_", ar, jmfr_fil, aldersgrupper_fil, ".png")

      # underlaget för källtexten är de rader som ritas (stapeldata och eventuell jämförelsedata)
      caption <- skapa_caption(dplyr::bind_rows(stapel_df, linje_df))
      p <- rita(stapel_df, linje_df, stapel_txt, jmfr_txt, titel, y_max, caption)

      if (skriv_diagramfil) {
        rddiagram::skriv_till_diagramfil(p, utmapp, filnamn, diagramfil_bredd = 12, diagramfil_hojd = 7, logga = logga)
        gif_filer <- c(gif_filer, paste0(sub("/?$", "/", utmapp), filnamn))
      }
      gg_list[[stringr::str_remove(filnamn, "\\.[^.]+$")]] <- p
    }

    # animerad gif över åren, i årsordning
    if (skapa_gif && length(gif_filer) > 1) {
      gif_filnamn <- paste0(sub("/?$", "/", utmapp), "befpyramid_", intern_matchning_filtext(reg_txt), "_alla_ar.gif")
      gifski::gifski(rev(gif_filer), gif_file = gif_filnamn, width = 2000, height = 1200,
                     delay = min(1, gif_langd_sek / length(gif_filer)))
      message("Skapade ", gif_filnamn)
    }
  }

  message("Det tog ", round(as.numeric(difftime(Sys.time(), start, units = "mins")), 2), " minuter att skapa pyramiderna.")
  if (returnera_figur) return(gg_list)
  invisible(NULL)
}
