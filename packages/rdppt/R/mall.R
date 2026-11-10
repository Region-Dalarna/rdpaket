# Region Dalarnas standardmall - används om ingen mall har angetts och filen går att nå.
intern_rd_standardmall <- "G:/skript/ppt/mall_rus_utan_bakgrund.pptx"

# Den kopia av samma mall som följer med paketet - används när filen på G: inte går att nå, eller
# när man ber om den med mall = "regiondalarna". "" om den saknas.
intern_rd_medfoljande_mall <- function() {
  system.file("mallar", "mall_rus_utan_bakgrund.pptx", package = "rdppt")
}

# De "roller" en layout kan ha i en presentation, och vilka layoutnamn som i tur och ordning prövas
# för varje roll. Namnen är dels Region Dalarnas egna, dels PowerPoints standardnamn på svenska och
# engelska - de flesta mallar utgår från dem, så rollerna hittas oftast utan konfiguration.
intern_roll_kandidater <- list(
  titel       = c("Rubrikbild", "Title Slide"),
  bild        = c("Bild utan text och rubrik", "Tom", "Blank"),
  bild_rubrik = c("Endast rubrik", "Title Only"),
  bild_text   = c("Bild och text till höger", "Bild med bildtext", "Picture with Caption",
                  "Två innehållsdelar", "Two Content"),
  punktlista  = c("Rubrik och innehåll", "Title and Content"),
  avsnitt     = c("Avsnittsrubrik", "Section Header"),
  jamforelse  = c("Jämförelse", "Comparison")
)

intern_roll_beskrivning <- c(
  titel       = "titelsidan",
  bild        = "diagram/bild utan rubrik och text",
  bild_rubrik = "diagram/bild med rubrik",
  bild_text   = "diagram/bild med text (och rubrik)",
  punktlista  = "rubrik och punktlista",
  avsnitt     = "avsnittsrubrik och avslutsbild",
  jamforelse  = "tvåspaltig sida (rapportlistan)"
)

# Region Dalarnas formatering av rubriker - den som tidigare var hårdkodad i skapa_ppt_fran_lista().
intern_rd_stil <- list(
  farg_rubrik     = "#2286AC",
  titel_stlk      = 40,
  undertitel_stlk = 20,
  namn_stlk       = 18,
  avsnitt_stlk    = 60
)

#' Beskriv en pptx-mall
#'
#' En mallprofil talar om vilken pptx-mall en presentation byggs utifrån
#' och vilken layout i mallen som används till vad (titelsida, diagram,
#' punktlista osv). Normalt behöver du inte skapa en själv - ange bara
#' mallfilen med [ppt_mall_ange()], så hittas layouterna automatiskt
#' utifrån sina namn. Använd `layouter` när din mall har egna layoutnamn
#' (se [ppt_mall_visa()] för vilka som finns).
#'
#' @param mall_fil Sökväg till en pptx-mall, eller `NULL` för den mall som
#'   är inbyggd i paketet `officer` (en vanlig vit Office-mall).
#' @param master Namnet på mallens master. `NULL` (standard) = den första
#'   i mallen.
#' @param layouter En namngiven vektor som pekar ut vilken layout som
#'   används för en viss roll, t.ex. `c(bild = "Tom", punktlista = "Min
#'   lista")`. Rollerna är `titel`, `bild`, `bild_rubrik`, `bild_text`,
#'   `punktlista`, `avsnitt` och `jamforelse`. Roller som inte anges väljs
#'   automatiskt utifrån layoutnamnen i mallen.
#' @param platshallare Behövs sällan. En namngiven lista, ett element per
#'   layoutnamn, som pekar ut platshållare med namn när det automatiska
#'   valet inte räcker, t.ex. `list("Min layout" = c(bild = "Platshållare
#'   för innehåll 3", text = "Platshållare för text 2"))`.
#' @param stil Formatering av rubriker (titelsida och avsnittsrubriker).
#'   En lista med något av `farg_rubrik`, `titel_stlk`, `undertitel_stlk`,
#'   `namn_stlk` och `avsnitt_stlk` - det som inte anges får Region
#'   Dalarnas standardvärden. `NULL` = ingen egen formatering, mallens
#'   egna typsnitt, storlekar och färger används.
#'
#' @return En mallprofil (klassen `rdppt_mallprofil`).
#' @seealso [ppt_mall_ange()], [ppt_mall_visa()]
#' @export
ppt_mallprofil <- function(mall_fil = NULL,
                           master = NULL,
                           layouter = NULL,
                           platshallare = NULL,
                           stil = list()) {

  pres <- intern_las_pptx(mall_fil)
  layouter_i_mall <- officer::layout_summary(pres)

  if (is.null(master)) {
    master <- layouter_i_mall$master[1]
  } else if (!master %in% layouter_i_mall$master) {
    stop("Mastern '", master, "' finns inte i mallen. Finns: ",
         paste0("'", unique(layouter_i_mall$master), "'", collapse = ", "), ".", call. = FALSE)
  }
  finns <- layouter_i_mall$layout[layouter_i_mall$master == master]

  roller <- vapply(intern_roll_kandidater, function(kandidater) {
    traff <- kandidater[kandidater %in% finns]
    if (length(traff) > 0) traff[1] else NA_character_
  }, character(1))

  if (!is.null(layouter)) {
    layouter <- unlist(layouter)
    okanda_roller <- setdiff(names(layouter), names(roller))
    if (is.null(names(layouter)) || length(okanda_roller) > 0) {
      stop("layouter ska vara en namngiven vektor med roller som namn. Giltiga roller: ",
           paste(names(roller), collapse = ", "), ".", call. = FALSE)
    }
    saknas <- setdiff(layouter, finns)
    if (length(saknas) > 0) {
      stop("Layouten ", paste0("'", saknas, "'", collapse = ", "), " finns inte i mallen. Finns: ",
           paste0("'", finns, "'", collapse = ", "), ".", call. = FALSE)
    }
    roller[names(layouter)] <- layouter
  }

  if (!is.null(stil)) stil <- utils::modifyList(intern_rd_stil, as.list(stil))

  structure(
    list(
      mall_fil = mall_fil,
      master = master,
      layouter = roller,
      platshallare = platshallare,
      stil = stil,
      # det användaren själv angett - behövs om profilen ska flyttas till en annan fil (ppt_befintlig())
      angivet = list(master = master, layouter = layouter, platshallare = platshallare)
    ),
    class = "rdppt_mallprofil"
  )
}

#' @export
print.rdppt_mallprofil <- function(x, ...) {
  cat("Mall:   ", if (is.null(x$mall_fil)) "officers inbyggda standardmall" else x$mall_fil, "\n", sep = "")
  cat("Master: ", x$master, "\n", sep = "")
  cat("Layouter:\n")
  for (roll in names(x$layouter)) {
    layout <- x$layouter[[roll]]
    # format() i stället för sprintf("%-28s"), som räknar byte och därför förskjuter rader med å/ä/ö
    cat("  ", format(roll, width = 12), " ",
        format(if (is.na(layout)) "- saknas -" else paste0("'", layout, "'"), width = 28),
        " (", intern_roll_beskrivning[[roll]], ")\n", sep = "")
  }
  cat("Rubrikformatering: ", if (is.null(x$stil)) "mallens egen" else paste0("Region Dalarnas (", x$stil$farg_rubrik, ")"), "\n", sep = "")
  invisible(x)
}

#' Välj vilken pptx-mall som ska användas
#'
#' Ställer in vilken mall presentationer byggs utifrån, för resten av
#' R-sessionen. Lägg anropet i din `.Rprofile` om du vill ha samma mall
#' varje gång. Har du inte valt någon mall används Region Dalarnas
#' standardmall: filen `G:/skript/ppt/mall_rus_utan_bakgrund.pptx` om den
#' går att nå, annars den kopia av mallen som följer med paketet.
#'
#' @param mall Sökväg till en pptx-mall, `"regiondalarna"` för den kopia
#'   av Region Dalarnas mall som följer med paketet, `"inbyggd"` för den
#'   vita Office-mall som är inbyggd i `officer`, en mallprofil från
#'   [ppt_mallprofil()], eller `NULL` för att återgå till standardvalet.
#' @param ... Skickas vidare till [ppt_mallprofil()] (`master`,
#'   `layouter`, `platshallare`, `stil`).
#'
#' @return Mallprofilen, osynligt.
#' @seealso [ppt_mall_visa()] för att se vilka layouter mallen har.
#' @examples
#' \dontrun{
#' ppt_mall_ange("G:/skript/ppt/mall_rus_utan_bakgrund.pptx")
#'
#' # kopian som följer med paketet, även när G: går att nå
#' ppt_mall_ange("regiondalarna")
#'
#' # en mall där diagram ska ligga på layouten "Tom"
#' ppt_mall_ange("min_mall.pptx", layouter = c(bild = "Tom"))
#' }
#' @export
ppt_mall_ange <- function(mall = NULL, ...) {
  if (is.null(mall) && ...length() == 0) {
    options(rdppt.mall = NULL)
    message("Mallvalet är återställt till standardvalet.")
    return(invisible(NULL))
  }
  profil <- intern_mallprofil_fran(mall, ...)
  options(rdppt.mall = profil)
  print(profil)
  invisible(profil)
}

#' Visa vilka layouter som finns i en pptx-mall
#'
#' Skriver ut en tabell över mallens layouter: vilka platshållare (rubrik,
#' text, bild) varje layout har, och vilka som används för vad. Alla
#' layouter i tabellen går att använda genom att ange deras namn, t.ex.
#' `ppt_bild(..., layout = "Två innehållsdelar")`.
#'
#' @param mall Mallen som ska visas, se [ppt_mall_ange()]. `NULL`
#'   (standard) = den mall som är vald just nu.
#' @param layout Namnet på en layout att titta närmare på. Då listas dess
#'   platshållare med namn och storlek, och layouten ritas upp.
#'
#' @return En `data.frame` med en rad per layout (eller, med `layout`, en
#'   rad per platshållare), osynligt.
#' @seealso [ppt_mall_exempel()] för att få mallens layouter som en
#'   pptx-fil.
#' @export
ppt_mall_visa <- function(mall = NULL, layout = NULL) {
  profil <- intern_mall_aktuell(mall)
  pres <- intern_las_pptx(profil$mall_fil)
  lp <- officer::layout_properties(pres)
  layouter <- officer::layout_summary(pres)
  layouter <- layouter$layout[layouter$master == profil$master]

  if (!is.null(layout)) {
    if (!layout %in% layouter) {
      stop("Layouten '", layout, "' finns inte i mallen. Finns: ",
           paste0("'", layouter, "'", collapse = ", "), ".", call. = FALSE)
    }
    ph <- intern_platshallare(lp, layout, profil$master)
    tabell <- data.frame(
      platshallare = ph$ph_label,
      typ = ph$roll,
      bredd_tum = round(ph$cx, 1),
      hojd_tum = round(ph$cy, 1),
      fran_vanster_tum = round(ph$offx, 1),
      fran_toppen_tum = round(ph$offy, 1)
    )
    cat("Layout: '", layout, "'\n\n", sep = "")
    if (nrow(tabell) == 0) cat("(inga platshållare - en bild läggs över hela sidan)\n") else print(tabell, row.names = FALSE)
    officer::plot_layout_properties(pres, layout = layout, master = profil$master)
    return(invisible(tabell))
  }

  tabell <- do.call(rbind, lapply(layouter, function(namn) {
    ph <- intern_platshallare(lp, namn, profil$master)
    roller <- names(profil$layouter)[!is.na(profil$layouter) & profil$layouter == namn]
    data.frame(
      layout = namn,
      rubrik = if (any(ph$roll == "rubrik")) "ja" else "-",
      text = sum(ph$roll == "text"),
      bild = if (any(ph$roll == "bild")) "ja" else "-",
      anvands_for = paste(roller, collapse = ", ")
    )
  }))

  storlek <- officer::slide_size(pres)
  cat("Mall:   ", if (is.null(profil$mall_fil)) "officers inbyggda standardmall" else profil$mall_fil, "\n", sep = "")
  cat("Master: ", profil$master, "   Sidstorlek: ", round(storlek$width, 1), " x ", round(storlek$height, 1), " tum\n\n", sep = "")
  print(tabell, row.names = FALSE, right = FALSE)
  saknas <- names(profil$layouter)[is.na(profil$layouter)]
  if (length(saknas) > 0) {
    cat("\nIngen layout hittades för: ", paste(saknas, collapse = ", "),
        ". Peka ut en med ppt_mall_ange(..., layouter = c(", saknas[1], " = \"layoutnamn\")).\n", sep = "")
  }
  cat("\nTitta närmare på en layout med ppt_mall_visa(layout = \"layoutnamn\").\n")
  invisible(tabell)
}

#' Skapa en exempelpresentation med alla layouter i en mall
#'
#' Bygger en pptx-fil med en slide per layout i mallen, där varje
#' platshållare är ifylld med sitt eget namn. Ett enkelt sätt att se hur
#' layouterna faktiskt ser ut, och vad platshållarna heter, innan du
#' väljer vilka du vill använda.
#'
#' @param mall Mallen, se [ppt_mall_ange()]. `NULL` (standard) = den mall
#'   som är vald just nu.
#' @param fil Sökväg till pptx-filen som skapas. `NULL` (standard) = en
#'   temporär fil.
#' @param oppna `TRUE` om filen ska öppnas direkt.
#'
#' @return Sökvägen till filen, osynligt.
#' @export
ppt_mall_exempel <- function(mall = NULL, fil = NULL, oppna = interactive()) {
  profil <- intern_mall_aktuell(mall)
  pres <- intern_las_pptx(profil$mall_fil)
  lp <- officer::layout_properties(pres)
  layouter <- officer::layout_summary(pres)
  layouter <- layouter$layout[layouter$master == profil$master]

  # slides som redan ligger i mallen hör inte hemma i exempelfilen
  for (i in rev(seq_len(length(pres)))) pres <- officer::remove_slide(pres, index = i)

  exempelbild <- ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = "Bild", size = 12, colour = "grey40") +
    ggplot2::theme_void() +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "grey92", colour = "grey60"))

  for (namn in layouter) {
    ph <- intern_platshallare(lp, namn, profil$master)
    textrutor <- ph$ph_label[ph$roll == "text"]
    pres <- intern_lagg_till_slide(
      pres, lp, profil, layout = namn, master = profil$master,
      rubrik = if (any(ph$roll == "rubrik")) namn else NULL,
      text = if (length(textrutor) > 0) stats::setNames(as.list(textrutor), textrutor) else NULL,
      bild = if (any(ph$roll == "bild")) exempelbild else NULL,
      anteckningar = paste0("Layout: ", namn)
    )
  }

  if (is.null(fil)) fil <- tempfile("mall_exempel_", fileext = ".pptx")
  intern_skriv_pptx(pres, fil)
  message("Exempelfil med ", length(layouter), " layouter sparad: ", fil,
          "\nLayoutens namn står även i anteckningarna till varje slide.")
  if (isTRUE(oppna)) intern_oppna_fil(fil)
  invisible(fil)
}

# Gör en mallprofil av det användaren angett: en färdig profil, en sökväg, "regiondalarna",
# "inbyggd" eller NULL (= Region Dalarnas standardmall, från G: om den går att nå och annars den
# kopia som följer med paketet).
intern_mallprofil_fran <- function(mall, ...) {
  if (inherits(mall, "rdppt_mallprofil")) {
    if (...length() > 0) stop("Ange antingen en färdig mallprofil eller en mallfil med inställningar, inte både och.", call. = FALSE)
    return(mall)
  }
  if (identical(mall, "inbyggd")) {
    argument <- list(...)
    if (!"stil" %in% names(argument)) argument["stil"] <- list(NULL)
    return(do.call(ppt_mallprofil, c(list(mall_fil = NULL), argument)))
  }
  medfoljande <- intern_rd_medfoljande_mall()
  if (identical(mall, "regiondalarna")) {
    if (!nzchar(medfoljande)) stop("Region Dalarnas mall saknas i paketet - installera om rdppt.", call. = FALSE)
    return(ppt_mallprofil(mall_fil = medfoljande, ...))
  }
  if (!is.null(mall)) {
    if (!is.character(mall) || length(mall) != 1) {
      stop("mall ska vara en sökväg till en pptx-fil, \"regiondalarna\", \"inbyggd\" eller en mallprofil från ppt_mallprofil().",
           call. = FALSE)
    }
    return(ppt_mallprofil(mall_fil = mall, ...))
  }
  if (file.exists(intern_rd_standardmall)) return(ppt_mallprofil(mall_fil = intern_rd_standardmall, ...))
  if (nzchar(medfoljande)) return(ppt_mallprofil(mall_fil = medfoljande, ...))

  message("Ingen mall är vald och Region Dalarnas standardmall går inte att nå - officers inbyggda ",
          "standardmall används. Välj en egen med ppt_mall_ange().")
  intern_mallprofil_fran("inbyggd", ...)
}

# Den mallprofil som gäller: det som skickats med, annars det som valts med ppt_mall_ange(), annars
# standardvalet. grundfil är en pptx med färdiga slides som presentationen ska byggas vidare på (se
# ppt_befintlig()) - den ersätter då mallfilen, men behåller profilens övriga inställningar.
intern_mall_aktuell <- function(mall = NULL, grundfil = NULL) {
  if (is.null(mall)) mall <- getOption("rdppt.mall")

  if (is.null(grundfil)) return(intern_mallprofil_fran(mall))
  if (is.null(mall)) return(ppt_mallprofil(mall_fil = grundfil))

  profil <- intern_mallprofil_fran(mall)
  ppt_mallprofil(
    mall_fil = grundfil,
    layouter = profil$angivet$layouter,
    platshallare = profil$angivet$platshallare,
    stil = profil$stil
  )
}

# Läser en pptx-fil, eller officers inbyggda mall om fil är NULL.
intern_las_pptx <- function(fil = NULL) {
  if (is.null(fil)) return(officer::read_pptx())
  if (!is.character(fil) || length(fil) != 1 || !file.exists(fil)) {
    stop("Hittar inte pptx-filen:\n  ", fil,
         "\nKontrollera sökvägen, eller välj en annan mall med ppt_mall_ange().", call. = FALSE)
  }
  officer::read_pptx(fil)
}

# De riktiga platshållarna i en layout, med vilken roll de kan fylla (rubrik, text eller bild),
# ordnade uppifrån och ned, vänster till höger. Logotyper och annat fast innehåll i layouten (som
# officer också listar, men utan ph-tagg) och sidfotsfälten räknas inte.
intern_platshallare <- function(lp, layout, master) {
  ph <- lp[lp$name == layout & lp$master_name == master &
             !is.na(lp$ph) & !lp$type %in% c("dt", "ftr", "sldNum", "hdr"), , drop = FALSE]
  ph$roll <- ifelse(ph$type %in% c("title", "ctrTitle"), "rubrik",
                    ifelse(ph$type == "pic", "bild", "text"))
  ph$yta <- ph$cx * ph$cy
  ph[order(round(ph$offy, 1), ph$offx), , drop = FALSE]
}
