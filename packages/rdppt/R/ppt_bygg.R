#' Börja på en ny presentation
#'
#' Första steget när en presentation byggs steg för steg: skapa den med
#' `ppt_ny()`, lägg till slides med [ppt_lagg_till_slide()] (diagram, bild,
#' text, slutsatser, avsnitt, rapporter, avslut och färdiga slides) och
#' spara med [ppt_spara()].
#' Slides hamnar i den ordning de läggs till.
#'
#' @param huvudrubrik Presentationens huvudrubrik (på titelsidan). `NULL`
#'   = ingen titelsida.
#' @param underrubrik Underrubrik på titelsidan, eller `NA` för ingen.
#'   Standard: "Skapad <dagens datum> av Samhällsanalys\\nRegionala
#'   utvecklingsförvaltningen, Region Dalarna".
#' @param mall Mallen presentationen byggs utifrån: en sökväg till en
#'   pptx-fil, `"regiondalarna"`, `"inbyggd"` eller en mallprofil från
#'   [ppt_mallprofil()]. `NULL` (standard) = den mall som är vald med
#'   [ppt_mall_ange()], annars Region Dalarnas standardmall (från G: om
#'   den går att nå, annars den kopia som följer med paketet).
#' @param bildmapp Mapp där diagrambilderna från `ppt_lagg_till_slide("diagram", ...)` sparas.
#'   `NULL` (standard) = en temporär mapp som du inte behöver bry dig om.
#' @param anv_uppgifter Vems namn/e-post som ska stå på titel- och
#'   avslutssidan: `NULL` (standard) = inloggad användare, ett namn som
#'   slås upp med [rddeploy::anv_hamta_namn_epost_fran_lista()], eller en
#'   lista `list(namn = "...", epost = "...")`.
#' @param rubrik_namn TRUE om presentatörens namn ska skrivas ut på
#'   titelsidan.
#' @param stil Formatering av titelsidans och avsnittens rubriker för just
#'   den här presentationen, t.ex. `list(titel_stlk = 54)`. Det som inte
#'   anges får mallens stil (se [ppt_mallprofil()]) och det som anges med
#'   `options(rdppt.stil = list(...))`.
#' @param loggfil Sökväg till en textfil där det som går fel när
#'   presentationen byggs (ett diagram som hoppas över, och varför) loggas
#'   fortlöpande. `NULL` (standard) = `options(rdppt.loggfil)`, annars ingen
#'   loggfil. Problemen sammanfattas också när presentationen sparas.
#'
#' @return En presentation (klassen `rdppt`) att bygga vidare på.
#' @examples
#' \dontrun{
#' ppt_ny("Arbetsmarknad och befolkning i Dalarna") |>
#'   ppt_lagg_till_slide("diagram", funktion = rddiagramkatalog::diag_arbetsmarknadsstatus_tidsserie_scb,
#'                       region_vekt = c("20", "2080", "2081")) |>
#'   ppt_lagg_till_slide("slutsatser", text = c("Första slutsatsen", "Andra slutsatsen")) |>
#'   ppt_lagg_till_slide("rapporter") |>
#'   ppt_lagg_till_slide("avslut") |>
#'   ppt_spara("arbetsmarknad_befolkning.pptx")
#' }
#' @export
ppt_ny <- function(huvudrubrik = NULL,
                   underrubrik = intern_standard_underrubrik(),
                   mall = NULL,
                   bildmapp = NULL,
                   anv_uppgifter = NULL,
                   rubrik_namn = TRUE,
                   stil = NULL,
                   loggfil = NULL) {

  profil <- intern_mall_aktuell(mall)
  message("rdppt: ny presentation påbörjad", if (intern_har_innehall(huvudrubrik)) paste0(": ", huvudrubrik) else "", ".")
  if (!is.null(stil) && !is.null(profil$stil)) profil$stil <- utils::modifyList(profil$stil, as.list(stil))
  if (is.null(loggfil)) loggfil <- getOption("rdppt.loggfil")
  layouter <- officer::layout_summary(intern_las_pptx(profil$mall_fil))

  if (is.null(bildmapp)) {
    bildmapp <- tempfile("rdppt_bilder_")
  }
  if (!dir.exists(bildmapp)) dir.create(bildmapp, recursive = TRUE)
  # diagramfunktionerna bygger sökvägar med paste0(utmapp, filnamn), så mappen ska sluta med snedstreck
  bildmapp <- paste0(sub("[/\\\\]+$", "", bildmapp), "/")

  structure(
    list(
      huvudrubrik = huvudrubrik,
      underrubrik = underrubrik,
      profil = profil,
      layouter = layouter$layout[layouter$master == profil$master],
      bildmapp = bildmapp,
      anv_uppgifter = anv_uppgifter,
      starttid = Sys.time(),
      rubrik_namn = rubrik_namn,
      grundfil = NULL,
      loggfil = loggfil,
      logg = intern_tom_logg(),
      slides = intern_tom_ppt_lista()
    ),
    class = "rdppt"
  )
}

#' @export
print.rdppt <- function(x, ...) {
  cat("Presentation: ", if (intern_har_innehall(x$huvudrubrik)) x$huvudrubrik else "(utan titelsida)", "\n", sep = "")
  cat("Mall:         ", if (is.null(x$profil$mall_fil)) "officers inbyggda standardmall" else x$profil$mall_fil, "\n", sep = "")
  if (!is.null(x$grundfil)) cat("Färdiga slides hämtas från: ", x$grundfil, "\n", sep = "")

  slides <- x$slides
  if (nrow(slides) == 0) {
    cat("Inga slides tillagda än.\n")
    return(invisible(x))
  }
  innehall <- vapply(seq_len(nrow(slides)), function(i) {
    if (!is.na(slides$titel[i])) return(slides$titel[i])
    if (!is.na(slides$dia_path[i])) return(basename(slides$dia_path[i]))
    if (intern_ar_ggplot(slides$gg_obj[[i]])) return("(ggplot-diagram)")
    ""
  }, character(1))
  cat(nrow(slides), " slides", if (intern_har_innehall(x$huvudrubrik)) " (utöver titelsidan)", ":\n", sep = "")
  print(data.frame(layout = ifelse(is.na(slides$mall), "(väljs automatiskt)", slides$mall),
                   innehall = innehall),
        right = FALSE)
  invisible(x)
}

# Själva körningen av diagramfunktionen. Utbruten så att ppt_lagg_till_slide(layout = "diagram") kan
# använda samma kod; funktion_namn är funktionens namn så som användaren skrev det (till felmeddelanden).
intern_ppt_diagram <- function(ppt, funktion, funktion_namn, argument, region_vekt, rubrik, text, layout,
                               anteckningar, regionvariabel, per_region, stoppa_vid_fel) {

  if (!grepl("^[a-zA-Z0-9_.:]+$", funktion_namn)) funktion_namn <- "diagramfunktionen"
  if (is.character(funktion)) funktion <- eval(parse(text = funktion))
  if (!is.function(funktion)) stop("funktion ska vara en funktion, t.ex. rddiagramkatalog::diag_befolkningsprognos_profet_scb.", call. = FALSE)

  # en tom vektor med regioner betyder att det inte finns något att köra för (t.ex. inga kommunkoder) - hoppa över
  if (!is.null(region_vekt) && length(region_vekt) == 0) {
    return(intern_logga(ppt, funktion_namn, NA, "hoppad", "inga regioner angivna"))
  }

  parametrar <- names(formals(funktion))
  okanda <- setdiff(names(argument), parametrar)
  if (!"..." %in% parametrar && (length(okanda) > 0 || (length(argument) > 0 && is.null(names(argument))))) {
    stop(funktion_namn, " har ingen parameter som heter ", paste0("'", okanda, "'", collapse = ", "),
         ". Parametrar till diagramfunktionen anges med namn.", call. = FALSE)
  }
  if (!is.null(region_vekt) && !regionvariabel %in% parametrar) {
    stop(funktion_namn, " har ingen parameter som heter '", regionvariabel,
         "'. Ange vad regionparametern heter med regionvariabel, eller utelämna region_vekt.", call. = FALSE)
  }

  if ("utmapp" %in% parametrar && !"utmapp" %in% names(argument)) argument$utmapp <- ppt$bildmapp
  if ("skriv_diagramfil" %in% parametrar && !"skriv_diagramfil" %in% names(argument)) argument$skriv_diagramfil <- TRUE
  # mappen där bilderna hamnar - finns ingen sådan parameter, eller skrivs inga filer, används de
  # returnerade ggplot-objekten (annars kan en äldre bild med samma namn i mappen plockas upp)
  skriver_filer <- "utmapp" %in% names(argument) && !isFALSE(argument$skriv_diagramfil)
  mapp <- if (skriver_filer) argument$utmapp else NULL

  korningar <- if (is.null(region_vekt)) {
    list(NULL)
  } else if (isTRUE(per_region)) {
    as.list(region_vekt)
  } else {
    list(region_vekt)
  }

  antal_fore <- nrow(ppt$slides)
  for (region in korningar) {
    argument_korning <- argument
    if (!is.null(region)) argument_korning[[regionvariabel]] <- region
    for_region_txt <- if (is.null(region)) "" else paste0(" för ", paste(region, collapse = ", "))

    bilder_fore <- intern_png_i_mapp(mapp)
    retur <- tryCatch(
      rdverktyg::skriptrader_upprepa_om_fel(
        do.call(funktion, argument_korning),
        exportera_till_globalenv = FALSE,
        vila_sek = 3,
        upprepa_vid_felmeddelande_som_innehaller =
          c("recv failure", "connection was reset", "curl_fetch_memory", "timeout", "cannot open the connection")
      ),
      error = function(e) {
        if (isTRUE(stoppa_vid_fel)) stop(funktion_namn, " gav fel", for_region_txt, ": ", conditionMessage(e), call. = FALSE)
        warning(funktion_namn, " gav fel", for_region_txt, " och hoppas över: ", conditionMessage(e), call. = FALSE)
        ppt <<- intern_logga(ppt, funktion_namn, region, "fel", conditionMessage(e))
        NULL
      }
    )
    if (is.null(retur)) next

    bilder <- intern_diagram_fran_retur(retur, mapp, bilder_fore)
    if (length(bilder) == 0) {
      warning(funktion_namn, " gav inga diagram", for_region_txt, ".", call. = FALSE)
      ppt <- intern_logga(ppt, funktion_namn, region, "inga diagram", "funktionen gav inga diagram")
      next
    }
    anteckningar_korning <- tryCatch(
      intern_anteckningar_for(anteckningar, region),
      error = function(e) {
        warning("anteckningar för ", funktion_namn, for_region_txt, " kunde inte skapas och utelämnas: ", conditionMessage(e), call. = FALSE)
        ppt <<- intern_logga(ppt, funktion_namn, region, "fel", paste("anteckningar:", conditionMessage(e)))
        NULL
      }
    )
    for (bild in bilder) {
      ppt <- intern_ppt_lagg_till(ppt, bild = bild, titel = rubrik, beskr = text, mall = layout, anteckningar = anteckningar_korning)
    }
  }

  antal_nya <- nrow(ppt$slides) - antal_fore
  message(funktion_namn, ": ", antal_nya, " diagram ", if (antal_nya == 1) "tillagt." else "tillagda.")
  ppt
}

# Lägger in färdiga slides från en pptx-fil (en "slides-bank") på den plats i ordningen där den anropas.
# Filen används som grund för hela presentationen, så den ska vara skapad utifrån samma mall som resten
# (öppna mallen i PowerPoint, lägg till slides och spara under ett nytt namn). Färdiga slides kan därför bara
# hämtas från en fil per presentation, och slides i filen som inte läggs in kommer inte med.
# fil = NULL: samma fil som senast, eller - första gången - options(rdppt.slidebank), annars mallfilen själv.
intern_ppt_befintlig <- function(ppt, fil = NULL, slide = NULL) {
  intern_kontrollera_ppt(ppt)

  if (is.null(fil) && is.null(ppt$grundfil)) fil <- getOption("rdppt.slidebank")

  if (!is.null(fil)) {
    if (!is.null(ppt$grundfil) && normalizePath(fil, mustWork = FALSE) != normalizePath(ppt$grundfil, mustWork = FALSE)) {
      stop("Färdiga slides hämtas redan från ", ppt$grundfil, " - de kan bara hämtas från en fil per presentation. ",
           "Samla de färdiga slidesen i en och samma pptx-fil.", call. = FALSE)
    }
    ppt$grundfil <- fil
  }
  kalla <- if (is.null(ppt$grundfil)) ppt$profil$mall_fil else ppt$grundfil
  antal <- length(intern_las_pptx(kalla))
  kalla_txt <- if (is.null(kalla)) "officers inbyggda standardmall" else kalla

  if (antal == 0) stop("Det finns inga färdiga slides i ", kalla_txt, ".", call. = FALSE)
  if (is.null(slide)) slide <- seq_len(antal)
  if (!is.numeric(slide) || any(is.na(slide)) || any(slide < 1) || any(slide > antal) || any(slide != round(slide))) {
    stop("slide ska vara nummer mellan 1 och ", antal, " - så många slides finns i ", kalla_txt, ".", call. = FALSE)
  }
  redan <- intersect(paste0("befintlig:", slide), ppt$slides$mall)
  if (length(redan) > 0 || anyDuplicated(slide) > 0) {
    stop("En färdig slide kan bara läggas in på ett ställe i presentationen",
         if (length(redan) > 0) paste0(" (redan inlagd: slide ", paste(sub("befintlig:", "", redan), collapse = ", "), ")"), ".",
         call. = FALSE)
  }

  for (nr in slide) ppt <- intern_ppt_lagg_till(ppt, mall = paste0("befintlig:", nr))
  ppt
}

#' Visa vilka färdiga slides en pptx-fil innehåller
#'
#' Listar sliderna i en pptx-fil i den ordning de visas i PowerPoint, med
#' rubrik, den första texten utöver rubriken, vad sliden i övrigt
#' innehåller (bilder, tabeller, diagram, grupperade figurer) och vilken
#' layout den har.
#' Numret i kolumnen `slide` är det du anger i `ppt_lagg_till_slide("befintlig", slide = )`.
#'
#' @param fil Sökväg till pptx-filen med färdiga slides.
#' @param bilder TRUE för att också visa bilderna av sliderna, se
#'   [ppt_forhandsvisa()].
#' @param oppna TRUE för att också öppna filen i PowerPoint.
#'
#' @return Osynligt: en `data.frame` med en rad per slide och kolumnerna
#'   `slide`, `rubrik`, `text`, `innehall` och `layout` (texterna i sin
#'   helhet - i utskriften är de avkortade).
#' @seealso [ppt_lagg_till_slide()], [ppt_forhandsvisa()]
#' @examples
#' \dontrun{
#' ppt_befintlig_visa("G:/skript/ppt/mall_rus_utan_bakgrund_demografi.pptx")
#' }
#' @export
ppt_befintlig_visa <- function(fil, bilder = FALSE, oppna = FALSE) {
  slides <- intern_slides_i_fil(intern_las_pptx(fil))

  cat("Fil: ", fil, "\n", sep = "")
  if (nrow(slides) == 0) {
    cat("Filen innehåller inga slides.\n")
    return(invisible(slides))
  }
  korta <- function(x, bredd) {
    x <- gsub("\\s+", " ", x)
    ifelse(nchar(x) > bredd, paste0(substr(x, 1, bredd - 3), "..."), x)
  }
  utskrift <- slides
  utskrift$rubrik <- korta(utskrift$rubrik, 40)
  utskrift$text <- korta(utskrift$text, 40)
  cat("\n")
  print(utskrift, right = FALSE, row.names = FALSE)
  cat("\nLägg in dem med ppt_lagg_till_slide(\"befintlig\", fil = , slide = <nummer>).\n")

  if (isTRUE(bilder)) ppt_forhandsvisa(fil)
  if (isTRUE(oppna)) intern_oppna_fil(fil)
  invisible(slides)
}

#' Spara presentationen som en pptx-fil
#'
#' Sista steget: bygger presentationen och skriver den till fil. Har
#' något gått fel när presentationen byggdes (ett diagram som hoppades
#' över) sammanfattas det här.
#'
#' Med flera personer i `anv_uppgifter` sparas en fil per person - samma
#' slides, men med personens namn och e-post på titel- och avslutssidan.
#' Diagrammen körs bara en gång, så det går snabbt. Anges ett enda namn
#' (`"peter"`) får filnamnet också namnet (`utmaningar_peter.pptx`).
#'
#' När presentationen är sparad skrivs den tid det tagit sedan [ppt_ny()]
#' ut i konsolen.
#'
#' @param ppt En presentation från [ppt_ny()].
#' @param filnamn Filnamn, eller en hel sökväg, på pptx-filen. `NULL`
#'   (standard) = ett filnamn som bildas av huvudrubriken. Med flera
#'   personer läggs personens namn till filnamnet (`utmaningar_peter.pptx`).
#' @param mapp Mapp att spara i, om `filnamn` inte är en hel sökväg.
#' @param oppna `TRUE` om presentationen ska öppnas direkt.
#' @param spara_backup_om_fil_redan_finns `TRUE` om en befintlig fil med
#'   samma namn ska säkerhetskopieras i stället för att skrivas över.
#' @param nytt_namn_om_last `TRUE` (standard) sparar under ett annat namn (`namn_2.pptx`, `namn_3.pptx` ...) om
#'   filen är öppen i PowerPoint eller låst, och säger till om det. `FALSE` ger i stället ett felmeddelande.
#' @param anv_uppgifter Vems namn/e-post som ska stå på titel- och
#'   avslutssidan. `NULL` (standard) = det som angavs i [ppt_ny()], annars
#'   inloggad användare. Ett namn som slås upp med
#'   [rddeploy::anv_hamta_namn_epost_fran_lista()] (t.ex. `"peter"`),
#'   flera sådana namn (`c("peter", "jon")`, en fil per person), eller en
#'   lista `list(namn = "...", epost = "...")`.
#'
#' @return Sökvägen (en per fil) till den eller de sparade
#'   presentationerna, osynligt.
#' @export
ppt_spara <- function(ppt,
                      filnamn = NULL,
                      mapp = rdverktyg::utskriftsmapp(),
                      oppna = FALSE,
                      spara_backup_om_fil_redan_finns = FALSE,
                      anv_uppgifter = NULL,
                      nytt_namn_om_last = TRUE) {

  intern_kontrollera_ppt(ppt)
  if (is.null(anv_uppgifter)) anv_uppgifter <- ppt$anv_uppgifter

  if (is.null(filnamn)) {
    filnamn <- if (intern_har_innehall(ppt$huvudrubrik)) {
      gsub("^_+|_+$", "", gsub("[^a-z0-9]+", "_", chartr("åäöéü", "aaoeu", tolower(ppt$huvudrubrik))))
    } else {
      "presentation"
    }
    if (!nzchar(filnamn)) filnamn <- "presentation"
  } else if (basename(filnamn) != filnamn) {
    mapp <- dirname(filnamn)
    filnamn <- basename(filnamn)
  }

  # flera personer: en fil per person
  flera <- is.character(anv_uppgifter) && length(anv_uppgifter) >= 1
  personer <- if (flera) as.list(anv_uppgifter) else list(anv_uppgifter)
  filer <- vapply(personer, function(person) {
    filnamn_person <- filnamn
    if (flera) {
      bas <- sub("\\.pptx$", "", filnamn, ignore.case = TRUE)
      filnamn_person <- paste0(bas, "_", gsub("[^a-z0-9]+", "_", chartr("åäöéü", "aaoeu", tolower(person))), ".pptx")
    }
    intern_bygg_pptx(
      pptlista = ppt$slides,
      huvudrubrik = ppt$huvudrubrik,
      underrubrik = ppt$underrubrik,
      output_mapp = mapp,
      spara_backup_om_fil_redan_finns = spara_backup_om_fil_redan_finns,
      filnamn = filnamn_person,
      mall_fil = intern_mall_aktuell(ppt$profil, grundfil = ppt$grundfil),
      rubrik_namn = ppt$rubrik_namn,
      anv_uppgifter = person,
      nytt_namn_om_last = nytt_namn_om_last
    )
  }, character(1))

  if (!is.null(ppt$starttid)) {
    message("Det tog ", intern_tid_text(as.numeric(difftime(Sys.time(), ppt$starttid, units = "secs"))),
            " att skapa presentationen.")
  }
  intern_logg_sammanfatta(ppt)
  if (isTRUE(oppna)) for (fil in filer) intern_oppna_fil(fil)
  invisible(filer)
}

# Sekunder som text: "45 s", "3 min 12 s", "1 h 05 min".
intern_tid_text <- function(sek) {
  sek <- round(sek)
  if (sek < 60) return(paste0(sek, " s"))
  if (sek < 3600) return(paste0(sek %/% 60, " min ", sek %% 60, " s"))
  paste0(sek %/% 3600, " h ", formatC((sek %% 3600) %/% 60, width = 2, flag = "0"), " min")
}

intern_kontrollera_ppt <- function(ppt) {
  if (!inherits(ppt, "rdppt")) {
    stop("Första argumentet ska vara en presentation skapad med ppt_ny(), t.ex. ppt_ny(\"Rubrik\") |> ppt_lagg_till_slide(\"text\", ...).", call. = FALSE)
  }
  invisible(ppt)
}

# Kontrollerar direkt när en slide läggs till att layouten finns i mallen, så att ett felstavat namn
# inte märks först när presentationen sparas. Returnerar NA (= välj automatiskt) om ingen angetts.
intern_kontrollera_layout <- function(ppt, layout) {
  if (is.null(layout) || is.na(layout)) return(NA_character_)
  if (!layout %in% ppt$layouter && !grepl("^roll:", layout)) {
    stop("Layouten '", layout, "' finns inte i mallen. Finns: ", paste0("'", ppt$layouter, "'", collapse = ", "),
         ".\nSe hur de ser ut med ppt_mall_visa().", call. = FALSE)
  }
  layout
}

# Lägger till en rad (= en slide) sist i presentationens ppt-lista.
intern_ppt_lagg_till <- function(ppt, bild = NULL, titel = NULL, beskr = NULL, mall = NA_character_,
                                 anteckningar = NULL, txt_size = NA, antal = NA_integer_) {
  rad <- data.frame(
    dia_path = if (is.character(bild)) bild else NA_character_,
    titel = if (intern_har_innehall(titel)) as.character(titel)[1] else NA_character_,
    master = NA_character_,
    mall = mall,
    txt_size = as.double(txt_size),
    antal = as.integer(antal)
  )
  rad$beskr <- list(beskr)
  rad$gg_obj <- list(if (intern_ar_ggplot(bild)) bild else NULL)
  rad$anteckningar <- list(anteckningar)
  ppt$slides <- dplyr::bind_rows(ppt$slides, rad)
  ppt
}

# Sliderna i en inläst pptx-fil, en rad per slide i den ordning de visas. Läses ur slidernas xml,
# så att även sådant officer inte kan redovisa (SmartArt, grupperade figurer m.m.) räknas med.
intern_slides_i_fil <- function(pres) {
  tom <- data.frame(slide = integer(0), rubrik = character(0), text = character(0),
                    innehall = character(0), layout = character(0))
  if (length(pres) == 0) return(tom)

  # officer håller sliderna i filordning (slide1.xml, slide2.xml ...) - visningsordningen står i
  # presentationens egen förteckning
  filnamn <- pres$slide$get_metadata()$name
  visningsordning <- match(basename(pres$presentation$slide_data()$target), filnamn)
  layouter <- pres$slideLayouts$get_metadata()

  rader <- lapply(seq_along(visningsordning), function(nr) {
    slide <- pres$slide$get_slide(visningsordning[nr])
    xml <- slide$get()
    antal <- function(xpath) length(xml2::xml_find_all(xml, xpath))
    stycken <- function(noder) {
      texter <- vapply(noder, function(nod) {
        paste(xml2::xml_text(xml2::xml_find_all(nod, ".//*[local-name()='t']")), collapse = "")
      }, character(1))
      trimws(texter[nzchar(trimws(texter))])
    }

    ar_rubrik <- "*[local-name()='nvSpPr']/*[local-name()='nvPr']/*[local-name()='ph'][@type='title' or @type='ctrTitle']"
    rubrik <- stycken(xml2::xml_find_all(xml, paste0("//*[local-name()='sp'][", ar_rubrik, "]//*[local-name()='p']")))
    # text i grupperade figurer räknas inte - i ett diagram är det axeletiketter och liknande
    ovrig_text <- stycken(xml2::xml_find_all(
      xml, paste0("//*[local-name()='sp'][not(", ar_rubrik, ")][not(ancestor::*[local-name()='grpSp'])]//*[local-name()='p']")))

    delar <- c(bild = antal("//*[local-name()='pic']"),
               tabell = antal("//*[local-name()='tbl']"),
               diagram = antal("//*[local-name()='graphicFrame']//*[local-name()='chart']"),
               SmartArt = antal("//*[local-name()='graphicFrame']//*[local-name()='relIds']"),
               figurgrupp = antal("/*/*[local-name()='cSld']/*[local-name()='spTree']/*[local-name()='grpSp']"))
    flertal <- c(bild = "bilder", tabell = "tabeller", diagram = "diagram", SmartArt = "SmartArt",
                 figurgrupp = "figurgrupper")
    delar <- delar[delar > 0]
    innehall <- paste0(delar, " ", ifelse(delar == 1, names(delar), flertal[names(delar)]), collapse = ", ")

    data.frame(
      slide = nr,
      rubrik = paste(rubrik, collapse = " "),
      text = if (length(ovrig_text) == 0) "" else ovrig_text[1],
      innehall = if (length(delar) == 0) "" else innehall,
      layout = layouter$name[match(slide$layout_name(), basename(layouter$filename))]
    )
  })
  do.call(rbind, rader)
}

# Png-filerna i en mapp med tidpunkten de senast ändrades - för att se vilka en diagramfunktion skapat.
intern_png_i_mapp <- function(mapp) {
  if (is.null(mapp) || !dir.exists(mapp)) return(NULL)
  filer <- list.files(mapp, pattern = "\\.png$", full.names = TRUE, ignore.case = TRUE)
  stats::setNames(file.mtime(filer), filer)
}

# Diagrammen från en körning av en diagramfunktion, som en lista med sökvägar till png-filer eller
# ggplot-objekt. I första hand de filer som heter som elementen i funktionens returvärde (så gör
# diagramfunktionerna i rddiagramkatalog), i andra hand de png-filer som tillkommit eller ändrats i
# mappen under körningen, i tredje hand de ggplot-objekt funktionen returnerat.
intern_diagram_fran_retur <- function(retur, mapp, bilder_fore) {
  if (intern_ar_ggplot(retur)) retur <- list(retur)

  if (!is.null(mapp)) {
    if (is.list(retur) && !is.null(names(retur))) {
      filer <- paste0(mapp, names(retur), ".png")
      if (all(file.exists(filer))) return(as.list(filer))
    }
    bilder_efter <- intern_png_i_mapp(mapp)
    nya <- names(bilder_efter)[!names(bilder_efter) %in% names(bilder_fore) |
                                 bilder_efter > bilder_fore[names(bilder_efter)]]
    nya <- nya[!is.na(nya)]
    if (length(nya) > 0) return(as.list(nya[order(bilder_efter[nya])]))
  }

  if (is.list(retur)) Filter(intern_ar_ggplot, unname(retur)) else list()
}
