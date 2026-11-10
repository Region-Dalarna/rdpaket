#' Börja på en ny presentation
#'
#' Första steget när en presentation byggs steg för steg: skapa den med
#' `ppt_ny()`, lägg till slides med [ppt_diagram()], [ppt_bild()],
#' [ppt_text()], [ppt_avsnitt()], [ppt_slutsatser()], [ppt_rapporter()],
#' [ppt_befintlig()] och [ppt_avslut()], och spara med [ppt_spara()].
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
#' @param bildmapp Mapp där diagrambilderna från [ppt_diagram()] sparas.
#'   `NULL` (standard) = en temporär mapp som du inte behöver bry dig om.
#' @param anv_uppgifter Vems namn/e-post som ska stå på titel- och
#'   avslutssidan: `NULL` (standard) = inloggad användare, ett namn som
#'   slås upp med [rddeploy::anv_hamta_namn_epost_fran_lista()], eller en
#'   lista `list(namn = "...", epost = "...")`.
#' @param rubrik_namn TRUE om presentatörens namn ska skrivas ut på
#'   titelsidan.
#'
#' @return En presentation (klassen `rdppt`) att bygga vidare på.
#' @examples
#' \dontrun{
#' ppt_ny("Arbetsmarknad och befolkning i Dalarna") |>
#'   ppt_diagram(rddiagramkatalog::diag_arbetsmarknadsstatus_tidsserie_scb,
#'               region_vekt = c("20", "2080", "2081")) |>
#'   ppt_slutsatser(c("Första slutsatsen", "Andra slutsatsen")) |>
#'   ppt_rapporter() |>
#'   ppt_avslut() |>
#'   ppt_spara("arbetsmarknad_befolkning.pptx")
#' }
#' @export
ppt_ny <- function(huvudrubrik = NULL,
                   underrubrik = intern_standard_underrubrik(),
                   mall = NULL,
                   bildmapp = NULL,
                   anv_uppgifter = NULL,
                   rubrik_namn = TRUE) {

  profil <- intern_mall_aktuell(mall)
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
      rubrik_namn = rubrik_namn,
      grundfil = NULL,
      slides = initiera_ppt_df()
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

#' Lägg till diagram från en diagramfunktion
#'
#' Kör en diagramfunktion (t.ex. ur `rddiagramkatalog`) och lägger till en
#' slide per diagram den skapar. Med `region_vekt` körs funktionen en gång
#' per region. Bilderna sparas och hittas automatiskt - du behöver inte
#' ange någon mapp.
#'
#' Funktionens parametrar `utmapp` och `skriv_diagramfil` sätts automatiskt
#' (till presentationens bildmapp respektive `TRUE`) om funktionen har dem
#' och du inte anger dem själv. Anger du `skriv_diagramfil = FALSE` läggs
#' diagrammen i stället in som redigerbar vektorgrafik direkt från de
#' ggplot-objekt funktionen returnerar.
#'
#' @param ppt En presentation från [ppt_ny()].
#' @param funktion Diagramfunktionen, t.ex.
#'   `rddiagramkatalog::diag_befolkningsprognos_profet_scb`.
#' @param ... Parametrar till diagramfunktionen.
#' @param region_vekt De kommun-/regionkoder funktionen ska köras för.
#'   `NULL` (standard) = funktionen körs en gång, utan region.
#' @param rubrik,text Rubrik och text på varje slide. Normalt behövs ingen
#'   - diagrammen har sina egna rubriker.
#' @param layout Namnet på den layout i mallen som ska användas, se
#'   [ppt_mall_visa()]. `NULL` (standard) = väljs automatiskt.
#' @param anteckningar Anteckningar till varje slide.
#' @param regionvariabel Namnet på funktionens parameter för region, om
#'   det inte är `"region_vekt"`.
#' @param per_region `TRUE` (standard) = funktionen körs en gång per
#'   region i `region_vekt`. `FALSE` = en gång, med hela `region_vekt`
#'   (t.ex. för ett diagram som jämför regionerna).
#' @param stoppa_vid_fel `FALSE` (standard) = om funktionen ger fel för
#'   en region hoppas den över med en varning. `TRUE` = stoppa.
#'
#' @return Presentationen, med nya slides tillagda.
#' @examples
#' \dontrun{
#' ppt <- ppt_ny("Befolkning") |>
#'   ppt_diagram(rddiagramkatalog::diag_befolkningsprognos_profet_scb,
#'               region_vekt = c("20", "2080"))
#' }
#' @export
ppt_diagram <- function(ppt,
                        funktion,
                        ...,
                        region_vekt = NULL,
                        rubrik = NULL,
                        text = NULL,
                        layout = NULL,
                        anteckningar = NULL,
                        regionvariabel = "region_vekt",
                        per_region = TRUE,
                        stoppa_vid_fel = FALSE) {

  intern_kontrollera_ppt(ppt)
  layout <- intern_kontrollera_layout(ppt, layout)

  funktion_namn <- if (is.character(funktion)) funktion else paste(deparse(substitute(funktion)), collapse = "")
  if (!grepl("^[a-zA-Z0-9_.:]+$", funktion_namn)) funktion_namn <- "diagramfunktionen"
  if (is.character(funktion)) funktion <- eval(parse(text = funktion))
  if (!is.function(funktion)) stop("funktion ska vara en funktion, t.ex. rddiagramkatalog::diag_befolkningsprognos_profet_scb.", call. = FALSE)

  parametrar <- names(formals(funktion))
  argument <- list(...)
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
        NULL
      }
    )
    if (is.null(retur)) next

    bilder <- intern_diagram_fran_retur(retur, mapp, bilder_fore)
    if (length(bilder) == 0) {
      warning(funktion_namn, " gav inga diagram", for_region_txt, ".", call. = FALSE)
      next
    }
    for (bild in bilder) {
      ppt <- intern_ppt_lagg_till(ppt, bild = bild, titel = rubrik, beskr = text, mall = layout, anteckningar = anteckningar)
    }
  }

  message(funktion_namn, ": ", nrow(ppt$slides) - antal_fore, " diagram tillagda.")
  ppt
}

#' Lägg till en bild eller ett diagram
#'
#' Lägger till en slide med en färdig bild (en png-fil) eller ett
#' ggplot-objekt. Flera bilder ger en slide per bild.
#'
#' @param ppt En presentation från [ppt_ny()].
#' @param bild Sökväg till en eller flera png-filer, ett ggplot-objekt,
#'   eller en lista med ggplot-objekt (t.ex. det en diagramfunktion
#'   returnerar). Ggplot-objekt läggs in som redigerbar vektorgrafik.
#' @param rubrik Rubrik på sliden.
#' @param text Text på sliden: en textsträng, eller en vektor som blir en
#'   punktlista.
#' @param layout Namnet på den layout i mallen som ska användas, se
#'   [ppt_mall_visa()]. `NULL` (standard) = väljs automatiskt efter om
#'   sliden har rubrik och text.
#' @param anteckningar Anteckningar till sliden.
#' @param txt_size Textstorlek. `NA` (standard) = mallens egen.
#'
#' @return Presentationen, med nya slides tillagda.
#' @export
ppt_bild <- function(ppt, bild, rubrik = NULL, text = NULL, layout = NULL, anteckningar = NULL, txt_size = NA) {
  intern_kontrollera_ppt(ppt)
  layout <- intern_kontrollera_layout(ppt, layout)

  if (intern_ar_ggplot(bild)) {
    bild <- list(bild)
  } else if (is.character(bild)) {
    saknas <- bild[!file.exists(bild)]
    if (length(saknas) > 0) stop("Hittar inte bildfilen:\n  ", paste(saknas, collapse = "\n  "), call. = FALSE)
    bild <- as.list(bild)
  } else if (!(is.list(bild) && length(bild) > 0 && all(vapply(bild, intern_ar_ggplot, logical(1))))) {
    stop("bild ska vara en sökväg till en png-fil, ett ggplot-objekt eller en lista med ggplot-objekt.", call. = FALSE)
  }

  for (en_bild in bild) {
    ppt <- intern_ppt_lagg_till(ppt, bild = en_bild, titel = rubrik, beskr = text, mall = layout,
                                anteckningar = anteckningar, txt_size = txt_size)
  }
  ppt
}

#' Lägg till en slide med rubrik och text
#'
#' @param ppt En presentation från [ppt_ny()].
#' @param rubrik Rubrik på sliden.
#' @param text Text på sliden: en vektor med ett element per stycke/punkt.
#'   För layouter med flera textrutor: en namngiven lista med ett element
#'   per platshållare, t.ex. `list("Platshållare för text 2" = "Vänster",
#'   "Platshållare för text 4" = "Höger")` - se vad de heter med
#'   `ppt_mall_visa(layout = "...")`.
#' @param layout Namnet på den layout i mallen som ska användas, se
#'   [ppt_mall_visa()]. `NULL` (standard) = mallens layout för rubrik och
#'   punktlista.
#' @param anteckningar Anteckningar till sliden.
#' @param txt_size Textstorlek. `NA` (standard) = mallens egen.
#'
#' @return Presentationen, med en ny slide tillagd.
#' @export
ppt_text <- function(ppt, rubrik = NULL, text = NULL, layout = NULL, anteckningar = NULL, txt_size = NA) {
  intern_kontrollera_ppt(ppt)
  layout <- intern_kontrollera_layout(ppt, layout)
  intern_ppt_lagg_till(ppt, titel = rubrik, beskr = text, mall = layout, anteckningar = anteckningar, txt_size = txt_size)
}

#' Lägg till en avsnittsrubrik
#'
#' En slide som inleder ett nytt avsnitt i presentationen, med stor
#' rubrik.
#'
#' @param ppt En presentation från [ppt_ny()].
#' @param rubrik Avsnittets rubrik.
#' @param text Eventuell text under rubriken.
#' @param anteckningar Anteckningar till sliden.
#'
#' @return Presentationen, med en ny slide tillagd.
#' @export
ppt_avsnitt <- function(ppt, rubrik, text = NULL, anteckningar = NULL) {
  intern_kontrollera_ppt(ppt)
  intern_ppt_lagg_till(ppt, titel = rubrik, beskr = text, mall = "roll:avsnitt", anteckningar = anteckningar)
}

#' Lägg till en slide med slutsatser
#'
#' En slide med rubrik och punktlista.
#'
#' @param ppt En presentation från [ppt_ny()].
#' @param punkter Slutsatserna, en vektor med ett element per punkt.
#' @param rubrik Slidens rubrik.
#' @param anteckningar Anteckningar till sliden.
#' @param txt_size Textstorlek. `NA` (standard) = mallens egen.
#'
#' @return Presentationen, med en ny slide tillagd.
#' @export
ppt_slutsatser <- function(ppt, punkter, rubrik = "Slutsatser", anteckningar = NULL, txt_size = NA) {
  intern_kontrollera_ppt(ppt)
  intern_ppt_lagg_till(ppt, titel = rubrik, beskr = punkter, mall = "Slutsatser_Samhallsanalys",
                       anteckningar = anteckningar, txt_size = txt_size)
}

#' Lägg till sidan med Samhällsanalys rapporter
#'
#' En slide med aktuella rapporter från regiondalarna.se i två spalter
#' (webbrapporter och pdf-rapporter), hämtade med
#' [rapporter_rd_webb_hamta()] när presentationen sparas. Går rapporterna
#' inte att hämta då hoppas sidan över.
#'
#' @param ppt En presentation från [ppt_ny()].
#' @param anteckningar Anteckningar till sliden.
#'
#' @return Presentationen, med en ny slide tillagd.
#' @export
ppt_rapporter <- function(ppt, anteckningar = NULL) {
  intern_kontrollera_ppt(ppt)
  intern_ppt_lagg_till(ppt, mall = "Rapporter_Samhallsanalys", anteckningar = anteckningar)
}

#' Lägg till en avslutsbild
#'
#' Samhällsanalys avslutsbild: "Tack för mig!" med ditt namn och din
#' e-post samt gruppens e-postadress, om du inte anger något annat.
#'
#' @param ppt En presentation från [ppt_ny()].
#' @param rubrik Rubrik. `NULL` (standard) = "Tack för mig!".
#' @param text Text under rubriken. `NULL` (standard) = namn och e-post.
#' @param anteckningar Anteckningar till sliden.
#'
#' @return Presentationen, med en ny slide tillagd.
#' @export
ppt_avslut <- function(ppt, rubrik = NULL, text = NULL, anteckningar = NULL) {
  intern_kontrollera_ppt(ppt)
  intern_ppt_lagg_till(ppt, titel = rubrik, beskr = text, mall = "Avslutsbild_Samhallsanalys", anteckningar = anteckningar)
}

#' Lägg in färdiga slides från en befintlig pptx-fil
#'
#' För slides som är svåra att skapa automatiskt: gör dem för hand i
#' PowerPoint, spara dem i en pptx-fil, och låt `ppt_befintlig()` lägga
#' in dem på rätt ställe i presentationen. De hamnar där i ordningen
#' `ppt_befintlig()` anropas - mellan de slides som läggs till före och
#' efter.
#'
#' Filen med de färdiga slidesen används som grund för hela
#' presentationen, så den ska vara skapad utifrån samma mall som resten
#' av presentationen (öppna mallen i PowerPoint, lägg till dina slides och
#' spara under ett nytt namn). Det går därför bara att hämta färdiga
#' slides från en fil per presentation. Slides i filen som inte läggs in
#' med `ppt_befintlig()` kommer inte med i presentationen.
#'
#' @param ppt En presentation från [ppt_ny()].
#' @param fil Sökväg till pptx-filen med färdiga slides. `NULL` (standard)
#'   = samma fil som senast, eller - första gången - mallfilen själv.
#' @param slide Vilka slides i filen som ska läggas in här, som nummer
#'   (t.ex. `2` eller `c(1, 3)`). `NULL` (standard) = alla.
#'   [ppt_befintlig_visa()] visar vilka slides filen innehåller och vilket
#'   nummer de har.
#'
#' @return Presentationen, med de färdiga slidesen tillagda.
#' @seealso [ppt_befintlig_visa()]
#' @examples
#' \dontrun{
#' fardiga <- "G:/skript/ppt/mall_rus_utan_bakgrund_demografi.pptx"
#'
#' ppt_ny("Demografi i Dalarna") |>
#'   ppt_diagram(rddiagramkatalog::diag_befolkningsprognos_profet_scb, region_vekt = "20") |>
#'   ppt_befintlig(fardiga, slide = 1:2) |>   # de två första färdiga slidesen hamnar här
#'   ppt_slutsatser(c("Första slutsatsen", "Andra slutsatsen")) |>
#'   ppt_befintlig(slide = 3) |>              # och den tredje här
#'   ppt_avslut() |>
#'   ppt_spara("demografi.pptx")
#' }
#' @export
ppt_befintlig <- function(ppt, fil = NULL, slide = NULL) {
  intern_kontrollera_ppt(ppt)

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
#' Numret i kolumnen `slide` är det du anger i `ppt_befintlig(slide = )`.
#'
#' @param fil Sökväg till pptx-filen med färdiga slides.
#' @param oppna TRUE för att också öppna filen i PowerPoint.
#'
#' @return Osynligt: en `data.frame` med en rad per slide och kolumnerna
#'   `slide`, `rubrik`, `text`, `innehall` och `layout` (texterna i sin
#'   helhet - i utskriften är de avkortade).
#' @seealso [ppt_befintlig()]
#' @examples
#' \dontrun{
#' ppt_befintlig_visa("G:/skript/ppt/mall_rus_utan_bakgrund_demografi.pptx")
#' }
#' @export
ppt_befintlig_visa <- function(fil, oppna = FALSE) {
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
  cat("\nLägg in dem med ppt_befintlig(fil, slide = <nummer>).\n")

  if (isTRUE(oppna)) intern_oppna_fil(fil)
  invisible(slides)
}

#' Spara presentationen som en pptx-fil
#'
#' Sista steget: bygger presentationen och skriver den till fil.
#'
#' @param ppt En presentation från [ppt_ny()].
#' @param filnamn Filnamn, eller en hel sökväg, på pptx-filen. `NULL`
#'   (standard) = ett filnamn som bildas av huvudrubriken.
#' @param mapp Mapp att spara i, om `filnamn` inte är en hel sökväg.
#' @param oppna `TRUE` om presentationen ska öppnas direkt.
#' @param spara_backup_om_fil_redan_finns `TRUE` om en befintlig fil med
#'   samma namn ska säkerhetskopieras i stället för att skrivas över.
#'
#' @return Sökvägen till den sparade presentationen, osynligt.
#' @export
ppt_spara <- function(ppt,
                      filnamn = NULL,
                      mapp = rdverktyg::utskriftsmapp(),
                      oppna = FALSE,
                      spara_backup_om_fil_redan_finns = FALSE) {

  intern_kontrollera_ppt(ppt)

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

  fil <- skapa_ppt_fran_lista(
    pptlista = ppt$slides,
    huvudrubrik = ppt$huvudrubrik,
    underrubrik = ppt$underrubrik,
    output_mapp = mapp,
    spara_backup_om_fil_redan_finns = spara_backup_om_fil_redan_finns,
    filnamn = filnamn,
    mall_fil = intern_mall_aktuell(ppt$profil, grundfil = ppt$grundfil),
    rubrik_namn = ppt$rubrik_namn,
    anv_uppgifter = ppt$anv_uppgifter
  )
  if (isTRUE(oppna)) intern_oppna_fil(fil)
  invisible(fil)
}

intern_kontrollera_ppt <- function(ppt) {
  if (!inherits(ppt, "rdppt")) {
    stop("Första argumentet ska vara en presentation skapad med ppt_ny(), t.ex. ppt_ny(\"Rubrik\") |> ppt_bild(...).", call. = FALSE)
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
                                 anteckningar = NULL, txt_size = NA) {
  rad <- data.frame(
    dia_path = if (is.character(bild)) bild else NA_character_,
    titel = if (intern_har_innehall(titel)) as.character(titel)[1] else NA_character_,
    master = NA_character_,
    mall = mall,
    txt_size = as.double(txt_size)
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
