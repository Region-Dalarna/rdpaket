# De sorters slides som ppt_lagg_till_slide() kan lägga till, med de parametrar som gäller för var och en och
# vilka som krävs. En slide väljs med layout = "<typ>"; en layout i mallen väljs med layoutens eget namn.
intern_diagramparametrar <- c("funktion", "funktionsparametrar", "region_vekt", "regionvariabel", "per_region", "stoppa_vid_fel")

intern_slidetyper <- list(
  diagram = list(
    beskr = "ett diagram (en slide per diagram) från en diagramfunktion, t.ex. i rddiagramkatalog",
    param = c(intern_diagramparametrar, "rubrik", "text", "anteckningar"),
    krav = "funktion"),
  bild = list(
    beskr = "en färdig bild (png-fil) eller ett ggplot-objekt",
    param = c("bild", "rubrik", "text", "anteckningar", "txt_size"),
    krav = "bild"),
  text = list(
    beskr = "rubrik och text eller punktlista",
    param = c("rubrik", "text", "anteckningar", "txt_size"),
    krav = character(0)),
  slutsatser = list(
    beskr = "rubrik (standard \"Slutsatser\") och punktlista",
    param = c("text", "rubrik", "anteckningar", "txt_size"),
    krav = "text"),
  avsnitt = list(
    beskr = "avsnittsrubrik som inleder ett nytt avsnitt",
    param = c("rubrik", "text", "anteckningar"),
    krav = "rubrik"),
  rapporter = list(
    beskr = "Samhällsanalys senaste rapporter (hämtas från regiondalarna.se när presentationen sparas)",
    param = c("antal_rapporter", "anteckningar"),
    krav = character(0)),
  avslut = list(
    beskr = "avslutsbild (standard \"Tack för mig!\" med namn och e-post)",
    param = c("rubrik", "text", "anteckningar"),
    krav = character(0)),
  befintlig = list(
    beskr = "färdiga slides från en pptx-fil (en slides-bank), se ppt_befintlig_visa()",
    param = c("fil", "slide"),
    krav = character(0))
)

#' Vilka sorters slides går att lägga till?
#'
#' Skriver ut de sorters slides som [ppt_lagg_till_slide()] kan lägga till,
#' med vad var och en är till för och vilka parametrar som gäller för den.
#' Utöver dessa går varje layout i mallen att använda genom att ange dess
#' namn som `layout`, se [ppt_mall_visa()].
#'
#' @return En `data.frame` med kolumnerna `layout`, `beskrivning` och
#'   `parametrar`, osynligt.
#' @seealso [ppt_lagg_till_slide()], [ppt_mall_visa()]
#' @examples
#' ppt_slidetyper()
#' @export
ppt_slidetyper <- function() {
  res <- data.frame(
    layout = names(intern_slidetyper),
    beskrivning = vapply(intern_slidetyper, function(x) x$beskr, character(1)),
    parametrar = vapply(intern_slidetyper, function(x) {
      paste0(ifelse(x$param %in% x$krav, paste0(x$param, "*"), x$param), collapse = ", ")
    }, character(1)),
    row.names = NULL, stringsAsFactors = FALSE
  )
  print(res, right = FALSE, row.names = FALSE)
  cat("\n* = måste anges. Parametrar till diagramfunktionen anges också, med namn.\n",
      "Layouterna i mallen går att använda med sitt eget namn som layout, t.ex. layout = \"Två innehållsdelar\" - se ppt_mall_visa().\n",
      sep = "")
  invisible(res)
}

#' Lägg till en slide
#'
#' Lägger till en slide i presentationen. `layout` anger vilken sorts slide
#' det är, och vilka övriga parametrar som gäller beror på den - se alla
#' sorter och deras parametrar med [ppt_slidetyper()]:
#'
#' | `layout` | Slide | Parametrar |
#' |---|---|---|
#' | `"diagram"` | diagram från en diagramfunktion | `funktion`*, parametrar till funktionen, `region_vekt`, `rubrik`, `text` |
#' | `"bild"` | en png-fil eller ett ggplot-objekt | `bild`*, `rubrik`, `text` |
#' | `"text"` | rubrik och text/punktlista | `rubrik`, `text` |
#' | `"slutsatser"` | rubrik ("Slutsatser") och punktlista | `text`*, `rubrik` |
#' | `"avsnitt"` | avsnittsrubrik | `rubrik`*, `text` |
#' | `"rapporter"` | Samhällsanalys rapporter i tre spalter | `antal_rapporter` |
#' | `"avslut"` | avslutsbild | `rubrik`, `text` |
#' | `"befintlig"` | färdiga slides från en pptx-fil | `fil`, `slide` |
#'
#' Alla utom `"befintlig"` kan också ha `anteckningar`.
#'
#' **Diagram.** Diagramfunktionen körs en gång per region i `region_vekt`
#' och bilderna sparas och hittas automatiskt - du behöver inte ange någon
#' mapp. Funktionens parametrar `utmapp` och `skriv_diagramfil` sätts
#' automatiskt (till presentationens bildmapp respektive `TRUE`) om
#' funktionen har dem och du inte anger dem själv. Med
#' `skriv_diagramfil = FALSE` läggs diagrammen in som redigerbar
#' vektorgrafik. Ger funktionen fel för en region hoppas den över med en
#' varning (och loggas, se `loggfil` i [ppt_ny()]), och resten av
#' presentationen byggs ändå; `stoppa_vid_fel = TRUE` stoppar i stället.
#' Parametrar som funktionen inte har ger ett fel direkt.
#'
#' **Färdiga slides.** `layout = "befintlig"` lägger in slides som gjorts
#' för hand i PowerPoint. Filen med dem används som grund för hela
#' presentationen, så den ska vara skapad utifrån samma mall som resten
#' (öppna mallen, lägg till dina slides och spara under ett nytt namn).
#' Färdiga slides kan därför bara hämtas från en fil per presentation, och
#' slides i filen som inte läggs in kommer inte med.
#'
#' Man kan också ange namnet på en layout i mallen, t.ex.
#' `layout = "Två innehållsdelar"`. Sliden får då den layouten, och
#' innehållet blir det man anger: `funktion` (diagram), `bild`, eller
#' `rubrik` och `text`. Vilka layouter mallen har visar [ppt_mall_visa()].
#'
#' @param ppt En presentation från [ppt_ny()].
#' @param layout Vilken sorts slide, eller en layout i mallen. Se ovan.
#' @param rubrik Slidens rubrik.
#' @param text Text på sliden: en textsträng, eller en vektor som blir en
#'   punktlista (för `"slutsatser"` är det punkterna). För layouter med
#'   flera textrutor: en namngiven lista med ett element per
#'   platshållare, se [ppt_mall_visa()].
#' @param bild Sökväg till en eller flera png-filer, ett ggplot-objekt eller
#'   en lista med ggplot-objekt. Ggplot-objekt läggs in som redigerbar
#'   vektorgrafik. Flera bilder ger en slide per bild.
#' @param funktion Diagramfunktionen, t.ex.
#'   `rddiagramkatalog::diag_befolkningsprognos_profet_scb`.
#' @param ... Parametrar till diagramfunktionen, med namn. Parametrarna
#'   `utmapp` och `skriv_diagramfil` sätts automatiskt om funktionen har
#'   dem och du inte anger dem själv.
#' @param region_vekt De kommun-/regionkoder diagramfunktionen ska köras
#'   för, en gång per region. `NULL` (standard) = en gång, utan region.
#'   En tom vektor (`character(0)`, t.ex. när det inte finns några
#'   kommunkoder) betyder att diagrammet hoppas över.
#' @param regionvariabel Namnet på diagramfunktionens parameter för region,
#'   om det inte är `"region_vekt"`.
#' @param per_region `TRUE` (standard) = funktionen körs en gång per region
#'   i `region_vekt`. `FALSE` = en gång med alla regionerna (t.ex. för ett
#'   diagram som jämför regionerna).
#' @param stoppa_vid_fel `FALSE` (standard) = om diagramfunktionen ger fel
#'   för en region hoppas den över med en varning. `TRUE` = stoppa.
#' @param funktionsparametrar En namngiven lista med parametrar till
#'   diagramfunktionen, för de som har samma namn som parametrar till
#'   `ppt_lagg_till_slide()` själv - framför allt när funktionens egen
#'   `region_vekt` ska ha ett fast värde medan `regionvariabel` styr vad
#'   som körs per region, t.ex.
#'   `funktionsparametrar = list(region_vekt = c("20", "2021", "2023"))`.
#' @param fil,slide För `"befintlig"`: pptx-filen med färdiga slides
#'   (standard: den som anges i `options(rdppt.slidebank = )`) och vilka
#'   slides i den som ska läggas in, som nummer (t.ex. `2` eller `c(1, 3)`,
#'   standard alla). [ppt_befintlig_visa()] visar vilka slides filen
#'   innehåller och vilket nummer de har. Slidesen hamnar på den plats i
#'   ordningen där du anropar funktionen.
#' @param anteckningar Text i slidens anteckningsfält (under sliden i
#'   PowerPoint): en textsträng, eller en vektor med ett stycke per
#'   element. För diagram kan det också vara en vektor eller lista med
#'   ett namn per region (namnen är regionkoderna, t.ex.
#'   `c("20" = "Text för Dalarna", "2080" = "Text för Falun")`), där varje
#'   regions diagram får sin egen text. Elementet för en region kan
#'   i sin tur ha flera stycken (en lista med textvektorer). Går också
#'   med en funktion av regionen, `\(region) mina_anteckningar(region)`,
#'   som körs en gång per region.
#' @param txt_size Textstorlek. `NA` (standard) = mallens egen.
#' @param antal_rapporter För `"rapporter"`: högst så många rapporter i varje spalt (interaktiva rapporter,
#'   analysrapporter och publicerade rapporter). Standard 6. Finns det fler markeras det som ett urval.
#'
#' @return Presentationen, med nya slides tillagda.
#' @seealso [ppt_slidetyper()], [ppt_mall_visa()], [ppt_befintlig_visa()]
#' @examples
#' \dontrun{
#' ppt_ny("Befolkning i Dalarna") |>
#'   ppt_lagg_till_slide("avsnitt", rubrik = "Befolkningsutveckling") |>
#'   ppt_lagg_till_slide("diagram",
#'                       funktion = rddiagramkatalog::diag_befolkningsprognos_profet_scb,
#'                       region_vekt = c("20", "2080"),
#'                       anteckningar = "Prognosen bygger på ...") |>
#'   ppt_lagg_till_slide("slutsatser", text = c("Första slutsatsen", "Andra slutsatsen")) |>
#'   ppt_lagg_till_slide("rapporter") |>
#'   ppt_lagg_till_slide("avslut") |>
#'   ppt_spara("befolkning.pptx")
#' }
#' @export
ppt_lagg_till_slide <- function(ppt,
                                layout = NULL,
                                rubrik = NULL,
                                text = NULL,
                                bild = NULL,
                                funktion = NULL,
                                ...,
                                region_vekt = NULL,
                                regionvariabel = "region_vekt",
                                per_region = TRUE,
                                stoppa_vid_fel = FALSE,
                                funktionsparametrar = NULL,
                                fil = NULL,
                                slide = NULL,
                                anteckningar = NULL,
                                txt_size = NA,
                                antal_rapporter = 6) {

  intern_kontrollera_ppt(ppt)
  if (is.null(layout)) {
    stop("Ange vilken sorts slide som ska läggas till med layout, t.ex. ppt_lagg_till_slide(ppt, \"diagram\", ...).\n",
         "Sorter: ", paste0("\"", names(intern_slidetyper), "\"", collapse = ", "), ". Se ppt_slidetyper().", call. = FALSE)
  }
  if (!is.character(layout) || length(layout) != 1 || is.na(layout)) {
    stop("layout ska vara en sorts slide (se ppt_slidetyper()) eller namnet på en layout i mallen (se ppt_mall_visa()).", call. = FALSE)
  }

  # en sorts slide, eller en layout i mallen
  typ <- if (layout %in% names(intern_slidetyper)) layout
    else if (!layout %in% ppt$layouter && tolower(layout) %in% names(intern_slidetyper)) tolower(layout)
    else NULL
  if (is.null(typ)) {
    if (!layout %in% ppt$layouter) {
      stop("'", layout, "' är varken en sorts slide eller en layout i mallen.\n",
           "Sorter: ", paste0("\"", names(intern_slidetyper), "\"", collapse = ", "), " (se ppt_slidetyper()).\n",
           "Layouter i mallen: ", paste0("'", ppt$layouter, "'", collapse = ", "), " (se ppt_mall_visa()).", call. = FALSE)
    }
    mall <- layout
    # en layout i mallen får det innehåll som anges
    effektiv <- if (!is.null(funktion)) "diagram" else if (!is.null(bild)) "bild" else "text"
  } else {
    mall <- NA_character_
    effektiv <- typ
  }
  beskrivning <- intern_slidetyper[[effektiv]]

  # kontrollera att bara parametrar som gäller för sliden angetts
  angivna <- setdiff(names(match.call(expand.dots = FALSE))[-1], c("ppt", "layout", "..."))
  tillatna <- beskrivning$param
  fel <- setdiff(angivna, tillatna)
  if (length(fel) > 0) {
    stop(if (is.null(typ)) paste0("layout = '", layout, "' med ", if (effektiv == "diagram") "funktion" else if (effektiv == "bild") "bild" else "text") else paste0("layout = \"", typ, "\""),
         " har ingen parameter som heter ", paste0("'", fel, "'", collapse = ", "),
         ". Parametrar som gäller: ", paste(tillatna, collapse = ", "), ".\nSe ppt_slidetyper().", call. = FALSE)
  }
  saknas <- setdiff(beskrivning$krav, angivna)
  if (length(saknas) > 0) {
    stop("layout = \"", effektiv, "\" kräver ", paste(saknas, collapse = ", "), ".\nSe ppt_slidetyper().", call. = FALSE)
  }
  argument <- list(...)
  if (!is.null(funktionsparametrar)) {
    if (!is.list(funktionsparametrar) || is.null(names(funktionsparametrar)) || any(names(funktionsparametrar) == "")) {
      stop("funktionsparametrar ska vara en namngiven lista, t.ex. list(region_vekt = c(\"20\", \"2021\")).", call. = FALSE)
    }
    dubbla <- intersect(names(argument), names(funktionsparametrar))
    if (length(dubbla) > 0) {
      stop("Parametern ", paste0("'", dubbla, "'", collapse = ", "), " anges både direkt och i funktionsparametrar.", call. = FALSE)
    }
    argument <- c(argument, funktionsparametrar)
  }
  if (length(argument) > 0 && effektiv != "diagram") {
    stop("Parametrar utöver de som gäller för sliden kan bara anges för diagram (parametrar till diagramfunktionen). ",
         "Se ppt_slidetyper().", call. = FALSE)
  }
  if (is.function(anteckningar) && effektiv != "diagram") {
    stop("anteckningar som funktion (en funktion av regionen) går bara för diagram.", call. = FALSE)
  }
  if (!is.function(anteckningar) && intern_anteckningar_per_region(anteckningar) && effektiv != "diagram") {
    stop("anteckningar med ett namn per region går bara för diagram. Ange en text utan namn.", call. = FALSE)
  }
  if (!is.null(txt_size) && !is.na(txt_size) && !"txt_size" %in% tillatna) {
    stop("txt_size gäller inte för den här sortens slide.", call. = FALSE)
  }

  switch(
    effektiv,
    diagram = {
      funktion_namn <- if (is.character(funktion)) funktion else paste(deparse(substitute(funktion)), collapse = "")
      intern_ppt_diagram(ppt, funktion, funktion_namn, argument, region_vekt = region_vekt, rubrik = rubrik, text = text,
                         layout = mall, anteckningar = anteckningar, regionvariabel = regionvariabel,
                         per_region = per_region, stoppa_vid_fel = stoppa_vid_fel)
    },
    bild = intern_lagg_till_bilder(ppt, bild, rubrik, text, mall, anteckningar, txt_size),
    text = intern_ppt_lagg_till(ppt, titel = rubrik, beskr = text, mall = mall, anteckningar = anteckningar, txt_size = txt_size),
    slutsatser = intern_ppt_lagg_till(ppt, titel = if (is.null(rubrik)) "Slutsatser" else rubrik, beskr = text,
                                      mall = "Slutsatser_Samhallsanalys", anteckningar = anteckningar, txt_size = txt_size),
    avsnitt = intern_ppt_lagg_till(ppt, titel = rubrik, beskr = text, mall = "roll:avsnitt", anteckningar = anteckningar),
    rapporter = {
      if (length(antal_rapporter) != 1 || is.na(antal_rapporter) || !is.numeric(antal_rapporter) || antal_rapporter < 1) {
        stop("antal_rapporter ska vara ett heltal på minst 1.", call. = FALSE)
      }
      intern_ppt_lagg_till(ppt, mall = "Rapporter_Samhallsanalys", anteckningar = anteckningar,
                           antal = as.integer(antal_rapporter))
    },
    avslut = intern_ppt_lagg_till(ppt, titel = rubrik, beskr = text, mall = "Avslutsbild_Samhallsanalys", anteckningar = anteckningar),
    befintlig = intern_ppt_befintlig(ppt, fil = fil, slide = slide)
  )
}

# En slide per bild: png-filer, ett ggplot-objekt eller en lista med ggplot-objekt.
intern_lagg_till_bilder <- function(ppt, bild, rubrik, text, mall, anteckningar, txt_size) {
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
    ppt <- intern_ppt_lagg_till(ppt, bild = en_bild, titel = rubrik, beskr = text, mall = mall,
                                anteckningar = anteckningar, txt_size = txt_size)
  }
  ppt
}
