#' Lägg till en bakgrundskarta i en leaflet-karta
#'
#' Lägger en ljus, neutral bakgrundskarta under en leaflet-karta, i stil med
#' CartoDB Positron. Kartan hämtas från [OpenFreeMap](https://openfreemap.org),
#' som bygger på OpenStreetMap-data och är gratis att använda **utan
#' API-nyckel och utan begränsning av antalet anrop**. OpenFreeMap levererar
#' vektorkartor, och därför följer det JavaScript som visar dem
#' ([MapLibre GL JS](https://maplibre.org)) med i paketet och bäddas in i
#' kartan - inget hämtas från något CDN.
#'
#' Kartan ritas när webbsidan visas, så den kräver internetuppkoppling i
#' webbläsaren för att bakgrunden ska synas. Utan uppkoppling visas resten av
#' kartan som vanligt, på en tom bakgrund.
#'
#' I en Shiny-app används funktionen där kartan byggs, i `renderLeaflet()`,
#' i stället för `addTiles()` eller `addProviderTiles()`. Ändringar via
#' `leafletProxy()` (t.ex. `clearShapes()` eller nya polygoner) rör inte
#' bakgrunden. Samma funktion finns också i paketet rddiagram.
#'
#' @param karta Ett leaflet-objekt, t.ex. `leaflet::leaflet()`.
#' @param stil Vilken stil bakgrunden ska ha: `"positron"` (standard, ljusgrå
#'   och diskret), `"bright"` eller `"liberty"` (mer färg), eller en url
#'   till en egen MapLibre-stil (en json-fil).
#' @param etiketter Vilken text bakgrunden ska ha: `TRUE` (standard) = ortnamn,
#'   vägnamn, vattennamn m.m., `"orter"` = bara namn på orter, städer, län och
#'   länder (inga vägar, vatten eller flygplatser), `FALSE` = ingen text, för
#'   kartor där du lägger dina egna etiketter ovanpå.
#' @param etikett_opacitet Hur kraftig texten ska vara, mellan 0 (osynlig) och 1
#'   (som i originalstilen). Standard är `0.5`, en diskret text som inte tar
#'   uppmärksamhet från det som ritas ovanpå kartan.
#' @param sprak Språk för namnen på kartan, som språkkod: `"sv"` (standard,
#'   svenska), `"en"`, `"de"` osv. Där det valda språket saknas används engelska
#'   och därefter ortens eget namn. `NULL` behåller stilens originaltext
#'   (engelska).
#' @param landsnamn `FALSE` (standard) döljer landsnamn (t.ex. "Sverige"), `TRUE`
#'   visar dem. Övrig text påverkas inte.
#'
#' @return Leaflet-objektet, med bakgrundskartan tillagd.
#' @examples
#' \dontrun{
#' leaflet::leaflet() |>
#'   lagg_till_bakgrundskarta() |>
#'   leaflet::setView(lng = 15.63, lat = 60.61, zoom = 8)
#'
#' # utan text, för en karta med egna etiketter
#' leaflet::leaflet() |> lagg_till_bakgrundskarta(etiketter = FALSE)
#'
#' # med landsnamn och full textstyrka (originalstilens text)
#' leaflet::leaflet() |> lagg_till_bakgrundskarta(landsnamn = TRUE, etikett_opacitet = 1)
#'
#' # bara ortnamn
#' leaflet::leaflet() |> lagg_till_bakgrundskarta(etiketter = "orter")
#' }
#' @export
lagg_till_bakgrundskarta <- function(karta, stil = "positron", etiketter = TRUE, etikett_opacitet = 0.5,
                                     landsnamn = FALSE, sprak = "sv") {
  if (!inherits(karta, "leaflet")) {
    stop("karta ska vara ett leaflet-objekt, t.ex. skapat med leaflet::leaflet().", call. = FALSE)
  }
  if (!is.character(stil) || length(stil) != 1 || is.na(stil)) {
    stop("stil ska vara \"positron\", \"bright\", \"liberty\" eller en url till en MapLibre-stil.", call. = FALSE)
  }

  if (!(isTRUE(etiketter) || isFALSE(etiketter) || identical(etiketter, "orter"))) {
    stop("etiketter ska vara TRUE, FALSE eller \"orter\".", call. = FALSE)
  }
  if (!is.numeric(etikett_opacitet) || length(etikett_opacitet) != 1 || is.na(etikett_opacitet) ||
      etikett_opacitet < 0 || etikett_opacitet > 1) {
    stop("etikett_opacitet ska vara ett tal mellan 0 och 1.", call. = FALSE)
  }

  if (!is.null(sprak) && !(is.character(sprak) && length(sprak) == 1 && grepl("^[a-z]{2,3}(-[A-Za-z]+)?$", sprak))) {
    stop("sprak ska vara en språkkod som \"sv\" eller \"en\", eller NULL.", call. = FALSE)
  }
  if (!(isTRUE(landsnamn) || isFALSE(landsnamn))) stop("landsnamn ska vara TRUE eller FALSE.", call. = FALSE)

  stil_url <- if (stil %in% c("positron", "bright", "liberty")) {
    paste0("https://tiles.openfreemap.org/styles/", stil)
  } else if (grepl("^https://", stil)) {
    stil
  } else {
    stop("Okänd stil: '", stil, "'. Ange \"positron\", \"bright\", \"liberty\" eller en https-url till en MapLibre-stil.", call. = FALSE)
  }

  mapp <- system.file("leaflet-maplibre", package = "rdshinyappar")
  if (!nzchar(mapp)) stop("Hittar inte de medföljande MapLibre-filerna - installera om rdshinyappar.", call. = FALSE)

  karta$dependencies <- c(karta$dependencies, list(
    htmltools::htmlDependency("maplibre-gl", "4.7.1", src = c(file = mapp),
                              script = "maplibre-gl.js", stylesheet = "maplibre-gl.css"),
    htmltools::htmlDependency("maplibre-gl-leaflet", "0.0.22", src = c(file = mapp),
                              script = "leaflet-maplibre-gl.js")
  ))

  # Stilen hämtas som json så att etiketterna (lager av typen "symbol") kan tas bort eller tonas ner
  # innan den används. Misslyckas hämtningen (t.ex. utan internet) visas kartan utan bakgrund.
  js <- "function(el, x, data) {
    var karta = this;
    fetch(data.stil_url)
      .then(function(svar) { return svar.json(); })
      .then(function(stil) {
        if (data.etiketter === 'inga') {
          stil.layers = stil.layers.filter(function(lager) { return lager.type !== 'symbol'; });
        } else if (data.etiketter === 'orter') {
          stil.layers = stil.layers.filter(function(lager) {
            return lager.type !== 'symbol' || lager['source-layer'] === 'place';
          });
        }
        if (data.sprak) {
          // namnen väljs på valt språk först, sedan engelska och slutligen ortens eget namn
          var byt = function(uttryck) {
            if (Array.isArray(uttryck)) {
              if (uttryck.length === 3 && uttryck[0] === 'coalesce' &&
                  JSON.stringify(uttryck[1]) === '[\"get\",\"name_en\"]' &&
                  JSON.stringify(uttryck[2]) === '[\"get\",\"name\"]') {
                return ['coalesce', ['get', 'name:' + data.sprak], uttryck[1], uttryck[2]];
              }
              return uttryck.map(byt);
            }
            return uttryck;
          };
          stil.layers.forEach(function(lager) {
            if (lager.type === 'symbol' && lager.layout && lager.layout['text-field']) {
              lager.layout['text-field'] = byt(lager.layout['text-field']);
            }
          });
        }
        if (!data.landsnamn) {
          stil.layers = stil.layers.filter(function(lager) { return !/^label_country/.test(lager.id); });
        }
        if (data.etikett_opacitet < 1) {
          stil.layers.forEach(function(lager) {
            if (lager.type === 'symbol') {
              lager.paint = lager.paint || {};
              lager.paint['text-opacity'] = data.etikett_opacitet;
              lager.paint['icon-opacity'] = data.etikett_opacitet;
            }
          });
        }
        L.maplibreGL({style: stil, attribution: data.attribution}).addTo(karta);
      })
      .catch(function(fel) { console.warn('Bakgrundskartan kunde inte hamtas:', fel); });
  }"

  htmlwidgets::onRender(karta, js, data = list(
    stil_url = stil_url,
    etiketter = if (isFALSE(etiketter)) "inga" else if (identical(etiketter, "orter")) "orter" else "alla",
    etikett_opacitet = etikett_opacitet,
    landsnamn = landsnamn,
    sprak = if (is.null(sprak)) FALSE else sprak,
    attribution = paste0(
      '<a href="https://openfreemap.org" target="_blank">OpenFreeMap</a> ',
      '&copy; <a href="https://www.openmaptiles.org/" target="_blank">OpenMapTiles</a> ',
      'Data from <a href="https://www.openstreetmap.org/copyright" target="_blank">OpenStreetMap</a>')
  ))
}
