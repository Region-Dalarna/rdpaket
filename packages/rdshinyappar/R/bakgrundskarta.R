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
#' @param etiketter `TRUE` (standard) för bakgrund med ortnamn, vägnummer och
#'   liknande text. `FALSE` tar bort all text, för kartor där du lägger dina
#'   egna etiketter ovanpå.
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
#' }
#' @export
lagg_till_bakgrundskarta <- function(karta, stil = "positron", etiketter = TRUE) {
  if (!inherits(karta, "leaflet")) {
    stop("karta ska vara ett leaflet-objekt, t.ex. skapat med leaflet::leaflet().", call. = FALSE)
  }
  if (!is.character(stil) || length(stil) != 1 || is.na(stil)) {
    stop("stil ska vara \"positron\", \"bright\", \"liberty\" eller en url till en MapLibre-stil.", call. = FALSE)
  }

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

  # Stilen hämtas som json så att etiketterna (lager av typen "symbol") kan tas bort innan den
  # används. Misslyckas hämtningen (t.ex. utan internet) visas kartan utan bakgrund.
  js <- "function(el, x, data) {
    var karta = this;
    fetch(data.stil_url)
      .then(function(svar) { return svar.json(); })
      .then(function(stil) {
        if (!data.etiketter) {
          stil.layers = stil.layers.filter(function(lager) { return lager.type !== 'symbol'; });
        }
        L.maplibreGL({style: stil, attribution: data.attribution}).addTo(karta);
      })
      .catch(function(fel) { console.warn('Bakgrundskartan kunde inte hamtas:', fel); });
  }"

  htmlwidgets::onRender(karta, js, data = list(
    stil_url = stil_url,
    etiketter = isTRUE(etiketter),
    attribution = paste0(
      '<a href="https://openfreemap.org" target="_blank">OpenFreeMap</a> ',
      '&copy; <a href="https://www.openmaptiles.org/" target="_blank">OpenMapTiles</a> ',
      'Data from <a href="https://www.openstreetmap.org/copyright" target="_blank">OpenStreetMap</a>')
  ))
}
