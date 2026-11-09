#' Karta: arbetspendling mellan kommuner
#'
#' Flödeskartor över arbetspendling (RAMS/BAS) mellan en eller flera valda
#' kommuner och övriga kommuner, för en eller flera pendlingstyper
#' (bruttopendling, nettopendling, in- eller utpendling). Till skillnad
#' från [karta_befolkningsprognos_scb()] kan den här funktionen producera
#' FLERA kartor i ett och samma anrop - en kombination av `vald_kommun_kod`
#' och `vald_pendlingstyp` i taget, precis som källskriptets batch-mönster.
#'
#' ggplot-versionen av varje karta är en enklare, regional choroplettkarta
#' över nettopendling per kommun (inpendlare minus utpendlare, oavsett
#' vald `vald_pendlingstyp`) i det län den valda kommunen ligger i - inte
#' samma flödeslinjer som leaflet-versionen. De två versionerna visar
#' alltså kompletterande vyer, inte samma information i två format. Är
#' flera av de valda kommunerna i samma län återanvänds samma
#' ggplot-karta för dem i stället för att räknas om i onödan.
#'
#' leaflet-versionen behåller källskriptets detaljerade flödeslinjer
#' (linjebredd = antal pendlare, färg = pendlingstyp/nettotecken) till och
#' från den specifika kommunen.
#'
#' @param vald_kommun_kod En eller flera kommunkoder.
#' @param valt_ar År. `"9999"` (standard) = senaste tillgängliga.
#' @param vald_pendlingstyp En eller flera av `"Bruttopendling"`,
#'   `"Nettopendling"`, `"Inpendling"`, `"Utpendling"`, eller `"alla"`
#'   (standard) för samtliga fyra.
#' @param grans_for_antal_pendlare Pendlingsrelationer med färre pendlare
#'   än detta filtreras bort.
#' @param legend_intervaller Antal intervaller i leaflet-legenden.
#' @param linjefarg_positiva_tal,linjefarg_negativa_tal Linjefärg vid
#'   nettopendling, beroende på tecken. Återanvänds även som
#'   ggplot-choroplettens färgskala (positiv/negativ nettopendling).
#' @param linjefarg_utpendling,linjefarg_inpendling Linjefärg vid ren
#'   in- eller utpendling.
#' @param linjebredd_min,linjebredd_max Linjebredd (px) för minsta
#'   respektive största antal pendlare.
#' @param utmapp Mapp där kartfilerna sparas. Måste finnas om
#'   `skriv_diagramfil`, `skriv_leaflet_fil` eller `skriv_gpkg_fil` är
#'   `TRUE`.
#' @param skriv_diagramfil TRUE om ggplot-choropletten ska skrivas som
#'   bildfil.
#' @param skriv_leaflet_fil TRUE om leaflet-flödeskartan ska skrivas till
#'   fil.
#' @param leaflet_filformat `"html"` (standard, interaktiv) eller `"png"`
#'   (statisk ögonblicksbild via webshot2, se
#'   [rddiagram::skriv_leaflet_till_fil()]).
#' @param skriv_gpkg_fil TRUE om pendlingslinjerna (för den aktuella
#'   kombinationen) dessutom ska skrivas som en GeoPackage-fil
#'   (SWEREF99 TM, EPSG:3006) med linjefärg/-bredd som attribut, för
#'   vidare GIS-bearbetning i t.ex. QGIS/ArcGIS. Oberoende av
#'   `skriv_leaflet_fil` - båda kan vara `TRUE` samtidigt.
#' @param skriv_stilfiler Bara relevant om `skriv_gpkg_fil = TRUE`. TRUE
#'   (standard) skriver en `.qml`- och en `.lyrx`-fil (QGIS respektive
#'   ArcGIS Pro) med samma basnamn som gpkg-filen, samt en inbäddad
#'   QGIS-stil i själva gpkg-filen, så linjerna redan är färdigstilade.
#' @param returnera_figur Skall kartorna returneras.
#'
#' @return Om bara EN kombination av `vald_kommun_kod`/`vald_pendlingstyp`
#'   efterfrågas: en namngiven lista `list(ggplot =, leaflet =)`, precis
#'   som [karta_befolkningsprognos_scb()]. Om FLERA kombinationer
#'   efterfrågas: en namngiven lista (en per kombination, t.ex.
#'   `"Falun_Nettopendling"`) där varje element självt är
#'   `list(ggplot =, leaflet =)`.
#' @export
karta_pendling_scb <- function(
    vald_kommun_kod,
    valt_ar = "9999",
    vald_pendlingstyp = "alla",
    grans_for_antal_pendlare = 10,
    legend_intervaller = 4,
    linjefarg_positiva_tal = "darkblue",
    linjefarg_negativa_tal = "darkred",
    linjefarg_utpendling = "darkred",
    linjefarg_inpendling = "darkblue",
    linjebredd_min = 3,
    linjebredd_max = 25,
    utmapp = rdverktyg::utskriftsmapp(),
    skriv_diagramfil = TRUE,
    skriv_leaflet_fil = TRUE,
    leaflet_filformat = "html",
    skriv_gpkg_fil = FALSE,
    skriv_stilfiler = TRUE,
    returnera_figur = TRUE
) {

  # ========================================== Allmän info ============================================
  # Flödeskartor över arbetspendling. leaflet-delen är en trogen portning av källskriptets
  # flödeslinjekarta (en per kommun/pendlingstyp). ggplot-delen är ett medvetet ANNORLUNDA,
  # kompletterande innehåll (regional nettopendling-choroplett, inte flödeslinjer) - beslutat med
  # användaren, se KONVENTIONER.md och NEWS.md för motivering.
  # ========================================== Inställningar ============================================

  utmapp <- intern_sakerstall_utmapp(utmapp, skriv_diagramfil || skriv_leaflet_fil || skriv_gpkg_fil)

  if (skriv_gpkg_fil && skriv_stilfiler) {
    if (!requireNamespace("RSQLite", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
      stop("Paketen 'RSQLite' och 'jsonlite' krävs för stilfiler till gpkg-utdata (skriv_stilfiler = TRUE). ",
           "Installera med install.packages(c(\"RSQLite\", \"jsonlite\")), eller sätt skriv_stilfiler = FALSE.",
           call. = FALSE)
    }
  }

  if (any(tolower(vald_pendlingstyp) == "alla")) {
    vald_pendlingstyp <- c("Bruttopendling", "Nettopendling", "Inpendling", "Utpendling")
  }

  regionnyckel <- rdverktyg::hamtaregtab()

  # Datan hämtas för HELA länet/länen de valda kommunerna ligger i, inte bara de uttryckligen valda
  # kommunerna - källskriptet hämtade bara data för vald_kommun_kod (räckte där, eftersom det bara
  # behövde flödeslinjer till/från just de kommunerna). Den nya ggplot-choropletten behöver däremot
  # nettopendling för SAMTLIGA kommuner i länet, inte bara de valda - därför den bredare hämtningen.
  lanskoder <- unique(stringr::str_sub(vald_kommun_kod, 1, 2))
  lanets_kommunkoder <- unique(unlist(lapply(lanskoder, function(l) rdverktyg::hamtakommuner(l, FALSE, FALSE))))
  region_vekt_hamta <- unique(c(vald_kommun_kod, lanets_kommunkoder))

  pendling_uttag <- rdverktyg::funktion_upprepa_forsok_om_fel(function()
    rdhamtadata::hamta_pendling_rams_bas_scb(region_vekt = region_vekt_hamta, tid_vekt = valt_ar)
  )

  pendling_df <- pendling_uttag |>
    dplyr::filter(regionkod_bo != regionkod_arb,
           pendlare > 0)

  pendling_enkelriktad <- pendling_df |>
    dplyr::mutate(pendl_id = ifelse(regionkod_bo < regionkod_arb, paste0(regionkod_bo, regionkod_arb), paste0(regionkod_arb, regionkod_bo))) |>
    dplyr::group_by(år, pendl_id) |>
    dplyr::summarise(pendlare = sum(pendlare, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(regionkod_bo = stringr::str_sub(pendl_id, 1, 4),
           regionkod_arb = stringr::str_sub(pendl_id, 5, 8)) |>
    dplyr::left_join(dplyr::rename(regionnyckel, bostadsregion = region), by = c("regionkod_bo" = "regionkod")) |>
    dplyr::left_join(dplyr::rename(regionnyckel, arbetsställeregion = region), by = c("regionkod_arb" = "regionkod"))

  unikt_ar <- unique(pendling_df$år)

  # ============================ hämta och bearbeta kartlager =====================================
  # Hela Sveriges kommuner hämtas en gång (inte bara de valda) - flödeslinjerna kan gå till/från
  # kommuner långt utanför det egna länet, och centroiderna behövs för att rita linjerna.
  kommuner_gis_sweref <- rdgis::hamta_karta("kommuner")
  kommuner_point_sweref <- suppressWarnings(sf::st_centroid(kommuner_gis_sweref))
  kommuner_gis <- sf::st_transform(kommuner_gis_sweref, crs = 4326)
  kommuner_point <- sf::st_transform(kommuner_point_sweref, crs = 4326)

  # ========== hjälpfunktioner: skriv .qml-/.lyrx-stilfiler bredvid gpkg-filen (skriv_gpkg_fil = TRUE) ====
  # Stilsätter linjerna (färg från fältet linjefarg, bredd från fältet linjebredd, rundade ändar/hörn) så
  # att man slipper göra det för hand i QGIS/ArcGIS. .qml är ett enkelt, väldokumenterat XML-format och
  # testat (öppnas korrekt i QGIS). .lyrx (ArcGIS Pro:s egna JSON-baserade CIM-format) är betydligt mer
  # komplext och är INTE testat mot riktig ArcGIS Pro härifrån - sätt skriv_stilfiler = FALSE om den inte
  # fungerar som väntat, och stilsätt i så fall för hand med fälten linjefarg/linjebredd i stället.

  bygg_qml_text <- function(min_bredd, max_bredd) {
    paste0(
'<!DOCTYPE qgis PUBLIC \'http://mrcc.com/qgis.dtd\' \'SYSTEM\'>
<qgis version="3.34" styleCategories="Symbology">
  <renderer-v2 type="singleSymbol" symbollevels="0" forceraster="0" enableorderby="0">
    <symbols>
      <symbol type="line" name="0" alpha="1" clip_to_extent="1" force_rhr="0">
        <layer class="SimpleLine" enabled="1" locked="0" pass="0">
          <Option type="Map">
            <Option type="QString" name="capstyle" value="round"/>
            <Option type="QString" name="joinstyle" value="round"/>
            <Option type="QString" name="line_color" value="35,35,35,255"/>
            <Option type="QString" name="line_style" value="solid"/>
            <Option type="QString" name="line_width" value="', round((min_bredd + max_bredd) / 2, 2), '"/>
            <Option type="QString" name="line_width_unit" value="Pixel"/>
          </Option>
          <data_defined_properties>
            <Option type="Map">
              <Option type="QString" name="name" value=""/>
              <Option name="properties" type="Map">
                <Option name="outlineColor" type="Map">
                  <Option name="active" type="bool" value="true"/>
                  <Option name="field" type="QString" value="linjefarg"/>
                  <Option name="type" type="int" value="2"/>
                </Option>
                <Option name="outlineWidth" type="Map">
                  <Option name="active" type="bool" value="true"/>
                  <Option name="field" type="QString" value="linjebredd"/>
                  <Option name="type" type="int" value="2"/>
                </Option>
              </Option>
              <Option type="QString" name="type" value="collection"/>
            </Option>
          </data_defined_properties>
        </layer>
      </symbol>
    </symbols>
  </renderer-v2>
</qgis>
')
  }

  skriv_qml_stilfil <- function(gpkg_filnamn, qml_text) {
    writeLines(qml_text, stringr::str_replace(gpkg_filnamn, "\\.gpkg$", ".qml"), useBytes = TRUE)
  }

  # Skriver in samma QML-stil i gpkg-filens egen layer_styles-tabell (GeoPackage är i grunden en SQLite-
  # databas) med useAsDefault = 1 - detta är den konvention QGIS faktiskt läser för att automatiskt
  # stilsätta ett GeoPackage-lager när det läggs till. En fristående .qml bredvid filen läses INTE in
  # automatiskt för GeoPackage (bara för enfilsformat som Shapefile), så utan detta steg visas lagret
  # med QGIS standardsymbol (svart linje) tills man manuellt kör Style > Load Style...
  skriv_stil_i_gpkg <- function(gpkg_filnamn, lagernamn, qml_text) {
    con <- RSQLite::dbConnect(RSQLite::SQLite(), gpkg_filnamn)
    on.exit(RSQLite::dbDisconnect(con), add = TRUE)

    geom_kolumn <- RSQLite::dbGetQuery(con,
      "SELECT column_name FROM gpkg_geometry_columns WHERE table_name = ?", params = list(lagernamn))$column_name

    RSQLite::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS layer_styles (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        f_table_catalog TEXT(256),
        f_table_schema TEXT(256),
        f_table_name TEXT(256),
        f_geometry_column TEXT(256),
        styleName TEXT(30),
        styleQML TEXT,
        styleSLD TEXT,
        useAsDefault BOOLEAN,
        description TEXT,
        owner TEXT(30),
        ui_fileid TEXT,
        update_time DATETIME DEFAULT CURRENT_TIMESTAMP
      )")
    RSQLite::dbExecute(con, "DELETE FROM layer_styles WHERE f_table_name = ?", params = list(lagernamn))
    RSQLite::dbExecute(con,
      "INSERT INTO layer_styles (f_table_catalog, f_table_schema, f_table_name, f_geometry_column,
                                 styleName, styleQML, useAsDefault, description, owner)
       VALUES ('', '', ?, ?, ?, ?, 1, 'Automatiskt genererad stil (farg/bredd fran faelten linjefarg/linjebredd)', '')",
      params = list(lagernamn, geom_kolumn, lagernamn, qml_text))
  }

  skriv_lyrx_stilfil <- function(gpkg_filnamn, lagernamn, giskarta_vald_kommun, min_bredd, max_bredd) {
    unika_farger <- unique(giskarta_vald_kommun$linjefarg)

    klasser <- lapply(unika_farger, function(farg) {
      rgb <- as.integer(grDevices::col2rgb(farg))
      list(
        type = "CIMUniqueValueClass",
        label = farg,
        patch = "Default",
        symbol = list(
          type = "CIMSymbolReference",
          symbol = list(
            type = "CIMLineSymbol",
            symbolLayers = list(list(
              type = "CIMSolidStroke",
              enable = TRUE,
              capStyle = "Round",
              joinStyle = "Round",
              lineStyle3D = "Strip",
              miterLimit = 10,
              width = (min_bredd + max_bredd) / 2,
              color = list(type = "CIMRGBColor", values = c(rgb, 100))
            ))
          )
        ),
        values = list(list(type = "CIMUniqueValue", fieldValues = list(farg)))
      )
    })

    renderer <- list(
      type = "CIMUniqueValueRenderer",
      fields = list("linjefarg"),
      groups = list(list(heading = "", classes = klasser)),
      useDefaultSymbol = FALSE,
      defaultLabel = "Övriga värden",
      visualVariables = list(list(
        type = "CIMSizeVisualVariable",
        authoringInfo = list(type = "CIMVisualVariableAuthoringInfo",
                             minSliderValue = min_bredd, maxSliderValue = max_bredd),
        minSize = min_bredd, maxSize = max_bredd,
        minValue = min_bredd, maxValue = max_bredd,
        valueExpressionInfo = list(type = "CIMExpressionInfo", expression = "$feature.linjebredd",
                                   title = "linjebredd"),
        variableType = "Graduated"
      ))
    )

    lager <- list(
      type = "CIMFeatureLayer",
      name = lagernamn,
      uRI = paste0("CIMPATH=map/", lagernamn, ".xml"),
      featureTable = list(
        type = "CIMFeatureTable",
        displayField = "linjefarg",
        editable = TRUE,
        dataConnection = list(
          type = "CIMStandardDataConnection",
          workspaceConnectionString = paste0("DATABASE=", basename(gpkg_filnamn)),
          workspaceFactory = "Geopackage",
          dataset = lagernamn,
          datasetType = "esriDTFeatureClass"
        )
      ),
      renderer = renderer
    )

    dok <- list(
      type = "CIMLayerDocument",
      version = "2.9.0",
      layers = list(paste0("CIMPATH=map/", lagernamn, ".xml")),
      layerDefinitions = list(lager)
    )

    jsonlite::write_json(dok, stringr::str_replace(gpkg_filnamn, "\\.gpkg$", ".lyrx"),
                         auto_unbox = TRUE, pretty = TRUE, null = "null")
  }

  # ==================== nettopendling per kommun i länet (ggplot-choropletten) ====================
  # Inpendlare (regionkod_arb = kommunen) minus utpendlare (regionkod_bo = kommunen), oavsett
  # vald_pendlingstyp - choropletten visar alltid NETTO, se roxygen-dokumentationen ovan för varför.
  berakna_nettopendling_regional <- function(kommunkoder) {
    inp <- pendling_df |>
      dplyr::filter(regionkod_arb %in% kommunkoder) |>
      dplyr::group_by(regionkod = regionkod_arb) |>
      dplyr::summarise(inpendlare = sum(pendlare, na.rm = TRUE), .groups = "drop")
    utp <- pendling_df |>
      dplyr::filter(regionkod_bo %in% kommunkoder) |>
      dplyr::group_by(regionkod = regionkod_bo) |>
      dplyr::summarise(utpendlare = sum(pendlare, na.rm = TRUE), .groups = "drop")
    dplyr::full_join(inp, utp, by = "regionkod") |>
      dplyr::mutate(dplyr::across(c(inpendlare, utpendlare), ~ tidyr::replace_na(.x, 0)),
             nettopendling = inpendlare - utpendlare)
  }

  ggplot_cache <- new.env(parent = emptyenv())

  bygg_nettopendling_ggplot <- function(lanskod) {
    cache_key <- lanskod
    if (!is.null(ggplot_cache[[cache_key]])) return(ggplot_cache[[cache_key]])

    kommunkoder_i_lan <- rdverktyg::hamtakommuner(lanskod, FALSE, FALSE)
    netto_df <- berakna_nettopendling_regional(kommunkoder_i_lan)

    netto_karta <- kommuner_gis |>
      dplyr::filter(knkod %in% kommunkoder_i_lan) |>
      dplyr::left_join(netto_df, by = c("knkod" = "regionkod")) |>
      dplyr::mutate(nettopendling = tidyr::replace_na(nettopendling, 0))

    max_abs <- max(abs(netto_karta$nettopendling), na.rm = TRUE)
    breaks <- pretty(c(-max_abs, max_abs), n = 6)
    lan_namn <- unique(rdverktyg::hamtaregion_kod_namn(lanskod)$region)

    gg <- rddiagram::skapa_koropletkarta_ggplot(
      sf_objekt = netto_karta,
      vardekolumn = "nettopendling",
      klassindelning = breaks,
      karta_fargvektor = c(linjefarg_negativa_tal, "white", linjefarg_positiva_tal),
      legend_titel = "Nettopendling",
      karta_titel = paste0("Nettopendling per kommun i ", lan_namn),
      karta_caption = paste0("år ", unikt_ar, "\nKälla: SCB (RAMS/BAS)\nBearbetning: Samhällsanalys, Region Dalarna\nNettopendling = inpendlare minus utpendlare"),
      filnamn = if (skriv_diagramfil) paste0("nettopendling_", lan_namn, "_ar", unikt_ar, ".png") else NULL,
      output_mapp = if (skriv_diagramfil) utmapp else NULL
    )
    assign(cache_key, gg, envir = ggplot_cache)
    gg
  }

  # ==================================== funktion för att skapa EN karta ========================================

  skapa_karta <- function(karta_kommunkod, skickad_pendlingstyp) {

    if (skickad_pendlingstyp == "Inpendling") {
      vald_pendlingstyp_df <- dplyr::filter(pendling_df, regionkod_arb %in% karta_kommunkod)

    } else if (skickad_pendlingstyp == "Utpendling") {
      vald_pendlingstyp_df <- dplyr::filter(pendling_df, regionkod_bo %in% karta_kommunkod)

    } else if (skickad_pendlingstyp == "Nettopendling") {
      vald_pendlingstyp_df <- pendling_df |>
        dplyr::mutate(pendl_id = ifelse(regionkod_bo < regionkod_arb, paste0(regionkod_bo, regionkod_arb), paste0(regionkod_arb, regionkod_bo))) |>
        dplyr::group_by(år, pendl_id) |>
        dplyr::summarise(pendlare = sum(pendlare[!regionkod_bo %in% karta_kommunkod], na.rm = TRUE) - sum(pendlare[regionkod_bo %in% karta_kommunkod], na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(regionkod_bo = stringr::str_sub(pendl_id, 1, 4),
               regionkod_arb = stringr::str_sub(pendl_id, 5, 8)) |>
        dplyr::left_join(dplyr::rename(regionnyckel, bostadsregion = region), by = c("regionkod_bo" = "regionkod")) |>
        dplyr::left_join(dplyr::rename(regionnyckel, arbetsställeregion = region), by = c("regionkod_arb" = "regionkod"))

    } else if (skickad_pendlingstyp == "Bruttopendling") {
      vald_pendlingstyp_df <- pendling_enkelriktad
    }

    giskarta <- vald_pendlingstyp_df |>
      dplyr::filter(pendlare >= grans_for_antal_pendlare |
               pendlare <= (grans_for_antal_pendlare * -1)) |>
      dplyr::left_join(dplyr::select(kommuner_point_sweref, knkod, start_geom = geometry), by = c("regionkod_bo" = "knkod")) |>
      dplyr::left_join(dplyr::select(kommuner_point_sweref, knkod, dest_geom = geometry), by = c("regionkod_arb" = "knkod")) |>
      sf::st_as_sf()

    # bygg linjer mellan punkter
    giskarta <- giskarta |>
      dplyr::mutate(geom_ny = sf::st_sfc(mapply(function(a, b) { sf::st_cast(sf::st_union(a, b), "LINESTRING") }, start_geom, dest_geom, SIMPLIFY = FALSE), crs = 3006),
             geom_bage = sf::st_simplify(geom_ny, dTolerance = 0.1)) |>
      dplyr::select(-c(start_geom, dest_geom))

    sf::st_geometry(giskarta) <- "geom_bage"
    giskarta <- giskarta |>
      dplyr::select(-c(start_geom, geom_ny)) |>
      sf::st_as_sf(crs = 3006)
    giskarta <- sf::st_transform(giskarta, crs = 4326)

    giskarta_vald_kommun <- giskarta |>
      dplyr::filter(regionkod_bo == karta_kommunkod | regionkod_arb == karta_kommunkod) |>
      dplyr::mutate(relationskommun = ifelse(regionkod_bo == karta_kommunkod, arbetsställeregion, bostadsregion),
             relationskommun_kod = ifelse(regionkod_bo == karta_kommunkod, regionkod_arb, regionkod_bo),
             pendlare_txt = as.character(pendlare))

    if (skickad_pendlingstyp == "Inpendling") giskarta_vald_kommun <- dplyr::filter(giskarta_vald_kommun, regionkod_arb == karta_kommunkod)
    if (skickad_pendlingstyp == "Utpendling") giskarta_vald_kommun <- dplyr::filter(giskarta_vald_kommun, regionkod_bo == karta_kommunkod)

    kommunnamn <- rdverktyg::hamtaregion_kod_namn(karta_kommunkod)$region

    # ========= beräkningar för linjefärg/-tjocklek (html-legend/karta OCH gpkg-attribut) =========
    calculateWeightContinuous <- function(pends, min_width, max_width, min_pend, max_pend) {
      min_pend <- abs(min_pend)
      max_pend <- abs(max_pend)
      if (min_pend > max_pend) {
        temp <- min_pend
        min_pend <- max_pend
        max_pend <- temp
      }
      if (min_pend > grans_for_antal_pendlare) min_pend <- grans_for_antal_pendlare
      scales::rescale(abs(pends), to = c(min_width, max_width), from = c(min_pend, max_pend))
    }

    min_pend <- min(giskarta_vald_kommun$pendlare)
    max_pend <- max(giskarta_vald_kommun$pendlare)
    min_width <- linjebredd_min
    max_width <- linjebredd_max

    calculated_weights <- calculateWeightContinuous(
      giskarta_vald_kommun$pendlare, min_width, max_width, min_pend, max_pend)

    farg_pos_neg_tal <- function(antal_pendlare) {
      retur_varde <- ifelse(antal_pendlare > 0, linjefarg_positiva_tal, linjefarg_negativa_tal)
      if (skickad_pendlingstyp == "Utpendling") retur_varde <- linjefarg_utpendling
      if (skickad_pendlingstyp == "Inpendling") retur_varde <- linjefarg_inpendling
      return(retur_varde)
    }

    filnamn_bas <- paste0(tolower(skickad_pendlingstyp), "_pendling_", kommunnamn, "_ar", unique(giskarta_vald_kommun$år))

    if (skriv_gpkg_fil) {
      linjefarg_hex <- apply(grDevices::col2rgb(farg_pos_neg_tal(giskarta_vald_kommun$pendlare)), 2,
                             function(rgb) grDevices::rgb(rgb[1], rgb[2], rgb[3], maxColorValue = 255))
      giskarta_gpkg <- dplyr::mutate(giskarta_vald_kommun,
                                     linjefarg = linjefarg_hex,
                                     linjebredd = calculated_weights)
      filnamn_gpkg <- paste0(utmapp, filnamn_bas, ".gpkg")
      lagernamn_gpkg <- tools::file_path_sans_ext(basename(filnamn_gpkg))
      sf::st_write(sf::st_transform(giskarta_gpkg, crs = 3006), filnamn_gpkg, layer = lagernamn_gpkg,
                  delete_dsn = TRUE, quiet = TRUE)
      if (skriv_stilfiler) {
        qml_text <- bygg_qml_text(linjebredd_min, linjebredd_max)
        skriv_qml_stilfil(filnamn_gpkg, qml_text)
        skriv_stil_i_gpkg(filnamn_gpkg, lagernamn_gpkg, qml_text)
        skriv_lyrx_stilfil(filnamn_gpkg, lagernamn_gpkg, giskarta_gpkg, linjebredd_min, linjebredd_max)
      }
    }

    lanskod_vald <- stringr::str_sub(karta_kommunkod, 1, 2)
    lanets_kommuner_gis <- dplyr::filter(kommuner_gis, lanskod_tx == lanskod_vald)
    vald_kommun_gis <- dplyr::filter(kommuner_gis, knkod == karta_kommunkod)
    lanets_kommuner_centroid <- suppressWarnings(sf::st_centroid(lanets_kommuner_gis))
    kom_point <- dplyr::filter(kommuner_point, knkod %in% giskarta_vald_kommun$relationskommun_kod)

    vald_kommun_centroid <- sf::st_centroid(sf::st_geometry(vald_kommun_gis))
    centroid_coords <- as.numeric(sf::st_coordinates(vald_kommun_centroid))

    pendlare_intervaller <- rdverktyg::skapa_intervaller(giskarta_vald_kommun$pendlare, antal_intervaller = legend_intervaller)
    pendlare_bredd <- calculateWeightContinuous(pendlare_intervaller, min_width, max_width, min_pend, max_pend)

    legend_rows <- purrr::map2(pendlare_bredd, pendlare_intervaller, ~ htmltools::div(
      style = "display: flex; align-items: center; margin-bottom: 5px; white-space: nowrap;",
      htmltools::span(style = paste0("display: inline-block; width: 70px; height: ", .x, "px; background-color: ", farg_pos_neg_tal(.y), ";")),
      htmltools::span(" ", .y, style = "margin-left: 5px;")
    ))

    legend_html <- htmltools::div(
      id = "custom-legend",
      style = "background-color: rgba(255, 255, 255, 0.7); padding: 5px; padding-top: 0px; position: absolute; bottom: 50px; right: 10px;",
      htmltools::h4(paste0(skickad_pendlingstyp, " år ", unique(giskarta_vald_kommun$år)), style = "margin-top: 0px; margin-bottom: 5px;"),
      legend_rows
    )

    attrib_txt <- dplyr::case_when(skickad_pendlingstyp == "Nettopendling" ~ paste0(", dvs. inpendlare minus utpendlare till och från ", kommunnamn, " och andra kommuner"),
                            skickad_pendlingstyp == "Bruttopendling" ~ paste0(", dvs. in- och utpendlare summerat, mellan ", kommunnamn, " och andra kommuner"),
                            skickad_pendlingstyp == "Inpendling" ~ paste0(" till ", kommunnamn, " från andra kommuner"),
                            skickad_pendlingstyp == "Utpendling" ~ paste0(" från ", kommunnamn, " till andra kommuner"))

    attribution_html <- htmltools::tags$div(
        style = "background-color: rgba(220, 220, 220, 0.4); padding: 0px; font-size: 10px; border-radius: 1px;",
        htmltools::tags$p(paste0("Karta skapad av: Samhällsanalys, Region Dalarna. Kartan visar ", tolower(skickad_pendlingstyp), attrib_txt, " där det finns minst ", grans_for_antal_pendlare, " pendlare totalt. Klicka på linjerna för att se antal pendlare."), style = "margin: 0;")
      )

    popup_txt <- dplyr::case_when(skickad_pendlingstyp == "Nettopendling" ~ "Inpendlare minus utpendlare (nettopendling) till och från {relationskommun}: {pendlare_txt}",
                           skickad_pendlingstyp == "Bruttopendling" ~ "Antal in- och utpendlare summerat till och från {relationskommun}: {pendlare_txt}",
                           skickad_pendlingstyp == "Inpendling" ~ "Antal inpendlare från {relationskommun}: {pendlare_txt}",
                           skickad_pendlingstyp == "Utpendling" ~ "Antal utpendlare till {relationskommun}: {pendlare_txt}")

    bbox_lan <- sf::st_bbox(lanets_kommuner_gis)
    hojd <- as.numeric(bbox_lan$ymax - bbox_lan$ymin)
    bredd <- as.numeric(bbox_lan$xmax - bbox_lan$xmin)
    avstand_zoom <- ifelse(hojd > bredd, hojd, bredd)

    calculate_zoom_level <- function(avstand) {
      a <- -1.1047
      b <- 9.0761
      a * log(avstand) + b
    }
    zoom_niva <- calculate_zoom_level(avstand_zoom)

    kom_point_utanfor <- dplyr::filter(kom_point, lanskod_tx != lanskod_vald)

    leaflet_map <- leaflet::leaflet() |>
      leaflet::addProviderTiles("CartoDB.PositronNoLabels") |>
      leaflet::addPolygons(data = lanets_kommuner_gis,
                           color = "white", weight = 0.5,
                           opacity = 0.5,
                           fillColor = "grey98", fillOpacity = 0.1) |>
      leaflet::addPolygons(data = vald_kommun_gis,
                           color = "darkgrey", weight = 1) |>
      leaflet::setView(lng = centroid_coords[1], lat = centroid_coords[2], zoom = zoom_niva) |>
      leaflet::addPolygons(data = dplyr::filter(kommuner_gis, lanskod_tx != lanskod_vald),
                           color = "darkgrey", fillOpacity = 0,
                           fillColor = "darkgrey", weight = 0.3,
                           opacity = 0.2)

    if (nrow(kom_point_utanfor) > 0) {
      leaflet_map <- leaflet_map |>
        leaflet::addLabelOnlyMarkers(
          data = kom_point_utanfor,
          label = ~as.character(knnamn),
          labelOptions = leaflet::labelOptions(noHide = TRUE, direction = 'top',
                                               textOnly = TRUE,
                                               style = list("color" = "darkgrey",
                                                            "opacity" = "0.2")))
    }
    leaflet_map <- leaflet_map |>
      leaflet::addLabelOnlyMarkers(data = lanets_kommuner_centroid,
                                   label = ~as.character(knnamn),
                                   labelOptions = leaflet::labelOptions(noHide = TRUE, direction = 'top',
                                                                        textOnly = TRUE,
                                                                        style = list("color" = "#777777",
                                                                                     "opacity" = "0.2"))) |>
      leaflet::addPolylines(data = giskarta_vald_kommun,
                            weight = ~calculated_weights,
                            color = ~farg_pos_neg_tal(pendlare),
                            popup = ~glue::glue(popup_txt)) |>
      leaflet::addControl(html = as.character(legend_html), position = "bottomright", className = "fieldset {border: 0;}") |>
      leaflet::addControl(html = as.character(attribution_html), position = "bottomleft", className = "fieldset {border: 0;}")

    # OBS: källskriptet försökte här bifoga ett eget CSS-stilblad (leaflet_justering.css) från en
    # lokal G:-sökväg (G:/skript/mallar) för att flytta legenden - enligt egen kommentar fick
    # författaren det aldrig att fungera. Borttaget här: pekade mot en sökväg som bara finns på en
    # specifik dators G:-enhet (inte en delad resurs) och gjorde under alla omständigheter ingenting.

    if (skriv_leaflet_fil) {
      rddiagram::skriv_leaflet_till_fil(leaflet_map, utmapp, filnamn_bas, karta_filformat = leaflet_filformat)
    }

    list(ggplot = bygg_nettopendling_ggplot(lanskod_vald), leaflet = leaflet_map)
  } # slut skapa_karta()

  # ==================================== bygg alla kombinationer ========================================

  kombinationer <- expand.grid(kommun = vald_kommun_kod, typ = vald_pendlingstyp, stringsAsFactors = FALSE)

  resultat_lista <- purrr::map2(kombinationer$kommun, kombinationer$typ, skapa_karta)
  names(resultat_lista) <- paste0(
    rdverktyg::hamtaregion_kod_namn(kombinationer$kommun)$region, "_", kombinationer$typ
  )

  if (returnera_figur) {
    if (length(resultat_lista) == 1) resultat_lista[[1]] else resultat_lista
  }
}
