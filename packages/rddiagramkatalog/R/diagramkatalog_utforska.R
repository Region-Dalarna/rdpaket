#' Utforska och sök bland diagramfunktionerna (RStudio-addin)
#'
#' Öppnar ett eget, större fönster ovanpå RStudio där man kan söka bland
#' paketets diagramfunktioner (samma sökning som [sok_diagram()]), läsa
#' hjälptexten för en vald funktion, fylla i parametrar, och antingen köra
#' diagrammet direkt för en förhandsvisning, skriva det till fil eller kopiera
#' motsvarande R-kod till urklipp. Nås enklast via RStudios Addins-meny ("Utforska
#' diagramkatalog").
#'
#' Parameterformuläret byggs generiskt utifrån `formals()` för den valda
#' funktionen - det är alltså inte skräddarsytt per diagram, utan fungerar
#' automatiskt för alla funktioner i katalogen, även framtida tillskott.
#' Varje fält visar parameterns standardvärde som R-kod (t.ex. `"20"` eller
#' `c("män", "kvinnor")`) och man redigerar fältet som R-kod - fältet klistras
#' in rakt av i det genererade funktionsanropet. Booleska parametrar (`TRUE`/
#' `FALSE`) får en kryssruta i stället.
#'
#' Där de valbara värdena är kända får parametern en sökbar listruta i stället för
#' en textruta (man kan också skriva ett eget värde):
#' * parametrar som valideras med `match.arg()` (t.ex. `diagramtyp`) får en listruta med de giltiga valen,
#' * `region_vekt` och `jmfr_region` får en lista med riket, län och kommuner (namnet visas, koden skickas),
#' * parametrar som funktionen skickar direkt till ett SCB-uttag (`pxweb2r::pxweb2_get_data()`) - t.ex.
#'   `kon_klartext`, `alder_klartext`, `cont_klartext` och `tid_koder` - får SCB:s giltiga värden för just den
#'   tabellen och variabeln. Värdena slås upp när funktionen väljs och cachas under sessionen.
#'
#' Allt annat är en textruta som tidigare. En tom listruta betyder funktionens standardvärde.
#'
#' "Visa hjälp" visar funktionens hjälptext (samma innehåll som
#' `?funktionsnamn`) i en egen ruta ovanpå gadgeten, i stället för i RStudios
#' Hjälp-ruta - annars hamnar den bakom gadgetens eget dialogfönster och syns
#' inte.
#'
#' "Skapa diagram" visar bara diagrammet i förhandsvisningen. Anropet körs med
#' `skriv_diagramfil = FALSE` (`skriv_leaflet_fil = FALSE` för kartorna) oavsett
#' vad formuläret säger, så ingen fil skrivs.
#'
#' "Skriv till fil" körs bara om funktionen har både `utmapp`- och
#' `skriv_diagramfil`-parametrar (annars visas ett felmeddelande) och
#' `utmapp`-fältet i formuläret är ifyllt. Har diagrammet nyss skapats med
#' "Skapa diagram", och varken parametrar eller funktion ändrats sedan dess, skrivs
#' det redan skapade diagrammet direkt till fil (om funktionen bygger på
#' [rddiagram::SkapaStapelDiagram()] eller [rddiagram::SkapaLinjeDiagram()], som bär med sig sina
#' filinställningar), utan att data hämtas på nytt. Annars körs anropet om med
#' `skriv_diagramfil = TRUE`. En tillfällig notis visar sedan sökväg(ar) + filnamn
#' för de fil(er) som skrevs/uppdaterades i `utmapp`.
#'
#' "Kopiera kod" lägger anropet med valda parametrar i urklipp (via `clipr`) och
#' lämnar gadgeten öppen. Går urklippet inte att nå infogas koden i det aktiva
#' skriptet i stället (kräver `rstudioapi` och RStudio), annars skrivs den ut i konsolen.
#'
#' Kräver `shiny` och `miniUI` (`install.packages(c("shiny", "miniUI"))`),
#' som bara är valfria beroenden till paketet eftersom de bara behövs för
#' den här interaktiva funktionen.
#'
#' @return Ingenting (anropas interaktivt).
#' @export
diagramkatalog_utforska <- function() {
  if (!requireNamespace("shiny", quietly = TRUE) ||
      !requireNamespace("miniUI", quietly = TRUE)) {
    stop(
      "Paketen 'shiny' och 'miniUI' krävs för diagramkatalog_utforska().\n",
      "Installera med: install.packages(c(\"shiny\", \"miniUI\"))",
      call. = FALSE
    )
  }

  index <- intern_diagram_index_cached()

  # Exempelbilder (man/figures/<funktion>.png i källan) installeras av R
  # under help/figures/ i det installerade paketet - inte figures/ direkt,
  # trots att man/figures/ är källmappen. Registreras som en egen
  # resursväg så img-taggar nedan kan referera till dem direkt.
  figurmapp <- system.file("help", "figures", package = "rddiagramkatalog")
  if (nzchar(figurmapp)) {
    shiny::addResourcePath("rddiagramkatalog_exempel", figurmapp)
  }

  # Knappstorlek - miniTitleBarButton() ger bara "btn-sm" (Bootstrap har
  # ingen mindre standardstorlek), så en liten extra stil krymper dem
  # ytterligare utan att texten blir oläsbar.
  knappstil <- "padding: 3px 10px; font-size: 12px;"
  litenknapp <- function(input_id, etikett, farg) {
    shiny::tagAppendAttributes(
      miniUI::miniTitleBarButton(input_id, etikett, primary = farg),
      style = knappstil
    )
  }

  ui <- miniUI::miniPage(
    # Scrollar automatiskt ner till diagrammet när det är klart - annars
    # kan det hamna utom synhåll under ett långt parameterformulär i den
    # scrollbara högerrutan. scrollIntoView() hanterar den inbäddade
    # scrollningen i rutan automatiskt, inga koordinater att räkna ut.
    shiny::tags$script(shiny::HTML(
      "Shiny.addCustomMessageHandler('rddiagramkatalog_scrolla_till_diagram', function(id) {
         var el = document.getElementById(id);
         if (el) { el.scrollIntoView({behavior: 'smooth', block: 'start'}); }
       });"
    )),
    miniUI::gadgetTitleBar(
      "Diagramkatalog",
      # OBS: right måste vara EN tagg, inte en tagList() med flera knappar -
      # miniTitleBar() lägger på klassen "pull-right" via
      # tagAppendAttributes(right, class = "pull-right"), vilket bara
      # fungerar på en enskild tagg. Med en tagList() hamnar "pull-right"
      # i stället som synlig text i gränssnittet. Knapparna radas därför
      # upp inuti en gemensam div.
      right = shiny::div(
        litenknapp("hjalp", "Visa hjälp", "info"),
        litenknapp("visa", "Skapa diagram", TRUE),
        litenknapp("skriv_fil", "Skriv till fil", "success"),
        litenknapp("kopiera", "Kopiera kod", TRUE)
      )
    ),
    miniUI::miniContentPanel(
      shiny::fillRow(
        flex = c(2, 3),
        shiny::div(
          style = "padding: 10px; overflow-y: auto; height: 100%; border-right: 1px solid #ddd;",
          shiny::textInput("sok", "Sök", placeholder = "t.ex. arbetslöshet"),
          shiny::uiOutput("funktionslista")
        ),
        shiny::div(
          style = "padding: 10px; overflow-y: auto; height: 100%;",
          shiny::uiOutput("exempelbild"),
          shiny::uiOutput("parameterformular"),
          shiny::plotOutput("forhandsvisning", height = "380px")
        )
      )
    )
  )

  server <- function(input, output, session) {

    traffar <- shiny::reactive({
      if (is.null(input$sok) || nchar(input$sok) == 0) return(index$funktion)
      index$funktion[grepl(input$sok, index$sokbar_text, ignore.case = TRUE)]
    })

    output$funktionslista <- shiny::renderUI({
      val <- traffar()
      if (length(val) == 0) {
        return(shiny::em("Ingen träff."))
      }
      shiny::radioButtons("vald_funktion", NULL, choices = val, selected = val[1])
    })

    vald_fn <- shiny::reactive({
      shiny::req(input$vald_funktion)
      input$vald_funktion
    })

    vald_args <- shiny::reactive({
      args <- formals(vald_fn())
      args[!names(args) %in% "..."]
    })

    output$exempelbild <- shiny::renderUI({
      fn <- vald_fn()
      finns <- function(filnamn) nzchar(figurmapp) && file.exists(file.path(figurmapp, filnamn))
      har_bild <- finns(paste0(fn, ".png"))
      har_html <- finns(paste0(fn, ".html"))

      if (!har_bild && !har_html) {
        return(shiny::tags$p(
          shiny::tags$em("Inget exempel sparat för den här funktionen än."),
          style = "color: #999; font-size: 12px;"
        ))
      }

      shiny::tagList(
        if (har_bild) {
          shiny::tagList(
            shiny::tags$p(
              shiny::tags$em("Exempel (standardinställningar - kan vara inaktuellt):"),
              style = "margin-bottom: 4px; color: #666; font-size: 12px;"
            ),
            shiny::tags$img(
              src = file.path("rddiagramkatalog_exempel", paste0(fn, ".png")),
              alt = paste("Exempeldiagram för", fn),
              style = "max-width: 100%; border: 1px solid #ddd; margin-bottom: 10px;"
            )
          )
        },
        # interaktiva kartor (karta_lf_*()) har även en html-fil, som öppnas i webbläsaren
        if (har_html) {
          shiny::tags$p(
            shiny::tags$a(
              if (har_bild) "Öppna den interaktiva exempelkartan" else "Öppna den interaktiva exempelkartan (ingen bild sparad)",
              href = file.path("rddiagramkatalog_exempel", paste0(fn, ".html")),
              target = "_blank"
            ),
            style = "font-size: 12px;"
          )
        }
      )
    })

    # Listrutornas värden (match.arg, regioner och SCB-värden). Slås upp när funktionen väljs; ett fel här
    # ger bara textrutor som tidigare.
    listrutor <- shiny::reactive({
      fn <- vald_fn()
      shiny::withProgress(
        message = "Hämtar värden till listrutorna...", value = 0.5,
        tryCatch(intern_utforska_listrutor(fn), error = function(e) list())
      )
    })

    output$parameterformular <- shiny::renderUI({
      args <- vald_args()
      spec <- listrutor()

      shiny::tagList(
        shiny::h4(vald_fn()),
        lapply(names(args), function(namn) intern_utforska_inputfalt(namn, args[[namn]], spec[[namn]]))
      )
    })

    # Plain funktion (inte shiny::reactive()) - "Skriv till fil"-knappen nedan behöver kunna skicka
    # med overrides (t.ex. tvinga skriv_diagramfil = TRUE oavsett kryssrutans läge), vilket en
    # parameterlös reactive() inte stöder.
    bygg_anrop <- function(overrides = list()) {
      args <- vald_args()
      spec <- listrutor()

      argument_text <- vapply(names(args), function(namn) {
        if (namn %in% names(overrides)) return(paste0(namn, " = ", overrides[[namn]]))
        varde <- input[[paste0("param_", namn)]]
        paste0(namn, " = ", intern_utforska_kodtext(args[[namn]], varde, spec[[namn]]))
      }, character(1))

      paste0(vald_fn(), "(\n  ", paste(argument_text, collapse = ",\n  "), "\n)")
    }

    # Det senast skapade diagrammet: anropskoden (med skrivparametern satt till FALSE) och resultatet.
    # Används av "Skriv till fil" för att slippa köra om skriptet när inget ändrats.
    senaste <- NULL

    # Namnet på parametern som styr filskrivning (skriv_diagramfil, eller skriv_leaflet_fil för kartor)
    skrivparam <- function() intersect(c("skriv_diagramfil", "skriv_leaflet_fil"), names(vald_args()))[1]

    # anropskoden som förhandsvisningen kör: skrivparametern tvingas till FALSE
    forhandskod <- function() {
      p <- skrivparam()
      bygg_anrop(overrides = if (is.na(p)) list() else stats::setNames(list("FALSE"), p))
    }

    shiny::observeEvent(input$visa, {
      kod <- forhandskod()
      senaste <<- NULL
      output$forhandsvisning <- shiny::renderPlot({
        # Notisen visas direkt (skickas till webbläsaren oberoende av den
        # pågående beräkningen) och måste därför läggas HÄR, inne i
        # renderPlot()s uttryck - där det faktiska (ofta långsamma)
        # eval()-anropet mot API:et sker - inte i observeEvent() ovanför,
        # vars kod körs klart innan renderPlot() faktiskt utvärderas.
        notis_id <- shiny::showNotification("Skapar diagram...", duration = NULL, type = "message")
        on.exit({
          shiny::removeNotification(notis_id)
          session$sendCustomMessage("rddiagramkatalog_scrolla_till_diagram", "forhandsvisning")
        }, add = TRUE)

        resultat <- tryCatch(eval(parse(text = kod)), error = function(e) {
          shiny::showNotification(paste("Fel:", conditionMessage(e)), type = "error", duration = NULL)
          NULL
        })
        if (is.null(resultat)) return(NULL)
        senaste <<- list(kod = kod, resultat = resultat)
        # Alla funktioner returnerar en namngiven lista; förhandsvisningen visar det första
        # elementet. karta_lf_*() ger leaflet-kartor, som inte kan ritas av renderPlot() - de öppnas
        # i stället i webbläsaren (en tillfällig html-fil).
        if (inherits(resultat[[1]], "leaflet")) {
          html <- tempfile(fileext = ".html")
          htmlwidgets::saveWidget(resultat[[1]], html, selfcontained = FALSE)
          utils::browseURL(html)
          shiny::showNotification("Den interaktiva kartan har öppnats i webbläsaren.", type = "message", duration = 7)
          return(NULL)
        }
        resultat[[1]]
      })
    })

    shiny::observeEvent(input$skriv_fil, {
      args <- vald_args()

      skriv_param <- intersect(c("skriv_diagramfil", "skriv_leaflet_fil"), names(args))[1]
      if (!"utmapp" %in% names(args) || is.na(skriv_param)) {
        shiny::showNotification(
          "Den här funktionen har ingen utmapp/skriv_diagramfil-parameter - kan inte skriva till fil.",
          type = "error", duration = NULL
        )
        return()
      }

      utmapp_varde <- tryCatch(eval(parse(text = input$param_utmapp)), error = function(e) NA)
      if (length(utmapp_varde) != 1 || is.na(utmapp_varde) || !nzchar(utmapp_varde)) {
        shiny::showNotification(
          "Ange en utmapp i formuläret innan du skriver till fil.",
          type = "error", duration = NULL
        )
        return()
      }

      # skriv_diagramfil (skriv_leaflet_fil för karta_lf_*()) tvingas till TRUE oavsett kryssrutans läge - det är hela poängen med
      # knappen. innan_tid används för att efteråt avgöra vilka fil(er) i utmapp som faktiskt
      # skrevs/uppdaterades av just detta anrop (funktionerna returnerar bara ggplot-/leaflet-
      # objekt, inte vilket filnamn de skrev till).
      innan_tid <- Sys.time()
      notis_id <- shiny::showNotification("Skriver diagram till fil...", duration = NULL, type = "message")

      # Oförändrat anrop sedan "Skapa diagram" och diagram som bär med sig sina filinställningar
      # (attributet skrivinfo): skriv det redan skapade diagrammet utan att köra om skriptet.
      kan_ateranvanda <- !is.null(senaste) && identical(senaste$kod, forhandskod()) &&
        is.list(senaste$resultat) &&
        all(vapply(senaste$resultat, function(x) !is.null(attr(x, "skrivinfo")), logical(1)))

      resultat <- tryCatch({
        if (kan_ateranvanda) {
          lapply(senaste$resultat, rddiagram::skriv_diagram_fran_skrivinfo)
        } else {
          kod <- bygg_anrop(overrides = stats::setNames(list("TRUE"), skriv_param))
          eval(parse(text = kod))
        }
      }, error = function(e) {
        shiny::showNotification(paste("Fel:", conditionMessage(e)), type = "error", duration = NULL)
        NULL
      })
      shiny::removeNotification(notis_id)
      if (is.null(resultat)) return()

      nya_filer <- if (dir.exists(utmapp_varde)) {
        alla <- list.files(utmapp_varde, full.names = TRUE)
        # file.mtime(), inte file.info(): den senare slår på Windows upp filernas ägare och varnar för
        # filer vars ägarkonto inte går att slå upp
        alla[file.mtime(alla) >= innan_tid - 1]
      } else {
        character(0)
      }

      if (length(nya_filer) == 0) {
        shiny::showNotification(
          paste0("Diagrammet kördes, men hittade ingen ny/uppdaterad fil i:\n", utmapp_varde),
          type = "warning", duration = 7
        )
      } else {
        shiny::showNotification(
          paste0("Diagrammet har skapats:\n", paste(nya_filer, collapse = "\n")),
          type = "message", duration = 7
        )
      }
    })

    shiny::observeEvent(input$hjalp, {
      # Visas i en egen modal i stället för RStudios Hjälp-ruta - den ligger
      # i RStudios huvudfönster, som hamnar bakom gadgetens eget
      # dialogfönster (dialogViewer()) och syns då inte.
      shiny::showModal(shiny::modalDialog(
        title = vald_fn(),
        shiny::tags$pre(
          style = "white-space: pre-wrap; font-family: monospace; font-size: 12px;",
          shiny::HTML(intern_funktion_hjalptext_html(vald_fn()))
        ),
        size = "l",
        easyClose = TRUE,
        footer = shiny::modalButton("Cancel")
      ))
    })

    shiny::observeEvent(input$kopiera, {
      kod <- bygg_anrop()
      kopierad <- requireNamespace("clipr", quietly = TRUE) && clipr::clipr_available() &&
        !inherits(try(clipr::write_clip(kod, allow_non_interactive = TRUE), silent = TRUE), "try-error")
      if (kopierad) {
        shiny::showNotification("Koden är kopierad till urklipp.", type = "message", duration = 4)
      } else if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
        rstudioapi::insertText(kod)
        shiny::showNotification("Urklippet gick inte att nå - koden har infogats i skriptet.", type = "warning", duration = 7)
      } else {
        message(kod)
        shiny::showNotification("Urklippet gick inte att nå - koden har skrivits ut i konsolen.", type = "warning", duration = 7)
      }
    })

    shiny::observeEvent(input$done, shiny::stopApp())
  }

  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("Diagramkatalog", width = 1100, height = 750))
}
