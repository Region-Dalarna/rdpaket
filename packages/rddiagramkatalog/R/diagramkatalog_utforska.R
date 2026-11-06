#' Utforska och sök bland diagramfunktionerna (RStudio-addin)
#'
#' Öppnar ett eget, större fönster ovanpå RStudio där man kan söka bland
#' paketets diagramfunktioner (samma sökning som [sok_diagram()]), läsa
#' hjälptexten för en vald funktion, fylla i parametrar, och antingen köra
#' diagrammet direkt för en förhandsvisning eller infoga motsvarande R-kod i
#' det aktiva skriptet. Nås enklast via RStudios Addins-meny ("Utforska
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
#' "Visa hjälp" visar funktionens hjälptext (samma innehåll som
#' `?funktionsnamn`) i en egen ruta ovanpå gadgeten, i stället för i RStudios
#' Hjälp-ruta - annars hamnar den bakom gadgetens eget dialogfönster och syns
#' inte.
#'
#' Kräver `shiny` och `miniUI` (`install.packages(c("shiny", "miniUI"))`),
#' som bara är valfria beroenden till paketet eftersom de bara behövs för
#' den här interaktiva funktionen. "Infoga kod" kräver dessutom `rstudioapi`
#' och att man kör RStudio - annars skrivs koden ut i konsolen istället.
#'
#' @return Ingenting (anropas interaktivt). Avslutas med `stopApp()`.
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
        litenknapp("infoga", "Infoga kod", TRUE)
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
      bildfil <- if (nzchar(figurmapp)) file.path(figurmapp, paste0(fn, ".png")) else ""

      if (nzchar(bildfil) && file.exists(bildfil)) {
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
      } else {
        shiny::tags$p(
          shiny::tags$em("Inget exempel sparat för den här funktionen än."),
          style = "color: #999; font-size: 12px;"
        )
      }
    })

    output$parameterformular <- shiny::renderUI({
      args <- vald_args()

      shiny::tagList(
        shiny::h4(vald_fn()),
        lapply(names(args), function(namn) {
          standard <- args[[namn]]
          input_id <- paste0("param_", namn)
          ar_boolesk <- identical(standard, TRUE) || identical(standard, FALSE)

          if (ar_boolesk) {
            shiny::checkboxInput(input_id, namn, value = isTRUE(standard))
          } else {
            shiny::textInput(input_id, namn, value = paste(deparse(standard), collapse = " "))
          }
        })
      )
    })

    bygg_anrop <- shiny::reactive({
      args <- vald_args()

      argument_text <- vapply(names(args), function(namn) {
        input_id <- paste0("param_", namn)
        standard <- args[[namn]]
        ar_boolesk <- identical(standard, TRUE) || identical(standard, FALSE)
        varde <- input[[input_id]]
        if (is.null(varde)) return(paste0(namn, " = ", paste(deparse(standard), collapse = " ")))
        if (ar_boolesk) {
          paste0(namn, " = ", if (isTRUE(varde)) "TRUE" else "FALSE")
        } else {
          paste0(namn, " = ", varde)
        }
      }, character(1))

      paste0(vald_fn(), "(\n  ", paste(argument_text, collapse = ",\n  "), "\n)")
    })

    shiny::observeEvent(input$visa, {
      kod <- bygg_anrop()
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
        resultat[[1]]
      })
    })

    shiny::observeEvent(input$hjalp, {
      # Visas i en egen modal i stället för RStudios Hjälp-ruta - den ligger
      # i RStudios huvudfönster, som hamnar bakom gadgetens eget
      # dialogfönster (dialogViewer()) och syns då inte.
      shiny::showModal(shiny::modalDialog(
        title = vald_fn(),
        shiny::tags$pre(
          style = "white-space: pre-wrap; font-family: monospace; font-size: 12px;",
          intern_funktion_hjalptext(vald_fn())
        ),
        size = "l",
        easyClose = TRUE,
        footer = shiny::modalButton("Cancel")
      ))
    })

    shiny::observeEvent(input$infoga, {
      kod <- bygg_anrop()
      if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
        rstudioapi::insertText(kod)
      } else {
        message(kod)
      }
      shiny::stopApp()
    })

    shiny::observeEvent(input$done, shiny::stopApp())
  }

  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("Diagramkatalog", width = 1100, height = 750))
}
