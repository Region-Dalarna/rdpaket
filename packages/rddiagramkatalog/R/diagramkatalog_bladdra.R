#' Bläddra och sök bland diagramfunktionerna (RStudio-addin)
#'
#' Öppnar ett eget, större fönster ovanpå RStudio där man kan söka bland
#' paketets diagramfunktioner (samma sökning som [sok_diagram()]), läsa
#' hjälptexten för en vald funktion, fylla i parametrar, och antingen köra
#' diagrammet direkt för en förhandsvisning eller infoga motsvarande R-kod i
#' det aktiva skriptet. Nås enklast via RStudios Addins-meny ("Bläddra
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
#' "Visa hjälp" öppnar funktionens vanliga hjälpsida (samma som `?funktionsnamn`)
#' i RStudios Hjälp-ruta.
#'
#' Kräver `shiny` och `miniUI` (`install.packages(c("shiny", "miniUI"))`),
#' som bara är valfria beroenden till paketet eftersom de bara behövs för
#' den här interaktiva funktionen. "Infoga kod" kräver dessutom `rstudioapi`
#' och att man kör RStudio - annars skrivs koden ut i konsolen istället.
#'
#' @return Ingenting (anropas interaktivt). Avslutas med `stopApp()`.
#' @export
diagramkatalog_bladdra <- function() {
  if (!requireNamespace("shiny", quietly = TRUE) ||
      !requireNamespace("miniUI", quietly = TRUE)) {
    stop(
      "Paketen 'shiny' och 'miniUI' krävs för diagramkatalog_bladdra().\n",
      "Installera med: install.packages(c(\"shiny\", \"miniUI\"))",
      call. = FALSE
    )
  }

  index <- intern_diagram_index_cached()

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Diagramkatalog", right = NULL),
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
          shiny::uiOutput("parameterformular"),
          shiny::fluidRow(
            shiny::column(4, shiny::actionButton("visa", "Visa diagram", class = "btn-primary", width = "100%")),
            shiny::column(4, shiny::actionButton("hjalp", "Visa hjälp", width = "100%")),
            shiny::column(4, shiny::actionButton("infoga", "Infoga kod", width = "100%"))
          ),
          shiny::hr(),
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
        resultat <- tryCatch(eval(parse(text = kod)), error = function(e) {
          shiny::showNotification(paste("Fel:", conditionMessage(e)), type = "error", duration = NULL)
          NULL
        })
        if (is.null(resultat)) return(NULL)
        resultat[[1]]
      })
    })

    shiny::observeEvent(input$hjalp, {
      print(utils::help(vald_fn(), package = "rddiagramkatalog"))
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
