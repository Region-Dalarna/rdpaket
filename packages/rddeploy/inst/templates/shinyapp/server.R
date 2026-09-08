shinyServer(function(input, output, session) {
<<telemetri_server_rad>>
  output$example_text <- renderText({
    'Byt ut detta mot din egen serverlogik.'
  })

})
