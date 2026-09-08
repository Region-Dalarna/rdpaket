test_that("intern_veckodag_namn är sju svenska dagar i ISO-ordning", {
  expect_length(intern_veckodag_namn, 7)
  expect_equal(intern_veckodag_namn[1], "måndag")
  expect_equal(intern_veckodag_namn[7], "söndag")
})

test_that("intern_sessionsstart_per_veckodag_timme tar tidigaste tid per session", {
  rader <- data.frame(
    session = c("a", "a", "b"),
    time = as.POSIXct(c("2026-01-05 08:30", "2026-01-05 09:00", "2026-01-06 14:15"), tz = "UTC")
  )
  ss <- intern_sessionsstart_per_veckodag_timme(rader)
  expect_equal(nrow(ss), 2)
  expect_equal(ss$veckodag[ss$session == "a"], "måndag")
  expect_equal(ss$timme[ss$session == "a"], 8L)
  expect_equal(ss$veckodag[ss$session == "b"], "tisdag")
})

test_that("skapa_telemetry returnerar NULL utan shiny.telemetry", {
  skip_if(requireNamespace("shiny.telemetry", quietly = TRUE))
  expect_warning(res <- skapa_telemetry("test"), "shiny.telemetry")
  expect_null(res)
})

test_that("telemetri_ui och telemetri_server hanterar NULL", {
  expect_null(telemetri_ui(NULL))
  expect_null(telemetri_server(NULL, "nav"))
})
