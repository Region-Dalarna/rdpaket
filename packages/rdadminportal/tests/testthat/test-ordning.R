test_that("ordning_satt skickar rätt parametrar", {
  samlade <- list()
  testthat::local_mocked_bindings(
    intern_con_skriv = function() "fakecon",
    intern_regenerera_landningssida = function() { samlade[[length(samlade) + 1]] <<- list(typ = "regenerera"); invisible(TRUE) }
  )
  testthat::local_mocked_bindings(
    dbExecute = function(con, sql, params = NULL) { samlade[[length(samlade) + 1]] <<- list(typ = "ex", params = params); 1L },
    dbGetQuery = function(con, sql, params = NULL) { samlade[[length(samlade) + 1]] <<- list(typ = "fraga", sql = sql, params = params); data.frame() },
    dbWithTransaction = function(conn, code, ...) force(code),
    dbDisconnect = function(con) invisible(TRUE),
    .package = "DBI"
  )

  suppressMessages(landningssida_ordning_satt("publik", c("brott", "befolkning", "export"), andrad_av = "tester"))
  inlagg <- Filter(function(x) x$typ == "ex", samlade)
  expect_equal(length(inlagg), 3)
  expect_equal(vapply(inlagg, function(x) x$params[[2]], ""), c("brott", "befolkning", "export"))
  expect_equal(vapply(inlagg, function(x) x$params[[3]], 1L), 1:3)
  expect_true(all(vapply(inlagg, function(x) x$params[[1]] == "publik" && x$params[[4]] == "tester", TRUE)))
  notis <- Filter(function(x) x$typ == "fraga", samlade)[[1]]
  expect_match(notis$sql, "pg_notify")
  expect_equal(notis$params, list("adminportal_publik", "landningssida_ordning"))
  expect_false(any(vapply(samlade, function(x) x$typ == "regenerera", TRUE)))

  # intern: ingen notifiering, i stället regenereras landningssidan direkt
  samlade <- list()
  suppressMessages(landningssida_ordning_satt("intern", c("a", "b")))
  expect_false(any(vapply(samlade, function(x) x$typ == "fraga", TRUE)))
  expect_true(any(vapply(samlade, function(x) x$typ == "regenerera", TRUE)))
})

test_that("ordning_satt godkänner pdf-namn men avvisar ogiltiga namn och dubbletter", {
  testthat::local_mocked_bindings(intern_con_skriv = function() stop("ska inte nås"))
  expect_error(landningssida_ordning_satt("publik", c("brott", "brott")), "dubbletter")
  expect_error(landningssida_ordning_satt("publik", "app namn"), "Ogiltiga")
  expect_error(landningssida_ordning_satt("annan", "brott"), "should be one of|Ogiltigt")
  expect_error(landningssida_ordning_satt("publik", "brott"), "ska inte nås")   # giltigt namn går vidare till databasen
})

test_that("översikten fungerar utan ordningstabell och sorterar appar, rapporter, pdf", {
  sql_text <- NULL
  testthat::local_mocked_bindings(intern_con_las = function() "fakecon")
  testthat::local_mocked_bindings(
    dbGetQuery = function(con, sql, params = NULL) {
      if (grepl("to_regclass", sql)) return(data.frame(finns = FALSE))
      sql_text <<- sql
      data.frame()
    },
    dbDisconnect = function(con) invisible(TRUE),
    .package = "DBI"
  )
  landningssida_app_oversikt("publik")
  expect_false(grepl("landningssida_ordning", sql_text))
  expect_match(sql_text, "NULL::integer AS ordning", fixed = TRUE)
  expect_match(sql_text, "WHEN 'app' THEN 1 WHEN 'rapport' THEN 2", fixed = TRUE)

  testthat::local_mocked_bindings(
    dbGetQuery = function(con, sql, params = NULL) {
      if (grepl("to_regclass", sql)) return(data.frame(finns = TRUE))
      sql_text <<- sql
      data.frame()
    },
    dbDisconnect = function(con) invisible(TRUE),
    .package = "DBI"
  )
  landningssida_app_oversikt("publik")
  expect_match(sql_text, "LEFT JOIN adminshiny.landningssida_ordning", fixed = TRUE)
  expect_match(sql_text, "o.ordning NULLS LAST, a.namn", fixed = TRUE)
})
