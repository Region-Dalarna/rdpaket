test_that("rdpaket_uppdatera_alla kräver 'ägare/repo'-format", {
  expect_error(
    rdpaket_uppdatera_alla(paket = "rdverktyg", repo = "fel-format"),
    "ägare/repo"
  )
})

test_that("rdpaket_uppdatera_alla returnerar tom status när inga paket är installerade", {
  expect_message(
    status <- rdpaket_uppdatera_alla(paket = character(0)),
    "Inga rd\\*-paket"
  )
  expect_equal(nrow(status), 0)
  expect_named(status, c("paket", "installerad_version", "github_version", "uppdaterad"))
})

test_that("rdpaket_uppdatera_alla flaggar nyare GitHub-version utan att installera", {
  local_mocked_bindings(
    intern_rdpaket_github_version = function(...) "999.0.0"
  )

  expect_message(
    status <- rdpaket_uppdatera_alla(paket = "rdverktyg", installera = FALSE),
    "ny version tillg.*ngli"
  )
  expect_equal(status$github_version, "999.0.0")
  expect_false(status$uppdaterad)
})

test_that("rdpaket_uppdatera_alla ser samma version som redan uppdaterad", {
  installerad <- as.character(utils::packageVersion("rdverktyg"))
  local_mocked_bindings(
    intern_rdpaket_github_version = function(...) installerad
  )

  expect_message(
    status <- rdpaket_uppdatera_alla(paket = "rdverktyg"),
    "redan senaste versionen"
  )
  expect_false(status$uppdaterad)
})

test_that("rdpaket_uppdatera_alla hanterar okänd GitHub-version (NA) utan fel", {
  local_mocked_bindings(
    intern_rdpaket_github_version = function(...) NA_character_
  )

  status <- rdpaket_uppdatera_alla(paket = "rdverktyg", installera = FALSE)
  expect_true(is.na(status$github_version))
  expect_false(status$uppdaterad)
})

test_that("rdpaket_uppdatera_alla installerar om via remotes när nyare version finns", {
  local_mocked_bindings(
    intern_rdpaket_github_version = function(...) "999.0.0"
  )
  installerat_med <- NULL
  local_mocked_bindings(
    install_github = function(repo, subdir, ref, ...) {
      installerat_med <<- list(repo = repo, subdir = subdir, ref = ref)
      invisible(NULL)
    },
    .package = "remotes"
  )

  status <- rdpaket_uppdatera_alla(paket = "rdverktyg", installera = TRUE)

  expect_true(status$uppdaterad)
  expect_equal(installerat_med$repo, "Region-Dalarna/rdpaket")
  expect_equal(installerat_med$subdir, "packages/rdverktyg")
  expect_equal(installerat_med$ref, "main")
})
