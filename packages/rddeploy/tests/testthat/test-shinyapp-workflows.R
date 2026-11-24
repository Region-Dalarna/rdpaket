# Gammal deploy.yml (före korta mallen): samma form, men med egna steg efter shiny_deploy.sh
gammal_deploy <- function(suffix = "") {
  paste0('name: Deploy Shiny app\n',
         'jobs:\n  deploy:\n    steps:\n',
         '      - name: Deploy app using server-side script\n        run: |\n',
         '          TEMP_DIR="${GITHUB_WORKSPACE}', suffix, '"\n',
         '          /usr/local/bin/shiny_deploy.sh "${{ github.event.repository.name }}" "$TEMP_DIR"\n',
         '      - name: Restart Shiny Server if available\n        run: sudo systemctl restart shiny-server\n')
}

test_that("intern_workflow_nya_texter behåller mappsuffix och känner igen mallen", {
  nya <- intern_workflow_nya_texter(gammal_deploy("/app"))
  expect_match(nya$deploy, 'TEMP_DIR="${GITHUB_WORKSPACE}/app"', fixed = TRUE)
  expect_no_match(nya$deploy, "systemctl restart", fixed = TRUE)
  expect_match(nya$avpublicera, "shiny_avpublicera.sh", fixed = TRUE)

  nya0 <- intern_workflow_nya_texter(gammal_deploy(""))
  expect_match(nya0$deploy, 'TEMP_DIR="${GITHUB_WORKSPACE}"', fixed = TRUE)

  # handskriven workflow som inte kommer från mallen lämnas
  expect_null(intern_workflow_nya_texter("name: Min egen deploy\njobs: {}\n"))
})

test_that("intern_workflow_normalisera ignorerar registreringsraden och tomrader sist", {
  a <- "name: x\n\n# (touch för workflow-registrering)\n"
  expect_identical(intern_workflow_normalisera(a), intern_workflow_normalisera("name: x\n"))
})

test_that("shinyapp_uppdatera_workflows: torrkörning ändrar inget och rapporterar per repo", {
  skip_if_not_installed("gert")
  grund <- withr::local_tempdir()
  gor_repo <- function(namn, deploy_txt) {
    d <- file.path(grund, namn)
    dir.create(file.path(d, ".github", "workflows"), recursive = TRUE)
    writeLines(deploy_txt, file.path(d, ".github", "workflows", "deploy.yml"))
    gert::git_init(d)
    gert::git_add(".", repo = d)
    gert::git_commit("init", author = "t <t@t.se>", committer = "t <t@t.se>", repo = d)
    d
  }
  d1 <- gor_repo("app1", gammal_deploy(""))
  gor_repo("egen", "name: Min egen deploy\n")
  nytt <- intern_workflow_nya_texter(gammal_deploy(""))
  d3 <- gor_repo("klar", nytt$deploy)
  writeLines(nytt$avpublicera, file.path(d3, ".github", "workflows", "avpublicera.yml"))
  gert::git_add(".", repo = d3)
  gert::git_commit("avpub", author = "t <t@t.se>", committer = "t <t@t.se>", repo = d3)

  vid <- suppressMessages(shinyapp_uppdatera_workflows(grundsokvag = grund, torrkorning = TRUE))
  expect_setequal(vid$repo, c("app1", "klar"))     # 'egen' kommer inte från mallen och tas inte med
  expect_match(vid$status[vid$repo == "app1"], "skulle uppdatera deploy.yml och avpublicera.yml")
  expect_equal(vid$status[vid$repo == "klar"], "redan uppdaterad")
  # inget ändrat i app1
  expect_match(paste(readLines(file.path(d1, ".github", "workflows", "deploy.yml")), collapse = "\n"),
               "systemctl restart", fixed = TRUE)
})
