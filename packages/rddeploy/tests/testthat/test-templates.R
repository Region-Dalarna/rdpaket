test_that("mallfiler finns med i paketet", {
  for (f in c("deploy.yml", "avpublicera.yml", "global.R", "ui.R", "server.R",
              "_dependencies.R", "app.css", "gitignore",
              "_publicering_till_server.yml", "README.md", "README_fork.md")) {
    expect_true(nzchar(system.file("templates", "shinyapp", f, package = "rddeploy")),
                info = f)
  }
})

test_that("intern_las_mall fyller i <<variabler>>", {
  txt <- intern_las_mall("shinyapp", "app.css", list(github_repo = "minapp"))
  expect_match(txt, "minapp", fixed = TRUE)
  expect_no_match(txt, "<<github_repo>>", fixed = TRUE)
})

test_that("deploy.yml behåller GitHub Actions-uttryck men fyller i suffix", {
  txt <- intern_las_mall("shinyapp", "deploy.yml", list(temp_dir_suffix = "/app"))
  expect_match(txt, "${{ github.event.repository.name }}", fixed = TRUE)
  expect_match(txt, 'TEMP_DIR="${GITHUB_WORKSPACE}/app"', fixed = TRUE)
})

test_that("intern_skriv_mall skriver ifylld fil", {
  tmp <- withr::local_tempfile(fileext = ".yml")
  intern_skriv_mall("shinyapp", "_publicering_till_server.yml", tmp,
                    list(target = "intern"))
  expect_match(paste(readLines(tmp), collapse = "\n"), "target: intern")
})
