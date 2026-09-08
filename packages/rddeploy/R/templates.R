# Mallhantering. Mallfiler ligger i inst/templates/<kategori>/ och använder
# <<platshållare>> (glue med <<>>-delimiters, så att GitHub Actions-uttryck
# som ${{ ... }} kan stå orörda i YAML-mallarna).

intern_mall_sokvag <- function(kategori, filnamn) {
  p <- system.file("templates", kategori, filnamn, package = "rddeploy")
  if (!nzchar(p)) cli::cli_abort("Hittar inte mallen {.file {kategori}/{filnamn}}.")
  p
}

# Läs en mall och fyll i <<variabler>>. variabler = NULL -> returnera råtexten.
intern_las_mall <- function(kategori, filnamn, variabler = NULL) {
  raw <- readLines(intern_mall_sokvag(kategori, filnamn), warn = FALSE, encoding = "UTF-8")
  raw <- paste(raw, collapse = "\n")
  if (is.null(variabler)) return(raw)
  glue::glue_data(variabler, raw, .open = "<<", .close = ">>")
}

# Läs en mall och skriv den ifylld till malfil.
intern_skriv_mall <- function(kategori, filnamn, malfil, variabler = NULL) {
  txt <- intern_las_mall(kategori, filnamn, variabler)
  dir.create(dirname(malfil), recursive = TRUE, showWarnings = FALSE)
  writeLines(txt, malfil, useBytes = TRUE)
  invisible(malfil)
}
