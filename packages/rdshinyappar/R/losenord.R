# Lösenordshantering via ~/.Renviron
#
# Lösenord lagras som miljövariabler <SERVICE>_PWD i användarens ~/.Renviron.
# Filen sätts till rättighet 600 på unix. Detta är medvetet enkelt: appar i
# drift läser lösenord med shiny_get_password() utan beroende på keyring eller
# annan interaktiv nyckelring.

intern_kontrollera_service <- function(service) {
  if (!grepl("^[A-Za-z0-9_]+$", service)) {
    stop("Service-namnet får bara innehålla A-Z, a-z, 0-9 och '_'.", call. = FALSE)
  }
  invisible(service)
}

intern_renviron_sokvag <- function() {
  file.path(Sys.getenv("HOME"), ".Renviron")
}

#' Spara ett lösenord i ~/.Renviron
#'
#' Skriver (eller ersätter) miljövariabeln `<service>_PWD` i användarens
#' `~/.Renviron` och sätter den i den aktiva sessionen. Utan `losenord`
#' efterfrågas det interaktivt.
#'
#' @param service Tjänstnamn, bara `A-Z a-z 0-9 _`.
#' @param losenord Lösenordssträng. `NULL` (standard) frågar interaktivt.
#'
#' @return `TRUE` osynligt om något skrevs, annars `FALSE`.
#' @export
shiny_set_password <- function(service, losenord = NULL) {

  intern_kontrollera_service(service)

  varname <- paste0(service, "_PWD")
  renv_file <- intern_renviron_sokvag()

  existing <- if (file.exists(renv_file)) readLines(renv_file) else character()

  if (is.null(losenord)) {

    existing_match <- grep(paste0("^", varname, "="), existing, value = TRUE)

    if (length(existing_match) > 0) {
      cat("Det finns redan ett lösenord för tjänsten '", service, "'.\n", sep = "")
      overwrite <- tolower(readline("Vill du skriva över det? (j/n): "))

      if (overwrite != "j") {
        cat("✔ Inget ändrat.\n")
        return(invisible(FALSE))
      }
    }

    cat("Ange lösenord för tjänsten '", service, "': ", sep = "")
    password <- readline()
  } else {
    password <- losenord
  }

  # ta bort ev. gammal rad med samma variabel
  existing <- existing[!grepl(paste0("^", varname, "="), existing)]

  new_content <- c(existing, paste0(varname, "=", password))

  writeLines(new_content, renv_file)

  if (.Platform$OS.type == "unix") {
    system(paste("chmod 600", shQuote(renv_file)))
  }

  args <- list(password)
  names(args) <- varname
  do.call(Sys.setenv, args)

  cat("✔ Installerat: ", varname, " i ", renv_file, "\n", sep = "")
  invisible(TRUE)
}

#' Hämta ett sparat lösenord
#'
#' Läser om `~/.Renviron` och returnerar `<service>_PWD`.
#'
#' @param service Tjänstnamn, bara `A-Z a-z 0-9 _`.
#'
#' @return Lösenordssträngen. Fel om variabeln saknas.
#' @export
shiny_get_password <- function(service) {

  intern_kontrollera_service(service)

  varname <- paste0(service, "_PWD")
  readRenviron(intern_renviron_sokvag())
  pw <- Sys.getenv(varname, unset = NA)

  if (is.na(pw) || !nzchar(pw)) {
    stop("Lösenord saknas. Variabeln '", varname, "' finns inte i miljön.", call. = FALSE)
  }

  pw
}

#' Ta bort ett sparat lösenord
#'
#' Tar bort raden `<service>_PWD=...` ur `~/.Renviron`.
#'
#' @param service Tjänstnamn, bara `A-Z a-z 0-9 _`.
#'
#' @return `NULL` osynligt.
#' @export
shiny_delete_password <- function(service) {

  intern_kontrollera_service(service)

  varname <- paste0(service, "_PWD")
  readRenviron(intern_renviron_sokvag())
  pw <- Sys.getenv(varname, unset = NA)

  if (is.na(pw) || !nzchar(pw)) {
    stop("Lösenord saknas. Variabeln '", varname, "' finns inte i miljön.", call. = FALSE)
  }

  renv_file <- intern_renviron_sokvag()

  if (!file.exists(renv_file)) {
    stop("Filen ", renv_file, " finns inte. Inget att ta bort.", call. = FALSE)
  }

  existing <- readLines(renv_file)
  new_content <- existing[!grepl(paste0("^", varname, "="), existing)]
  writeLines(new_content, renv_file)

  # ta bort ur den aktiva sessionen också, annars ligger det gamla värdet kvar
  Sys.unsetenv(varname)

  cat("✔ Borttaget: ", varname, " från ", renv_file, "\n", sep = "")
  invisible(NULL)
}

#' Lista tjänster med sparade lösenord
#'
#' @return Teckenvektor med tjänstnamn (variabler som slutar på `_PWD` i
#'   `~/.Renviron`, utan suffixet).
#' @export
shiny_list_passwords <- function() {

  renv_file <- intern_renviron_sokvag()

  if (!file.exists(renv_file)) {
    stop(".Renviron-filen finns inte på denna maskin: ", renv_file, call. = FALSE)
  }

  lines <- readLines(renv_file)
  matches <- grep("^[A-Za-z0-9_]+_PWD=", lines, value = TRUE)

  if (length(matches) == 0) {
    stop("Inga tjänster hittades i .Renviron.", call. = FALSE)
  }

  services <- sub("_PWD=.*$", "", matches)

  cat("Tjänster med sparade lösenord:\n")
  services
}
