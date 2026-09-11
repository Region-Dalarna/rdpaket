# CSV/zip/base64 - utbrutna ur func_API.R.

#' @keywords internal
intern_separator_gissa <- function(fil) {
  forsta_raden <- readLines(fil, n = 1, warn = FALSE)
  if (length(forsta_raden) == 0) return(",")
  if (nchar(gsub("[^;]", "", forsta_raden)) > nchar(gsub("[^,]", "", forsta_raden))) ";" else ","
}

#' Spara en eller flera data.frames som csv-filer i en zip
#'
#' @param df_list En `data.frame` eller en (namngiven) lista av `data.frame`s.
#' @param output_mapp Målmapp. `NULL` = [utskriftsmapp()].
#' @param zipfilnamn Namn på zip-filen. `NA` = härled.
#' @param pre_namn_csv_fil_utan_namn Prefix för csv-filer vars listelement saknar namn.
#' @param meddelande Om `TRUE` skrivs en bekräftelse ut.
#'
#' @return Inget - skriver zip-filen.
#' @export
spara_som_csv_i_zip <- function(df_list, output_mapp = NULL, zipfilnamn = NA,
                                pre_namn_csv_fil_utan_namn = "df_", meddelande = TRUE) {
  if (!requireNamespace("zip", quietly = TRUE)) stop("Paketet 'zip' krävs.", call. = FALSE)

  if (is.data.frame(df_list)) {
    df_list <- stats::setNames(list(df_list), deparse(substitute(df_list)))
  }
  if (is.null(output_mapp)) output_mapp <- utskriftsmapp()

  namn <- names(df_list)
  namn[is.na(namn) | grepl("^\\.[0-9]+", namn) | !nzchar(namn)] <-
    paste0(pre_namn_csv_fil_utan_namn, seq_along(df_list))[is.na(namn) | grepl("^\\.[0-9]+", namn) | !nzchar(namn)]

  csvfil_lista <- vapply(seq_along(df_list), function(i) {
    csv_fil <- fs::path(output_mapp, paste0(namn[i], ".csv"))
    utils::write.csv(df_list[[i]], csv_fil, row.names = FALSE)
    csv_fil
  }, character(1))

  if (is.na(zipfilnamn)) {
    zipfilnamn <- sub("\\.csv$", ".zip", unique(sub("[0-9]", "", basename(csvfil_lista)))[1])
  }
  if (!grepl("\\.zip$", zipfilnamn)) zipfilnamn <- paste0(zipfilnamn, ".zip")

  zip_full <- fs::path(output_mapp, zipfilnamn)
  if (file.exists(zip_full)) file.remove(zip_full)
  zip::zip(zip_full, files = csvfil_lista, mode = "cherry-pick")
  file.remove(csvfil_lista)

  if (meddelande) message("Filen ", zipfilnamn, " har sparats i mappen ", output_mapp)
  invisible(zip_full)
}

#' Läs csv-filer direkt ur en eller flera zip-filer
#'
#' @param zip_sokvagar Sökväg(ar) till zip-filer som innehåller csv-filer.
#' @param kalla_som_kolumn Om `TRUE` och `bind_ihop_dataseten = TRUE` läggs
#'   kolumner `zip_fil` och `csv_fil` till som visar varifrån varje rad
#'   kommer. Påverkar inte listläget (där varje element redan är namngivet
#'   efter zip- och csv-fil).
#' @param bind_ihop_dataseten Om `TRUE` binds alla csv-filers innehåll ihop
#'   till en enda `tibble` med [dplyr::bind_rows()] (det gamla beteendet).
#'   Om `FALSE` (standard) returneras i stället en namngiven lista med en
#'   `tibble` per csv-fil, vilket passar bäst när filerna inte nödvändigtvis
#'   har samma kolumner. Vill man binda ihop listan själv efteråt går det
#'   lika gärna med `dplyr::bind_rows()`/`purrr::list_rbind()`.
#'
#' @return Om `bind_ihop_dataseten = FALSE` (standard): en namngiven lista
#'   med en `tibble` per csv-fil, med namn `"<zipfil>/<csvfil>"`. Om
#'   `bind_ihop_dataseten = TRUE`: en enda `tibble` med allt ihopbundet.
#' @export
csv_fran_zipfiler_inlasning <- function(zip_sokvagar, kalla_som_kolumn = FALSE,
                                         bind_ihop_dataseten = FALSE) {
  per_zip <- stats::setNames(lapply(zip_sokvagar, function(zip_path) {
    tmp_dir <- tempfile()
    dir.create(tmp_dir)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    csv_namn <- Filter(function(f) grepl("\\.csv$", f),
                       utils::unzip(zip_path, list = TRUE)$Name)
    utils::unzip(zip_path, files = csv_namn, exdir = tmp_dir)

    stats::setNames(lapply(csv_namn, function(f) {
      full_path <- file.path(tmp_dir, f)
      sep <- intern_separator_gissa(full_path)
      utils::read.delim(full_path, sep = sep, check.names = FALSE)
    }), csv_namn)
  }), basename(zip_sokvagar))

  if (bind_ihop_dataseten) {
    per_zip <- lapply(per_zip, function(per_csv) {
      dplyr::bind_rows(per_csv, .id = if (kalla_som_kolumn) "csv_fil" else NULL)
    })
    return(dplyr::bind_rows(per_zip, .id = if (kalla_som_kolumn) "zip_fil" else NULL))
  }

  # Platta ut till en enda lista: en tibble per csv-fil, namngiven
  # "<zipfil>/<csvfil>" så att namnen är unika även när flera zip-filer
  # innehåller likadant namngivna csv-filer.
  unlist(lapply(names(per_zip), function(zn) {
    stats::setNames(per_zip[[zn]], paste0(zn, "/", names(per_zip[[zn]])))
  }), recursive = FALSE)
}

#' Läs en base64-kodad fil och avkoda till text
#'
#' @param sokvag_filnamn Sökväg till filen.
#'
#' @return Det avkodade innehållet som en sträng.
#' @export
las_b64 <- function(sokvag_filnamn) {
  if (!file.exists(sokvag_filnamn)) {
    stop("Filen finns inte: ", sokvag_filnamn, call. = FALSE)
  }
  x <- trimws(readLines(sokvag_filnamn, warn = FALSE, encoding = "UTF-8"))
  x <- x[nzchar(x)]
  if (!length(x)) stop("Filen är tom: ", sokvag_filnamn, call. = FALSE)
  rawToChar(jsonlite::base64_dec(x[[1]]))
}
