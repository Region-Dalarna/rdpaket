# Fil- och mappfunktioner - utbrutna ur func_filer.R i Region-Dalarna/funktioner.

intern_dela_filnamn <- function(fil_namn) {
  bitar <- strsplit(fil_namn, "\\.")[[1]]
  if (length(bitar) < 2) {
    list(stam = fil_namn, andelse = "")
  } else {
    list(
      stam = paste(bitar[-length(bitar)], collapse = "."),
      andelse = bitar[length(bitar)]
    )
  }
}

#' Ge en fil ett unikt namn om den redan finns
#'
#' Om `full_path` redan finns läggs `_2`, `_3` ... till stammen tills namnet är
#' ledigt. Filen skrivs inte - funktionen returnerar bara en ledig sökväg.
#'
#' @param full_path Full sökväg (mapp + filnamn) till filen.
#' @param fraga Om `TRUE` frågas användaren interaktivt om att skriva över.
#' @param skrivover Om `TRUE` returneras `full_path` oförändrad.
#'
#' @return En sökväg som inte pekar på en befintlig fil.
#' @export
sparafil_unik <- function(full_path, fraga = FALSE, skrivover = FALSE) {
  if (!file.exists(full_path)) return(full_path)

  if (isTRUE(fraga)) {
    skrivover <- isTRUE(utils::askYesNo(
      "Filen finns redan, vill du skriva över den?", default = FALSE
    ))
  }
  if (isTRUE(skrivover)) return(full_path)

  mapp_namn <- dirname(full_path)
  delar <- intern_dela_filnamn(basename(full_path))

  nummer <- 2L
  repeat {
    kandidat <- file.path(mapp_namn, paste0(delar$stam, "_", nummer, ".", delar$andelse))
    if (!file.exists(kandidat)) return(kandidat)
    nummer <- nummer + 1L
  }
}

#' Skapa en mapp om den inte redan finns
#'
#' @param sokvag Sökväg till mappen. Skapas rekursivt.
#'
#' @return `sokvag` (osynligt).
#' @export
skapa_mapp_om_den_inte_finns <- function(sokvag) {
  fs::dir_create(sokvag)
  invisible(sokvag)
}

#' Döp om en befintlig fil till `_backup` innan en ny skrivs
#'
#' Om `full_path` finns döps den om till `<stam>_backup.<ändelse>` (en ev.
#' tidigare `_backup` skrivs över), så att den nya filen kan få originalnamnet.
#'
#' @param full_path Full sökväg till filen.
#'
#' @return `full_path` (oförändrad).
#' @export
sparafil_en_backup_nvdb <- function(full_path) {
  if (!file.exists(full_path)) return(full_path)

  mapp_namn <- dirname(full_path)
  delar <- intern_dela_filnamn(basename(full_path))
  backup_fil <- file.path(mapp_namn, paste0(delar$stam, "_backup.", delar$andelse))

  if (file.exists(backup_fil)) file.remove(backup_fil)
  file.rename(full_path, backup_fil)
  full_path
}

#' Ta en numrerad backup på en befintlig fil
#'
#' Om `full_path` finns kopieras den till `<stam>_old_<n>.<ändelse>` (lägsta
#' lediga `n`) och originalet tas bort, så att den nya filen kan få
#' originalnamnet.
#'
#' @inheritParams sparafil_unik
#'
#' @return `full_path` (oförändrad).
#' @export
sparafil_backup_omfinns <- function(full_path, fraga = FALSE, skrivover = FALSE) {
  if (!file.exists(full_path)) return(full_path)

  if (isTRUE(fraga)) {
    skrivover <- isTRUE(utils::askYesNo(
      paste0("Det finns redan en fil med samma namn, vill du göra en ",
             "backup på den och sedan skriva över den?"),
      default = FALSE
    ))
  }
  if (isTRUE(skrivover)) return(full_path)

  mapp_namn <- dirname(full_path)
  delar <- intern_dela_filnamn(basename(full_path))

  nummer <- 2L
  repeat {
    ny_path <- file.path(mapp_namn, paste0(delar$stam, "_old_", nummer, ".", delar$andelse))
    if (!file.exists(ny_path)) {
      file.copy(full_path, ny_path)
      file.remove(full_path)
      break
    }
    nummer <- nummer + 1L
  }
  full_path
}

#' Ladda ner en fil och packa upp om den är en zip
#'
#' Laddar ner filen som `fil_url` pekar direkt på till `output_mapp` (som skapas
#' om den saknas). Är filen en `.zip` packas den upp och zip-filen tas bort.
#'
#' @param output_mapp Mapp att spara filen i.
#' @param fil_url Direktlänk till en fil (inte en mapp eller ftp).
#'
#' @return Sökvägen till den nedladdade filen, eller till uppackningsmappen om
#'   det var en zip (osynligt).
#' @export
ladda_ned_fil <- function(output_mapp, fil_url) {
  fs::dir_create(output_mapp)

  fil_namn <- basename(fil_url)
  mal <- fs::path(output_mapp, fil_namn)

  utils::download.file(fil_url, destfile = mal, mode = "wb")

  if (tolower(fs::path_ext(fil_namn)) == "zip") {
    utils::unzip(mal, exdir = output_mapp)
    file.remove(mal)
    return(invisible(fs::path(output_mapp)))
  }
  invisible(mal)
}

## `%notin%` finns i base R sedan 4.5.0 - ingen egen definition behövs.

#' Datum för nästa förekomst av en viss veckodag
#'
#' `nextweekday(Sys.Date(), 2)` ger nästa måndag. Veckodag anges som
#' 1 = söndag, 2 = måndag, ... 7 = lördag (samma som `lubridate::wday()`).
#' Om `date` redan är rätt veckodag returneras dagen en vecka senare.
#'
#' @param date Ett datum (eller något `as.Date()` klarar).
#' @param wday Veckodag, 1 = söndag ... 7 = lördag.
#'
#' @return Ett `Date`.
#' @export
nextweekday <- function(date, wday) {
  date <- as.Date(date)
  aktuell <- as.integer(format(date, "%w")) + 1L   # %w: 0 = sön ... 6 = lör
  diff <- wday - aktuell
  if (diff < 1) diff <- diff + 7
  date + diff
}

