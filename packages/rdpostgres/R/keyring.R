# Lägga in databasinloggningar i keyring (OS:ets credential store).

#' Fråga efter ett lösenord i ett maskerat tcltk-fönster
#'
#' @param title Fönstertitel.
#' @param prompt Ledtext.
#' @return Lösenordet som sträng, eller `""` om användaren avbröt.
#' @export
get_password_tk <- function(title = "Inloggning - lösenord", prompt = "Skriv lösenord:") {
  if (!requireNamespace("tcltk", quietly = TRUE)) {
    stop("Paketet 'tcltk' krävs.", call. = FALSE)
  }
  tt <- tcltk::tktoplevel()
  tcltk::tkwm.title(tt, title)

  bredd <- 320; hojd <- 130
  sk_b <- as.integer(tcltk::tkwinfo("screenwidth", tt))
  sk_h <- as.integer(tcltk::tkwinfo("screenheight", tt))
  tcltk::tkwm.geometry(tt, sprintf("%dx%d+%d+%d", bredd, hojd,
                                   as.integer((sk_b - bredd) / 2),
                                   as.integer((sk_h - hojd) / 2)))

  ram <- tcltk::tkframe(tt, padx = 15, pady = 10)
  tcltk::tkpack(ram, expand = TRUE)
  tcltk::tkpack(tcltk::tklabel(ram, text = prompt), pady = 5)

  pw <- tcltk::tclVar("")
  entry <- tcltk::tkentry(ram, textvariable = pw, show = "*", width = 30)
  tcltk::tkpack(entry, pady = 5)

  ok <- FALSE
  stang <- function() { ok <<- TRUE; tcltk::tkdestroy(tt) }
  tcltk::tkpack(tcltk::tkbutton(ram, text = "OK", command = stang), pady = 5)
  tcltk::tkbind(entry, "<Return>", stang)
  tcltk::tkfocus(entry)
  tcltk::tkwait.window(tt)

  if (!ok) "" else tcltk::tclvalue(pw)
}

#' Lägg till en databasinloggning i keyring
#'
#' Frågar interaktivt efter användarnamn (via `svDialogs`) och lösenord (via
#' [get_password_tk()]) och sparar dem för `keyring_service`. Finns en
#' inloggning redan frågas om den ska ersättas.
#'
#' @param keyring_service Namn på keyring-servicen, t.ex. `"databas_adm"`.
#' @return Osynligt `NULL`.
#' @export
keyring_lagg_till_inloggning <- function(keyring_service) {
  if (!requireNamespace("keyring", quietly = TRUE)) stop("Paketet 'keyring' krävs.", call. = FALSE)
  if (!requireNamespace("svDialogs", quietly = TRUE)) stop("Paketet 'svDialogs' krävs.", call. = FALSE)

  username <- svDialogs::dlg_input("Skriv användarnamn:",
                                   default = Sys.info()[["user"]], title = "Inloggning")$res
  if (is.null(username) || !nzchar(username)) {
    message("Ingen inloggning skapad (användarnamn saknas).")
    return(invisible(NULL))
  }

  finns <- tryCatch({ keyring::key_get(keyring_service, username); TRUE },
                    error = function(e) FALSE)
  ersatt <- FALSE
  if (finns) {
    svar <- svDialogs::dlg_message(
      paste0("Inloggning '", keyring_service, "' / '", username,
             "' finns redan. Ta bort den och lägg in på nytt?"), type = "yesno")$res
    if (tolower(svar) == "no") return(invisible(NULL))
    ersatt <- TRUE
  }

  password <- get_password_tk()
  if (!nzchar(password)) {
    message("Ingen inloggning skapad (lösenord saknas).")
    return(invisible(NULL))
  }

  if (ersatt) {
    keyring::key_delete(service = keyring_service,
                        username = keyring::key_list(service = keyring_service)$username[1])
  }
  keyring::key_set_with_value(service = keyring_service, username = username, password = password)
  svDialogs::dlg_message(
    paste0("Inloggning för '", keyring_service, "' / '", username, "' sparad i keyring."),
    type = "ok")
  invisible(NULL)
}
