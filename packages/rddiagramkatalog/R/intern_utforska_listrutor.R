# Listrutor (valbara värden) för parameterformuläret i diagramkatalog_utforska().
#
# Tre källor, i den här ordningen, för varje parameter:
#   1. `match.arg()`-parametrar: valen står i standardvärdet (c("a", "b", ...)), enkelval.
#   2. Regionparametrar (`region_vekt`, `jmfr_region`): sökbar lista med riket, län och kommuner från
#      rdverktyg::hamtaregtab(), där namnet visas och koden skickas.
#   3. SCB-parametrar: koden läses ur funktionen. Används en parameter direkt i `query = list(...)` i ett anrop
#      till pxweb2r::pxweb2_get_data(table = "TABxxxx", ...) hör den till den tabellen och variabeln, och de giltiga
#      värdena slås upp med pxweb2r::pxweb2_get_values() när funktionen väljs (cachas under sessionen).
#      Kopplingen godkänns bara om parameterns standardvärde finns bland variabelns klartexter eller koder, så en
#      parameter som bara råkar användas i en `if`-sats (t.ex. en boolesk parameter) aldrig får en listruta.
# Allt som inte hittas får den vanliga textrutan, så inget slutar fungera.

.utforska_cache <- new.env(parent = emptyenv())

# standardvärdet för en parameter som R-värde (NULL om det inte går att utvärdera)
#' @noRd
intern_utforska_standardvarde <- function(fn, namn) {
  utrymme <- asNamespace("rddiagramkatalog")
  uttryck <- formals(get(fn, envir = utrymme))[[namn]]
  if (missing(uttryck) || identical(uttryck, quote(expr = ))) return(NULL)
  tryCatch(eval(uttryck, envir = utrymme), error = function(e) NULL)
}

# parametrar som valideras med match.arg() i funktionen
#' @noRd
intern_utforska_matcharg <- function(fn) {
  kropp <- paste(deparse(body(get(fn, envir = asNamespace("rddiagramkatalog")))), collapse = " ")
  namn <- setdiff(names(formals(get(fn, envir = asNamespace("rddiagramkatalog")))), "...")
  namn[vapply(namn, function(n) grepl(paste0("match.arg\\(", n, "[,)]"), kropp), logical(1))]
}

# koppling parameter -> (tabell, variabel) ur pxweb2_get_data-anrop i funktionens kod. Följer enkla tilldelningar
# (`tid_vekt <- tid_koder[...]` och sedan `Tid = tid_vekt`) upp till tre led, så att parametrar som bearbetas lite innan
# de skickas också hittas. Felaktiga kopplingar sorteras bort senare, när standardvärdet jämförs med SCB:s värden.
#' @noRd
intern_utforska_pxweb_koppling <- function(fn) {
  fun <- get(fn, envir = asNamespace("rddiagramkatalog"))
  params <- setdiff(names(formals(fun)), "...")

  # första passet: alla tilldelningar (variabel -> variabler i högerledet)
  tilldelningar <- list()
  samla <- function(e) {
    if (missing(e) || !is.call(e)) return(invisible())
    if (is.symbol(e[[1]]) && as.character(e[[1]]) %in% c("<-", "=") && length(e) == 3 && is.symbol(e[[2]])) {
      namn <- as.character(e[[2]])
      tilldelningar[[namn]] <<- unique(c(tilldelningar[[namn]], all.vars(e[[3]])))
    }
    for (i in seq_along(e)) {
      el <- e[[i]]
      if (!missing(el)) samla(el)
    }
  }
  samla(body(fun))

  # parametrar som ett uttryck i slutänden bygger på
  till_parametrar <- function(symboler, djup = 3) {
    ut <- intersect(symboler, params)
    if (djup > 0) {
      for (sym in setdiff(symboler, params)) {
        if (!is.null(tilldelningar[[sym]])) ut <- c(ut, till_parametrar(tilldelningar[[sym]], djup - 1))
      }
    }
    unique(ut)
  }

  rader <- list()
  gå <- function(e) {
    if (missing(e) || !is.call(e)) return(invisible())
    fnamn <- tryCatch(paste(deparse(e[[1]]), collapse = ""), error = function(x) "")
    if (grepl("pxweb2_get_data$", fnamn)) {
      a <- as.list(e)[-1]
      nm <- names(a); if (is.null(nm)) nm <- rep("", length(a))
      tabell <- if ("table" %in% nm) a[["table"]] else a[[which(nm == "")[1]]]
      q <- a[["query"]]
      if (is.character(tabell) && length(tabell) == 1 && is.call(q) && identical(deparse(q[[1]]), "list")) {
        qa <- as.list(q)[-1]
        for (i in seq_along(qa)) {
          for (p in till_parametrar(all.vars(qa[[i]]))) {
            rader[[length(rader) + 1]] <<- data.frame(param = p, tabell = tabell, variabel = names(qa)[i])
          }
        }
      }
    }
    for (i in seq_along(e)) {
      el <- e[[i]]
      if (!missing(el)) gå(el)
    }
  }
  gå(body(fun))
  if (length(rader) == 0) return(data.frame(param = character(), tabell = character(), variabel = character()))
  koppling <- do.call(rbind, rader)
  # första träffen per parameter och variabel; parametrar som kopplas till flera olika variabler är för osäkra
  koppling <- koppling[!duplicated(koppling[c("param", "variabel")]), ]
  koppling[!(duplicated(koppling$param) | duplicated(koppling$param, fromLast = TRUE)), ]
}

# SCB-värden (kod och klartext) för en variabel i en tabell, cachade under sessionen
#' @noRd
intern_utforska_scb_varden <- function(tabell, variabel) {
  nyckel <- paste(tabell, variabel)
  if (is.null(.utforska_cache[[nyckel]])) {
    .utforska_cache[[nyckel]] <- tryCatch(
      as.data.frame(pxweb2r::pxweb2_get_values(tabell, variabel, quiet = TRUE))[, c("code", "label")],
      error = function(e) NA)
  }
  v <- .utforska_cache[[nyckel]]
  if (is.data.frame(v)) v else NULL
}

# regionlistan (riket, län, kommuner), cachad av rdverktyg
#' @noRd
intern_utforska_regioner <- function() {
  tryCatch(as.data.frame(rdverktyg::hamtaregtab()), error = function(e) NULL)
}

# Specifikation för en listruta: choices (värde = det som skickas, namn = det som visas), selected, multiple.
#' @noRd
intern_utforska_spec <- function(choices, standard, multiple) {
  valda <- standard[!is.na(standard)]
  # valda värden som saknas bland valen läggs till, så att standardvärdet alltid syns och kan skickas
  saknas <- setdiff(as.character(valda), unname(choices))
  choices <- c(choices, stats::setNames(saknas, saknas))
  list(choices = choices, selected = as.character(valda), multiple = multiple, numeriskt = is.numeric(standard))
}

# Alla listrutor för en funktion: en namngiven lista (parameternamn -> spec). Parametrar utan listruta saknas i listan.
#' @noRd
intern_utforska_listrutor <- function(fn) {
  namn <- setdiff(names(formals(get(fn, envir = asNamespace("rddiagramkatalog")))), "...")
  spec <- list()

  # 1. match.arg
  for (p in intern_utforska_matcharg(fn)) {
    standard <- intern_utforska_standardvarde(fn, p)
    if (is.character(standard) && length(standard) > 1) {
      spec[[p]] <- intern_utforska_spec(stats::setNames(standard, standard), standard[1], multiple = FALSE)
    }
  }

  # 2. regioner
  regionparametrar <- intersect(c("region_vekt", "jmfr_region"), namn)
  if (length(regionparametrar) > 0) {
    regtab <- intern_utforska_regioner()
    if (!is.null(regtab) && nrow(regtab) > 0) {
      val <- stats::setNames(regtab$regionkod, paste0(regtab$region, " (", regtab$regionkod, ")"))
      for (p in regionparametrar) {
        standard <- intern_utforska_standardvarde(fn, p)
        if (is.character(standard)) spec[[p]] <- intern_utforska_spec(val, standard, multiple = identical(p, "region_vekt"))
      }
    }
  }

  # 3. SCB-parametrar
  koppling <- intern_utforska_pxweb_koppling(fn)
  koppling <- koppling[!koppling$param %in% names(spec) & !koppling$param %in% c("region_vekt", "jmfr_region"), ]
  for (i in seq_len(nrow(koppling))) {
    p <- koppling$param[i]
    standard <- intern_utforska_standardvarde(fn, p)
    if (is.logical(standard) && !all(is.na(standard))) next          # booleska parametrar får kryssrutor
    if (is.function(standard) || is.list(standard)) next
    varden <- intern_utforska_scb_varden(koppling$tabell[i], koppling$variabel[i])
    if (is.null(varden)) next
    varden <- varden[!duplicated(varden$label), ]
    s <- standard[!is.na(standard)]

    if (identical(koppling$variabel[i], "Tid")) {
      koder <- varden$code[order(varden$code, decreasing = TRUE)]
      choices <- c("senaste år" = "9999", "alla år" = "*", stats::setNames(koder, koder))
      spec[[p]] <- intern_utforska_spec(choices, standard, multiple = TRUE)
      next
    }

    if (length(s) > 0) {
      s_text <- as.character(s)
      if (all(s_text %in% varden$label)) {
        choices <- stats::setNames(varden$label, varden$label)
      } else if (all(s_text %in% varden$code)) {
        choices <- stats::setNames(varden$code, paste0(varden$label, " (", varden$code, ")"))
      } else {
        next                      # standardvärdet finns inte bland variabelns värden - troligen fel koppling
      }
    } else if (grepl("klartext", p)) {
      choices <- stats::setNames(varden$label, varden$label)
    } else if (grepl("koder?$|_vekt$", p)) {
      choices <- stats::setNames(varden$code, paste0(varden$label, " (", varden$code, ")"))
    } else {
      next
    }
    spec[[p]] <- intern_utforska_spec(choices, standard, multiple = length(s) != 1)
  }
  spec
}

# Inmatningsfält för en parameter: listruta om det finns en spec, annars kryssruta eller textruta.
#' @noRd
intern_utforska_inputfalt <- function(namn, standard, spec = NULL) {
  input_id <- paste0("param_", namn)
  if (!is.null(spec)) {
    return(shiny::selectizeInput(
      input_id, namn, choices = spec$choices, selected = spec$selected, multiple = spec$multiple,
      options = list(create = TRUE, placeholder = "standard, eller skriv ett eget värde")))
  }
  if (identical(standard, TRUE) || identical(standard, FALSE)) {
    return(shiny::checkboxInput(input_id, namn, value = isTRUE(standard)))
  }
  shiny::textInput(input_id, namn, value = paste(deparse(standard), collapse = " "))
}

# R-koden för en parameters värde i det genererade anropet. `varde` är fältets värde; `spec` listrutans specifikation
# (NULL för text- och kryssrutor, som skickas som tidigare).
#' @noRd
intern_utforska_kodtext <- function(standard, varde, spec = NULL) {
  if (is.null(varde)) return(paste(deparse(standard), collapse = " "))
  if (is.null(spec)) {
    if (identical(standard, TRUE) || identical(standard, FALSE)) return(if (isTRUE(varde)) "TRUE" else "FALSE")
    return(as.character(varde))
  }
  varde <- varde[nzchar(varde)]
  if (length(varde) == 0) return(paste(deparse(standard), collapse = " "))    # tom listruta = standardvärdet
  if (isTRUE(spec$numeriskt) && !anyNA(suppressWarnings(as.numeric(varde)))) varde <- as.numeric(varde)
  paste(deparse(varde), collapse = " ")
}
