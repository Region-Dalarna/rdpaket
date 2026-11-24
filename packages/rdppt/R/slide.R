# Den generella motorn: lägger till en slide med valfri layout ur mallen och fyller i rubrik, text
# och bild efter vilka platshållare layouten har - inte efter vad platshållarna råkar heta.
#
# - rubrik läggs i layoutens rubrikplatshållare.
# - bild (en sökväg till en png-fil eller ett ggplot-objekt) läggs i layoutens bildplatshållare. Finns
#   ingen tar den den största textplatshållaren, och finns ingen sådan heller den fria ytan på sidan.
# - text läggs i den största lediga textplatshållaren. En namngiven lista lägger i stället varje
#   element i platshållaren med det namnet.
intern_lagg_till_slide <- function(pres, lp, profil, layout, master,
                                   rubrik = NULL, text = NULL, bild = NULL,
                                   txt_size = NA, anteckningar = NULL) {

  ph <- intern_platshallare(lp, layout, master)
  pres <- officer::add_slide(pres, layout = layout, master = master)

  # platshållare som pekats ut med namn i mallprofilen går före det automatiska valet
  egna <- profil$platshallare[[layout]]
  valj <- function(roll) {
    if (!is.null(egna) && roll %in% names(egna)) intern_platshallare_med_namn(ph, egna[[roll]], layout) else ph[ph$roll == roll, , drop = FALSE]
  }
  rubrik_ph <- valj("rubrik")
  bild_ph <- valj("bild")
  text_ph <- valj("text")

  har_rubrik <- intern_har_innehall(rubrik)
  har_text <- intern_har_innehall(text)
  text_med_namn <- har_text && is.list(text) && !is.null(names(text)) && !intern_ar_officer_text(text)

  if (har_rubrik) {
    if (nrow(rubrik_ph) == 0) {
      message("Layouten '", layout, "' har ingen rubrikplatshållare - rubriken utelämnas.")
    } else {
      pres <- officer::ph_with(pres, value = rubrik, location = officer::ph_location_id(id = rubrik_ph$id[1]))
    }
  }

  upptagna <- if (text_med_namn) names(text) else character(0)

  if (!is.null(bild)) {
    lediga <- text_ph[!text_ph$ph_label %in% upptagna, , drop = FALSE]
    if (nrow(bild_ph) > 0) {
      ruta <- bild_ph[1, ]
    } else if (nrow(lediga) > 0) {
      ruta <- lediga[which.max(lediga$yta), ]
      upptagna <- c(upptagna, ruta$ph_label)
    } else {
      ruta <- NULL
    }
    yta <- if (is.null(ruta)) {
      intern_fri_yta(pres, rubrik_ph, har_rubrik)
    } else {
      list(left = ruta$offx, top = ruta$offy, width = ruta$cx, height = ruta$cy)
    }
    pres <- intern_placera_bild(pres, bild, yta)
  }

  if (text_med_namn) {
    for (namn in names(text)) {
      mal <- intern_platshallare_med_namn(ph, namn, layout)
      pres <- officer::ph_with(pres, value = intern_text_varde(text[[namn]], txt_size),
                               location = officer::ph_location_id(id = mal$id[1]))
    }
  } else if (har_text) {
    lediga <- text_ph[!text_ph$ph_label %in% upptagna, , drop = FALSE]
    if (nrow(lediga) == 0) {
      message("Layouten '", layout, "' har ingen ledig textplatshållare - texten utelämnas.")
    } else {
      mal <- lediga[which.max(lediga$yta), ]
      pres <- officer::ph_with(pres, value = intern_text_varde(text, txt_size),
                               location = officer::ph_location_id(id = mal$id[1]))
    }
  }

  intern_satt_anteckningar(pres, anteckningar)
}

# Titelsidan: huvudrubrik i rubrikplatshållaren, underrubrik i den första textplatshållaren och
# presentatörens namn i den andra (om layouten har någon).
intern_slide_titel <- function(pres, lp, profil, master, huvudrubrik, underrubrik = NA, namn_txt = NULL) {
  layout <- profil$layouter[["titel"]]
  ph <- intern_platshallare(lp, layout, master)
  rubrik_ph <- ph[ph$roll == "rubrik", , drop = FALSE]
  text_ph <- ph[ph$roll == "text", , drop = FALSE]
  stil <- profil$stil

  pres <- officer::add_slide(pres, layout = layout, master = master)

  if (nrow(rubrik_ph) > 0) {
    pres <- officer::ph_with(
      pres,
      value = intern_formaterad(huvudrubrik, stil$titel_stlk, stil$farg_rubrik, fet = TRUE, justering = "center"),
      location = officer::ph_location_id(id = rubrik_ph$id[1]))
  }
  if (intern_har_innehall(underrubrik) && nrow(text_ph) >= 1) {
    pres <- officer::ph_with(
      pres,
      value = intern_formaterad(underrubrik, stil$undertitel_stlk, stil$farg_rubrik, fet = TRUE, kursiv = TRUE, justering = "center"),
      location = officer::ph_location_id(id = text_ph$id[1]))
  }
  if (intern_har_innehall(namn_txt) && nrow(text_ph) >= 2) {
    pres <- officer::ph_with(
      pres,
      value = intern_formaterad(namn_txt, stil$namn_stlk, stil$farg_rubrik, justering = "left"),
      location = officer::ph_location_id(id = text_ph$id[2]))
  }
  pres
}

# Hur många av texterna (en per punkt, brutna efter spaltens bredd) som ryms i en ruta. Bredden mäts i
# teckensnittet (systemfonts) om det går, annars med en grov uppskattning. NULL max = så många som ryms.
intern_antal_som_ryms <- function(texter, bredd_tum, hojd_tum, storlek, mellanrum_pt = 8, indrag_tum = 0.25,
                                  max = NULL, familj = "Arial") {
  if (length(texter) == 0) return(0L)
  max_pt <- 0.95 * (bredd_tum - 0.2 - indrag_tum) * 72
  ord_bredd <- function(x) {
    if (requireNamespace("systemfonts", quietly = TRUE)) {
      b <- tryCatch(vapply(x, function(t) systemfonts::shape_string(t, family = familj, size = storlek)$metrics$width, 0),
                    error = function(e) NULL)
      if (!is.null(b)) return(b)
    }
    nchar(x) * storlek * 0.5
  }
  antal_rader <- function(text) {
    ord <- strsplit(trimws(text), "[[:space:]]+")[[1]]
    rader <- 1L; rad <- ""
    for (o in ord) {
      kand <- if (nchar(rad) == 0) o else paste(rad, o)
      if (nchar(rad) > 0 && ord_bredd(kand) > max_pt) { rader <- rader + 1L; rad <- o } else rad <- kand
    }
    rader
  }
  tillgangligt <- hojd_tum * 72 - 8
  hojd_text <- vapply(texter, function(t) antal_rader(t) * storlek * 1.2 + mellanrum_pt, 0)
  n <- sum(cumsum(hojd_text) - mellanrum_pt <= tillgangligt)
  n <- max(n, 1L)
  if (!is.null(max)) n <- min(n, max)
  as.integer(min(n, length(texter)))
}

# Rapportsidan: tre spalter med Samhällsanalys interaktiva rapporter (appar på shiny.regiondalarna.se),
# analysrapporter (webb) och publicerade rapporter (pdf) som punktlistor, länkar till landningssidan och
# Samhällsanalys webbsida, och en rad om podden. Spalterna är textrutor under en "endast rubrik"-layout, alla med
# samma höjd och topplacerad text. Varje spalt fylls med så många rapporter som ryms (högst `antal` om det anges).
# Det som inte går att hämta lämnas ut; hämtas ingenting alls hoppas sidan över.
intern_slide_rapporter <- function(pres, lp, profil, layout, master, anteckningar = NULL, antal = NULL) {
  hamta <- function(fun, vad) {
    tryCatch(fun(), error = function(e) {
      message("OBS: Kunde inte hämta ", vad, " (", conditionMessage(e), "). Spalten hoppas över.")
      NULL
    })
  }
  rd <- hamta(rapporter_rd_webb_hamta, "rapporter från regiondalarna.se")
  interaktiva <- hamta(rapporter_rd_interaktiva_hamta, "interaktiva rapporter från shiny.regiondalarna.se")

  spalter <- list(
    list(rubrik = "Interaktiva rapporter", rader = interaktiva$text),
    list(rubrik = "Analysrapporter", rader = rd$text[rd$typ == "webb"]),
    list(rubrik = "Publicerade rapporter (pdf)", rader = rd$text[rd$typ == "pdf"]))
  spalter <- Filter(function(sp) length(sp$rader) > 0, spalter)
  if (length(spalter) == 0) return(pres)

  ph <- intern_platshallare(lp, layout, master)
  yta <- intern_fri_yta(pres, ph[ph$roll == "rubrik", , drop = FALSE], TRUE, marginal = 0.45)
  luft <- 0.35
  bredd <- (yta$width - luft * (length(spalter) - 1)) / length(spalter)
  lista_hojd <- 3.55
  textstorlek <- 14

  for (i in seq_along(spalter)) {
    spalter[[i]]$visa <- utils::head(spalter[[i]]$rader, intern_antal_som_ryms(
      spalter[[i]]$rader, bredd, lista_hojd, textstorlek, max = antal))
  }
  message("Rapportsidan: ", paste0(vapply(spalter, function(sp) {
    paste0(tolower(sub(" \\(pdf\\)", "", sp$rubrik)), " ", length(sp$visa), " av ", length(sp$rader))
  }, ""), collapse = ", "), ".")

  pres <- intern_lagg_till_slide(
    pres, lp, profil, layout = layout, master = master,
    rubrik = intern_formaterad("Rapporter", if (is.null(profil$stil)) NULL else 30, profil$stil$farg_rubrik, fet = TRUE),
    anteckningar = anteckningar
  )

  stil <- intern_text_stil
  ruta <- function(pres, innehall, left, top, width, height, namn = "rapportruta") {
    officer::ph_with(pres, value = innehall, location = officer::ph_location(
      left = left, top = top, width = width, height = height, newlabel = namn))
  }
  lank <- function(text, href, storlek) {
    officer::hyperlink_ftext(text, href = href, prop = stil(storlek, fet = FALSE, farg = "blue", underlined = TRUE))
  }
  rubrikfarg <- if (is.null(profil$stil$farg_rubrik)) "#000000" else profil$stil$farg_rubrik

  # alla spalter har samma mått, så rubriker, listor och urvalsrader hamnar på samma höjd
  for (i in seq_along(spalter)) {
    sp <- spalter[[i]]
    vanster <- yta$left + (i - 1) * (bredd + luft)
    pres <- ruta(pres, officer::fpar(officer::ftext(sp$rubrik, stil(18, fet = TRUE, farg = rubrikfarg))),
                 vanster, yta$top + 0.1, bredd, 0.45)
    lista <- do.call(officer::block_list, lapply(sp$visa, function(rad) {
      officer::fpar(officer::ftext(rad, stil(textstorlek, fet = FALSE)), fp_p = officer::fp_par(padding.bottom = 8))
    }))
    pres <- ruta(pres, lista, vanster, yta$top + 0.55, bredd, lista_hojd, namn = "rapportlista")
    pres <- ruta(pres, officer::fpar(officer::ftext(
      if (length(sp$rader) > length(sp$visa)) paste0("Urval – ", length(sp$visa), " av ", length(sp$rader)) else paste0("Alla ", length(sp$rader)),
      stil(12, fet = FALSE, kursiv = TRUE, farg = "#595959"))),
      vanster, yta$top + 4.15, bredd, 0.35)
  }

  # länkarna längst ned: tre block i samma kolumner som listorna, med en rubrik och en länk i varje
  lankblock <- list(
    list(rubrik = "Här finns alla våra rapporter", text = "shiny.regiondalarna.se", href = "https://shiny.regiondalarna.se/"),
    list(rubrik = "Samhällsanalys webbsida", text = "Statistik och rapporter",
         href = "https://www.regiondalarna.se/verksamhet/regional-utveckling/statistik-och-rapporter/"),
    list(rubrik = "Lyssna på vår podd", text = "Läget i Dalarna på Spotify",
         href = "https://open.spotify.com/show/74b9qUtR2cT0DNQMAyvlRa"))
  blockbredd <- (yta$width - luft * 2) / 3
  for (i in seq_along(lankblock)) {
    lb <- lankblock[[i]]
    pres <- ruta(pres, list(
      officer::fpar(officer::ftext(lb$rubrik, stil(14, fet = TRUE, farg = "#595959"))),
      officer::fpar(lank(lb$text, lb$href, 14))) |> do.call(what = officer::block_list),
      yta$left + (i - 1) * (blockbredd + luft), yta$top + 4.65, blockbredd, 0.85)
  }

  intern_efterbehandla_rapportsida(pres$slide$get_slide(pres$cursor)$get())
  pres
}

# Finjusterar sidans xml: texten börjar överst i rutorna (mallens bodyPr kan annars centrera den vertikalt),
# listorna får punkter med hängande indrag, och texten markeras som svenska så att PowerPoint inte understryker
# den som felstavad.
intern_efterbehandla_rapportsida <- function(xml) {
  sok <- function(xpath) xml2::xml_find_all(xml, xpath)
  ar_rapport <- "[*[local-name()='nvSpPr']/*[local-name()='cNvPr'][starts-with(@name,'rapport')]]"
  for (n in sok(paste0("//*[local-name()='sp']", ar_rapport, "/*[local-name()='txBody']/*[local-name()='bodyPr']"))) {
    xml2::xml_set_attr(n, "anchor", "t")
  }
  lista <- "[*[local-name()='nvSpPr']/*[local-name()='cNvPr'][@name='rapportlista']]"
  for (n in sok(paste0("//*[local-name()='sp']", lista, "//*[local-name()='pPr']"))) {
    for (bu in xml2::xml_find_all(n, "./*[local-name()='buNone']")) xml2::xml_remove(bu)
    xml2::xml_set_attr(n, "marL", "228600")
    xml2::xml_set_attr(n, "indent", "-228600")
    xml2::xml_add_child(n, xml2::read_xml('<a:buFont xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" typeface="Arial"/>'))
    xml2::xml_add_child(n, xml2::read_xml('<a:buChar xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" char="&#8226;"/>'))
  }
  for (n in sok("//*[local-name()='rPr']")) xml2::xml_set_attr(n, "lang", "sv-SE")
  invisible(xml)
}

# Samhällsanalys avslutsbild: "Tack för mig!" med presentatörens namn och e-post, om inget annat anges.
intern_slide_avslut <- function(pres, lp, profil, layout, master, rubrik = NULL, text = NULL,
                                anv = NULL, anteckningar = NULL) {
  if (!intern_har_innehall(rubrik)) rubrik <- "Tack för mig!"

  if (!intern_har_innehall(text)) {
    lank <- function(epost) {
      officer::hyperlink_ftext(epost, href = paste0("mailto:", epost),
                               prop = officer::fp_text_lite(color = "blue", underlined = TRUE, font.size = 24))
    }
    delar <- list(lank("samhallsanalys@regiondalarna.se"))
    if (!is.null(anv)) {
      delar <- c(list(officer::ftext(anv$namn, officer::fp_text_lite(color = "gray", font.size = 24)), officer::ftext("\n"),
                      lank(anv$epost), officer::ftext("\n")),
                 delar)
    }
    text <- do.call(officer::fpar, c(delar, list(fp_p = officer::fp_par(text.align = "left"))))
  }

  intern_lagg_till_slide(
    pres, lp, profil, layout = layout, master = master,
    rubrik = intern_formaterad(rubrik, profil$stil$avsnitt_stlk, profil$stil$farg_rubrik, fet = TRUE),
    text = text,
    anteckningar = anteckningar
  )
}

# Flyttar slides så att de hamnar i ordningen onskad (en vektor med slidernas nuvarande positioner),
# och tar bort de slides som inte finns med där.
intern_ordna_slides <- function(pres, onskad) {
  aktuell <- seq_len(length(pres))
  for (mal in seq_along(onskad)) {
    nu <- match(onskad[mal], aktuell)
    if (nu != mal) {
      pres <- officer::move_slide(pres, index = nu, to = mal)
      aktuell <- append(aktuell[-nu], aktuell[nu], after = mal - 1)
    }
  }
  for (i in rev(seq_along(aktuell))) {
    if (i > length(onskad)) pres <- officer::remove_slide(pres, index = i)
  }
  pres
}

intern_platshallare_med_namn <- function(ph, namn, layout) {
  traff <- ph[ph$ph_label == namn, , drop = FALSE]
  if (nrow(traff) == 0) {
    stop("Layouten '", layout, "' har ingen platshållare som heter '", namn, "'. Finns: ",
         paste0("'", ph$ph_label, "'", collapse = ", "), ".", call. = FALSE)
  }
  traff
}

# Den yta på sidan en bild får när layouten saknar platshållare för den: hela sidan innanför en
# marginal, eller ytan under rubriken om sidan har en.
intern_fri_yta <- function(pres, rubrik_ph, har_rubrik, marginal = 0.25) {
  sida <- officer::slide_size(pres)
  topp <- if (har_rubrik && nrow(rubrik_ph) > 0) rubrik_ph$offy[1] + rubrik_ph$cy[1] + 0.1 else marginal
  list(left = marginal, top = topp, width = sida$width - 2 * marginal, height = sida$height - topp - marginal)
}

# Lägger en bild i en yta. En png-fil skalas så att den ryms i ytan med bibehållna proportioner och
# centreras; ett ggplot-objekt ritas som redigerbar vektorgrafik över hela ytan.
intern_placera_bild <- function(pres, bild, yta) {
  if (intern_ar_ggplot(bild)) {
    return(officer::ph_with(
      pres, value = rvg::dml(ggobj = bild),
      location = officer::ph_location(left = yta$left, top = yta$top, width = yta$width, height = yta$height)))
  }
  matt <- intern_png_matt(bild)
  skala <- min(yta$width / matt$bredd, yta$height / matt$hojd)
  bredd <- matt$bredd * skala
  hojd <- matt$hojd * skala
  officer::ph_with(
    pres,
    value = officer::external_img(bild, width = bredd, height = hojd),
    location = officer::ph_location(left = yta$left + (yta$width - bredd) / 2,
                                    top = yta$top + (yta$height - hojd) / 2,
                                    width = bredd, height = hojd))
}

# Bredd och höjd i pixlar för en png-fil, läst direkt ur filhuvudet (de första 24 byten) i stället
# för att läsa in hela bilden.
intern_png_matt <- function(fil) {
  huvud <- readBin(fil, what = "raw", n = 24)
  png_signatur <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))
  if (length(huvud) < 24 || !identical(huvud[1:8], png_signatur)) {
    stop("Filen är inte en png-bild (bara png stöds):\n  ", fil, call. = FALSE)
  }
  list(
    bredd = readBin(huvud[17:20], what = "integer", size = 4, endian = "big"),
    hojd = readBin(huvud[21:24], what = "integer", size = 4, endian = "big")
  )
}

# Gör om text till något officer kan lägga i en platshållare. Utan txt_size skickas texten som den
# är (ett stycke per element) och får då mallens egen formatering. Med txt_size blir det ett stycke
# (ett element) eller en punktlista (flera element) i den storleken.
intern_text_varde <- function(text, txt_size = NA) {
  if (intern_ar_officer_text(text)) return(text)
  text <- as.character(unlist(text))
  text[is.na(text)] <- ""
  if (is.null(txt_size) || is.na(txt_size)) return(text)

  stil <- intern_text_stil(txt_size)
  if (length(text) == 1) {
    officer::fpar(officer::ftext(text, stil))
  } else {
    intern_punktlista(text, stil)
  }
}

# Textstil med storlek (och ev. fet/kursiv/färg). officer 0.7.x skriver ingen teckenformatering alls för en
# fp_text_lite() utan färg, så färgen anges alltid (svart om inget annat anges).
intern_text_stil <- function(storlek, fet = NA, kursiv = NA, farg = "#000000", ...) {
  officer::fp_text_lite(font.size = storlek, bold = fet, italic = kursiv, color = farg, ...)
}

# Ett stycke per rad, så att punkterna kommer från platshållarens egen punktlista. officer::unordered_list()
# tappar textstilen när den är fp_text_lite() och storleken blir då mallens.
intern_punktlista <- function(rader, stil) {
  do.call(officer::block_list, lapply(rader, function(rad) officer::fpar(officer::ftext(rad, stil))))
}

# En rubrik med egen storlek/färg. Är varken storlek eller färg angiven returneras texten som den
# är, och får då mallens egen formatering.
intern_formaterad <- function(text, stlk = NULL, farg = NULL, fet = NA, kursiv = NA, justering = "left") {
  if (is.null(stlk) && is.null(farg)) return(text)
  officer::fpar(
    officer::ftext(text, officer::fp_text_lite(
      font.size = if (is.null(stlk)) NA else stlk,
      color = if (is.null(farg)) NA else farg,
      bold = fet, italic = kursiv)),
    fp_p = officer::fp_par(text.align = justering))
}

intern_satt_anteckningar <- function(pres, anteckningar) {
  anteckningar <- unlist(anteckningar)
  if (length(anteckningar) == 0 || all(is.na(anteckningar))) return(pres)
  officer::set_notes(pres, value = paste0(anteckningar, collapse = "\n"),
                     location = officer::notes_location_type("body"))
}

intern_ar_officer_text <- function(x) inherits(x, c("fpar", "block_list", "unordered_list"))

intern_ar_ggplot <- function(x) inherits(x, "ggplot")

intern_har_innehall <- function(x) {
  if (is.null(x) || length(x) == 0) return(FALSE)
  if (intern_ar_officer_text(x) || is.list(x)) return(TRUE)
  any(!is.na(x) & nzchar(as.character(x)))
}

# Skriver presentationen till fil, med ett begripligt fel om filen är låst.
intern_skriv_pptx <- function(pres, fil, nytt_namn_om_last = FALSE) {
  skriv <- function(sokvag) tryCatch(
    {print(pres, target = sokvag); NULL},
    error = function(e) e
  )
  fel <- skriv(fil)
  if (is.null(fel)) return(invisible(fil))

  # filen är troligen öppen i PowerPoint: prova fil_2, fil_3 ... om det är tillåtet
  ar_last <- grepl("is open|close it|Permission denied|being used|används av", conditionMessage(fel), ignore.case = TRUE)
  if (nytt_namn_om_last && ar_last) {
    bas <- sub("\\.pptx$", "", fil, ignore.case = TRUE)
    for (nr in 2:99) {
      alternativ <- paste0(bas, "_", nr, ".pptx")
      if (is.null(skriv(alternativ))) {
        message("OBS: ", basename(fil), " är öppen i PowerPoint eller låst. Sparade som ", basename(alternativ), " i stället.")
        return(invisible(alternativ))
      }
    }
  }
  stop("Kunde inte skriva presentationen till:\n  ", fil,
       "\nÄr filen öppen i PowerPoint? Stäng den i så fall och försök igen. (", conditionMessage(fel), ")",
       call. = FALSE)
}

intern_oppna_fil <- function(fil) {
  if (.Platform$OS.type == "windows") shell.exec(normalizePath(fil)) else utils::browseURL(fil)
  invisible(fil)
}
