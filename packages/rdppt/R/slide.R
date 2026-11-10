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

# Rapportsidan: aktuella rapporter från regiondalarna.se i två spalter (webb och pdf), på mallens
# tvåspaltiga layout. Går rapporterna inte att hämta hoppas sidan över.
intern_slide_rapporter <- function(pres, lp, profil, layout, master, anteckningar = NULL) {
  rapporter_rd <- tryCatch(
    rapporter_rd_webb_hamta(),
    error = function(e) {
      message("OBS: Kunde inte hämta rapporter från regiondalarna.se (", conditionMessage(e), "). Rapportsidan hoppas över.")
      NULL
    }
  )
  if (is.null(rapporter_rd)) return(pres)

  # spalterna fylls efter platshållarnas läge: de två övre är spaltrubriker, de två under är
  # listorna, och en eventuell femte längst ned får länken till webbsidan
  text_ph <- intern_platshallare(lp, layout, master)
  text_ph <- text_ph[text_ph$roll == "text", , drop = FALSE]
  if (nrow(text_ph) < 4) {
    message("Layouten '", layout, "' har färre än fyra textplatshållare och går inte att använda för rapportsidan. Rapportsidan hoppas över.")
    return(pres)
  }

  spaltrubrik <- function(txt) officer::fpar(officer::ftext(txt, officer::fp_text_lite(font.size = 18, bold = TRUE)))
  punktlista <- function(typ_varde) {
    rader <- rapporter_rd$text[rapporter_rd$typ == typ_varde]
    officer::unordered_list(str_list = rader, level_list = rep(1, length(rader)),
                            style = officer::fp_text_lite(font.size = 16))
  }
  innehall <- list(spaltrubrik("Webbrapporter"), spaltrubrik("PDF-rapporter"), punktlista("webb"), punktlista("pdf"))

  if (nrow(text_ph) >= 5) {
    innehall[[5]] <- officer::fpar(
      officer::ftext("Länk: ", officer::fp_text_lite(font.size = 18)),
      officer::hyperlink_ftext(
        "Statistik och rapporter - Samhällsanalys, Region Dalarna",
        href = "https://www.regiondalarna.se/verksamhet/regional-utveckling/statistik-och-rapporter/",
        prop = officer::fp_text_lite(color = "blue", underlined = TRUE, font.size = 18)),
      fp_p = officer::fp_par(text.align = "center"))
  }
  names(innehall) <- text_ph$ph_label[seq_along(innehall)]

  intern_lagg_till_slide(
    pres, lp, profil, layout = layout, master = master,
    rubrik = intern_formaterad("Rapporter", if (is.null(profil$stil)) NULL else 30, profil$stil$farg_rubrik, fet = TRUE),
    text = innehall,
    anteckningar = anteckningar
  )
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

  stil <- officer::fp_text_lite(font.size = txt_size)
  if (length(text) == 1) {
    officer::fpar(officer::ftext(text, stil))
  } else {
    officer::unordered_list(str_list = text, level_list = rep(1, length(text)), style = stil)
  }
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
intern_skriv_pptx <- function(pres, fil) {
  tryCatch(
    print(pres, target = fil),
    error = function(e) {
      stop("Kunde inte skriva presentationen till:\n  ", fil,
           "\nÄr filen öppen i PowerPoint? Stäng den i så fall och försök igen. (", conditionMessage(e), ")",
           call. = FALSE)
    }
  )
  invisible(fil)
}

intern_oppna_fil <- function(fil) {
  if (.Platform$OS.type == "windows") shell.exec(normalizePath(fil)) else utils::browseURL(fil)
  invisible(fil)
}
