#' Bygg en PowerPoint-presentation från en diagramlista
#'
#' Tar en "ppt-lista" (se [initiera_ppt_df()] och [ppt_lista_fyll_pa()]) -
#' en rad per bild/diagram, med vilken mall-layout som ska användas för
#' varje rad - och bygger en komplett `.pptx`-presentation utifrån Region
#' Dalarnas presentationsmallar.
#'
#' @param pptlista En ppt-lista, se [initiera_ppt_df()].
#' @param huvudrubrik Presentationens huvudrubrik (på titelsidan).
#' @param farg_huvudrubrik Hexfärg på huvudrubriken.
#' @param underrubrik Underrubrik på titelsidan. Standard: "Skapad
#'   <dagens datum> av Samhällsanalys\\nRegionala utvecklingsförvaltningen,
#'   Region Dalarna".
#' @param output_mapp Mapp där presentationen sparas.
#' @param spara_backup_om_fil_redan_finns TRUE om en befintlig fil med
#'   samma namn ska säkerhetskopieras i stället för att skrivas över.
#' @param filnamn Filnamn på den sparade presentationen.
#' @param mall_fil Sökväg till pptx-mallfilen presentationen byggs utifrån.
#' @param titelmaster Namnet på den master-layout titelsidan ska använda.
#' @param titel_text_stlk,undertitel_text_stlk Textstorlek på huvud-
#'   respektive underrubrik.
#' @param rubrik_namn TRUE om presentatörens namn ska skrivas ut på
#'   titelsidan.
#' @param rubrik_namn_stlk Textstorlek på presentatörens namn.
#' @param utan_rubriker,utan_text TRUE om rubriker respektive brödtext ska
#'   utelämnas när mall-layout väljs automatiskt (se `pptlista$mall`).
#' @param slides_flytta En lista med `list(fran =, till =)`-par för att
#'   flytta enskilda slides (t.ex. sådana som redan finns i `mall_fil`)
#'   till rätt position i den färdiga presentationen, eller `NULL`.
#' @param anv_uppgifter Vems namn/e-post ska användas på titel-/
#'   avslutssidan - `NULL` (standard) = inloggad användare, se
#'   [rddeploy::anv_hamta_namn_epost_fran_lista()].
#'
#' @return Inget - skriver presentationen till `output_mapp`.
#' @export
skapa_ppt_fran_lista <- function(pptlista,
                                 huvudrubrik,
                                 farg_huvudrubrik = "#2286AC",
                                 underrubrik = paste0("Skapad ", gsub("^0", "", format(Sys.Date(), "%d")), " ", format(Sys.Date(), "%b %Y"), " av Samhällsanalys\nRegionala utvecklingsförvaltningen, Region Dalarna"),
                                 output_mapp = rdverktyg::utskriftsmapp(),
                                 spara_backup_om_fil_redan_finns = FALSE,
                                 filnamn = "presentation.pptx",
                                 mall_fil,
                                 titelmaster = "VCdag",
                                 titel_text_stlk = 40,
                                 undertitel_text_stlk = 20,
                                 rubrik_namn = TRUE,
                                 rubrik_namn_stlk = 18,
                                 utan_rubriker = FALSE,
                                 utan_text = FALSE,
                                 slides_flytta = NULL,
                                 anv_uppgifter = NULL) {

  # hämta för- och efternamn samt epost
  anv_uppgifter_txt <- rddeploy::anv_hamta_namn_epost_fran_lista(anv_uppgifter)
  anv_namn <- anv_uppgifter_txt$namn
  anv_epost <- anv_uppgifter_txt$epost

  # Inställningar ==================================================
  rdverktyg::sakerstall_indatafil(mall_fil)
  pres <- officer::read_pptx(mall_fil) # Läs in pptx-mallen

  # =================================================== Titelslide =============================================================
  pres <- officer::add_slide(pres, layout = "Rubrikbild", master = titelmaster)

  rubrik_format <- officer::fp_text(font.size = titel_text_stlk, color = farg_huvudrubrik, bold = TRUE)
  rubrik_text <- officer::ftext(huvudrubrik, rubrik_format)
  cent_align <- officer::fp_par(text.align = "center")
  left_align <- officer::fp_par(text.align = "left")

  pres <- officer::ph_with(pres, value = officer::fpar(rubrik_text, fp_p = cent_align), location = officer::ph_location_label(ph_label = "Rubrik 1"))
  if (!is.na(underrubrik)) {
    underrubrik_format <- officer::fp_text(font.size = undertitel_text_stlk, color = farg_huvudrubrik, bold = TRUE, italic = TRUE)
    underrubrik_text <- officer::ftext(underrubrik, underrubrik_format)
    pres <- officer::ph_with(pres, value = officer::fpar(underrubrik_text, fp_p = cent_align), location = officer::ph_location_label(ph_label = "Underrubrik 2"))
  }

  if (rubrik_namn) {
    rubrik_namn_txt <- paste0(anv_namn, ", Samhällsanalytiker\nRegionala utvecklingsförvaltningen, Region Dalarna")

    rubrik_namn_format <- officer::fp_text(font.size = rubrik_namn_stlk, color = farg_huvudrubrik)
    rubrik_namn_txt <- officer::ftext(rubrik_namn_txt, rubrik_namn_format)

    pres <- officer::ph_with(pres, value = officer::fpar(rubrik_namn_txt, fp_p = left_align), location = officer::ph_location_label(ph_label = "Platshållare för text 8"))
  }

  # =========== Från slide 2 och framåt, skrivs från diagram-tabellen =========================

  for (diagr in seq_len(nrow(pptlista))) {
    # hämta värden från aktuell rad i df:n eller listan
    pic_path <- pptlista$dia_path[[diagr]]
    x <- pptlista$beskr[[diagr]]
    pic_text <- if (is.null(x) || length(x) == 0) {
      ""
    } else if (is.list(x) || length(x) > 1) {
      unlist(x)
    } else {
      x
    }

    pic_rubrik <- ifelse(is.na(pptlista$titel[[diagr]]), "", pptlista$titel[[diagr]])
    ppt_mall <- pptlista$mall[[diagr]]
    txt_size <- pptlista$txt_size[[diagr]]
    ppt_master <- pptlista$master[[diagr]]
    ppt_anteckningar <- pptlista$anteckningar[[diagr]]
    if (is.null(ppt_master)) ppt_master <- NA
    if (is.na(ppt_master)) ppt_master <- "VCdag"
    if (is.null(pptlista$gg_obj[[diagr]])) pptlista$gg_obj[[diagr]] <- NA
    # Bugfix (bekräftad): is_ggplot() fanns inte (varken i originalet eller i ggplot2, som bara har
    # is.ggplot()) - skulle ha kraschat så fort grenen nedan faktiskt körts mot ett ggplot-objekt.
    if (is.na(pptlista$gg_obj[[diagr]])) ggdiagram <- NA else ggdiagram <- rvg::dml(ggobj = pptlista$gg_obj[[diagr]])
    if (length(ppt_mall) > 0) if (is.na(ppt_mall)) ppt_mall <- NULL

    # om det inte skickas med något värde för vilken mall som ska användas så väljs utifrån vad som skickas med och ställs in
    if (is.null(ppt_mall)) {
      ppt_mall <- dplyr::case_when(
        (!is.na(pic_rubrik) & !utan_rubriker) & (!is.na(pic_text) & !utan_text) ~ "Bild och text till höger",
        (is.na(pic_rubrik) | utan_rubriker) & (!is.na(pic_text) & !utan_text) ~ "Bild och text till höger utan rubrik",
        (!is.na(pic_rubrik) & !utan_rubriker) & (is.na(pic_text) | utan_text) ~ "Bild och text till höger",
        (is.na(pic_rubrik) | utan_rubriker) & (is.na(pic_text) | utan_text) ~ "Bild utan text och rubrik",
        TRUE ~ "Bild och text till höger"
      )
    }

    # skapa slide utifrån vilken layout som finns i listan ====================================

    if (ppt_mall == "Bild och text till höger") {
      fp_size <- officer::fp_text(font.size = 16)
      if (!is.na(txt_size)) fp_size <- officer::fp_text(font.size = txt_size)
      pars <- officer::fpar(officer::ftext(pic_text, fp_size))

      pres <- officer::add_slide(pres, layout = ppt_mall, master = ppt_master) |>
        officer::ph_with(value = pic_rubrik, location = officer::ph_location_type(type = "title")) |>
        officer::ph_with(value = pars, location = officer::ph_location_label(ph_label = "Platshållare för text 5"))

      if (!is.na(ggdiagram)) {
        pres <- pres |> officer::ph_with(value = ggdiagram,
                                          location = officer::ph_location_label(ph_label = "Platshållare för bild 3"))
      } else {
        bild_pos_stlk <- intern_bild_position_stlk(pres_obj = pres,
                                            ppt_master = ppt_master,
                                            ppt_mall = ppt_mall,
                                            ph_label = "Platshållare för bild 3",
                                            img_url = pic_path)

        free_loc <- officer::ph_location(
          left = bild_pos_stlk$left,
          top = bild_pos_stlk$top,
          width = bild_pos_stlk$width,
          height = bild_pos_stlk$height)

        pres <- pres |>
          officer::ph_with(value = officer::external_img(pic_path, width = bild_pos_stlk$width, height = bild_pos_stlk$height),
                           location = free_loc)

        if (!is.null(ppt_anteckningar)) {
          anteckningar_txt <- paste0(ppt_anteckningar, collapse = "\n")
          pres <- pres |>
            officer::set_notes(value = anteckningar_txt, location = officer::notes_location_type("body"))
        }
      }

    } else if (ppt_mall == "Bild och text till höger utan rubrik") {
      fp_size <- officer::fp_text(font.size = 16)
      if (!is.na(txt_size)) fp_size <- officer::fp_text(font.size = txt_size)
      pars <- officer::fpar(officer::ftext(pic_text, fp_size))

      pres <- officer::add_slide(pres, layout = ppt_mall, master = ppt_master) |>
        officer::ph_with(value = pars, location = officer::ph_location_label(ph_label = "Platshållare för text 5"))

      if (!is.na(ggdiagram)) {
        pres <- pres |> officer::ph_with(value = ggdiagram,
                                          location = officer::ph_location_label(ph_label = "Platshållare för bild 3"))
      } else {
        bild_pos_stlk <- intern_bild_position_stlk(pres_obj = pres,
                                            ppt_master = ppt_master,
                                            ppt_mall = ppt_mall,
                                            ph_label = "Platshållare för bild 3",
                                            img_url = pic_path)

        free_loc <- officer::ph_location(left = bild_pos_stlk$left, top = bild_pos_stlk$top,
                                width = bild_pos_stlk$width, height = bild_pos_stlk$height)

        pres <- pres |> officer::ph_with(value = officer::external_img(pic_path, width = bild_pos_stlk$width, height = bild_pos_stlk$height),
                                          location = free_loc)

        if (!is.null(ppt_anteckningar)) {
          anteckningar_txt <- paste0(ppt_anteckningar, collapse = "\n")
          pres <- pres |>
            officer::set_notes(value = anteckningar_txt, location = officer::notes_location_type("body"))
        }
      }

    } else if (ppt_mall == "Bild utan text och rubrik") {

      pres <- officer::add_slide(pres, layout = ppt_mall, master = ppt_master)

      if (!is.na(ggdiagram)) {
        pres <- pres |> officer::ph_with(value = ggdiagram,
                                          location = officer::ph_location_label(ph_label = "Platshållare för bild 3"))
      } else {
        bild_pos_stlk <- intern_bild_position_stlk(pres_obj = pres,
                                            ppt_master = ppt_master,
                                            ppt_mall = ppt_mall,
                                            ph_label = "Platshållare för bild 3",
                                            img_url = pic_path)

        free_loc <- officer::ph_location(left = bild_pos_stlk$left, top = bild_pos_stlk$top,
                                         width = bild_pos_stlk$width, height = bild_pos_stlk$height)

        pres <- pres |> officer::ph_with(value = officer::external_img(pic_path, width = bild_pos_stlk$width, height = bild_pos_stlk$height),
                                          location = free_loc)

        if (!is.null(ppt_anteckningar)) {
          anteckningar_txt <- paste0(ppt_anteckningar, collapse = "\n")
          pres <- pres |>
            officer::set_notes(value = anteckningar_txt, location = officer::notes_location_type("body"))
        }
      }

    } else if (ppt_mall == "Rubrik och innehåll") {
      pars <- officer::unordered_list(str_list = pic_text,
                                      level_list = rep(1, length(pic_text)),
                                      style = officer::fp_text(font.size = txt_size))

      pres <- officer::add_slide(pres, layout = ppt_mall, master = ppt_master) |>
        officer::ph_with(value = pic_rubrik, location = officer::ph_location_type(type = "title")) |>
        officer::ph_with(value = pars, location = officer::ph_location_label(ph_label = "Platshållare för innehåll 2"))

      if (!is.null(ppt_anteckningar)) {
        anteckningar_txt <- paste0(ppt_anteckningar, collapse = "\n")
        pres <- pres |>
          officer::set_notes(value = anteckningar_txt, location = officer::notes_location_type("body"))
      }

    } else if (ppt_mall == "1_Rubrik och innehåll") {
      pars <- officer::unordered_list(str_list = pic_text,
                                      level_list = rep(1, length(pic_text)),
                                      style = officer::fp_text(font.size = txt_size))

      pres <- officer::add_slide(pres, layout = ppt_mall, master = ppt_master) |>
        officer::ph_with(value = pic_rubrik, location = officer::ph_location_type(type = "title")) |>
        officer::ph_with(value = pars,
                         location = officer::ph_location_label(ph_label = "Platshållare för innehåll 2"))

      if (!is.null(ppt_anteckningar)) {
        anteckningar_txt <- paste0(ppt_anteckningar, collapse = "\n")
        pres <- pres |>
          officer::set_notes(value = anteckningar_txt, location = officer::notes_location_type("body"))
      }

    } else if (ppt_mall == "Anpassad layout") {

      pres <- officer::add_slide(pres, layout = "Anpassad layout", master = ppt_master)

      if (!is.null(ppt_anteckningar)) {
        anteckningar_txt <- paste0(ppt_anteckningar, collapse = "\n")
        pres <- pres |>
          officer::set_notes(value = anteckningar_txt, location = officer::notes_location_type("body"))
      }

    } else if (ppt_mall == "Avsnittsrubrik") {

      huvudrubrik_format <- officer::fp_text(font.size = 60, color = farg_huvudrubrik, bold = TRUE)
      huvudrubrik_text <- officer::ftext(pic_rubrik, huvudrubrik_format)

      pres <- officer::add_slide(pres, layout = "Avsnittsrubrik", master = ppt_master) |>
        officer::ph_with(value = officer::fpar(huvudrubrik_text, fp_p = left_align), location = officer::ph_location_label(ph_label = "Rubrik 1")) |>
        officer::ph_with(value = pic_text, location = officer::ph_location_label(ph_label = "Platshållare för text 2"))

      if (!is.null(ppt_anteckningar)) {
        anteckningar_txt <- paste0(ppt_anteckningar, collapse = "\n")
        pres <- pres |>
          officer::set_notes(value = anteckningar_txt, location = officer::notes_location_type("body"))
      }

    } else if (ppt_mall == "Rapporter_Samhallsanalys") {

      rapporter_rd <- tryCatch(
        rapporter_rd_webb_hamta(),
        error = function(e) {
          message("OBS: Kunde inte hämta rapporter från regiondalarna.se (", conditionMessage(e), "). Rapportsidan hoppas över.")
          NULL
        }
      )

      if (!is.null(rapporter_rd)) {

        webb_rapporter_rd <- dplyr::filter(rapporter_rd, typ == "webb") |> dplyr::pull(text)
        pdf_rapporter_rd <- dplyr::filter(rapporter_rd, typ == "pdf") |> dplyr::pull(text)

        huvudrubrik_format <- officer::fp_text(font.size = 30, color = farg_huvudrubrik, bold = TRUE)
        left_align <- officer::fp_par(text.align = "left")
        center_align <- officer::fp_par(text.align = "center")
        huvudrubrik_text <- officer::ftext("Rapporter", huvudrubrik_format)
        rubrik_format <- officer::fp_text(font.size = 18, bold = TRUE)
        webb_rubrik_text <- officer::ftext("Webbrapporter", rubrik_format)
        pdf_rubrik_text <- officer::ftext("PDF-rapporter", rubrik_format)

        webb_lista_text <- officer::unordered_list(str_list = webb_rapporter_rd,
                                                   level_list = rep(1, length(webb_rapporter_rd)),
                                                   style = officer::fp_text(font.size = 16))

        pdf_lista_text <- officer::unordered_list(str_list = pdf_rapporter_rd,
                                                  level_list = rep(1, length(pdf_rapporter_rd)),
                                                  style = officer::fp_text(font.size = 16))

        prefix_text <- officer::ftext("Länk: ", officer::fp_text(color = "black", font.size = 18))

        link_text <- officer::hyperlink_ftext(
          "Statistik och rapporter - Samhällsanalys, Region Dalarna",
          href = "https://www.regiondalarna.se/verksamhet/regional-utveckling/statistik-och-rapporter/",
          prop = officer::fp_text(color = "blue", underlined = TRUE, font.size = 18)
        )

        webblank_text <- officer::fpar(prefix_text, link_text, fp_p = center_align)

        pres <- officer::add_slide(pres, layout = "Jämförelse", master = ppt_master) |>
          officer::ph_with(value = officer::fpar(huvudrubrik_text, fp_p = left_align), location = officer::ph_location_label(ph_label = "Rubrik 1")) |>
          officer::ph_with(value = officer::fpar(webb_rubrik_text), location = officer::ph_location_label(ph_label = "Platshållare för text 2")) |>
          officer::ph_with(value = officer::fpar(pdf_rubrik_text), location = officer::ph_location_label(ph_label = "Platshållare för text 4")) |>
          officer::ph_with(value = webb_lista_text, location = officer::ph_location_label(ph_label = "Platshållare för innehåll 3")) |>
          officer::ph_with(value = pdf_lista_text, location = officer::ph_location_label(ph_label = "Platshållare för innehåll 5")) |>
          officer::ph_with(value = webblank_text, location = officer::ph_location_label(ph_label = "Platshållare för text 7"))

        if (!is.null(ppt_anteckningar)) {
          anteckningar_txt <- paste0(ppt_anteckningar, collapse = "\n")
          pres <- pres |>
            officer::set_notes(value = anteckningar_txt, location = officer::notes_location_type("body"))
        }
      }

    } else if (ppt_mall == "Avslutsbild") {

      fp_size <- officer::fp_text(font.size = 16)
      if (!is.na(txt_size)) fp_size <- officer::fp_text(font.size = txt_size)
      pars <- officer::fpar(officer::ftext(pic_text, fp_size))

      pres <- officer::add_slide(pres, layout = ppt_mall, master = ppt_master) |>
        officer::ph_with(value = pic_rubrik, location = officer::ph_location_label(ph_label = "Rubrik 1")) |>
        officer::ph_with(value = pars,
                         location = officer::ph_location_label(ph_label = "Platshållare för text 2"))

      if (!is.null(ppt_anteckningar)) {
        anteckningar_txt <- paste0(ppt_anteckningar, collapse = "\n")
        pres <- pres |>
          officer::set_notes(value = anteckningar_txt, location = officer::notes_location_type("body"))
      }

    } else if (ppt_mall == "Avslutsbild_Samhallsanalys") {

      if (pic_rubrik == "") pic_rubrik <- "Tack för mig!"
      if (pic_text == "") {

        namn_text <- officer::ftext(anv_namn, officer::fp_text(color = "gray", font.size = 24))
        epost_1 <- officer::hyperlink_ftext(anv_epost,
                                            href = paste0("mailto:", anv_epost),
                                            prop = officer::fp_text(color = "blue", underlined = TRUE, font.size = 24))
        epost_2 <- officer::hyperlink_ftext("samhallsanalys@regiondalarna.se",
                                            href = "mailto:samhallsanalys@regiondalarna.se",
                                            prop = officer::fp_text(color = "blue", underlined = TRUE, font.size = 24))

        left_align <- officer::fp_par(text.align = "left")

        pic_text <- officer::fpar(
          namn_text, officer::ftext("\n"),
          epost_1, officer::ftext("\n"),
          epost_2,
          fp_p = left_align
        )

      }

      huvudrubrik_format <- officer::fp_text(font.size = 60, color = farg_huvudrubrik, bold = TRUE)
      huvudrubrik_text <- officer::ftext(pic_rubrik, huvudrubrik_format)

      pres <- officer::add_slide(pres, layout = "Avsnittsrubrik", master = ppt_master) |>
        officer::ph_with(value = officer::fpar(huvudrubrik_text, fp_p = left_align), location = officer::ph_location_label(ph_label = "Rubrik 1")) |>
        officer::ph_with(value = pic_text, location = officer::ph_location_label(ph_label = "Platshållare för text 2"))

      if (!is.null(ppt_anteckningar)) {
        anteckningar_txt <- paste0(ppt_anteckningar, collapse = "\n")
        pres <- pres |>
          officer::set_notes(value = anteckningar_txt, location = officer::notes_location_type("body"))
      }

    } else if (ppt_mall == "Slutsatser_Samhallsanalys") {

      pres <- officer::add_slide(pres, layout = "Rubrik och innehåll", master = ppt_master) |>
        officer::ph_with(value = pic_rubrik, location = officer::ph_location_label(ph_label = "Rubrik 1")) |>
        officer::ph_with(value = pic_text, location = officer::ph_location_label(ph_label = "Platshållare för innehåll 2"))

      if (!is.null(ppt_anteckningar)) {
        anteckningar_txt <- paste0(ppt_anteckningar, collapse = "\n")
        pres <- pres |>
          officer::set_notes(value = anteckningar_txt, location = officer::notes_location_type("body"))
      }

    } # slut på if-satser för olika layouter
  } # slut på for-loop som går igenom df eller lista med diagram

  # ============= här flyttar vi slides om så önskas

  if (!is.null(slides_flytta)) {
    for (flytt_par in slides_flytta) {
      pres <- officer::move_slide(pres, index = flytt_par$fran, to = flytt_par$till)
    }
  }

  # ================================================== Spara filen =============================================================
  fullpath <- paste0(output_mapp, filnamn)
  if (spara_backup_om_fil_redan_finns) fullpath <- rdverktyg::sparafil_backup_omfinns(fullpath)

  # officer finns deklarerat i Imports här (till skillnad från källskriptet, som bara anropade det med
  # officer::-prefix utan library(officer)) - det räcker för att R ska registrera alla S3-metoder
  # paketet exporterar, inklusive print.rpptx, så vanlig print()-dispatch fungerar. Den tidigare
  # lösningen med getFromNamespace("print.rpptx", "officer") behövs alltså inte längre.
  invisible(print(pres, target = fullpath))
}

#' Skapa en tom ppt-lista
#'
#' En ppt-lista är en `data.frame` med en rad per bild/diagram som ska bli
#' en slide i presentationen, som sedan byggs med [skapa_ppt_fran_lista()].
#' Fyll på den med [ppt_lista_fyll_pa()], eller lägg till rader för hand
#' (t.ex. med `dplyr::add_row()`) för enstaka specialslides (se
#' `mall`-kolumnen - `"Rapporter_Samhallsanalys"`, `"Slutsatser_Samhallsanalys"`,
#' `"Avslutsbild_Samhallsanalys"` m.fl. i [skapa_ppt_fran_lista()]).
#'
#' @return En tom `data.frame` med kolumnerna `dia_path`, `titel`,
#'   `master`, `mall`, `txt_size`, samt list-kolumnerna `beskr`, `gg_obj`
#'   och `anteckningar`.
#' @export
initiera_ppt_df <- function() {
  retur_df <- data.frame(
    dia_path = as.character(),
    titel = character(),
    master = character(),
    mall = character(),
    txt_size = double())
  retur_df$beskr <- list()
  retur_df$gg_obj <- list()
  retur_df$anteckningar <- list()
  return(retur_df)
}

# Anpassar en bilds storlek till en platshållare men behåller proportionerna, och räknar ut
# platshållarens position så bilden kan centreras i den.
intern_bild_position_stlk <- function(pres_obj,
                               ppt_master,
                               ppt_mall,
                               ph_label,
                               img_url,
                               minus_varde_w = 0,
                               minus_varde_h = 0
) {

  retur_list <- list(width = NA, height = NA, left = NA, top = NA)

  if (ph_label == "slide") {
    slide_stlk <- officer::slide_size(pres_obj)
    ph_w <- slide_stlk$width - 0.5 - minus_varde_w
    ph_h <- slide_stlk$height - 0.5 - minus_varde_h
    ph_dims <- list(width = ph_w, height = ph_h, left = 0.25, top = 0.25)
  } else {
    ph_dims <- intern_get_placeholder_dims(presentation = pres_obj, ppt_master = ppt_master, ph_label = ph_label, ppt_mall = ppt_mall)
  }

  # Bugfix (bekräftad - läste samma fil två gånger): png::readPNG() lästes in en gång för själva
  # bilddatan och en andra gång (med info = TRUE) bara för dpi-attributet. Läser nu in filen en gång
  # och återanvänder dpi-attributet som redan finns på det objektet.
  png_obj <- png::readPNG(img_url, info = TRUE)
  img_size <- dim(png_obj) / attr(png_obj, "info")$dpi[1]
  img_h <- img_size[1]
  img_w <- img_size[2]

  prop_w <- img_w / ph_dims$width
  ny_w <- img_w / prop_w
  ny_h <- img_h / prop_w

  if (img_h > ph_dims$height) {
    prop_h <- ny_h / ph_dims$height
    ny_w <- ny_w / prop_h
    ny_h <- ny_h / prop_h
  }
  retur_list$width <- ny_w
  retur_list$height <- ny_h
  retur_list$left <- ph_dims$left + ((ph_dims$width - ny_w) / 2)
  retur_list$top <- ph_dims$top + ((ph_dims$height - ny_h) / 2)
  return(retur_list)
}

# Hämtar bredd/höjd/position (i tum) för en namngiven platshållare i en given mall/master.
intern_get_placeholder_dims <- function(presentation, ppt_master, ppt_mall, ph_label, unit = "in") {
  layout <- officer::layout_properties(presentation)

  retur_df <- list()
  retur_df$width <- layout$cx[layout$ph_label == ph_label & layout$name == ppt_mall & layout$master_name == ppt_master]
  retur_df$height <- layout$cy[layout$ph_label == ph_label & layout$name == ppt_mall & layout$master_name == ppt_master]
  retur_df$left <- layout$offx[layout$ph_label == ph_label & layout$name == ppt_mall & layout$master_name == ppt_master]
  retur_df$top <- layout$offy[layout$ph_label == ph_label & layout$name == ppt_mall & layout$master_name == ppt_master]

  # Bugfix (bekräftad): originalet gjorde om/else-grenen här rakt av, men avslutade sedan
  # ovillkorligen med "return(retur_df)" EFTER if/else-blocket - resultatet av multiplikationen
  # kastades alltid bort, så unit = "px"/"cm" gav tyst samma (ej konverterade) värden som "in". Hade
  # dessutom kraschat om den väl användes ("list * 96" fungerar inte i R) - aldrig upptäckt eftersom
  # unit alltid lämnats på standardvärdet "in" i praktiken. Returnerar nu faktiskt det konverterade
  # värdet i respektive gren.
  if (unit == "in") {
    return(retur_df)
  } else if (unit == "px") {
    return(lapply(retur_df, `*`, 96))
  } else if (unit == "cm") {
    return(lapply(retur_df, `*`, 2.54))
  } else stop(glue::glue("Bara 'in', 'px' och 'cm' stöds som enhet. Du angav '{unit}'."))
}
