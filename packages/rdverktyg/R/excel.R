# Excel- och filformatsfunktioner - utbrutna ur func_API.R.
# excel_xml_* läser pivottabelldata genom att packa upp xlsx-XML direkt (xml2).

#' @keywords internal
intern_excel_xml_lasa_pivot_cache <- function(xlsx_fil, cache_nr = 1) {
  if (!requireNamespace("xml2", quietly = TRUE)) stop("Paketet 'xml2' krävs.", call. = FALSE)

  def_path <- paste0("xl/pivotCache/pivotCacheDefinition", cache_nr, ".xml")
  rec_path <- paste0("xl/pivotCache/pivotCacheRecords", cache_nr, ".xml")

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  utils::unzip(xlsx_fil, files = c(def_path, rec_path), exdir = tmp)

  def_doc <- xml2::read_xml(file.path(tmp, def_path))
  ns <- xml2::xml_ns(def_doc)

  fields <- xml2::xml_find_all(def_doc, ".//d1:cacheField", ns)
  col_namn <- xml2::xml_attr(fields, "name")
  antal_kol <- length(col_namn)

  shared_items <- vector("list", antal_kol)
  for (i in seq_along(fields)) {
    items_el <- xml2::xml_find_first(fields[[i]], "d1:sharedItems", ns)
    if (!is.na(items_el)) {
      barn <- xml2::xml_children(items_el)
      tags <- xml2::xml_name(barn)
      shared_items[[i]] <- ifelse(tags == "m", NA_character_, xml2::xml_attr(barn, "v"))
    }
  }

  raw <- paste(readLines(file.path(tmp, rec_path), warn = FALSE, encoding = "UTF-8"), collapse = "")
  antal_rader <- as.integer(regmatches(raw, regexpr('(?<=count=")[0-9]+', raw, perl = TRUE)))

  traffar <- regmatches(raw, gregexpr("<[xnbds] v=\"[^\"]*\"/>|</r>", raw, perl = TRUE))[[1]]
  ar_radslut <- traffar == "</r>"
  rad_nr <- cumsum(ar_radslut)[!ar_radslut] + 1L
  celler <- traffar[!ar_radslut]
  taggar <- substr(celler, 2, 2)
  varden <- regmatches(celler, regexpr('(?<=v=")[^"]+', celler, perl = TRUE))
  kol_nr <- sequence(tabulate(rad_nr))

  resultat <- stats::setNames(
    replicate(antal_kol, character(antal_rader), simplify = FALSE), col_namn
  )

  ar_x <- taggar == "x"
  if (any(!ar_x)) {
    idx_direkt <- which(!ar_x)
    for (k in seq_along(col_namn)) {
      mask <- idx_direkt[kol_nr[idx_direkt] == k]
      if (length(mask) > 0) resultat[[k]][rad_nr[mask]] <- varden[mask]
    }
  }
  if (any(ar_x)) {
    idx_x <- which(ar_x)
    for (k in seq_along(col_namn)) {
      mask <- idx_x[kol_nr[idx_x] == k]
      if (length(mask) > 0) {
        resultat[[k]][rad_nr[mask]] <- shared_items[[k]][as.integer(varden[mask]) + 1L]
      }
    }
  }

  df <- as.data.frame(resultat, stringsAsFactors = FALSE, check.names = FALSE)
  for (i in seq_along(col_namn)) {
    if (is.null(shared_items[[i]])) df[[col_namn[i]]] <- as.numeric(df[[col_namn[i]]])
  }
  df
}

#' @keywords internal
intern_excel_xml_hamta_kodmappningar <- function(xlsx_fil) {
  if (!requireNamespace("xml2", quietly = TRUE)) stop("Paketet 'xml2' krävs.", call. = FALSE)

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  alla_filer <- utils::unzip(xlsx_fil, list = TRUE)$Name
  cache_defs <- alla_filer[grepl("pivotCache/pivotCacheDefinition", alla_filer)]
  utils::unzip(xlsx_fil, files = c("xl/sharedStrings.xml", cache_defs), exdir = tmp)

  ss_doc <- xml2::read_xml(file.path(tmp, "xl/sharedStrings.xml"))
  ns_ss <- xml2::xml_ns(ss_doc)
  si_noder <- xml2::xml_find_all(ss_doc, ".//d1:si", ns_ss)
  strangar <- vapply(si_noder, function(si) {
    paste(xml2::xml_text(xml2::xml_find_all(si, ".//d1:t", ns_ss)), collapse = "")
  }, character(1))

  traffar <- strangar[grepl("^[A-ZÅÄÖ] {1,2}\\S", strangar)]
  mojliga_mappningar <- stats::setNames(
    trimws(substr(traffar, 2, nchar(traffar))), substr(traffar, 1, 1)
  )

  resultat <- list()
  for (cache_fil in cache_defs) {
    cache_doc <- xml2::read_xml(file.path(tmp, cache_fil))
    ns_c <- xml2::xml_ns(cache_doc)
    for (field in xml2::xml_find_all(cache_doc, ".//d1:cacheField", ns_c)) {
      kolnamn <- xml2::xml_attr(field, "name")
      items_el <- xml2::xml_find_first(field, "d1:sharedItems", ns_c)
      if (is.na(items_el)) next
      barn <- xml2::xml_children(items_el)
      varden <- xml2::xml_attr(barn[xml2::xml_name(barn) == "s"], "v")
      varden <- varden[!is.na(varden)]
      if (length(varden) == 0) next

      overlap <- varden[varden %in% names(mojliga_mappningar)]
      if (length(overlap) / length(varden) > 0.3) {
        resultat[[kolnamn]] <- mojliga_mappningar[overlap]
      }
    }
  }
  invisible(resultat)
}

#' Läs pivottabelldata ur en xlsx-fil
#'
#' Läser pivottabellcachen från en eller flera flikar och returnerar en
#' namngiven lista med en `data.frame` per flik. Lägger till `_klartext`-kolumner
#' för kodade variabler där mappningar hittas i filen.
#'
#' @param xlsx_fil Sökväg till en `.xlsx`-fil.
#' @param flikar `NULL` (alla), en teckenvektor med fliknamn, eller en
#'   numerisk vektor med positioner.
#'
#' @return En namngiven lista med en `data.frame` per läst flik.
#' @export
excel_xml_las_fil <- function(xlsx_fil, flikar = NULL) {
  if (!requireNamespace("xml2", quietly = TRUE)) stop("Paketet 'xml2' krävs.", call. = FALSE)

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  rel_filer <- c("xl/workbook.xml", "xl/_rels/workbook.xml.rels")
  alla_filer <- utils::unzip(xlsx_fil, list = TRUE)$Name
  sheet_rels <- alla_filer[grepl("xl/worksheets/_rels/sheet.*\\.rels", alla_filer)]
  pivot_rels <- alla_filer[grepl("xl/pivotTables/_rels/pivotTable.*\\.rels", alla_filer)]
  utils::unzip(xlsx_fil, files = c(rel_filer, sheet_rels, pivot_rels), exdir = tmp)

  kodmappningar <- intern_excel_xml_hamta_kodmappningar(xlsx_fil)

  wb_doc <- xml2::read_xml(file.path(tmp, "xl/workbook.xml"))
  ns_wb <- xml2::xml_ns(wb_doc)
  sheets <- xml2::xml_find_all(wb_doc, ".//d1:sheet", ns_wb)
  blad_namn <- xml2::xml_attr(sheets, "name")
  blad_rid <- xml2::xml_attr(sheets, "id")

  urval <- if (is.null(flikar)) {
    seq_along(blad_namn)
  } else if (is.numeric(flikar)) {
    ogiltiga <- flikar[flikar < 1 | flikar > length(blad_namn)]
    if (length(ogiltiga) > 0) {
      stop("Ogiltiga positioner: ", paste(ogiltiga, collapse = ", "),
           ". Filen har ", length(blad_namn), " flikar.")
    }
    as.integer(flikar)
  } else if (is.character(flikar)) {
    ogiltiga <- flikar[!flikar %in% blad_namn]
    if (length(ogiltiga) > 0) {
      stop("Fliknamn hittades inte: ", paste(ogiltiga, collapse = ", "),
           ".\nTillgängliga flikar: ", paste(blad_namn, collapse = ", "))
    }
    match(flikar, blad_namn)
  } else {
    stop("flikar måste vara NULL, en teckenvektor eller en numerisk vektor.")
  }

  wb_rels_doc <- xml2::read_xml(file.path(tmp, "xl/_rels/workbook.xml.rels"))
  ns_rels <- xml2::xml_ns(wb_rels_doc)
  rels <- xml2::xml_find_all(wb_rels_doc, ".//d1:Relationship", ns_rels)
  rid_till_target <- stats::setNames(xml2::xml_attr(rels, "Target"), xml2::xml_attr(rels, "Id"))

  resultat_lista <- stats::setNames(vector("list", length(urval)), blad_namn[urval])

  for (j in seq_along(urval)) {
    i <- urval[j]
    blad <- blad_namn[i]
    sheet_target <- rid_till_target[blad_rid[i]]
    sheet_nr <- sub(".*sheet(\\d+)\\.xml", "\\1", sheet_target)

    sheet_rels_path <- file.path(tmp, "xl", "worksheets", "_rels",
                                 paste0("sheet", sheet_nr, ".xml.rels"))
    if (!file.exists(sheet_rels_path)) next

    sr_rels <- xml2::xml_find_all(xml2::read_xml(sheet_rels_path), ".//d1:Relationship",
                                  xml2::xml_ns(xml2::read_xml(sheet_rels_path)))
    sr_types <- xml2::xml_attr(sr_rels, "Type")
    sr_targets <- xml2::xml_attr(sr_rels, "Target")

    pivot_idx <- which(grepl("pivotTable", sr_types))
    if (length(pivot_idx) == 0) next

    pivot_nr <- sub(".*pivotTable(\\d+)\\.xml", "\\1", sr_targets[pivot_idx[1]])
    pt_rels_path <- file.path(tmp, "xl", "pivotTables", "_rels",
                              paste0("pivotTable", pivot_nr, ".xml.rels"))
    pt_rels_doc <- xml2::read_xml(pt_rels_path)
    pt_rels <- xml2::xml_find_all(pt_rels_doc, ".//d1:Relationship", xml2::xml_ns(pt_rels_doc))
    cache_nr <- as.integer(sub(".*pivotCacheDefinition(\\d+)\\.xml", "\\1",
                               xml2::xml_attr(pt_rels, "Target")[1]))

    df <- intern_excel_xml_lasa_pivot_cache(xlsx_fil, cache_nr)

    for (kol in intersect(names(df), names(kodmappningar))) {
      pos <- which(names(df) == kol)
      klartext <- kodmappningar[[kol]][df[[kol]]]
      df <- data.frame(
        df[seq_len(pos)], klartext,
        if (pos < ncol(df)) df[seq(pos + 1, ncol(df))] else NULL,
        stringsAsFactors = FALSE, check.names = FALSE
      )
      names(df)[pos + 1] <- paste0(kol, "_klartext")
    }
    resultat_lista[[blad]] <- df
  }
  resultat_lista
}

#' Ladda ner en Excelfil från en URL
#'
#' @param url_excel URL till en `.xlsx`-fil.
#' @param skippa_rader Antal rader att hoppa över i varje flik.
#' @param df_om_bara_en_flik Returnera en `data.frame` i stället för en lista
#'   när filen bara har en flik.
#' @param hoppa_over_flikar Fliknamn som inte ska läsas in.
#' @param rbind_dataset Om `TRUE` binds alla flikar ihop till ett dataset.
#' @param mutate_flik Namn på en kolumn som fylls med fliknamnet, eller `NA`.
#'
#' @return En namngiven lista med en `tibble` per flik, eller en `tibble`.
#' @export
hamta_excel_dataset_med_url <- function(url_excel, skippa_rader = 0,
                                        df_om_bara_en_flik = TRUE,
                                        hoppa_over_flikar = NA,
                                        rbind_dataset = FALSE,
                                        mutate_flik = "kolumnnamn") {
  if (!requireNamespace("readxl", quietly = TRUE)) stop("Paketet 'readxl' krävs.", call. = FALSE)

  excel_fil <- tempfile(fileext = ".xlsx")
  curl::curl_fetch_disk(url_excel, path = excel_fil)

  flikar <- readxl::excel_sheets(excel_fil)
  if (!all(is.na(hoppa_over_flikar))) flikar <- flikar[!flikar %in% hoppa_over_flikar]

  dataset_lista <- stats::setNames(lapply(flikar, function(f) {
    df <- readxl::read_xlsx(excel_fil, sheet = f, skip = skippa_rader)
    if (!is.na(mutate_flik)) df[[mutate_flik]] <- f
    df
  }), flikar)

  if (rbind_dataset) return(dplyr::bind_rows(dataset_lista))
  if (df_om_bara_en_flik && length(dataset_lista) == 1) return(dataset_lista[[1]])
  dataset_lista
}

#' Konvertera ett dataset till ett annat filformat
#'
#' @param sokvag_fil Sökväg till indatafilen.
#' @param nytt_format_filandelse Ny filändelse, t.ex. `"xlsx"` eller `".csv"`.
#' @param teckenkodtabell Teckenkodning som skickas till `rio::import()`.
#'
#' @return Inget - skriver den nya filen bredvid originalet.
#' @export
konvertera_dataset_filformat <- function(sokvag_fil, nytt_format_filandelse,
                                         teckenkodtabell = "Latin-1") {
  if (!requireNamespace("rio", quietly = TRUE)) stop("Paketet 'rio' krävs.", call. = FALSE)

  dataset_df <- rio::import(sokvag_fil, encoding = teckenkodtabell)

  if (substr(nytt_format_filandelse, 1, 1) != ".") {
    nytt_format_filandelse <- paste0(".", nytt_format_filandelse)
  }
  nytt_filnamn <- paste0(fs::path_ext_remove(sokvag_fil), nytt_format_filandelse)
  rio::export(dataset_df, nytt_filnamn)
  invisible(nytt_filnamn)
}

#' Spara en eller flera data.frames som formaterad xlsx
#'
#' @param indata En `data.frame` eller en (namngiven) lista av `data.frame`s.
#' @param output_mapp Målmapp.
#' @param excelfil_namn Filnamn. `NA` = härled från `indata`.
#' @param auto_kolumnbredd Anpassa kolumnbredd efter innehåll.
#' @param fetstil_rader,fetstil_kolumner Rader/kolumner i fetstil, eller `NA`.
#' @param filnamnstillagg Filändelse om `excelfil_namn` härleds.
#' @param skriv_over_fil Skriv över befintlig fil.
#'
#' @return Inget - skriver filen.
#' @export
excelfil_spara_formaterad <- function(indata, output_mapp = utskriftsmapp(),
                                      excelfil_namn = NA, auto_kolumnbredd = TRUE,
                                      fetstil_rader = 1, fetstil_kolumner = NA,
                                      filnamnstillagg = "xlsx", skriv_over_fil = TRUE) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) stop("Paketet 'openxlsx' krävs.", call. = FALSE)
  stopifnot(dir.exists(output_mapp))

  if (is.na(excelfil_namn)) {
    excelfil_namn <- paste0(deparse(substitute(indata)), ".", filnamnstillagg)
  }
  if (is.data.frame(indata)) indata <- list(dataset = indata)
  if (is.null(names(indata))) names(indata) <- paste0("dataset_", seq_along(indata))

  wb <- openxlsx::createWorkbook()
  for (namn in names(indata)) {
    dt <- indata[[namn]]
    openxlsx::addWorksheet(wb, namn)
    openxlsx::writeData(wb, sheet = namn, x = dt)

    if (auto_kolumnbredd) {
      bredd <- pmax(
        vapply(dt, function(k) max(nchar(as.character(k)), na.rm = TRUE), numeric(1)),
        nchar(names(dt))
      ) + 2
      openxlsx::setColWidths(wb, sheet = namn, cols = seq_len(ncol(dt)), widths = bredd)
    }
    if (!is.na(fetstil_rader[1])) {
      openxlsx::addStyle(wb, sheet = namn, rows = fetstil_rader, cols = seq_len(ncol(dt)),
                         style = openxlsx::createStyle(textDecoration = "bold"), gridExpand = TRUE)
    }
    if (!is.na(fetstil_kolumner[1])) {
      openxlsx::addStyle(wb, sheet = namn, rows = seq_len(nrow(dt)), cols = fetstil_kolumner,
                         style = openxlsx::createStyle(textDecoration = "bold"), gridExpand = TRUE)
    }
  }
  openxlsx::saveWorkbook(wb, fs::path(output_mapp, excelfil_namn), overwrite = skriv_over_fil)
}
