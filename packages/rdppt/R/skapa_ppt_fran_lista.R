#' Bygg en PowerPoint-presentation från en ppt-lista
#'
#' Tar en "ppt-lista" (se [initiera_ppt_df()] och [ppt_lista_fyll_pa()]) -
#' en rad per slide - och bygger en komplett `.pptx`-presentation utifrån
#' en pptx-mall. Vill du bygga en presentation steg för steg i stället för
#' att fylla en lista, se [ppt_ny()].
#'
#' @section Vilken layout en rad får (kolumnen `mall`):
#' * `NA` - layouten väljs automatiskt efter vad raden innehåller: bild,
#'   bild med rubrik, bild med text, eller rubrik och punktlista.
#' * Namnet på en layout i mallen, t.ex. `"Två innehållsdelar"`. Alla
#'   layouter i mallen går att använda - rubrik, text och bild läggs i de
#'   platshållare layouten har. Se vilka som finns med [ppt_mall_visa()].
#' * `"roll:<roll>"`, t.ex. `"roll:bild_rubrik"` - den layout som mallen
#'   använder för den rollen, se [ppt_mallprofil()].
#' * `"Rapporter_Samhallsanalys"` - en sida med aktuella rapporter från
#'   regiondalarna.se, `"Slutsatser_Samhallsanalys"` - rubrik (`titel`) och
#'   punktlista (`beskr`), `"Avslutsbild_Samhallsanalys"` - "Tack för mig!"
#'   med ditt namn och din e-post.
#' * `"befintlig:<nr>"`, t.ex. `"befintlig:2"` - en färdig slide som redan
#'   ligger i mallfilen hamnar här i presentationen, se [ppt_befintlig()].
#'
#' Äldre layoutnamn som inte finns i mallen (`"Bild och text till höger"`,
#' `"Bild utan text och rubrik"` m.fl.) översätts till den layout i mallen
#' som har motsvarande roll.
#'
#' @param pptlista En ppt-lista, se [initiera_ppt_df()].
#' @param huvudrubrik Presentationens huvudrubrik (på titelsidan). `NULL`
#'   = ingen titelsida.
#' @param farg_huvudrubrik Hexfärg på huvudrubriken och avsnittsrubriker.
#'   `NULL` (standard) = mallprofilens, se [ppt_mallprofil()].
#' @param underrubrik Underrubrik på titelsidan, eller `NA` för ingen.
#'   Standard: "Skapad <dagens datum> av Samhällsanalys\\nRegionala
#'   utvecklingsförvaltningen, Region Dalarna".
#' @param output_mapp Mapp där presentationen sparas.
#' @param spara_backup_om_fil_redan_finns TRUE om en befintlig fil med
#'   samma namn ska säkerhetskopieras i stället för att skrivas över.
#' @param filnamn Filnamn på den sparade presentationen.
#' @param mall_fil Mallen presentationen byggs utifrån: en sökväg till en
#'   pptx-fil, `"regiondalarna"`, `"inbyggd"` eller en mallprofil från
#'   [ppt_mallprofil()]. `NULL` (standard) = den mall som är vald med
#'   [ppt_mall_ange()], annars Region Dalarnas standardmall (från G: om
#'   den går att nå, annars den kopia som följer med paketet).
#' @param titelmaster Namnet på den master titelsidan ska använda. `NULL`
#'   (standard) = mallprofilens.
#' @param titel_text_stlk,undertitel_text_stlk Textstorlek på huvud-
#'   respektive underrubrik. `NULL` (standard) = mallprofilens.
#' @param rubrik_namn TRUE om presentatörens namn ska skrivas ut på
#'   titelsidan (om titelsidans layout har en platshållare för det).
#' @param rubrik_namn_stlk Textstorlek på presentatörens namn. `NULL`
#'   (standard) = mallprofilens.
#' @param utan_rubriker,utan_text TRUE om rubriker respektive brödtext ska
#'   utelämnas på de slides där layouten väljs automatiskt.
#' @param slides_flytta En lista med `list(fran =, till =)`-par för att
#'   flytta enskilda slides i den färdiga presentationen, eller `NULL`.
#'   För färdiga slides som ligger i mallfilen är `"befintlig:<nr>"` i
#'   kolumnen `mall` (eller [ppt_befintlig()]) enklare.
#' @param anv_uppgifter Vems namn/e-post som ska användas på titel- och
#'   avslutssidan: `NULL` (standard) = inloggad användare, ett namn som
#'   slås upp med [rddeploy::anv_hamta_namn_epost_fran_lista()], eller en
#'   lista `list(namn = "...", epost = "...")`.
#'
#' @return Sökvägen till den sparade presentationen, osynligt.
#' @export
skapa_ppt_fran_lista <- function(pptlista,
                                 huvudrubrik,
                                 farg_huvudrubrik = NULL,
                                 underrubrik = intern_standard_underrubrik(),
                                 output_mapp = rdverktyg::utskriftsmapp(),
                                 spara_backup_om_fil_redan_finns = FALSE,
                                 filnamn = "presentation.pptx",
                                 mall_fil = NULL,
                                 titelmaster = NULL,
                                 titel_text_stlk = NULL,
                                 undertitel_text_stlk = NULL,
                                 rubrik_namn = TRUE,
                                 rubrik_namn_stlk = NULL,
                                 utan_rubriker = FALSE,
                                 utan_text = FALSE,
                                 slides_flytta = NULL,
                                 anv_uppgifter = NULL) {

  # Inställningar ==================================================
  profil <- intern_mall_aktuell(mall_fil)

  # det som anges i anropet går före mallprofilens rubrikformatering
  stil <- profil$stil
  if (!is.null(farg_huvudrubrik)) stil$farg_rubrik <- farg_huvudrubrik
  if (!is.null(titel_text_stlk)) stil$titel_stlk <- titel_text_stlk
  if (!is.null(undertitel_text_stlk)) stil$undertitel_stlk <- undertitel_text_stlk
  if (!is.null(rubrik_namn_stlk)) stil$namn_stlk <- rubrik_namn_stlk
  profil["stil"] <- list(stil)

  fullpath <- intern_utfil(output_mapp, filnamn)

  pres <- intern_las_pptx(profil$mall_fil)
  lp <- officer::layout_properties(pres)
  layouter <- officer::layout_summary(pres)
  antal_befintliga <- length(pres)
  anv <- intern_anv_hamtare(anv_uppgifter)

  # alla rader tolkas och kontrolleras innan något byggs, så att ett felstavat layoutnamn eller en
  # saknad bildfil märks direkt och inte efter att halva presentationen är klar
  rader <- intern_tolka_pptlista(pptlista, profil, layouter, antal_befintliga, utan_rubriker, utan_text)

  # ordning håller reda på var varje slide i den färdiga presentationen ligger just nu: de slides
  # som redan finns i mallfilen ligger först, och nya läggs till efter dem
  ordning <- integer(0)

  # =================================================== Titelslide =============================================================
  if (intern_har_innehall(huvudrubrik)) {
    if (is.null(titelmaster)) titelmaster <- profil$master
    titellayout <- intern_roll_layout(profil, "titel")
    har_namnruta <- sum(intern_platshallare(lp, titellayout, titelmaster)$roll == "text") >= 2

    namn_txt <- NULL
    if (isTRUE(rubrik_namn) && har_namnruta && !is.null(anv())) {
      namn_txt <- paste0(anv()$namn, ", Samhällsanalytiker\nRegionala utvecklingsförvaltningen, Region Dalarna")
    }
    pres <- intern_slide_titel(pres, lp, profil, master = titelmaster, huvudrubrik = huvudrubrik,
                               underrubrik = underrubrik, namn_txt = namn_txt)
    ordning <- length(pres)
  }

  # =========== Därefter en slide per rad i ppt-listan =========================
  for (rad in rader) {
    if (rad$typ == "befintlig") {
      ordning <- c(ordning, rad$index)
      next
    }
    antal_fore <- length(pres)

    if (rad$typ == "rapporter") {
      pres <- intern_slide_rapporter(pres, lp, profil, layout = rad$layout, master = rad$master,
                                     anteckningar = rad$anteckningar)
    } else if (rad$typ == "avslut") {
      pres <- intern_slide_avslut(pres, lp, profil, layout = rad$layout, master = rad$master,
                                  rubrik = rad$rubrik, text = rad$text,
                                  anv = if (intern_har_innehall(rad$text)) NULL else anv(),
                                  anteckningar = rad$anteckningar)
    } else {
      rubrik <- rad$rubrik
      if (rad$typ == "avsnitt" && intern_har_innehall(rubrik)) {
        rubrik <- intern_formaterad(rubrik, stil$avsnitt_stlk, stil$farg_rubrik, fet = TRUE)
      }
      pres <- intern_lagg_till_slide(pres, lp, profil, layout = rad$layout, master = rad$master,
                                     rubrik = rubrik, text = rad$text, bild = rad$bild,
                                     txt_size = rad$txt_size, anteckningar = rad$anteckningar)
    }
    # rapportsidan hoppas över om rapporterna inte går att hämta - då har ingen slide tillkommit
    if (length(pres) > antal_fore) ordning <- c(ordning, length(pres))
  }

  # ============= färdiga slides ur mallfilen flyttas till sin plats, de som inte används tas bort
  if (any(vapply(rader, function(rad) rad$typ == "befintlig", logical(1)))) {
    pres <- intern_ordna_slides(pres, ordning)
  }

  # ============= här flyttar vi slides om så önskas
  if (!is.null(slides_flytta)) {
    for (flytt_par in slides_flytta) {
      pres <- officer::move_slide(pres, index = flytt_par$fran, to = flytt_par$till)
    }
  }

  # ================================================== Spara filen =============================================================
  if (spara_backup_om_fil_redan_finns) fullpath <- rdverktyg::sparafil_backup_omfinns(fullpath)
  intern_skriv_pptx(pres, fullpath)
  message("Presentation med ", length(pres), " slides sparad: ", fullpath)
  invisible(fullpath)
}

#' Skapa en tom ppt-lista
#'
#' En ppt-lista är en `data.frame` med en rad per slide i presentationen,
#' som sedan byggs med [skapa_ppt_fran_lista()]. Fyll på den med
#' [ppt_lista_fyll_pa()], eller lägg till rader för hand (t.ex. med
#' `dplyr::add_row()`). Vilken layout en rad får styrs av kolumnen `mall`,
#' se [skapa_ppt_fran_lista()].
#'
#' Vill du slippa hantera listan själv, se [ppt_ny()].
#'
#' @return En tom `data.frame` med kolumnerna `dia_path` (sökväg till en
#'   png-bild), `titel`, `master`, `mall`, `txt_size`, samt list-kolumnerna
#'   `beskr` (text), `gg_obj` (ett ggplot-objekt, i stället för `dia_path`)
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

# Äldre layoutnamn (från den mall skapa_ppt_fran_lista() ursprungligen skrevs för) och vilken roll
# de motsvarar - används när layouten inte finns under det namnet i mallen.
intern_aldre_layoutnamn <- c(
  "Bild och text till höger"             = "bild_text",
  "Bild och text till höger utan rubrik" = "bild_text",
  "Bild utan text och rubrik"            = "bild",
  "Rubrik och innehåll"                  = "punktlista",
  "1_Rubrik och innehåll"                = "punktlista",
  "Avsnittsrubrik"                       = "avsnitt",
  "Avslutsbild"                          = "avsnitt"
)

intern_standard_underrubrik <- function() {
  paste0("Skapad ", gsub("^0", "", format(Sys.Date(), "%d")), " ", format(Sys.Date(), "%b %Y"),
         " av Samhällsanalys\nRegionala utvecklingsförvaltningen, Region Dalarna")
}

# Den layout mallprofilen använder för en roll, med ett fel som talar om hur man pekar ut en om
# mallen saknar någon med ett känt namn.
intern_roll_layout <- function(profil, roll) {
  if (!roll %in% names(profil$layouter)) {
    stop("'", roll, "' är ingen roll. Giltiga roller: ", paste(names(profil$layouter), collapse = ", "), ".", call. = FALSE)
  }
  layout <- profil$layouter[[roll]]
  if (is.na(layout)) {
    stop("mallen har ingen layout utpekad för ", intern_roll_beskrivning[[roll]], " (rollen '", roll,
         "'). Se vilka layouter som finns med ppt_mall_visa() och peka ut en med ppt_mall_ange(..., layouter = c(",
         roll, " = \"layoutnamn\")).", call. = FALSE)
  }
  layout
}

# Tolkar alla rader i en ppt-lista, och stoppar med en samlad lista över det som är fel.
intern_tolka_pptlista <- function(pptlista, profil, layouter, antal_befintliga, utan_rubriker, utan_text) {
  fel <- character(0)
  rader <- lapply(seq_len(nrow(pptlista)), function(i) {
    tryCatch(
      intern_tolka_rad(pptlista, i, profil, layouter, antal_befintliga, utan_rubriker, utan_text),
      error = function(e) {
        fel <<- c(fel, paste0("  Rad ", i, ": ", conditionMessage(e)))
        NULL
      }
    )
  })

  befintliga <- unlist(lapply(rader, function(rad) if (!is.null(rad) && rad$typ == "befintlig") rad$index))
  if (anyDuplicated(befintliga) > 0) {
    fel <- c(fel, paste0("  Samma färdiga slide (nr ", paste(unique(befintliga[duplicated(befintliga)]), collapse = ", "),
                         ") används mer än en gång - en slide kan bara ligga på ett ställe."))
  }

  if (length(fel) > 0) {
    visa <- utils::head(fel, 10)
    if (length(fel) > 10) visa <- c(visa, paste0("  ... och ", length(fel) - 10, " till."))
    if (any(grepl("finns inte i mallen", fel, fixed = TRUE))) {
      visa <- c(visa, paste0("Layouter i mallen: ", paste0("'", layouter$layout[layouter$master == profil$master], "'", collapse = ", "), "."))
    }
    stop("Presentationen kan inte byggas:\n", paste(visa, collapse = "\n"), call. = FALSE)
  }
  rader
}

# Tolkar en rad i ppt-listan: vilken sorts slide det är, vilken layout den får och vad den ska
# innehålla.
intern_tolka_rad <- function(pptlista, i, profil, layouter, antal_befintliga, utan_rubriker, utan_text) {
  cell <- function(kolumn) if (kolumn %in% names(pptlista)) pptlista[[kolumn]][[i]] else NULL
  varde <- function(kolumn) {
    x <- cell(kolumn)
    if (is.null(x) || length(x) == 0 || is.na(x[1])) NULL else x[1]
  }

  mall <- varde("mall")
  rubrik <- varde("titel")
  if (!is.null(rubrik)) rubrik <- as.character(rubrik)
  text <- intern_text_fran_cell(cell("beskr"))
  txt_size <- varde("txt_size")
  if (is.null(txt_size)) txt_size <- NA
  master <- varde("master")
  if (is.null(master)) master <- profil$master

  bild <- cell("gg_obj")
  if (!intern_ar_ggplot(bild)) {
    bild <- varde("dia_path")
    if (!is.null(bild)) bild <- as.character(bild)
  }

  rad <- list(typ = "generell", layout = NA_character_, master = master, rubrik = rubrik, text = text,
              bild = bild, txt_size = txt_size, anteckningar = cell("anteckningar"))
  finns <- layouter$layout[layouter$master == master]

  if (is.null(mall)) {
    # ingen layout angiven - välj efter vad raden innehåller
    if (isTRUE(utan_rubriker)) rad["rubrik"] <- list(NULL)
    if (isTRUE(utan_text)) rad["text"] <- list(NULL)
    roll <- if (is.null(bild)) {
      "punktlista"
    } else if (intern_har_innehall(rad$text)) {
      "bild_text"
    } else if (intern_har_innehall(rad$rubrik)) {
      "bild_rubrik"
    } else {
      "bild"
    }
    rad$layout <- intern_roll_layout(profil, roll)

  } else if (grepl("^befintlig:", mall)) {
    index <- suppressWarnings(as.integer(sub("^befintlig:", "", mall)))
    if (is.na(index) || index < 1 || index > antal_befintliga) {
      stop("'", mall, "' pekar på en slide som inte finns - mallfilen innehåller ",
           if (antal_befintliga == 0) "inga färdiga slides" else paste0(antal_befintliga, " färdiga slides"), ".", call. = FALSE)
    }
    rad$typ <- "befintlig"
    rad$index <- index
    return(rad)

  } else if (grepl("^roll:", mall)) {
    roll <- sub("^roll:", "", mall)
    rad$layout <- intern_roll_layout(profil, roll)
    if (roll == "avsnitt") rad$typ <- "avsnitt"

  } else if (mall == "Rapporter_Samhallsanalys") {
    rad$typ <- "rapporter"
    rad$layout <- intern_roll_layout(profil, "jamforelse")

  } else if (mall == "Avslutsbild_Samhallsanalys") {
    rad$typ <- "avslut"
    rad$layout <- intern_roll_layout(profil, "avsnitt")

  } else if (mall == "Slutsatser_Samhallsanalys") {
    rad$layout <- intern_roll_layout(profil, "punktlista")

  } else if (mall %in% finns) {
    rad$layout <- mall
    if (identical(mall, profil$layouter[["avsnitt"]])) rad$typ <- "avsnitt"

  } else if (mall %in% names(intern_aldre_layoutnamn)) {
    roll <- intern_aldre_layoutnamn[[mall]]
    rad$layout <- intern_roll_layout(profil, roll)
    if (mall == "Avsnittsrubrik") rad$typ <- "avsnitt"
    if (grepl("utan rubrik", mall, fixed = TRUE)) rad["rubrik"] <- list(NULL)
    # dessa layouter hade tidigare 16 punkter som standardstorlek på texten
    if (is.na(txt_size) && mall %in% c("Bild och text till höger", "Bild och text till höger utan rubrik", "Avslutsbild")) {
      rad$txt_size <- 16
    }

  } else {
    stop("layouten '", mall, "' finns inte i mallen.", call. = FALSE)
  }

  if (is.character(rad$bild) && !file.exists(rad$bild)) {
    stop("hittar inte bildfilen ", rad$bild, call. = FALSE)
  }
  rad
}

# Texten i en rad (kolumnen beskr): NULL om den är tom, annars en teckenvektor, ett officer-objekt
# eller en namngiven lista (ett element per platshållare).
intern_text_fran_cell <- function(x) {
  if (is.null(x) || length(x) == 0) return(NULL)
  if (intern_ar_officer_text(x)) return(x)
  if (is.list(x)) {
    if (!is.null(names(x)) && all(nzchar(names(x)))) return(x)
    if (all(vapply(x, inherits, logical(1), what = "fpar"))) return(do.call(officer::block_list, x))
  }
  x <- as.character(unlist(x))
  x <- x[!is.na(x)]
  if (length(x) == 0 || !any(nzchar(x))) NULL else x
}

# Sökvägen presentationen sparas till. Mappen får anges med eller utan avslutande snedstreck.
intern_utfil <- function(mapp, filnamn) {
  if (!grepl("\\.pptx$", filnamn, ignore.case = TRUE)) filnamn <- paste0(filnamn, ".pptx")
  mapp <- sub("[/\\\\]+$", "", mapp)
  if (nzchar(mapp) && !dir.exists(mapp)) {
    stop("Mappen presentationen ska sparas i finns inte:\n  ", mapp,
         "\nAnge en annan mapp, eller sätt en egen standardmapp med options(rdverktyg.utskriftsmapp = \"sökväg/till/mapp/\").",
         call. = FALSE)
  }
  if (nzchar(mapp)) file.path(mapp, filnamn) else filnamn
}

# Presentatörens namn och e-post slås upp först när de behövs, och bara en gång. Går de inte att
# slå upp utelämnas de (NULL) i stället för att hela presentationsbygget stoppas.
intern_anv_hamtare <- function(anv_uppgifter) {
  hamtad <- FALSE
  uppgifter <- NULL
  function() {
    if (!hamtad) {
      uppgifter <<- intern_anv_uppgifter(anv_uppgifter)
      hamtad <<- TRUE
    }
    uppgifter
  }
}

intern_anv_uppgifter <- function(anv_uppgifter) {
  if (is.list(anv_uppgifter) && all(c("namn", "epost") %in% names(anv_uppgifter))) {
    return(anv_uppgifter[c("namn", "epost")])
  }
  tryCatch({
    uppgifter <- rddeploy::anv_hamta_namn_epost_fran_lista(anv_uppgifter)
    if (!intern_har_innehall(uppgifter$namn) || !intern_har_innehall(uppgifter$epost)) stop("inga uppgifter hittades")
    uppgifter
  }, error = function(e) {
    message("Kunde inte slå upp namn och e-post till titel-/avslutssidan (", conditionMessage(e),
            ") - de utelämnas. Ange dem med anv_uppgifter = list(namn = \"...\", epost = \"...\").")
    NULL
  })
}
