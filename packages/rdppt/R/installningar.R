#' Se vilka inställningar som gäller
#'
#' Visar de inställningar som styr hur presentationer byggs, och var varje
#' värde kommer ifrån. Inställningarna kan sättas för en presentation med
#' parametrar (t.ex. `ppt_ny(mall = )`), eller för alla presentationer med
#' `options()` - lämpligen i `.Rprofile`, så slipper man ange dem varje
#' gång. En parameter går före en option, och en option före standardvärdet.
#'
#' | Option | Innebörd | Standard |
#' |---|---|---|
#' | `rdppt.mall` | Mallen: en sökväg till en pptx-fil, `"regiondalarna"`, `"inbyggd"` eller en mallprofil ([ppt_mall_ange()] sätter den) | Region Dalarnas mall på G: om den går att nå, annars den kopia som följer med paketet |
#' | `rdppt.slidebank` | Pptx-fil med färdiga slides som `ppt_lagg_till_slide("befintlig")` hämtar från | mallfilen |
#' | `rdppt.stil` | Rubrikformatering (titelsida, avsnitt): `list(farg_rubrik =, titel_stlk =, undertitel_stlk =, namn_stlk =, avsnitt_stlk =)`, se [ppt_mallprofil()] | Region Dalarnas standardvärden |
#' | `rdppt.loggfil` | Textfil där diagram som hoppas över loggas, se [ppt_ny()] | ingen loggfil |
#' | `rdverktyg.utskriftsmapp` | Mapp som [ppt_spara()] sparar i | `rdverktyg::utskriftsmapp()` |
#'
#' @return En `data.frame` med kolumnerna `installning`, `varde` och `kalla`,
#'   osynligt.
#' @seealso [ppt_mall_ange()], [ppt_ny()], [ppt_spara()], [ppt_lagg_till_slide()]
#' @examples
#' \dontrun{
#' # i .Rprofile
#' options(
#'   rdppt.mall = "G:/skript/ppt/mall_rus_utan_bakgrund.pptx",
#'   rdppt.slidebank = "G:/skript/ppt/mall_rus_utan_bakgrund_demografi.pptx",
#'   rdppt.stil = list(titel_stlk = 54, undertitel_stlk = 24),
#'   rdppt.loggfil = "G:/skript/ppt/ppt_logg.txt"
#' )
#'
#' ppt_installningar()
#' }
#' @export
ppt_installningar <- function() {
  mall_option <- getOption("rdppt.mall")
  mall <- tryCatch(intern_mall_aktuell(NULL), error = function(e) NULL)
  mall_txt <- if (is.null(mall)) "(går inte att läsa)" else if (is.null(mall$mall_fil)) "officers inbyggda mall" else mall$mall_fil
  mall_kalla <- if (!is.null(mall_option)) "option rdppt.mall" else if (is.null(mall)) "-" else
    if (identical(mall_txt, intern_rd_standardmall)) "standard (Region Dalarnas mall på G:)" else "standard (mallen som följer med paketet)"

  slidebank <- getOption("rdppt.slidebank")

  mapp <- tryCatch(rdverktyg::utskriftsmapp(), error = function(e) NA_character_)
  mapp_kalla <- if (!is.null(getOption("rdverktyg.utskriftsmapp"))) "option rdverktyg.utskriftsmapp" else "standard (rdverktyg::utskriftsmapp())"

  stil_option <- getOption("rdppt.stil")
  stil_txt <- if (is.null(stil_option)) "Region Dalarnas standardvärden" else
    paste0(names(stil_option), " = ", vapply(stil_option, function(x) paste(x, collapse = "/"), character(1)), collapse = ", ")
  loggfil <- getOption("rdppt.loggfil")

  res <- data.frame(
    installning = c("mall", "slidebank", "stil", "loggfil", "spara-mapp"),
    varde = c(mall_txt, if (is.null(slidebank)) "(ingen)" else slidebank, stil_txt,
              if (is.null(loggfil)) "(ingen)" else loggfil, mapp),
    kalla = c(mall_kalla, if (is.null(slidebank)) "-" else "option rdppt.slidebank",
              if (is.null(stil_option)) "standard" else "option rdppt.stil",
              if (is.null(loggfil)) "-" else "option rdppt.loggfil", mapp_kalla),
    stringsAsFactors = FALSE
  )
  print(res, right = FALSE, row.names = FALSE)
  invisible(res)
}
