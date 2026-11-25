# Region-hjälpare för presentationer som byggs för en eller flera regioner (län/kommuner).

#' Texter och koder som hör till de valda regionerna
#'
#' Tar de regionkoder en presentation byggs för och ger tillbaka det som annars skrivs för hand i varje
#' skript: länskoder, kommunkoder, regionnamn och färdiga texter. Samma uppgifter används automatiskt när du
#' anger `region_vekt` i [ppt_ny()], se där.
#'
#' @param region_vekt Läns- och/eller kommunkoder, t.ex. `c("20", "2080", "2081")`.
#'
#' @return En lista med
#'   * `koder` regionkoderna, `lan_koder` länskoderna (unika, även för kommunernas län) och `kommun_koder`
#'     kommunkoderna (de med fyra tecken).
#'   * `namn` regionernas namn (länens utan " län"), i den ordning koderna anges.
#'   * `text` namnen som en uppräkning med Dalarna sist, t.ex. "Gävleborg, Uppsala och Dalarna".
#'   * `text_genitiv` namnen med genitiv-s i den ordning koderna anges, t.ex. "Dalarnas, Falus och Borlänges".
#'   * `filnamn` namnen i genitiv med understreck och utan tecken som inte passar i filnamn, t.ex.
#'     "dalarnas_falus".
#' @examples
#' \dontrun{
#' ppt_regioner(c("20", "2080"))$text
#' }
#' @export
ppt_regioner <- function(region_vekt) {
  region_vekt <- as.character(region_vekt)
  if (length(region_vekt) == 0 || anyNA(region_vekt)) {
    stop("region_vekt ska vara en eller flera regionkoder, t.ex. c(\"20\", \"2080\").", call. = FALSE)
  }
  region_vekt <- unique(region_vekt)
  uppslag <- rdverktyg::hamtaregion_kod_namn(region_vekt)
  namn <- rdverktyg::skapa_kortnamn_lan(uppslag$region[match(region_vekt, uppslag$regionkod)])
  namn[is.na(namn)] <- region_vekt[is.na(namn)]

  sorterat <- if ("Dalarna" %in% namn) c(setdiff(sort(namn), "Dalarna"), "Dalarna") else sort(namn)
  genitiv <- ifelse(grepl("[sxz]$", namn), namn, paste0(namn, "s"))

  list(
    koder = region_vekt,
    lan_koder = unique(substr(region_vekt, 1, 2)),
    kommun_koder = region_vekt[nchar(region_vekt) == 4],
    namn = namn,
    text = rdverktyg::list_komma_och(sorterat),
    text_genitiv = rdverktyg::list_komma_och(genitiv),
    filnamn = paste(intern_filnamnsbas(genitiv), collapse = "_")
  )
}

# Text -> något som fungerar i ett filnamn: små bokstäver, å/ä/ö utan prickar, understreck mellan orden.
intern_filnamnsbas <- function(x) {
  gsub("^_+|_+$", "", gsub("[^a-z0-9]+", "_", chartr("åäöéü", "aaoeu", tolower(x))))
}

# Byter <region> (uppräkning, Dalarna sist) och <regions> (genitiv) i texter mot de valda regionerna. Lämnar
# allt annat än text orört, och allt om presentationen inte har några regioner.
intern_ersatt_regionplatshallare <- function(x, regioner) {
  if (is.null(regioner) || !is.character(x)) return(x)
  x <- gsub("<regions>", regioner$text_genitiv, x, fixed = TRUE)
  gsub("<region>", regioner$text, x, fixed = TRUE)
}

# Sökväg till loggfilen när loggfil = TRUE: utskriftsmappen, presentationens namn, användaren och _auto om
# körningen inte görs för hand (t.ex. schemalagd).
intern_loggfil_automatisk <- function(huvudrubrik, regioner = NULL) {
  bas <- if (intern_har_innehall(huvudrubrik)) intern_filnamnsbas(huvudrubrik) else "presentation"
  if (!nzchar(bas)) bas <- "presentation"
  anv <- intern_filnamnsbas(rddeploy::anv_anvandarkonto_hamta())
  delar <- c(bas, if (nzchar(anv)) anv, if (!interactive()) "auto")
  sokvag <- file.path(rdverktyg::utskriftsmapp(), paste0(paste(delar, collapse = "_"), ".txt"))
  if (!dir.exists(dirname(sokvag))) dir.create(dirname(sokvag), recursive = TRUE)
  sokvag
}
