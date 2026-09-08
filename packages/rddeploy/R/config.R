# Konfigurerbara standardvärden.
#
# Alla personberoende sökvägar och organisationsnamn läses via getOption() med
# ett paket-prefix, så att varje dator kan sätta sina egna värden i .Rprofile:
#
#   options(
#     rddeploy.gh_mapp    = "~/gh/",
#     rddeploy.gh_mapp_an = "~/gh_an/"
#   )

intern_opt <- function(namn, default) {
  getOption(paste0("rddeploy.", namn), default = default)
}

# Slutför en sökväg med avslutande snedstreck.
intern_slash <- function(x) {
  if (!grepl("/$", x)) paste0(x, "/") else x
}

#' Konfigurerbara standardvärden för rddeploy
#'
#' rddeploy läser alla personberoende sökvägar och organisationsnamn via
#' `getOption("rddeploy.<namn>", default)`. Sätt egna värden per dator i
#' `.Rprofile`:
#'
#' \describe{
#'   \item{`rddeploy.gh_mapp`}{Lokal föräldermapp för Region-Dalarna-repos
#'     (standard `"c:/gh/"`).}
#'   \item{`rddeploy.gh_mapp_an`}{Motsvarande för Analytikernätverket
#'     (standard `"c:/gh_an/"`).}
#'   \item{`rddeploy.gh_org`}{GitHub-organisation (standard `"Region-Dalarna"`).}
#'   \item{`rddeploy.gh_org_an`}{Analytikernätverkets organisation
#'     (standard `"Analytikernatverket"`).}
#'   \item{`rddeploy.epost_doman`}{E-postdomän [anv_epostadress_hamta()] letar
#'     efter (standard `"regiondalarna.se"`).}
#'   \item{`rddeploy.namn_epost_lista`}{Namngiven lista person -> `namn`/`epost`
#'     för [anv_hamta_namn_epost_fran_lista()].}
#' }
#'
#' Kör [rddeploy_config_status()] för att se aktuella värden.
#'
#' @name rddeploy-config
NULL

# Lokal mapp där Region-Dalarna-repos ligger (föräldermapp, inte repot självt).
intern_gh_mapp    <- function() intern_slash(intern_opt("gh_mapp", "c:/gh/"))
# Lokal mapp där Analytikernätverkets repos ligger.
intern_gh_mapp_an <- function() intern_slash(intern_opt("gh_mapp_an", "c:/gh_an/"))
# GitHub-organisation, standard resp. Analytikernätverket.
intern_gh_org     <- function() intern_opt("gh_org", "Region-Dalarna")
intern_gh_org_an  <- function() intern_opt("gh_org_an", "Analytikernatverket")
# E-postdomän som anv_epostadress_hamta() letar efter.
intern_epost_doman <- function() intern_opt("epost_doman", "regiondalarna.se")

# Känd person -> namn + e-post. Går att utöka via
# options(rddeploy.namn_epost_lista = list(anna = list(namn = "...", epost = "..."))).
intern_namn_epost_lista <- function() {
  intern_opt("namn_epost_lista", list(
    peter = list(namn = "Peter Möller",     epost = "peter.moller@regiondalarna.se"),
    mats  = list(namn = "Mats Andersson",   epost = "mats.b.andersson@regiondalarna.se"),
    jon   = list(namn = "Jon Frank",        epost = "jon.frank@regiondalarna.se")
  ))
}

#' Visa rddeploy:s konfigurerbara värden och om sökvägarna finns
#'
#' @return Osynligt: en `data.frame` med `namn`, `varde` och `finns`.
#' @export
rddeploy_config_status <- function() {
  rader <- list(
    c("rddeploy.gh_mapp",     intern_gh_mapp()),
    c("rddeploy.gh_mapp_an",  intern_gh_mapp_an()),
    c("rddeploy.gh_org",      intern_gh_org()),
    c("rddeploy.gh_org_an",   intern_gh_org_an()),
    c("rddeploy.epost_doman", intern_epost_doman())
  )
  df <- data.frame(
    namn  = vapply(rader, `[`, character(1), 1),
    varde = vapply(rader, `[`, character(1), 2),
    stringsAsFactors = FALSE
  )
  ar_sokvag <- grepl("mapp", df$namn)
  df$finns <- ifelse(ar_sokvag, dir.exists(df$varde), NA)

  cli::cli_h2("rddeploy-konfiguration")
  for (i in seq_len(nrow(df))) {
    status <- if (is.na(df$finns[i])) "" else if (df$finns[i]) " {.green (finns)}" else " {.red (saknas)}"
    cli::cli_li("{.field {df$namn[i]}}: {.val {df$varde[i]}}{status}")
  }
  invisible(df)
}
