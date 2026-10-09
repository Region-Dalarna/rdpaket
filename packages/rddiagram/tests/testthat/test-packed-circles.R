test_exempeldata <- function() {
  data.frame(
    bransch = c("Vård", "Handel", "Bygg", "IT"),
    HexCode = c("#ff4500", "#f0f0a0", "#ff00ff", "#add8e6"),
    value   = c(2500, 1800, 900, 400),
    stringsAsFactors = FALSE
  )
}

test_that("skapa_packed_circles stoppar med tydligt fel vid ogiltig jamforelse_*", {
  skip_if_not_installed("ggplot2")
  df <- test_exempeldata()

  expect_error(
    skapa_packed_circles(
      data = df, antal_kol = "value", grupp_kol = "bransch",
      jamforelse_namn = "Ej i arbete eller studier", jamforelse_varde = 1000,
      spara_bildfil = FALSE
    ),
    "grupp_kol"
  )

  expect_error(
    skapa_packed_circles(
      data = df, antal_kol = "value",
      jamforelse_namn = "Ej i arbete eller studier",
      spara_bildfil = FALSE
    ),
    "jamforelse_varde"
  )

  expect_error(
    skapa_packed_circles(
      data = df, antal_kol = "value",
      jamforelse_namn = "Ej i arbete eller studier", jamforelse_varde = -5,
      spara_bildfil = FALSE
    ),
    "jamforelse_varde"
  )
})

test_that("skapa_packed_circles ritar en extra, grå jamforelsecirkel utanför klungan", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggforce")
  df <- test_exempeldata()

  p_utan <- skapa_packed_circles(
    data = df, antal_kol = "value", autokoppla_branschnamn_farg = FALSE,
    spara_bildfil = FALSE
  )
  p_med <- skapa_packed_circles(
    data = df, antal_kol = "value", autokoppla_branschnamn_farg = FALSE,
    jamforelse_namn = "Ej i arbete eller studier", jamforelse_varde = 1200,
    spara_bildfil = FALSE
  )

  # Två extra lager: geom_circle (jämförelsecirkeln) + geom_text (etiketten)
  expect_equal(length(p_med$layers), length(p_utan$layers) + 2)

  # Jämförelsecirkeln ska ligga UTANFÖR klungan (dess egen kant, inte bara
  # centrum, ska vara längre från origo än dess radie), inte packad ihop med
  # branschcirklarna. Hittas via kolumnen "lbl" (finns bara i jämförelse-
  # cirkelns egna data, inte ringens).
  ar_jmf_lager <- vapply(p_med$layers, function(l) {
    inherits(l$geom, "GeomCircle") && is.data.frame(l$data) && "lbl" %in% names(l$data)
  }, logical(1))
  expect_equal(sum(ar_jmf_lager), 1)
  jmf_data <- p_med$layers[[which(ar_jmf_lager)]]$data
  expect_true(jmf_data$r > 0)
  expect_gt(sqrt(jmf_data$x^2 + jmf_data$y^2), jmf_data$r)

  # bildmatt ska finnas och vara giltiga tal i bada fallen (storleken/formen
  # paverkas av vilken riktning jamforelsecirkeln hamnar i - testas inte har)
  expect_true(all(is.finite(attr(p_med, "bildmatt"))))
  expect_true(all(attr(p_med, "bildmatt") > 0))
})

test_that("skapa_packed_circles: jamforelse_vinkel = NULL ger auto-vinkel, angiven vinkel styr", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggforce")
  df <- test_exempeldata()

  hamta_jmf_xy <- function(p) {
    ar_jmf <- vapply(p$layers, function(l) {
      inherits(l$geom, "GeomCircle") && is.data.frame(l$data) && "lbl" %in% names(l$data)
    }, logical(1))
    p$layers[[which(ar_jmf)]]$data[c("x", "y")]
  }

  # Explicit vinkel ska ge en forutsagbar position (rakt till hoger: y ~ 0, x > 0)
  p_hoger <- skapa_packed_circles(
    data = df, antal_kol = "value", autokoppla_branschnamn_farg = FALSE,
    jamforelse_namn = "Jmf", jamforelse_varde = 1200, jamforelse_vinkel = 0,
    spara_bildfil = FALSE
  )
  xy_hoger <- hamta_jmf_xy(p_hoger)
  expect_gt(xy_hoger$x, 0)
  expect_equal(xy_hoger$y, 0, tolerance = 1e-6)

  # Auto (NULL, standard) ska ge en annan vinkel an den tvingade (inte
  # garanterat rakt till hoger) - bara att den faktiskt racknas ut, inte
  # kraschar, och hamnar nagonstans utanfor origo
  p_auto <- skapa_packed_circles(
    data = df, antal_kol = "value", autokoppla_branschnamn_farg = FALSE,
    jamforelse_namn = "Jmf", jamforelse_varde = 1200,
    spara_bildfil = FALSE
  )
  xy_auto <- hamta_jmf_xy(p_auto)
  expect_true(is.finite(xy_auto$x) && is.finite(xy_auto$y))
  expect_gt(xy_auto$x^2 + xy_auto$y^2, 0)
})
