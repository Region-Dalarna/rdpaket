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

  # Jämförelsecirkeln ska ligga UTANFÖR klungans ring (y-centrum under
  # ringens nedre kant), inte packad ihop med branschcirklarna. Hittas via
  # kolumnen "lbl" (finns bara i jämförelsecirkelns egna data, inte ringens).
  ar_jmf_lager <- vapply(p_med$layers, function(l) {
    inherits(l$geom, "GeomCircle") && is.data.frame(l$data) && "lbl" %in% names(l$data)
  }, logical(1))
  expect_equal(sum(ar_jmf_lager), 1)
  jmf_data <- p_med$layers[[which(ar_jmf_lager)]]$data
  expect_true(jmf_data$y < 0)
  expect_true(jmf_data$r > 0)

  # bildmatt ska bli högre med jämförelsecirkeln (den sticker ut nedanför)
  expect_gt(attr(p_med, "bildmatt")[["hojd"]] / attr(p_med, "bildmatt")[["bredd"]],
            attr(p_utan, "bildmatt")[["hojd"]] / attr(p_utan, "bildmatt")[["bredd"]])
})
