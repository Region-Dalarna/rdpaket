test_that("skapa_kortnamn_lan tar bort ' län' och foge-s", {
  expect_equal(skapa_kortnamn_lan("Dalarnas län"), "Dalarna")
  expect_equal(skapa_kortnamn_lan("Stockholms län"), "Stockholm")
  expect_equal(skapa_kortnamn_lan("Västra Götalands län"), "Västra Götaland")
  expect_equal(skapa_kortnamn_lan("Riket"), "Riket")
  expect_equal(skapa_kortnamn_lan("Riket", byt_ut_riket_mot_sverige = TRUE), "Sverige")
  expect_equal(skapa_kortnamn_lan(NA_character_), NA_character_)
})

test_that("region_kolumn_splitta_kod_klartext delar kolumnen", {
  df <- data.frame(reg = c("2080 Falun", "20 Dalarnas län"))
  ut <- region_kolumn_splitta_kod_klartext(df, "reg")
  expect_equal(ut$regionkod, c("2080", "20"))
  expect_equal(ut$region, c("Falun", "Dalarnas län"))
  expect_false("reg" %in% names(ut))
})

test_that("hamtaregtab hämtar riket + län + kommuner (nätverk)", {
  skip_on_cran()
  skip_if_offline()

  rt <- hamtaregtab()
  expect_named(rt, c("regionkod", "region"))
  expect_true("00" %in% rt$regionkod)
  expect_true("20" %in% rt$regionkod)     # Dalarnas län
  expect_true("2080" %in% rt$regionkod)   # Falun
  expect_gt(nrow(rt), 300)
})

test_that("hamtakommuner och hamtaAllaLan (nätverk)", {
  skip_on_cran()
  skip_if_offline()

  dalarna <- hamtakommuner("20", tamedlan = FALSE, tamedriket = FALSE)
  expect_length(dalarna, 15)                 # Dalarna har 15 kommuner
  expect_true(all(nchar(dalarna) == 4))
  expect_true("2080" %in% dalarna)

  lan <- hamtaAllaLan(tamedriket = FALSE)
  expect_length(lan, 21)
  expect_false("00" %in% lan)

  expect_true(ar_alla_kommuner_i_ett_lan(dalarna))
  expect_equal(
    ar_alla_kommuner_i_ett_lan(dalarna, returnera_text = TRUE),
    "Dalarnas kommuner"
  )
  expect_true(ar_alla_lan_i_sverige(lan))
})
