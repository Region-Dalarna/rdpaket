test_that("list_komma_* sätter rätt bindeord", {
  expect_equal(list_komma_och(c("a", "b", "c")), "a, b och c")
  expect_equal(list_komma_eller(c("a", "b", "c")), "a, b eller c")
  expect_equal(list_komma_samt(c("a", "b", "c")), "a, b samt c")
})

test_that("list_komma_och hanterar korta vektorer", {
  expect_equal(list_komma_och("a"), "a")
  expect_equal(list_komma_och(c("a", "b")), "a och b")
  expect_equal(list_komma_och(character(0)), character(0))
})

test_that("dela_upp_strang_radbryt bryter långa strängar", {
  ut <- dela_upp_strang_radbryt("ett tva tre fyra fem sex", max_langd = 10)
  expect_length(ut, 1)
  expect_true(grepl("\n", ut))
  expect_true(all(nchar(strsplit(ut, "\n")[[1]]) <= 12))
})

test_that("dela_upp_strang_radbryt är vektoriserad", {
  ut <- dela_upp_strang_radbryt(c("kort", "en ganska lang strang har"), max_langd = 10)
  expect_length(ut, 2)
  expect_false(grepl("\n", ut[1]))
})

test_that("byt_ut_svenska_tecken translittererar", {
  expect_equal(byt_ut_svenska_tecken("Åäö ÅÄÖ"), "Aao AAO")
  expect_equal(byt_ut_svenska_tecken("Smöregård"), "Smoregard")
})

test_that("procent_till_text ger kända textformer", {
  expect_equal(procent_till_text(0), "ingen")
  expect_equal(procent_till_text(50), "varannan")
  expect_equal(procent_till_text(25), "var fjärde")
  expect_equal(procent_till_text(20), "var femte")
  # OBS: procent >= 95 ger "nästan alla"; branchen procent == 100 ~ "alla" nås
  # aldrig i originalkoden (känd bugg, ändras inte i denna extraktion).
  expect_equal(procent_till_text(100), "nästan alla")
})

test_that("forandring_till_text beskriver förändring", {
  expect_equal(forandring_till_text(100, 100), "varit oförändrad")
  expect_equal(forandring_till_text(100, 103), "ökat något")
  expect_equal(forandring_till_text(100, 200), "fördubblats")
  expect_equal(forandring_till_text(100, 30), "minskat mycket")
  expect_equal(forandring_till_text(100, 8), "halverats")
})
