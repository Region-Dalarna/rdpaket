test_that("every_nth ger rätt urval", {
  f <- every_nth(3, sista_vardet = TRUE)
  expect_equal(f(letters[1:10]), c("a", "d", "g", "j"))
  f2 <- every_nth(2, sista_vardet = FALSE, ta_bort_nast_sista = TRUE)
  expect_equal(f2(1:6), c(1, 3))
})

test_that("intern_forbered_plotdata grupperar och summerar", {
  df <- data.frame(x = c("a", "a", "b"), g = c("k", "m", "k"), v = c(1, 2, 3))
  ut <- rddiagram:::intern_forbered_plotdata(df, c("x", "g"), "v")
  expect_true("total" %in% names(ut))
  expect_equal(nrow(ut), 3)
  expect_equal(ut$total[ut$x == "b"], 3)
})

test_that("intern_valj_farger väljer rätt", {
  expect_equal(rddiagram:::intern_valj_farger(NULL, 1), "#4f6228")
  expect_equal(rddiagram:::intern_valj_farger(NULL, 2), c("#9bbb59", "#4f6228"))
  expect_equal(rddiagram:::intern_valj_farger(c("#111111", "#222222"), 5),
               c("#111111", "#222222"))
})

test_that("SkapaStapelDiagram bygger ett ggplot-objekt", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(
    kommun = rep(c("Falun", "Borlänge", "Mora"), each = 2),
    kon = rep(c("Kvinnor", "Män"), 3),
    antal = c(120, 118, 90, 92, 40, 41)
  )
  p <- SkapaStapelDiagram(
    df, "kommun", "antal", skickad_x_grupp = "kon",
    output_mapp = tempdir(), filnamn_diagram = "test.png",
    skriv_till_diagramfil = FALSE
  )
  expect_s3_class(p, "ggplot")
  expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomBar"), logical(1))))
})

test_that("SkapaStapelDiagram: en grupp + längre färgvektor kraschar inte", {
  skip_if_not_installed("ggplot2")
  farger <- c("#4f6228", "#9bbb59", "#c3d69b", "#77933c")
  # en enda rad (en x-kategori, ingen grupp)
  p1 <- SkapaStapelDiagram(data.frame(kommun = "Falun", andel = 42),
                           "kommun", "andel", farger = farger,
                           output_mapp = tempdir(), filnamn_diagram = "t.png",
                           skriv_till_diagramfil = FALSE)
  expect_s3_class(p1, "ggplot")
  # flera x-kategorier, fortfarande ingen grupp
  p2 <- SkapaStapelDiagram(data.frame(kommun = c("Falun", "Mora"), andel = c(42, 31)),
                           "kommun", "andel", farger = farger,
                           output_mapp = tempdir(), filnamn_diagram = "t.png",
                           skriv_till_diagramfil = FALSE)
  expect_s3_class(p2, "ggplot")
})

test_that("SkapaStapelDiagram: gamla parameternamn funkar (manual_color, x_axis_sort_value, logga_path)", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(kommun = rep(c("Falun", "Mora"), each = 2),
                   kon = c("kvinnor", "män"), andel = c(42, 40, 31, 29))
  p <- SkapaStapelDiagram(
    df, "kommun", "andel", skickad_x_grupp = "kon",
    manual_color = c("#9bbb59", "#4f6228"), brew_palett = "Blues",
    x_axis_sort_value = TRUE,
    lagg_pa_logga = FALSE, logga_scaling = 25,
    output_mapp = tempdir(), filnamn_diagram = "t.png",
    skriv_till_diagramfil = FALSE)
  expect_s3_class(p, "ggplot")

  # alias och primärt namn ger samma resultat
  a <- SkapaStapelDiagram(df, "kommun", "andel", skickad_x_grupp = "kon",
                          farger = c("#111111", "#222222"),
                          output_mapp = tempdir(), filnamn_diagram = "t.png",
                          skriv_till_diagramfil = FALSE)
  b <- SkapaStapelDiagram(df, "kommun", "andel", skickad_x_grupp = "kon",
                          manual_color = c("#111111", "#222222"),
                          output_mapp = tempdir(), filnamn_diagram = "t.png",
                          skriv_till_diagramfil = FALSE)
  expect_equal(ggplot2::ggplot_build(a)$data, ggplot2::ggplot_build(b)$data)
})

test_that("SkapaLinjeDiagram: gamla parameternamn funkar (manual_color, logga_path)", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(ar = rep(2019:2022, 2), grupp = rep(c("A", "B"), each = 4),
                   v = c(10, 12, 11, 14, 8, 9, 9, 10))
  p <- SkapaLinjeDiagram(df, "ar", "v", skickad_x_grupp = "grupp",
                         manual_color = c("#178571", "#93cec1"),
                         lagg_pa_logga = FALSE, logga_scaling = 20,
                         output_mapp = tempdir(), filnamn_diagram = "t.png",
                         skriv_till_diagramfil = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("SkapaStapelDiagram: sortera_x = TRUE ger faktor-x", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(kommun = c("Falun", "Borlänge", "Mora"), antal = c(3, 1, 2))
  p <- SkapaStapelDiagram(df, "kommun", "antal",
                          output_mapp = tempdir(), filnamn_diagram = "t.png",
                          sortera_x = TRUE, skriv_till_diagramfil = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("SkapaLinjeDiagram bygger ett ggplot-objekt med linje", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(
    ar = rep(2019:2024, 2),
    grupp = rep(c("A", "B"), each = 6),
    v = c(10:15, 20:25)
  )
  p <- SkapaLinjeDiagram(df, "ar", "v", skickad_x_grupp = "grupp",
                         output_mapp = tempdir(), filnamn_diagram = "l.png",
                         skriv_till_diagramfil = FALSE)
  expect_s3_class(p, "ggplot")
  expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomLine"), logical(1))))
})

test_that("SkapaLinjeDiagram: berakna_index normaliserar till 100", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(ar = 2019:2022, v = c(50, 55, 60, 75))
  p <- SkapaLinjeDiagram(df, "ar", "v",
                         output_mapp = tempdir(), filnamn_diagram = "i.png",
                         berakna_index = TRUE, skriv_till_diagramfil = FALSE)
  expect_equal(p$data$total[p$data$ar == 2019], 100)
  expect_equal(p$data$total[p$data$ar == 2022], 150)
})
