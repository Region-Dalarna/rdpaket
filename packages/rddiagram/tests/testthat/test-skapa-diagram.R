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
  expect_equal(rddiagram:::intern_valj_farger(NA, NA, 1), "#4f6228")
  expect_equal(rddiagram:::intern_valj_farger(NA, NA, 2), c("#9bbb59", "#4f6228"))
  expect_equal(rddiagram:::intern_valj_farger(c("#111111", "#222222"), NA, 5),
               c("#111111", "#222222"))
})

test_that("intern_valj_farger: en egen manuell färg går alltid före, oavsett antal grupper", {
  # Detta var trasigt: en enda manual_color ignorerades helt när det inte
  # fanns någon gruppering (antal_grupper <= 1), och "#4f6228" användes i
  # stället - samma bugg motsvarande kod i original-func_SkapaDiagram.R INTE
  # hade (manual_color kollas där separat och FÖRE brew_palett/standardfärgen).
  expect_equal(rddiagram:::intern_valj_farger("#178571", NA, 1), "#178571")
  expect_equal(rddiagram:::intern_valj_farger("#178571", "Greens", 1), "#178571")
  expect_equal(rddiagram:::intern_valj_farger("#178571", NA, 2), "#178571")
})

test_that("SkapaStapelDiagram: skickad_x_grupp = NA fungerar som NULL (ingen grupp)", {
  skip_if_not_installed("ggplot2")
  # NA används genomgående i anropande skript (manual_color, logga_path osv.)
  # som "inget värde" - skickad_x_grupp ska följa samma konvention och inte
  # krascha på plot_df[[NA]].
  df <- data.frame(ar = 2018:2022, varde = c(100, 102, 98, 101, 103))
  p <- SkapaStapelDiagram(
    df, "ar", "varde", skickad_x_grupp = NA,
    output_mapp = tempdir(), filnamn_diagram = "test.png",
    skriv_till_diagramfil = FALSE
  )
  expect_s3_class(p, "ggplot")
})

test_that("SkapaStapelDiagram: x_axis_visa_var_xe_etikett = NA fungerar som NULL (ingen gallring)", {
  skip_if_not_installed("ggplot2")
  # Samma NA-som-NULL-konvention som skickad_x_grupp. every_nth(NA, ...) gör
  # rep(FALSE, NA - 1) internt, vilket kraschar med "invalid 'times' argument"
  # om NA inte normaliseras bort innan every_nth() anropas.
  df <- data.frame(ar = 2018:2022, varde = c(100, 102, 98, 101, 103))
  p <- SkapaStapelDiagram(
    df, "ar", "varde", x_axis_visa_var_xe_etikett = NA,
    output_mapp = tempdir(), filnamn_diagram = "test.png",
    skriv_till_diagramfil = FALSE
  )
  expect_s3_class(p, "ggplot")
})

test_that("SkapaStapelDiagram: x_var_fokus = NA fungerar som NULL (ingen fokusering)", {
  skip_if_not_installed("ggplot2")
  # Samma NA-som-NULL-konvention som skickad_x_grupp/x_axis_visa_var_xe_etikett.
  # plot_df[[NA]] kraschar med "Can't extract column with `x_var_fokus`" om NA
  # inte normaliseras bort innan har_fokus/plot_df[[x_var_fokus]] används -
  # hittat vid migrering av diagram_arbetsmarknadsstatus_senastear.R, där
  # anropande kod skriver x_var_fokus = ifelse(..., "fokus", NA).
  df <- data.frame(ar = 2018:2022, varde = c(100, 102, 98, 101, 103))
  p <- SkapaStapelDiagram(
    df, "ar", "varde", x_var_fokus = NA,
    output_mapp = tempdir(), filnamn_diagram = "test.png",
    skriv_till_diagramfil = FALSE
  )
  expect_s3_class(p, "ggplot")
})

test_that("SkapaStapelDiagram: stödlinjer blir inte för många när noll tvingas in i ett stort, smalt spann", {
  skip_if_not_installed("ggplot2")
  # Verklighetsnära fall: befolkningstal som ligger tätt ihop men långt från
  # noll (t.ex. 275000-291000) och en stapel som ska börja vid noll. Steget
  # ska räknas ut för hela 0-till-max-spannet, inte bara för datats eget
  # smala spann (annars blir det dussintals/hundratals stödlinjer).
  df <- data.frame(ar = 1968:2024, varde = seq(275618, 291203, length.out = 57))
  p <- SkapaStapelDiagram(
    df, "ar", "varde",
    output_mapp = tempdir(), filnamn_diagram = "test.png",
    stodlinjer_avrunda_fem = TRUE,
    skriv_till_diagramfil = FALSE
  )
  brytpunkter <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y$breaks
  expect_lt(length(brytpunkter[!is.na(brytpunkter)]), 15)
})

test_that("SkapaLinjeDiagram: skickad_x_grupp = NA fungerar som NULL (ingen grupp)", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(ar = 2018:2022, varde = c(100, 102, 98, 101, 103))
  p <- SkapaLinjeDiagram(
    df, "ar", "varde", skickad_x_grupp = NA,
    output_mapp = tempdir(), filnamn_diagram = "test.png",
    skriv_till_diagramfil = FALSE
  )
  expect_s3_class(p, "ggplot")
})

test_that("SkapaLinjeDiagram: x_axis_visa_var_xe_etikett = NA fungerar som NULL (ingen gallring)", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(ar = 2018:2022, varde = c(100, 102, 98, 101, 103))
  p <- SkapaLinjeDiagram(
    df, "ar", "varde", x_axis_visa_var_xe_etikett = NA,
    output_mapp = tempdir(), filnamn_diagram = "test.png",
    skriv_till_diagramfil = FALSE
  )
  expect_s3_class(p, "ggplot")
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
