test_that("intern_hitta_pdf_filer hittar pdf-filer, även med å/ä/ö, och ignorerar andra filer", {
  rot <- withr::local_tempdir()
  expect_equal(intern_hitta_pdf_filer(rot, "pdf-rapporter"), character(0))   # mappen saknas

  dir.create(file.path(rot, "pdf-rapporter"))
  file.create(file.path(rot, "pdf-rapporter", c("B_rapport.pdf", "Årsrapport.PDF", "anteckningar.txt", ".dold.pdf")))
  expect_equal(intern_hitta_pdf_filer(rot, "pdf-rapporter"), sort(c("B_rapport.pdf", "Årsrapport.PDF")))
})
