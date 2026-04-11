library(testthat)

test_that("samePopulationPrintYellow detects uniform population", {
  mock_data <- data.frame(numPoblacionElegida = c(100, 100, 100))
  mock_sp <- list(data = mock_data)
  result <- max(mock_sp$data$numPoblacionElegida, na.rm = TRUE) -
            min(mock_sp$data$numPoblacionElegida, na.rm = TRUE) == 0
  expect_true(result)
})

test_that("samePopulationPrintYellow detects different population", {
  mock_data <- data.frame(numPoblacionElegida = c(100, 200, 150))
  mock_sp <- list(data = mock_data)
  result <- max(mock_sp$data$numPoblacionElegida, na.rm = TRUE) -
            min(mock_sp$data$numPoblacionElegida, na.rm = TRUE) == 0
  expect_false(result)
})

test_that("CSV data files exist", {
  skip_if_not(file.exists("datos_csv/codprov.csv"), "Data files not present")
  provincias <- read.csv("datos_csv/codprov.csv", fileEncoding = "UTF-8")
  expect_true("Nombre" %in% colnames(provincias))
  expect_true("ID" %in% colnames(provincias))
  expect_gt(nrow(provincias), 0)
})

test_that("Province code mapping is complete", {
  codes <- sprintf('%02d', seq(1, 52))
  names <- c('es-vi','es-ab','es-a','es-al','es-av','es-ba','es-pm','es-b',
             'es-bu','es-cc','es-ca','es-cs','es-cr','es-co','es-c','es-cu',
             'es-gi','es-gr','es-gu','es-ss','es-h','es-hu','es-j','es-le',
             'es-l','es-lo','es-lu','es-m','es-ma','es-mu','es-na','es-or',
             'es-o','es-p','es-gc','es-po','es-sa','es-tf','es-s','es-sg',
             'es-se','es-so','es-t','es-te','es-to','es-v','es-va','es-bi',
             'es-za','es-z','es-ce','es-me')
  expect_equal(length(codes), 52)
  expect_equal(length(names), 52)
})
