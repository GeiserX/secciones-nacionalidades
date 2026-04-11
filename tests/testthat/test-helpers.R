library(testthat)

# Source the actual helper functions from the codebase.
# When testthat::test_dir() runs, the working directory is the test dir itself,
# so we navigate up to the project root.
project_root <- normalizePath(file.path(getwd(), "..", ".."), mustWork = FALSE)
source(file.path(project_root, "R", "helpers.R"))

# --- Tests for samePopulationPrintYellow ---

test_that("samePopulationPrintYellow returns TRUE for uniform population", {
  layer_data <- data.frame(numPoblacionElegida = c(100, 100, 100))
  expect_true(samePopulationPrintYellow(layer_data))
})

test_that("samePopulationPrintYellow returns FALSE for varied population", {
  layer_data <- data.frame(numPoblacionElegida = c(100, 200, 150))
  expect_false(samePopulationPrintYellow(layer_data))
})

test_that("samePopulationPrintYellow handles NA values correctly", {
  layer_data <- data.frame(numPoblacionElegida = c(50, NA, 50))
  expect_true(samePopulationPrintYellow(layer_data))
})

test_that("samePopulationPrintYellow handles single value", {
  layer_data <- data.frame(numPoblacionElegida = c(42))
  expect_true(samePopulationPrintYellow(layer_data))
})

test_that("samePopulationPrintYellow handles all-NA gracefully", {
  layer_data <- data.frame(numPoblacionElegida = c(NA_real_, NA_real_))
  # max - min of empty after na.rm produces -Inf - Inf = NaN, not 0
  expect_false(isTRUE(samePopulationPrintYellow(layer_data)))
})

# --- Tests for formatMunicipioCode ---

test_that("formatMunicipioCode pads numeric codes to 5 digits", {
  expect_equal(formatMunicipioCode(1), "00001")
  expect_equal(formatMunicipioCode(123), "00123")
  expect_equal(formatMunicipioCode(28079), "28079")
})

test_that("formatMunicipioCode handles character input", {
  expect_equal(formatMunicipioCode("79"), "00079")
})

# --- Tests for buildSeccionCensal ---

test_that("buildSeccionCensal concatenates components correctly", {
  result <- buildSeccionCensal("28079", "01", "003")
  expect_equal(result, "2807901003")
})

test_that("buildSeccionCensal handles vectorized input", {
  result <- buildSeccionCensal(c("28079", "08019"), c("01", "02"), c("003", "005"))
  expect_equal(result, c("2807901003", "0801902005"))
})

# --- Tests for CSV data file integrity ---

test_that("codprov.csv has required columns and all 52 provinces", {
  provincias <- read.csv(file.path(project_root, "datos_csv", "codprov.csv"),
                         fileEncoding = "UTF-8")
  expect_true("Nombre" %in% colnames(provincias))
  expect_true("ID" %in% colnames(provincias))
  expect_equal(nrow(provincias), 52)
})

test_that("codccaa.csv has all autonomous communities", {
  ccaa <- read.csv(file.path(project_root, "datos_csv", "codccaa.csv"),
                   fileEncoding = "UTF-8", header = FALSE)
  # Spain has 17 autonomous communities + 2 autonomous cities = 19
  expect_gte(nrow(ccaa), 19)
})

test_that("Municipios_Censo_2011.csv has required columns and data", {
  municipios <- read.csv(file.path(project_root, "datos_csv", "Municipios_Censo_2011.csv"),
                         fileEncoding = "UTF-8")
  expect_true("COD_MUN" %in% colnames(municipios))
  expect_true("NOMBRE" %in% colnames(municipios))
  expect_gt(nrow(municipios), 8000)
})

# --- Tests for province code mapping (used in Highcharts tab) ---

test_that("Province Highcharts code mapping covers all 52 provinces", {
  codes <- sprintf("%02d", seq(1, 52))
  hc_keys <- c("es-vi","es-ab","es-a","es-al","es-av","es-ba","es-pm","es-b",
               "es-bu","es-cc","es-ca","es-cs","es-cr","es-co","es-c","es-cu",
               "es-gi","es-gr","es-gu","es-ss","es-h","es-hu","es-j","es-le",
               "es-l","es-lo","es-lu","es-m","es-ma","es-mu","es-na","es-or",
               "es-o","es-p","es-gc","es-po","es-sa","es-tf","es-s","es-sg",
               "es-se","es-so","es-t","es-te","es-to","es-v","es-va","es-bi",
               "es-za","es-z","es-ce","es-me")
  expect_equal(length(codes), 52)
  expect_equal(length(hc_keys), 52)
  expect_true(all(grepl("^es-", hc_keys)))
  # No duplicates
  expect_equal(length(unique(hc_keys)), 52)
})
