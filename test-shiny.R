suppressPackageStartupMessages({
  library(shinytest)
  library(testthat)
  library(rjson)
})

context("Test Foreign Insight WebApp")

setwd(".")
app <- ShinyDriver$new(".", loadTimeout = 100000)

test_that("Census section is correct", {
  app$setInputs(sidebarCollapsed = FALSE, wait_ = F, values_ = F)
  app$setInputs(selectYear = "2013")
  app$setInputs(selectProvincia = "Albacete")
  app$setInputs(selectMunicipio = "Abengibre (Albacete)")
  app$setInputs(selectNacionalidad = "Total Unión Europea")
  app$setInputs(porcentaje = TRUE)
  app$setInputs(hombreMujer = TRUE)

  censusId <- fromJSON(app$getAllValues()$output$mapa)$x$calls[[2]]$args[[2]]
  expect_equal(censusId, "0200101001")  
})

test_that("Province graph is correct", {
  app$setInputs(tabs = "tab2")
  app$setInputs(selectProvincia2 = "Albacete")
  app$setInputs(selectNacionalidad2 = "Total Unión Europea")
  app$setInputs(sort = TRUE)
  app$setInputs(manWoman = TRUE)
  app$setInputs(percentage2 = TRUE)
  
  peakPopulation <- fromJSON(app$getAllValues()$output$chart)$x$hc_opts$series[[1]]$data[1]
  expect_equal(peakPopulation, 2807)  
})

test_that("National map is correct", {
  app$setInputs(tabs = "tab3")
  app$setInputs(selectNacionalidad3 = "Total Unión Europea")
  
  populationSpain <- fromJSON(app$getAllValues()$output$spainmap)$x$hc_opts$series[[1]]$data[[2]]$value
  expect_equal(populationSpain, 11873)  
})