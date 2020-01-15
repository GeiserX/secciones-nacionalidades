#install.packages("shinytest")
#shinytest::installDependencies()
library(testthat)
library(shinytest)

test_that("Application works", {
  expect_pass(testApp(".", compareImages = T))
})

#recordTest(".")
#testApp(".", "mytest")
