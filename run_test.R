#install.packages("shinytest")
#shinytest::installDependencies()
library(testthat)
library(shinytest)

test_that("Application works", {
  expect_pass(testApp(appDir = getwd(), compareImages = F))
})


#recordTest()
#testApp(".", "mytest")
