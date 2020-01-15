#install.packages("shinytest")
#shinytest::installDependencies()
library(testthat)
library(shinytest)

print(getwd())

test_that("Application works", {
  expect_pass(testApp(paste0(getwd(), "/tests/mytest.R"), compareImages = T))
})

#recordTest(".")
#testApp(".", "mytest")
