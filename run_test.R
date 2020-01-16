#install.packages("shinytest")
#shinytest::installDependencies()
library(testthat)
library(shinytest)

test_that("Application works", {
  expect_pass(testApp(appDir = getwd(), compareImages = F))
})

if (requireNamespace("lintr", quietly = TRUE)) {
  context("lints")
  test_that("Package Style", {
    lintr::expect_lint_free(".")
  })
}


#recordTest()
#testApp(".", "mytest")
