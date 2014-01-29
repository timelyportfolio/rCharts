context("nPlot instantiation")

hairEye <- as.data.frame(HairEyeColor)

test_that("nPlot creates expected structure", {
  expect_that(nPlot(), throws_error())
  expect_that(
    n1 <- nPlot(
      Freq ~ Hair,
      group = "Sex",
      data = subset(hairEye, Eye=="Brown"),
      type = "multiBarChart"
    ),
    is_a("Nvd3")
  )
  expect_that(
    n1$params,
    is_a("list")
  )
  expect_identical(
    n1$params$x,
    "Hair"
  )
  expect_identical(
    n1$params$y,
    "Freq"
  )
})