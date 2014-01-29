context("nPlot instantiation")

hairEye <- data.frame(HairEyeColor)

test_that("nPlot creates expected structure", {
  expect_that(nPlot(), throws_error())
  expect_that(
    nPlot(
      Freq ~ Hair,
      group = "Sex",
      data = subset(hairEye, Eye=="Brown"),
      type = "multiBarChart"
    ),
    is_a("Nvd3")
  )
  expect_that(
    nPlot(
      Freq ~ Hair,
      group = "Sex",
      data = subset(hairEye, Eye=="Brown"),
      type = "multiBarChart"
    )$params,
    is_a("list")
  )
  expect_identical(
    nPlot(
      Freq ~ Hair,
      group = "Sex",
      data = subset(hairEye, Eye=="Brown"),
      type = "multiBarChart"
    )$params$x,
    "Hair"
  )
})