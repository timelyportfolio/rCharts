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
  expect_identical(
    n1$params$data,
    subset(hairEye,Eye=="Brown")
  )
})

context("nPlot makes things as expected")

test_that("nPlot output seems correct",{
  expect_that(
    n1$getPayload(chartId = n1$params$id),
    is_a("list")
  )
  expect_that(
    n1$html(),
    is_a("character")
  )
  #maybe use XML::htmlParse to test some things
  expect_that(
    doc <- XML::htmlParse(n1$html()),
    is_a("HTMLInternalDocument")
  )
  expect_that(
    XML::xpathSApply(doc,"//script",xmlValue),
    is_a("character")  # do we test for values with regex
  )
  #somehow test examples.R
  #placeholder since this will run all the examples
  #to visually inspect results
  expect_that(
    source("inst/libraries/nvd3/examples.R"),
    is_a("list")
  )
})