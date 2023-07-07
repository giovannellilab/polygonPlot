library(polygonPlot)
library(ggplot2)

test_that("polygonplot function return a ggplot2 object", {
  df <- read.csv(system.file(file.path("extdata", "example.csv"), 
                             package="polygonPlot"))
  plot <- polygonplot(df, shape = 4)
  expect_true(is.ggplot(plot))
})
