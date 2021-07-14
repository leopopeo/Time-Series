test_that("Plot_timeseries gibt ein ggplot2 Objekt aus", {
  plt <- plot_timeseries(arima.sim(n = 100, list(ar = c(0.8897,-0.4858),ma = c(-0.2279, 0.2488)), sd = sqrt(0.1796)))
  expect_s3_class(plt, c("ggplot"))
})

test_that("Plot_periodogram gibt ein ggplot2 Objekt aus", {
  plt <- plot_periodogram(perio(arima.sim(n = 100, list(ar = c(0.8897,-0.4858),ma = c(-0.2279, 0.2488)), sd = sqrt(0.1796))))
  expect_s3_class(plt, c("ggplot"))
})
