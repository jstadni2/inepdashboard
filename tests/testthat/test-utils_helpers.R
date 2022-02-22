test_that("percent adds percent variable to data.frame", {
  a <- seq(0, 10, length.out = 10)
  b <- seq(3, 18, length.out = 10)
  
  df_in <- data.frame(a, b)
  
  df_out <- df_in
  df_out$percent <- round(100 * (df_in$a / df_in$b))
  
  expect_equal(df_out, percent(df_in, "percent", "a", "b"))
})
