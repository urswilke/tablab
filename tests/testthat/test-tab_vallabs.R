test_that("result of tab_vallabs is a data.frame", {
  dftest <- data.frame(x = NA)
  expect_true(is.data.frame(tab_vallabs(dftest)))
})
