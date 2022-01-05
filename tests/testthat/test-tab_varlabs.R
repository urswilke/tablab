x <- haven::labelled(1:3, label = "varlab", labels = c("1lab" =1 , "2lab" = 2, "3lab" = 3))
y <- haven::labelled(LETTERS[1:3], label = "char_var_lab", labels = c("1lab" ="A" , "2lab" = "B", "3lab" = "C"))

dftest <- data.frame(id = 1:3, x, y)
vall <- tab_varlabs(dftest)
varl <- tab_varlabs(dftest)
allstats <- tab_all(dftest, id = "id")
test_that("result of tab_vallabs is a data.frame", {
  expect_true(is.data.frame(vall))
})

test_that("variable label table print is reproduced", {
  testthat::expect_snapshot_output({
    varl
  })
})
test_that("value label table print is reproduced", {
  testthat::expect_snapshot_output({
    vall
  })
})
test_that("all attributes table print is reproduced", {
  testthat::expect_snapshot_output({
    allstats
  })
})
