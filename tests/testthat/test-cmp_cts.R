test_that("results are the same, regardless of the name of the id variable", {
  # load spss data
  path <- system.file("examples", "iris.sav", package = "haven")
  df1 <- haven::read_sav(path) %>%
    # add id column
    tibble::rownames_to_column("id")

  # create a modified copy:
  df2 <- df1
  df2[1, "Species"] <- 2
  # modify the value label of "setosa"
  df2$Species <- haven::labelled(df2$Species,
                                 labels = c(setosa_mod = 1, versicolor = 2, virginica = 3))
  cmp1 <- cmp_cts(list(df1, df2))
  cmp2 <- cmp_cts(list(df1 %>% dplyr::rename(g = id), df2 %>% dplyr::rename(g = id)), id = "g")
  expect_equal(cmp1, cmp2)

})
