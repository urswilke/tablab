#' Compare counts and labels of a list dataframes
#'
#' @param l List of dataframes.
#' @param id name of the key variable in the dataframes.
#' @return Dataframe consisting of 9 columns \code{var}, \code{val}, \code{vallab1}, \code{vallab2}, \code{df1}, \code{df2} & \code{n}, containing a comparison of the counts of variable values (and their respective value labels) of the two dataframes in long format. \code{vals_differ} & \code{vallabs_differ} are logical columns indicating if all values / value labels are equal.
#' @importFrom dplyr full_join count group_by_at tally rename ungroup select matches mutate_at arrange_at group_by row_number
#' @importFrom purrr map imap reduce walk set_names map2 map_dfr map2_lgl
#' @importFrom assertthat assert_that not_empty is.string
#' @importFrom rlang .data
#' @export
#' @examples
#' # load spss data
#' path <- system.file("examples", "iris.sav", package = "haven")
#' df1 <- haven::read_sav(path) %>%
#'   # add id column
#'   tibble::rownames_to_column("id")
#'
#' # create a modified copy:
#' df2 <- df1
#' df2[1, "Species"] <- 2
#' # modify the value label of "setosa"
#' df2$Species <- haven::labelled(df2$Species,
#'                                labels = c(setosa_mod = 1, versicolor = 2, virginica = 3),
#'                                label = "Species")
#'
#' # compare the dataframes counts:
#' cmp_cts_n_labs(list(df1, df2))
#' # compare the dataframes and only show the counts where values have changed:
#' cmp_cts_n_labs(list(df1, df2)) %>% dplyr::filter(val1 != val2)
#' # compare the dataframes and only show the counts where value labels have changed:
#' cmp_cts_n_labs(list(df1, df2)) %>% dplyr::filter(vallab1 != vallab2)
#'
#' # Create another modified copy
#' df3 <- df2
#' df3[2, "Species"] <- 3
#' # modify the value label of "versicolor"
#' df2$Species <- haven::labelled(df2$Species,
#'                                labels = c(setosa = 1, versicolor_mod = 2, virginica = 3),
#'                                label = "Species_mod")
#'
#' # compare the dataframes counts:
#' l <- list(df1, df2, df3)
#' cmp <- cmp_cts_n_labs(l)
#' cmp
#'
#' # compare the dataframes and only show the counts where values have changed:
#' cmp %>% dplyr::filter(vals_differ)
#'
#' # To show where either values or value labels differ:
#' cmp %>% dplyr::filter(vals_differ | vallabs_differ)

cmp_cts_n_labs <- function(l, id = "id"){#, var = "var", val = "val", vallab = "vallab") {
  # argument checks
  assert_that(length(l) >= 2)
  walk(l, ~ assert_that(is.data.frame(.x)))
  walk(l, ~ assert_that(id %in% names(.x), msg = "At least one data.frame doesn't have the specified id."))
  walk(l, ~ assert_that(ncol(.x) >= 2, msg = "At least one data.frame has less than two columns."))
  walk(l, ~ assert_that(length(.x[[id]]) == length(unique(.x[[id]])), msg = "The key in the data.frame is not unique."))
  # assert_that(is.data.frame(df1))
  # assert_that(is.data.frame(df2))
  walk(l, ~ assert_that(not_empty(.x)))
  # not_empty(df1)
  # not_empty(df2)
  is.string(id)

  suffixes <- 1:length(l) %>% as.character()
  df_cols <- paste0("df", suffixes)
  val_cols <- paste0("val", suffixes)
  varlab_cols <- paste0("varlab", suffixes)
  vallab_cols <- paste0("vallab", suffixes)

  l_vallabs <-
    map2(l %>% map(tab_vallabs),
         suffixes,
         ~{names(.x)[2:3] <- paste0(names(.x)[2:3], .y); .x})
  l_varlabs <-
    map2(l %>% map(tab_varlabs),
         suffixes,
         ~{names(.x)[2] <- paste0(names(.x)[2], .y); .x})

  df_cnt <- compare_counts(l, {{id}})
  df_cmp <-
    reduce(l_vallabs, ~full_join(.x, .y, by = names(.y)[-3]), .init = df_cnt)
  df_cmp <-
    reduce(l_varlabs, ~full_join(.x, .y, by = "var"), .init = df_cmp)

  # # create the logical columns indicating differences:
  df_vallabs <- df_cmp[vallab_cols]
  df_varlabs <- df_cmp[varlab_cols]
  df_exist <- df_cmp[df_cols]
  same <- function(df) {
    df %>%
      map_dfr(~map2_lgl(.x, df[[1]], ~all.equal(.x, .y) %>% isTRUE))
  }
  df_cmp %>%
    mutate(
      not_in_all = rowSums(same(df_exist)) != length(l),
      varlabs_differ = rowSums(same(df_varlabs)) != length(l),
      vallabs_differ = rowSums(same(df_vallabs)) != length(l)
      ) %>%
    mutate(var = factor(.data$var, levels = unique(c(names(l[[1]]), .data$var)))) %>%
    group_by(.data$var) %>%
    # set the logical to false for each entry that's not the first of the grouping:
    mutate(varlabs_differ = ifelse(row_number() == 1, .data$varlabs_differ, FALSE)) %>%
    # the ifelse function puts NAs first (in order to make the variable labels
    # appear first):
    arrange_at(val_cols, ~ifelse(is.na(.), is.na(.), .), .by_group = TRUE) %>%
    ungroup()
  }

