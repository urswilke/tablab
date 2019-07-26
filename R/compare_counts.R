#' Compare counts of two dataframes
#'
#' @param l List of dataframes.
#' @param id name of the key variable in the dataframes.
#' @param var Name of the variable column in the resulting dataframe.
#' @param val Name of the value column in the resulting dataframe.
#' @param vallab Name of the value label column in the resulting dataframe.
#' @return Dataframe consisting of 7 columns \code{var}, \code{val}, \code{vallab1}, \code{vallab2}, \code{df1}, \code{df2} & \code{n}, containing a comparison of the counts of variable values (and their respective value labels) of the two dataframes in long format.
#' @importFrom dplyr full_join count group_by_at tally rename ungroup
#' @importFrom purrr map imap reduce walk set_names map2
#' @importFrom assertthat assert_that not_empty is.string
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
#'                                labels = c(setosa_mod = 1, versicolor = 2, virginica = 3))
#'
#' # compare the dataframes counts:
#' compare_counts(list(df1, df2))
#' # compare the dataframes and only show the counts where values have changed:
#' compare_counts(list(df1, df2)) %>% dplyr::filter(val1 != val2)
#' # compare the dataframes and only show the counts where value labels have changed:
#' compare_counts(list(df1, df2)) %>% dplyr::filter(vallab1 != vallab2)
#'
#' # Create another modified copy
#' df3 <- df2
#' df3[2, "Species"] <- 3
#' # modify the value label of "versicolor"
#' df2$Species <- haven::labelled(df2$Species,
#'                                labels = c(setosa = 1, versicolor_mod = 2, virginica = 3))
#'
#' # compare the dataframes counts:
#' l <- list(df1, df2, df3)
#' cmp <- compare_counts(l)
#' cmp
#'
#' # compare the dataframes and only show the counts where values have changed:
#' # (HACK to calculate logical where values differ):
#' library(dplyr)
#' vals_differ <-
#'   cmp %>%
#'   select(matches("val\\d+$")) %>%
#'   {. == cmp$val1} %>%
#'   {rowSums(.) != length(l)}
#' cmp %>% filter(vals_differ)
#'
#' # To show where either values or value labels differ:
#' vallabs_differ <-
#'   cmp %>%
#'   select(matches("vallab\\d+$")) %>%
#'   {. == cmp$val1} %>%
#'   {rowSums(.) != length(l)}
#' cmp %>% filter(vals_differ | vallabs_differ)

compare_counts <- function(l, id = "id", var = "var", val = "val", vallab = "vallab") {
  # argument checks
  assert_that(length(l) >= 2)
  walk(l, ~ assert_that(is.data.frame(.x)))
  # assert_that(is.data.frame(df1))
  # assert_that(is.data.frame(df2))
  walk(l, ~ assert_that(not_empty(.x)))
  # not_empty(df1)
  # not_empty(df2)
  is.string(id)
  is.string(var)
  is.string(val)

  suffixes <- 1:length(l) %>% as.character()
  df_colnames <- paste0("df", suffixes)
  result_cols <-
    c(df_colnames,
      var,
      paste0(val, suffixes),
      paste0(vallab, suffixes))

  longed_list <-
    l %>%
    set_names(df_colnames) %>%
    map(~longen(.x) %>% full_join(vall(.x), by = c(var, val))) %>%
    imap(~mutate(.x, !!.y := .y))
  longed_list <-
    map2(longed_list, suffixes, ~rename(.x, !!paste0(val, .y) := val))

  map2(longed_list, suffixes, ~rename(.x, !!paste0(vallab, .y) := vallab)) %>%
    reduce(full_join, by = c(id, var)) %>%
    mutate(var = factor(var, levels = unique(var))) %>%
    group_by_at(result_cols) %>%
    tally() %>%
    ungroup()
}
