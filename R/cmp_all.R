#' Compare counts and labels of a list dataframes
#'
#' @param l List of dataframes.
#' @param id name of the key variable in the dataframes.
#' @param include_ids Logical denoting whether a list column of the ids should be included in the results. The ids in the list show at which values of \code{id} the variable \code{var} contains the value \code{val1}.
#' @return Dataframe consisting of columns \code{var}, \code{val1}, \code{val2}, ...,  \code{vallab1}, \code{vallab2}, ...,  \code{df1}, \code{df2}, ..., and \code{n}, containing a comparison of the counts of variable values (and their respective value labels) of the dataframes in long format. The indices denote the dataframes in \code{l}. \code{vals_diff} & \code{vallabs_diff} are logical columns indicating if all values, variable / value labels are equal.
#' @importFrom dplyr full_join group_by_at rename_at ungroup select matches mutate_at arrange_at group_by row_number vars summarise n n_distinct
#' @importFrom purrr map imap reduce walk set_names map2 map_dfr map2_lgl map_int
#' @importFrom assertthat assert_that not_empty is.string
#' @importFrom rlang .data
#' @importFrom tidyr fill
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
#' cmp_all(list(df1, df2))
#' # compare the dataframes and only show the counts where values have changed:
#' cmp_all(list(df1, df2)) %>% dplyr::filter(val1 != val2)
#' # compare the dataframes and only show the counts where value labels have changed:
#' cmp_all(list(df1, df2)) %>% dplyr::filter(vallab1 != vallab2)
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
#' cmp <- cmp_all(l)
#' cmp
#'
#' # compare the dataframes and only show the counts where values have changed:
#' cmp %>% dplyr::filter(val_diff)
#'
#' # To show where either values or value labels differ:
#' cmp %>% dplyr::filter(val_diff | vallab_diff)

cmp_all <- function(l, id = "id", include_ids = FALSE){
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


  df_cmp <-
    map2(l %>% imap(~longen(.x, id = {{ id }}) %>% mutate(!!paste0("ex", .y) := TRUE)),
         l %>% map( ~ tab_labs(.x, rm_non_labelled = TRUE)),
         ~ full_join(.x, .y, by = c("var", "val"))) %>%
    imap(~rename_at(.x, vars(c("varlab", "val", "vallab")), ~paste0(., !!.y))) %>%
    reduce(full_join, by = c(id, "var")) %>%
    mutate(var = factor(.data$var, levels = unique(.data$var))) %>%
    group_by_at(vars("var", matches("\\d+$"))) %>%
    summarise(n = n(), ids = list(!!ensym(id))) %>%
    group_by(.data$var) %>%
    fill(matches("^varlab\\d+$")) %>%
    ungroup()

  if (include_ids == FALSE){
    df_cmp <- df_cmp %>% select(-.data$ids)
  }

  # create the logical columns indicating differences:
  col_specs <- c("ex", "varlab", "val", "vallab")
  # The reduce command creates logical columns in the dataframe for each element
  # in col_spec, indicating if there are differences in the rows in df_cmp,
  # e.g., for the element "val" in col_specs, it creates:
  # df_cmp %>% mutate(val_diff = select(df_cmp, matches("^val\\d+$")) %>% t %>% as.data.frame() %>% map_int(n_distinct) > 1)
  df_cmp <-
    reduce(col_specs, cols_differ, .init = df_cmp)

  df_cmp %>%
    group_by(.data$var) %>%
    # the ifelse function puts NAs first (in order to make the variable labels
    # appear first):
    arrange_at(vars(matches("^val\\d+$")), ~ifelse(is.na(.), is.na(.), .), .by_group = TRUE) %>%
    # set the logical to false for each entry that's not the first of the grouping:
    mutate(varlab_diff = ifelse(row_number() == 1, .data$varlab_diff, FALSE)) %>%
    ungroup() %>%
    mutate(any_diff = .data$ex_diff | .data$val_diff | .data$varlab_diff | .data$vallab_diff)
}

cols_differ <- function(df_cmp, col_spec) {
  match_str <- paste0("^", col_spec, "\\d+$")
  col_name <- paste0(col_spec, "_diff")
  diff_lgl <- df_cmp %>% select(matches(match_str)) %>% is_diff_in_cols()
  df_cmp %>% mutate(!!col_name := diff_lgl)
}

is_diff_in_cols <- function(df) {
  df %>% t %>% as.data.frame() %>% map_int(n_distinct) > 1
}
