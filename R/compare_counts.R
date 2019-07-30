#' Compare counts and labels of a list dataframes
#'
#' @param l List of dataframes.
#' @param id name of the key variable in the dataframes.
#' @return Dataframe consisting of 9 columns \code{var}, \code{val}, \code{vallab1}, \code{vallab2}, \code{df1}, \code{df2} & \code{n}, containing a comparison of the counts of variable values (and their respective value labels) of the two dataframes in long format. \code{vals_differ} & \code{vallabs_differ} are logical columns indicating if all values / value labels are equal.
#' @importFrom dplyr full_join count group_by_at tally rename ungroup select matches mutate_at
#' @importFrom purrr map imap reduce walk set_names map2 map_dfr map2_lgl
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
#'
#' # compare the dataframes counts:
#' compare_counts(list(df1, df2))
#' # compare the dataframes and only show the counts where values have changed:
#' compare_counts(list(df1, df2)) %>% dplyr::filter(val1 != val2)
#'
#' # Create another modified copy
#' df3 <- df2
#' df3[2, "Species"] <- 3
#'
#' # compare the dataframes counts:
#' l <- list(df1, df2, df3)
#' cmp <- compare_counts(l)
#' cmp
#'
#' # compare the dataframes and only show the counts where values have changed:
#' cmp %>% dplyr::filter(vals_differ)
#'

compare_counts <- function(l, id = "id") {
  # argument checks
  assert_that(length(l) >= 2)
  walk(l, ~ assert_that(is.data.frame(.x)))
  walk(l, ~ assert_that(id %in% names(.x), msg = "At least one data.frame doesn't have the specified id."))
  walk(l, ~ assert_that(ncol(.x) >= 2, msg = "At least one data.frame has less than two columns."))
  walk(l, ~ assert_that(length(.x[[id]]) == length(unique(.x[[id]])), msg = "The key in the data.frame is not unique."))

  walk(l, ~ assert_that(not_empty(.x)))

  is.string(id)

  suffixes <- 1:length(l) %>% as.character()
  df_colnames <- paste0("df", suffixes)
  val_cols <- paste0("val", suffixes)

  result_cols <-
    c(df_colnames,
      "var",
      paste0("val", suffixes))

  longed_list <-
    l %>%
    set_names(df_colnames) %>%
    imap(~longen(.x, id = {{ id }}) %>% mutate(!!.y := TRUE))
    # imap(~mutate(.x, !!ensym(.y) := TRUE))
  longed_list <-
    map2(longed_list, val_cols, ~rename(.x, !!.y := "val"))

  df_cmp <-
    longed_list %>%
    reduce(full_join, by = c(id, "var"))
  df_cmp <-
    df_cmp %>%
    # mutate(var = factor(var, levels = unique(var))) %>%
    group_by_at(result_cols) %>%
    tally() %>%
    ungroup()

  # create the columns vals_differ & vallabs_differ:
  df_vals <- select(df_cmp, matches("val\\d+$"))
  same <- function(df) {
    df %>% map_dfr(~map2_lgl(.x, df[[1]], ~all.equal(.x, .y) %>% isTRUE))
  }
  df_cmp %>%
    mutate(
      vals_differ = rowSums(same(df_vals)) != length(l)
      )
  }
