#' Compare counts of a list dataframes
#'
#' @param l List of dataframes.
#' @param id name of the key variable in the dataframes.
#' @param include_ids Logical denoting whether a list column of the ids should be included in the results. The ids in the list show at which values of \code{id} the variable \code{var} contains the value \code{val1}.
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
#' cmp_cts(list(df1, df2))
#' # compare the dataframes and only show the counts where values have changed:
#' cmp_cts(list(df1, df2)) %>% dplyr::filter(val1 != val2)
#'
#' # Create another modified copy
#' df3 <- df2
#' df3[2, "Species"] <- 3
#'
#' # compare the dataframes counts:
#' l <- list(df1, df2, df3)
#' cmp <- cmp_cts(l)
#' cmp
#'
#' # compare the dataframes and only show the counts where values have changed:
#' cmp %>% dplyr::filter(!(val1 == val2 & val2 == val3))
#'

cmp_cts <- function(l, id = "id", include_ids = FALSE) {
  # argument checks
  assert_that(length(l) >= 2)
  walk(l, ~ assert_that(is.data.frame(.x)))
  walk(l, ~ assert_that(id %in% names(.x), msg = "At least one data.frame doesn't have the specified id."))
  walk(l, ~ assert_that(ncol(.x) >= 2, msg = "At least one data.frame has less than two columns."))
  walk(l, ~ assert_that(length(.x[[id]]) == length(unique(.x[[id]])), msg = "The key in the data.frame is not unique."))

  walk(l, ~ assert_that(not_empty(.x)))

  is.string(id)


  df_cts <-
    list_longed_ex(l, id) %>%
    # imap(~rename_at(.x, vars(c("val")), ~paste0(., !!.y))) %>%
    add_list_suffix(c("val")) %>%
    reduce(full_join, by = c(id, "var")) %>%
    mutate(var = factor(.data$var, levels = unique(.data$var))) %>%
    group_by_at(vars("var", matches("\\d+$"))) %>%
    summarise(n = n(), ids = list(!!ensym(id))) %>%
    ungroup()
  if (include_ids == FALSE){
    df_cts <- df_cts %>% select(-.data$ids)
  }
  df_cts

}


list_longed_ex <- function(l, id) {
  l %>% imap(~longen(.x, id = {{ id }}) %>% mutate(!!paste0("ex", .y) := TRUE))
}
