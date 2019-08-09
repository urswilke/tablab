#' Compare value labels not occuring in any of a list of dataframes
#'
#' @param l List of dataframes.
#' @param id name of the key variable in the dataframes.
#' @return Dataframe consisting of columns \code{var}, \code{val1}, \code{val2}, ..., \code{ex1}, \code{ex2}, ...,  \code{vallab1}, \code{vallab2}, ..., containing a comparison of the labelled variable values in the dataframes that don't occur in the data.
#' @importFrom dplyr left_join full_join anti_join  select
#' @importFrom purrr map map2 reduce
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
#' # Add some value labels to Sepal.Length in both dataframes:
#' df1$Sepal.Length <- haven::labelled(df1$Sepal.Length,
#'                                     labels=c(`Sepal.Length = 1` = 1,
#'                                              `Sepal.Length = 6` = 6,
#'                                              `Sepal.Length = 9` = 9))
#' # Add some value labels to Sepal.Length
#' df2$Sepal.Length <- haven::labelled(df1$Sepal.Length,
#'                                     labels=c(`Sepal.Length = 1` = 1,
#'                                              `Sepal.Length = 3` = 3,
#'                                              `Sepal.Length = 5` = 5))
#'
#' # compare the dataframes counts:
#' cmp_no_cts_vallabs(list(df1, df2))

cmp_no_cts_vallabs <- function(l, id = "id") {
  # This dataframe is similar to the final result, except that there is a uniform vallab column. Hence, when joining all vallab tables together afterwards, equal vallabs appear in the same rows.
  df_non_ex_vallabs_uniform <-
    map2(l %>% map(tab_vallabs),
         list_longed_ex(l, id) %>%
           map(~distinct(.x, .data$var, .data$val)),
         ~anti_join(.x, .y) %>% mutate(ex = FALSE)) %>%
    add_list_suffix(c("val", "ex")) %>%
    reduce(full_join)

  df_non_ex_vallabs <-
    # df_non_ex_vallabs %>%
    reduce(l %>% map(tab_vallabs) %>% add_list_suffix(c("val", "vallab")),
           left_join,
           .init = df_non_ex_vallabs_uniform) %>%
    select(-.data$vallab)
  df_non_ex_vallabs
}
