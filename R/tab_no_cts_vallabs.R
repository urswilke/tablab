#' Compare value labels not occuring in any of a list of dataframes
#'
#' @param df List of dataframes.
#' @param id name of the key variable in the dataframes.
#' @return Dataframe consisting of columns \code{var}, \code{val} and
#'   \code{vallab}, containing a tabulation of the labelled variable values in
#'   the dataframe \code{df} that don't occur in the data.
#' @importFrom stringr str_remove
#' @importFrom dplyr select
#' @importFrom rlang .data
#' @export
#' @examples
#' # load spss data
#' path <- system.file("examples", "iris.sav", package = "haven")
#' df1 <- haven::read_sav(path) %>%
#'   # add id column
#'   tibble::rownames_to_column("id")
#' # Add some value labels to Sepal.Length
#' df1$Sepal.Length <- haven::labelled(df1$Sepal.Length,
#'                                     labels=c(`Sepal.Length = 1` = 1,
#'                                              `Sepal.Length = 3` = 3,
#'                                              `Sepal.Length = 5` = 5))
#'
#' tab_no_cts_vallabs(df1)

tab_no_cts_vallabs <- function(df, id = "id") {
  # HACK: this function is the special case of cmp_no_cts_vallabs(l) for lists
  # only containing one dataframe:
  df_no_cts_vallabs <- list(df) %>% cmp_no_cts_vallabs(id)
  # Therefore, the indices are removed from the column names:
  names(df_no_cts_vallabs) <- names(df_no_cts_vallabs) %>% str_remove("\\d+")
  # The "ex" column is removed (because it's FALSE for all rows, and thus
  # doesn't contain additional information):
  df_no_cts_vallabs %>% select(-.data$ex)
}
