#' make long format of dataframe
#'
#' @param df Dataframe with a key variable (\code{id}).
#' @param id name of the key variable in the dataframe.
#' @return Dataframe consisting of 3 columns \code{id}, \code{var} & \code{val}, containing the dataframe in long format (based on dplyr::gather).
#' @importFrom tidyr gather
#' @importFrom dplyr mutate mutate_all arrange
#' @importFrom assertthat assert_that not_empty is.string
#' @importFrom rlang := ensym .data
#' @export
#' @examples
#' # load spss data
#' path <- system.file("examples", "iris.sav", package = "haven")
#' df <- haven::read_sav(path) %>%
#'   # add id column
#'   tibble::rownames_to_column("id")
#' df %>% longen()
longen <- function(df, id = "id") {
  # argument checks
  assert_that(is.data.frame(df))
  not_empty(df)
  is.string(id)

  # select the id variable and all numeric variables:
  df <- df %>% select({{ id }}, which(sapply(df, class) != "character"))
  df %>%
    # remove attributes to prevent warning when gathering
    mutate_all(as.vector) %>%
    gather("var", "val", -{{ id }}) %>%
    mutate(val = as.numeric(.data$val))
}
