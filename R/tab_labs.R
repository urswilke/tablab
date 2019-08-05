#' Create a variable & value label dataframe
#'
#' @param df Labelled dataframe, e.g., resulting of the read_sav / read_dta functions  of the package haven.
#' @return Dataframe consisting of 4 columns \code{var}, \code{varlab}, \code{val}  and \code{vallab}, showing all variable & value labels in \code{df}.
#' @export
#' @importFrom assertthat assert_that not_empty is.string
#' @importFrom dplyr full_join
#' @importFrom tibble tibble
#' @importFrom rlang :=
#' @examples
#' # load spss data
#' path <- system.file("examples", "iris.sav", package = "haven")
#' df <- haven::read_sav(path)
#' tab_labs(df)
tab_labs <- labs <- function(df, rm_non_labelled = FALSE){
  # argument checks
  assert_that(is.data.frame(df))
  not_empty(df)

  result <-
    tibble(var = names(df)) %>%
    full_join(df %>% varl(), by = "var") %>%
    full_join(df %>% vall(), by = "var")
  if (rm_non_labelled == TRUE){
    result <-
      result %>% filter(!(is.na(varlab) & is.na(val)))
  }
  result
}
