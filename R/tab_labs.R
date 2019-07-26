#' Create a variable & value label dataframe
#'
#' @param df Labelled dataframe, e.g., resulting of the read_sav / read_dta functions  of the package haven.
#' @return Dataframe consisting of 4 columns var, varlab, val  and vallab, showing all variable & value labels in \code{df}.
#' @export
#' @importFrom assertthat assert_that not_empty is.string
#' @importFrom dplyr full_join
#' @importFrom tibble tibble
#' @examples
#' # load spss data
#' path <- system.file("examples", "iris.sav", package = "haven")
#' df <- haven::read_sav(path)
#' tab_labs(df)
tab_labs <- labs <- function(df){
  # argument checks
  assert_that(is.data.frame(df))
  not_empty(df)


  tibble(var = names(df)) %>%
    full_join(df %>% varl(), by = "var") %>%
    full_join(df %>% vall(), by = "var")
}
