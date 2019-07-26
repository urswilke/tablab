#' Create a variable label dataframe
#'
#' @param df Labelled dataframe, e.g., resulting of the read_sav / read_dta functions  of the package haven.
#' @param var Name of the variable column in the resulting dataframe.
#' @param varlab Name of the variable label column in the resulting dataframe.
#' @return Dataframe consisting of two columns \code{var} and \code{varlab}, showing all variable labels in \code{df}.
#' @export
#' @importFrom purrr map_lgl map_dfr
#' @importFrom tibble tibble enframe
#' @importFrom assertthat assert_that not_empty is.string has_attr
#' @examples
#' # load spss data
#' path <- system.file("examples", "iris.sav", package = "haven")
#' df <- haven::read_sav(path)
#' attr(df$Sepal.Width, "label") <- "width of the sepal"
#' tab_varlabs(df)
tab_varlabs <- varl <- function(df, var = "var", varlab = "varlab") {
  # argument checks
  assert_that(is.data.frame(df))
  not_empty(df)
  is.string(var)
  is.string(varlab)

  # check if dataframe has labelled variables:
  if (any(map_lgl(df, ~has_attr(.x, "label")))) {
    df %>%
      map_dfr(~attr(.x, "label", exact = TRUE) %>% enframe(name = NULL, value = varlab), .id = var)
  }
  else {
    message("No variable in the data.frame has a variable label")
    tibble(var = character(), varlab = character())
  }

}
