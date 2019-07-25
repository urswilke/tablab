#' Create a variable label dataframe
#'
#' @param df Labelled dataframe, e.g., resulting of the read_sav / read_dta functions  of the package haven.
#' @param var Name of the variable column in the resulting dataframe.
#' @param varlab Name of the variable label column in the resulting dataframe.
#' @return Dataframe consisting of two columns \code{var} and \code{varlab}, showing all variable labels in \code{df}.
#' @export
#' @examples
#' # load spss data
#' path <- system.file("examples", "iris.sav", package = "haven")
#' df <- haven::read_sav(path)
#' attr(df$Sepal.Width, "label") <- "width of the sepal"
#' tab_varlabs(df)
tab_varlabs <- varl <- function(df, var = "var", varlab = "varlab") {
  # argument checks
  assertthat::assert_that(is.data.frame(df))
  assertthat::not_empty(df)
  assertthat::is.string(var)
  assertthat::is.string(varlab)

  # check if dataframe has labelled variables:
  if (any(purrr::map_lgl(df, ~assertthat::has_attr(.x, "label")))) {
    df %>%
      purrr::map_dfr(~attr(.x, "label", exact = TRUE) %>% tibble::enframe(name = NULL, value = varlab), .id = var)
  }
  else {
    message("No variable in the data.frame has a variable label")
    tibble::tibble(var = character(), varlab = character())
  }

}
