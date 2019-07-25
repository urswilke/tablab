#' Create a value label dataframe
#'
#' @param df Labelled dataframe, e.g., resulting of the read_sav / read_dta functions  of the package haven.
#' @param var Name of the variable column in the resulting dataframe.
#' @param val Name of the value column in the resulting dataframe.
#' @param vallab Name of the value label column in the resulting dataframe.
#' @return Dataframe consisting of three columns \code{var}, \code{val}  and \code{vallab}, showing all value labels in \code{df}.
#' @export
#' @examples
#' # load spss data
#' path <- system.file("examples", "iris.sav", package = "haven")
#' df <- haven::read_sav(path)
#' tab_vallabs(df)
tab_vallabs <- function(df, var = "var", val = "val", vallab = "vallab") {
  # argument checks
  assertthat::assert_that(is.data.frame(df))
  assertthat::assert_that(is.character(var))
  assertthat::assert_that(is.character(val))
  assertthat::assert_that(is.character(varlab))
  assertthat::assert_that(length(var) == 1)
  assertthat::assert_that(length(val) == 1)
  assertthat::assert_that(length(varlab) == 1)

  # check if dataframe has labelled variables:
  if (any(purrr::map_lgl(df, haven::is.labelled))) {
    # function body
    df %>%
      dplyr::select_if(haven::is.labelled) %>%
      purrr::map_dfr(~attr(.x, "labels", exact = TRUE) %>% tibble::enframe(name = vallab, value = val), .id = var) %>%
      # reorder columns
      dplyr::select(var, val, vallab)
  }
  # if no labelled variables exist, return empty tibble:
  else {
    message("no variable in data.frame of type haven::labelled")
    tibble::tibble(var = character(), val = double(), vallab = character())
  }
}
