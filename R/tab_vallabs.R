#' Create a value label dataframe
#'
#' @param df Labelled dataframe, e.g., resulting of the read_sav / read_dta
#'   functions  of the package haven.
#' @return Dataframe consisting of three columns \code{var}, \code{val}  and
#'   \code{vallab}, showing all value labels in \code{df}.
#' @export
#' @importFrom assertthat assert_that not_empty
#' @importFrom purrr map_lgl map_dfr
#' @importFrom tibble enframe
#' @importFrom dplyr select select_if
#' @examples
#' # load spss data
#' path <- system.file("examples", "iris.sav", package = "haven")
#' df <- haven::read_sav(path)
#' tab_vallabs(df)
tab_vallabs <- vall <- function(df) {
  # argument checks
  assert_that(is.data.frame(df))
  not_empty(df)
  # check if dataframe has labelled variables:
  if (any(map_lgl(df, haven::is.labelled))) {
    # function body
    df %>%
      select_if(haven::is.labelled) %>%
      map_dfr(~attr(.x, "labels", exact = TRUE) %>% enframe(name = "vallab", value = "val"), .id = "var") %>%
      # reorder columns
      select(.data$var, .data$val, .data$vallab)
  }
  # if no labelled variables exist, return empty tibble:
  else {
    message("no variable in data.frame of type haven::labelled")
    tibble(var = character(), val = double(), vallab = character())
  }
}
