#' Create a variable & value label dataframe
#'
#' @param df Labelled dataframe, e.g., resulting of the read_sav / read_dta functions  of the package haven.
#' @param var Name of the variable column in the resulting dataframe.
#' @param varlab Name of the variable label column in the resulting dataframe.
#' @param val Name of the value column in the resulting dataframe.
#' @param vallab Name of the value label column in the resulting dataframe.
#' @return Dataframe consisting of 4 columns \code{var}, \code{varlab}, \code{val}  and \code{vallab}, showing all variable & value labels in \code{df}.
#' @export
#' @importFrom assertthat assert_that not_empty is.string
#' @importFrom dplyr full_join
#' @importFrom tibble tibble
#' @examples
#' # load spss data
#' path <- system.file("examples", "iris.sav", package = "haven")
#' df <- haven::read_sav(path)
#' tab_labs(df)
tab_labs <- labs <- function(df, var = "var", varlab = "varlab", val = "val", vallab = "vallab"){#, format.spss = NULL, r_type = NULL) {
  # argument checks
  assert_that(is.data.frame(df))
  not_empty(df)
  is.string(var)
  is.string(varlab)
  is.string(val)
  is.string(vallab)

  # # check if dataframe has labelled variables:
  # if (any(purrr::map_lgl(df, haven::is.labelled))) {
    # function body

  # df %>%
  #   map(attributes) %>%
  #   map(~{.x$vallab <- names(.x$labels); .x}) %>%
  #   map_dfr(~tibble(!!!.x), .id = "var") %>%
  #   select(var, varlab = label, val = labels, vallab, format.spss, r_type = class)

  tibble({{var}} := names(df)) %>%
    full_join(df %>% varl({{var}}, {{varlab}}), by = var) %>%
    full_join(df %>% vall({{var}}, {{val}}, {{vallab}}), by = var)
  # df %>%
  #   purrr::map(attributes) %>%
  #   purrr::map(~{.x$vallab <- .x$labels %>% names; .x}) %>%
  #   purrr::map_dfr(~tibble::tibble(!!!.x), .id = var) %>%
  #   # the use of .data$ ... is explained here:
  #   # https://dplyr.tidyverse.org/articles/programming.html
  #   dplyr::select(var, varlab = .data$label, val = labels, vallab)#, .data$format.spss, r_type = .data$class)


#   }
#   # if no labelled variables exist, return empty tibble:
#   else {
#     message("no variable in data.frame of type haven::labelled")
#     tibble::tibble(var = character(), val = double(), vallab = character())
#   }
}
