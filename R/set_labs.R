#' Add a variable label and/or value labels (or none) to a variable.
#'
#' @param x A vector.
#' @param vallabs Named vector of value labels.
#' @param varlab A string containing the variable label.
#'
#' @return The function returns the variable \code{x} with variable label and
#'   value labels. If \code{vallabs} is NULL, the function returns \code{x} with
#'   a variable label stored as the attribute "label". If \code{vallabs} is not
#'   NULL, the function returns \code{x} of type haven::labelled with with the
#'   value labels stored in the attribute "labels" (For more information type
#'   ?haven::labelled in the console).
#' @export
#'
#' @examples
#' # Create toy dataframe:
#' df <-
#'   tibble::tibble(
#'     fbnr = 1:10,
#'     sex = haven::labelled(c(2, 1, 2, 1, 1, 2, 2, 1, 2, 1),
#'                           label = "sex",
#'                           labels = c(MALES = 1, FEMALES = 2)),
#'     age = c(24, 23, 23, 41, 23, 39, 30, 18, 31, 48),
#'     marital = haven::labelled(
#'       c(1, 7, 2, 6, 4, 5, 3, 8, 4, 2),
#'       label = "marital status",
#'       labels = c(
#'         "single" = 1,
#'         "steady relationship" = 2,
#'         "living with partner" = 3,
#'         "married first time" = 4,
#'         "remarried" = 5,
#'         "separated" = 6,
#'         "divorced" = 7,
#'         "non-occuring label" = 9
#'       )
#'     )
#'   )
#' df
#' # store all labels in df_attr:
#' df_attrs <- df %>% tab_attrs()
#' df_attrs
#' # Remove all attributes from df:
#' df <- df %>% dplyr::mutate_all(as.vector)
#' df
#' # Write back label information from df_attrs
#' df <-
#'   df %>%
#'   purrr::map2_dfc(.,
#'                   df_attrs %>% purrr::transpose(),
#'                   ~{set_labs(.x, .y$labels, .y$varlab)})
#' df
#' # Constructed in this way, the dataframe including the label information could be
#' # exported to SPSS or Stata via haven:
#' # haven::write_sav(df, "test.sav")
#' # haven::write_dta(df, "test.dta")
set_labs <- function(x, vallabs, varlab) {
  if (is.null(vallabs) & is.null(varlab)) {
    # print(x)
    res <- x
  }
  else if (!is.null(vallabs)) {
    # print(x)
    res <- haven::labelled(x, labels = vallabs, label = varlab)
  }
  else if (is.null(vallabs)) {
    # print(x)
    res <- x
    attr(res, "label") <- varlab
  }
  res
}
