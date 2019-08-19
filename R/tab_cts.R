#' Tabulate a dataframe's variables' value counts
#'
#' @param df Dataframe
#' @param id Character string of the id variable
#' @param include_ids Logical denoting whether a list column \code{ids} should
#'   be included in the results.
#'   The ids in each list show at which values of
#'   \code{id} the variable \code{var} contains the value \code{val}.
#'
#' @return Dataframe consisting of columns \code{var}, \code{val} & \code{n}
#'   containing a tabulation of the counts of variables' values.
#' @export
#'
#' @examples
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
#'         "widowed" = 8
#'       )
#'     )
#'   )
#' tab_cts(df, "fbnr")
tab_cts <- function(df, id = "id", include_ids = FALSE) {
  res <-
    # df %>%
    # # select(-{{ id }}) %>%
    # mutate_all(as.vector) %>%
    # gather("var", "val", -{{ id }}, factor_key = TRUE) %>%
    longen(df, id) %>%
    group_by(.data$var, .data$nv, .data$cv) %>%
    summarise(n = n(), ids = list(!!rlang::ensym(id))) %>%
    ungroup() %>%
    mutate(var = as.character(.data$var))
  if (include_ids == FALSE) {
    res <-
      res %>% select(-.data$ids)
  }
  res
}
