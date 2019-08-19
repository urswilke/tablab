#' Remove attributes from all variables of a dataframe
#'
#' @param df A dataframe
#'
#' @return A copy of \code{df} with all attributes removed.
#' @importFrom dplyr mutate_if mutate_all
#' @export
#'
#' @examples
#' path <- system.file("examples", "iris.sav", package = "haven")
#' df <- haven::read_sav(path)
#' df %>% unattr()

unattr <- function(df) {
  df <- df %>%
    mutate_if(is.factor, as_labelled) %>%
    mutate_if(is.integer, as.numeric) %>%
    mutate_all(as.vector)
  stopifnot(unique(map_chr(df, class)) %in% c("character", "numeric"))
  df
}


#' Convert a factor to a haven::labelled variable
#'
#' @param x Factor.
#'
#' @return Returns a labelled variable with values and labels according
#'   to the levels in the factor. This should roundtrip with forcats::as_factor.
#'   Additionally, a variable label can be set.
#' @importFrom forcats as_factor
#' @export
#'
#' @examples
#' x <- factor(c("b", "b", "a"))
#' attr(x, "label") <- "variable label"
#' x
#' as_labelled(x)
#' all.equal(x,
#'           x %>%
#'             as_labelled() %>%
#'             forcats::as_factor())
as_labelled <- function(x) {
  labs <- set_names(sort(unique(as.numeric(x))),
                    levels(x))

  haven::labelled(as.numeric(x), labels = labs, label = attr(x, "label"))
  # map(c(vall=levels, val=unclass), ~.x(x)) %>% as_tibble() %>% distinct() %>% deframe()
}
