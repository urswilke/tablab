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
tab_vallabs <- function(df) {
  # argument checks
  assert_that(is.data.frame(df))
  not_empty(df)
  # check if dataframe has labelled variables:
  if (any(map_lgl(df, haven::is.labelled))) {
    # function body
    res <- df %>%
      select_if(haven::is.labelled) %>%
      # map_dfr(~attr(.x, "labels", exact = TRUE) %>% enframe(name = "vallab", value = "val"), .id = "var") %>%
      map(~attr(.x, "labels")) %>%
      enframe("var", "val") %>%
      mutate(vallab=map(.data$val, names)) %>%
      left_join(df %>% tab_types(), by = "var")
    res <-
      res %>%
      split(res$type)

    # print(res)
    res <- res %>%
      imap_dfr(~unnest(.x, cols = c(val, vallab)) %>% rename(!!.y := val))
    if (!"cv" %in% names(res)) {
      res["cv"] <- NA_character_
    }
    if (!"nv" %in% names(res)) {
      res["nv"] <- NA_real_
    }
    res %>%
      # reorder columns
      select(-.data$type, .data$var, .data$nv, .data$cv, .data$vallab) %>%
      mutate(var = factor(.data$var, levels = names(df))) %>%
      arrange(.data$var) %>%
      mutate(var = as.character(.data$var))
  }
  # if no labelled variables exist, return empty tibble:
  else {
    message("no variable in data.frame of type haven::labelled")
    tibble(var = character(), nv = double(), cv = character(), vallab = character())
  }
}
