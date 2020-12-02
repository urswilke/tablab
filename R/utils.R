

#' Create long format of a dataframe, keeping the id column.
#'
#' @param df Dataframe with a key variable (\code{id}).
#' @param id name of the key variable in the dataframe.
#' @return Dataframe consisting of 3 columns \code{id}, \code{var} & \code{val},
#'   containing the dataframe in long format (based on dplyr::gather).
#' @importFrom tidyr gather spread
#' @importFrom dplyr mutate mutate_all arrange
#' @importFrom purrr imap_dfr
#' @importFrom assertthat assert_that not_empty is.string
#' @importFrom rlang := ensym .data
#' @export
#' @examples
#' # load spss data
#' path <- system.file("examples", "iris.sav", package = "haven")
#' df <- haven::read_sav(path) %>%
#'   # add id column
#'   tibble::rownames_to_column("id")
#' df %>% longen()

longen <- function(df, id = "id") {
  res <-
    df %>%
    unattr() %>%
    gather("var", "val", -{{ id }}) %>%
    full_join(df %>% select(-{{ id }}) %>% tab_types(), by = "var") %>%
    spread(.data$type, .data$val, convert = T) %>%
    factor_arrange(levels = names(df))
  # mutate(var = factor(.data$var, levels = names(df))) %>%
  # arrange(.data$var) %>%
  # mutate(var = as.character(.data$var))
  if (!"cv" %in% names(res)) {
    res["cv"] <- NA_character_
  }
  if (!"nv" %in% names(res)) {
    res["nv"] <- NA_real_
  }
  res

}

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
  # print(df %>% map_chr(typeof))
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




factor_arrange <- function(df, var = var, levels) {
  df %>% mutate(var = factor(.data$var, levels = levels)) %>%
    arrange(.data$var) %>%
    mutate(var = as.character(.data$var))
}


cols_differ <- function(df_cts, col_spec) {
  match_str <- paste0("^", col_spec, "\\d+$")
  col_name <- paste0(col_spec, "_diff")
  diff_lgl <- df_cts %>% select(matches(match_str)) %>% is_diff_in_cols()
  df_cts %>% mutate(!!col_name := diff_lgl)
}

is_diff_in_cols <- function(df) {
  df %>% t() %>% as.data.frame() %>% map_int(n_distinct) > 1
}

add_list_suffix <- function(l, cols) {
  l %>%
    imap(~rename_at(.x, vars(cols), ~paste0(., !!.y)))
}

list_join <- function(l, join=full_join, by) {
  col_names <- names(l[[1]])
  by <- match.arg(by, col_names, several.ok = TRUE)
  l %>% add_list_suffix(setdiff(col_names, by)) %>% reduce(join, by = by)
}

# Taken from:
# http://www.cookbook-r.com/Manipulating_data/Comparing_vectors_or_factors_with_NA/
compareNA <- function(v1, v2) {
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  same
}
