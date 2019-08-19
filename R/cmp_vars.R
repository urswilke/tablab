#' Compare the existence of variables in a list of dataframes
#'
#' @param l A list of dataframes.
#'
#' @return A dataframe comparing the existence \code{ex1},  \code{ex2}, ... for
#'   all variables \code{var} in the dataframes in \code{l}. The logical column
#'   \code{ex_diff} indicates, if there are differences.
#' @export
#'
#' @examples
#' df <- data.frame(fbnr = 1:10,
#'                  sex = c(2, 1, 2, 1, 1, 2, 2, 1, 2, 1),
#'                  age = c("24", "23", "23", "41", "23", "39", "30", "18", "31", "48"))
#' df2 <- df %>% dplyr::mutate(new_var = 1) %>% dplyr::select(-sex)
#' cmp_vars(list(df, df2))
cmp_vars <- function(l) {
  l <- unname(l)
  map(l, ~tibble(var = names(.x))) %>% imap(~mutate(.x, !!paste0("ex",.y) := TRUE)) %>%
    reduce(full_join, by = "var") %>%
    cols_differ("ex")

}







#' Compare the types of variables in a list of dataframes
#'
#' @param l A list of dataframes.
#'
#' @return A dataframe comparing the types \code{type1},  \code{type2}, ... for
#'   all variables \code{var} in the dataframes in \code{l}. The logical column
#'   \code{type_diff} indicates, if there are differences.
#' @export
#'
#' @examples
#' df <- data.frame(fbnr = 1:10,
#'                  sex = c(2, 1, 2, 1, 1, 2, 2, 1, 2, 1),
#'                  age = c("24", "23", "23", "41", "23", "39", "30", "18", "31", "48"))
#' df2 <- df %>%
#' dplyr::mutate(new_var = 1,
#'               age = as.numeric(age)) %>%
#'   dplyr::select(-sex)
#' cmp_types(list(df, df2))
cmp_types <- function(l) {
  l <- unname(l)
  map(l, ~unattr(.x) %>% tab_types()) %>%
    add_list_suffix("type") %>%
    # %>% mutate(class = map_chr(unattr(.x), class))) %>%
    reduce(full_join, by = "var") %>%
    cols_differ("type")

}

#' Tabulate the variables' types in a dataframe
#'
#' @param df A dataframe.
#'
#' @return A dataframe consisting of 2 columns: The \code{var}iable and its
#'   \code{type}.
#' @export
#' @description First \code{unattr()} is being called on df. This should result
#'   in a dataframe of only two types: numeric or character without attributes.
#'   These 2 types are then returned.
#' @examples
#' df <- data.frame(fbnr = 1:10,
#'                  sex = c(2, 1, 2, 1, 1, 2, 2, 1, 2, 1),
#'                  age = c("24", "23", "23", "41", "23", "39", "30", "18", "31", "48"))
#' tab_types(df)
tab_types <- function(df) {
  df %>%
    unattr() %>%
    map_chr(class) %>%
    enframe("var", "type") %>%
    mutate(type = dplyr::case_when(type == "numeric"   ~ "nv",
                                   type == "character" ~ "cv")) %>%
    mutate(var = factor(.data$var, levels = names(df))) %>%
    arrange(.data$var) %>%
    mutate(var = as.character(.data$var))
}
