#' Compare counts of a list of dataframes
#'
#' @param l List of dataframes.
#' @param id name of the key variable in the dataframes.
#' @param include_ids Logical denoting whether a list column of the ids should
#'   be included in the results. The ids in the list show at which values of
#'   \code{id} the variable \code{var} contains the numeric value \code{nv1} or character (string) value \code{cv1}.
#' @return Dataframe consisting of columns \code{var}, \code{val}, ...,
#'   \code{vallab1}, \code{vallab2}, ..., & \code{n},
#'   containing a comparison of the counts of variable values (and their
#'   respective value labels) of the two dataframes in long format.
#'   \code{nv_differ} or \code{cv_differ} & \code{vallabs_differ} are logical columns indicating
#'   if all values / value labels are equal.
#' @importFrom dplyr full_join count group_by_at tally rename ungroup select
#'   matches mutate_at mutate
#' @importFrom purrr map imap reduce walk set_names map2 map_dfr map2_lgl
#' @importFrom assertthat assert_that not_empty is.string
#' @importFrom tibble lst
#' @importFrom rlang sym quo
#' @export
#' @examples
#' # load spss data
#' path <- system.file("examples", "iris.sav", package = "haven")
#' df1 <- haven::read_sav(path) %>%
#'   # add id column
#'   tibble::rownames_to_column("id")
#'
#' # create a modified copy:
#' df2 <- df1
#' df2[1, "Species"] <- 2
#'
#' # compare the dataframes counts:
#' cmp_cts(list(df1, df2))
#' # compare the dataframes and only show the counts where values have changed:
#' cmp_cts(list(df1, df2)) %>% dplyr::filter(nv1 != nv2)
#'
#' # Create another modified copy
#' df3 <- df2
#' df3[2, "Species"] <- 3
#'
#' # compare the dataframes counts:
#' l <- list(df1, df2, df3)
#' cmp <- cmp_cts(l)
#' cmp
#'
#' # compare the dataframes and only show the counts where values have changed:
#' cmp %>% dplyr::filter(!(nv1 == nv2 & nv2 == nv3))
#'

cmp_cts <- function(l, id = "id", include_ids = FALSE) {
  # argument checks
  assert_that(length(l) >= 2)
  walk(l, ~ assert_that(is.data.frame(.x)))
  walk(l, ~ assert_that(id %in% names(.x), msg = "At least one data.frame doesn't have the specified id."))
  walk(l, ~ assert_that(ncol(.x) >= 2, msg = "At least one data.frame has less than two columns."))
  walk(l, ~ assert_that(length(.x[[id]]) == length(unique(.x[[id]])), msg = "The key in the data.frame is not unique."))

  walk(l, ~ assert_that(not_empty(.x)))

  is.string(id)
  # val_cols <- paste0("val", 1:length(l)) %>% set_names() %>% map_chr(~paste0("quo(", ., "[1])")) %>% rlang::parse_exprs() %>% set_names(~paste0("val", 1:length(l)))
  # funs <- lst(n = quo(n()), !!!val_cols, type = quo(.data$type[1]), ids = quo(list(!!sym(id))))
  df_cts <-
    l %>% map(~longen(.x, id)) %>%
  # print(df_cts)
  # df_cts %>%
    # imap(~rename_at(.x, vars(c("val")), ~paste0(., !!.y))) %>%
    # add_list_suffix(c("nv", "cv")) %>%
    # reduce(full_join, by = c(id, "var")) %>%
    list_join(by = c(id, "var")) %>%
    # mutate(var = factor(.data$var, levels = unique(.data$var))) %>%
    group_by_at(vars("var", matches("^(n|c)v\\d+$"))) %>%
    summarise(n = n(), ids = list(!!ensym(id))) %>%
    ungroup() %>%
    factor_arrange(levels = l %>% map(names) %>% reduce(union))
    # mutate(var = as.character(.data$var))
  if (include_ids == FALSE){
    df_cts <- df_cts %>% select(-.data$ids)
  }
  df_cts

}


# list_longed_ex <- function(l, id) {
#   l %>% imap(~longen(.x, id = {{ id }}) %>% mutate(!!paste0("ex", .y) := TRUE))
# }




#' Compare the variable labels between a list of dataframes
#'
#' @param l A list of dataframes.
#'
#' @return A dataframe comparing the \code{varlab}s for all variables in the
#'   dataframes. The logical column \code{varlab_diff} indicates, if there are
#'   differences.
#' @export
#'
#' @examples
#' df <- data.frame(fbnr = 1:10,
#' sex = haven::labelled(c(2, 1, 2, 1, 1, 2, 2, 1, 2, 1),
#'                       label = "sex",
#'                       labels = c(MALES = 1, FEMALES = 2)),
#'                       age = c(24, 23, 23, 41, 23, 39, 30, 18, 31, 48))
#' df2 <- df %>% dplyr::mutate(new_var = 1)
#' attr(df2$new_var, "label")  <-  "some variable label"
#' attr(df2$sex, "label")  <-  NULL
#' cmp_varlabs(list(df, df2))
cmp_varlabs <- function(l) {
  l <- unname(l)
  l %>%
    map(tab_varlabs) %>%
    # add_list_suffix("varlab") %>%
    # reduce(full_join, by = "var") %>%
    list_join(by = c("var")) %>%
    cols_differ("varlab")
}




#' Compare the value labels between a list of dataframes
#'
#' @param l A list of dataframes.
#'
#' @return A dataframe comparing the \code{vallab}s for all labelled values of the variables in the
#'   dataframes. The logical column \code{vallab_diff} indicates, if there are
#'   differences.
#' @export
#'
#' @examples
#' df <- data.frame(fbnr = 1:10,
#' sex = haven::labelled(c(2, 1, 2, 1, 1, 2, 2, 1, 2, 1),
#'                       label = "sex",
#'                       labels = c(MALES = 1, FEMALES = 2)),
#'                       age = c(24, 23, 23, 41, 23, 39, 30, 18, 31, 48))
#' df2 <- data.frame(fbnr = 1:10,
#'                   sex = haven::labelled(c(2, 1, 2, 1, 1, 2, 2, 1, 2, 1),
#'                                         label = "sex",
#'                                         labels = c(m = 0, f = 1)),
#'                   age = c(24, 23, 23, 41, 23, 39, 30, 18, 31, 48))
#' cmp_vallabs(list(df, df2))
#' # The values are ordered by appearance in the dataframes. Compare:
#' cmp_vallabs(list(df2, df))
cmp_vallabs <- function(l) {
  l <- unname(l)
  l %>%
    map(tab_vallabs) %>%
    # add_list_suffix("vallab") %>%
    # reduce(full_join, by = c("var", "nv", "cv")) %>%
    list_join(by = c("var", "nv", "cv")) %>%
    cols_differ("vallab")
}




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
    # add_list_suffix("type") %>%
    # # %>% mutate(class = map_chr(unattr(.x), class))) %>%
    # reduce(full_join, by = "var") %>%
    list_join(by = "var") %>%
    cols_differ("type")

}
