#' Compare counts and labels of a list of dataframes
#'
#' @param l List of dataframes.
#' @param id name of the key variable in the dataframes.
#' @param include_ids Logical denoting whether a list column \code{ids} should
#'   be included in the results.
#'   The ids in each list show at which values of
#'   \code{id} the variable \code{var} contains the value \code{val1},
#'   \code{val2}, ...
#' @param col_groups String specifying the order of columns in the resulting
#'   dataframe. "index" groups columns by indices. "spec" groups columns by the
#'   specification types   c("ex", "varlab", "val", "vallab")
#' @param include_diffs Logical denoting whether in the resulting dataframe,
#'   logical columns suffixes "_diff" should be included, indicating if there
#'   are differences between the dataframes in \code{l} regarding the
#'   specification types given in \code{spec_diffs}.
#' @param spec_diffs Character vector (defaults to \code{c("ex", "varlab",
#'   "val", "vallab")}) specifying the attributes that should be compared in the
#'   resulting dataframe.
#'   If \code{include_diffs} = TRUE, this adds columns
#'   prefixed by spec_diffs, and each suffixed by "_diff". Additionally a column
#'   \code{any_diff} is added denoting if any of the former is TRUE.
#' @return Dataframe consisting of columns \code{var}, \code{val1}, \code{val2},
#'   ..., \code{vallab1}, \code{vallab2}, ..., \code{varlab1}, \code{varlab2},
#'   ..., \code{ex1}, \code{ex2}, ..., and \code{n}, containing a comparison of
#'   the counts of variable values (and their respective value labels) of the
#'   dataframes in long format. The indices denote the dataframes in \code{l}.
#'   \code{vals_diff} & \code{vallabs_diff} are logical columns indicating if
#'   all values, variable / value labels are equal.
#' @importFrom dplyr left_join full_join anti_join group_by_at rename_at ungroup
#'   select matches mutate_at arrange arrange_at group_by row_number vars
#'   summarise n n_distinct distinct bind_rows everything pull
#' @importFrom purrr map imap reduce walk set_names map2 map_dfr map2_lgl
#'   map_int
#' @importFrom assertthat assert_that not_empty is.string
#' @importFrom rlang .data parse_exprs parse_expr
#' @importFrom forcats as_factor
#' @importFrom tidyr unite separate_rows unnest
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
#' # modify the value label of "setosa"
#' df2$Species <- haven::labelled(df2$Species,
#'                                labels = c(setosa_mod = 1, versicolor = 2, virginica = 3),
#'                                label = "Species")
#'
#' # compare the dataframes counts:
#' cmp_all(list(df1, df2))
#' # compare the dataframes and only show the counts where values have changed:
#' cmp_all(list(df1, df2)) %>% dplyr::filter(nv1 != nv2)
#' # This results in the same rows:
#' cmp_all(list(df1, df2), spec_diffs = "nv") %>% dplyr::filter(any_diff)
#' # Or alternatively:
#' cmp_all(list(df1, df2)) %>% dplyr::filter(nv_diff)
#' # compare the dataframes and only show the counts where value labels have changed:
#' cmp_all(list(df1, df2)) %>% dplyr::filter(vallab1 != vallab2)
#'
#' # Create another modified copy
#' df3 <- df2
#' df3[2, "Species"] <- 3
#' # modify the value label of "versicolor"
#' df2$Species <- haven::labelled(df2$Species,
#'                                labels = c(setosa = 1, versicolor_mod = 2, virginica = 3),
#'                                label = "Species_mod")
#'
#' # compare the dataframes counts:
#' l <- list(df1, df2, df3)
#' cmp <- cmp_all(l)
#' cmp
#'
#' # compare the dataframes and only show the counts where values have changed:
#' cmp %>% dplyr::filter(nv_diff)
#'
#' # Show where either values or value labels differ:
#' cmp %>% dplyr::filter(nv_diff | vallab_diff)

cmp_all <- function(l, id = "id",
                    include_ids = FALSE,
                    spec_diffs = c("nv", "cv", "vallab", "varlab"),
                    include_diffs = TRUE,
                    col_groups = c("spec", "index")){
  # Change list element names to 1, ..., length(l) (if list is named):
  l <- unname(l)
  col_groups <- match.arg(col_groups)
  spec_diffs <- match.arg(spec_diffs, several.ok = TRUE)
  # spec_diffs <- c("val", spec_diffs)

  # df_cts <- cmp_cts(l, id, include_ids)
  #
  # df_cts_n_vallabs <-
  #   l %>% map( ~ tab_vallabs(.x)) %>%
  #   add_list_suffix(c("val", "vallab")) %>%
  #   reduce(left_join, .init = df_cts %>% mutate_at(vars(matches("^val\\d+$")), ~ unlist(.)))
  #
  # df_non_ex_vallabs <-cmp_no_cts_vallabs(l, id)
  #
  # df_all_vallabs <-
  #   bind_rows(df_cts_n_vallabs,
  #             df_non_ex_vallabs)
  # print(df_all_vallabs)

  df_all <-
    map(l, ~join_labs2long(.x, id)) %>%
    # add_list_suffix(c("nv", "cv", "vallab", "varlab")) %>%
    # reduce(full_join, by = c(id, "var", "nv_join", "cv_join")) %>%
    list_join(by = c(id, "var", "nv_join", "cv_join")) %>%
    group_by_at(vars(.data$var, matches("^(nv|cv|varlab|vallab)\\d+"))) %>%
    summarise(n=sum(!is.na(!!ensym(id))), ids=ifelse(n == 0, list(NULL), list(!!ensym(id)))) %>%
    ungroup()
  # print(df_all)


  # df_all <-
  #   l %>% map( ~ tab_varlabs(.x)) %>%
  #   add_list_suffix("varlab") %>%
  #   reduce(left_join, .init = df_long_vall)


  # create the logical columns indicating differences:
  # spec_diffs <- c("ex", "varlab", "val", "vallab")
  # The reduce command creates logical columns in the dataframe for each element
  # in col_spec, indicating if there are differences in the rows in df_cts,
  # e.g., for the element "val" in spec_diffs, it creates:
  # df_all %>% mutate(val_diff = select(df_all, matches("^val\\d+$")) %>% t %>% as.data.frame() %>% map_int(n_distinct) > 1)
  if (include_diffs) {
    # This creates a logical OR of the columns in spec_diffs, e.g., if
    # spec_diffs = c("ex", "val") any_diff = ex_diff | val_diff:
    any_diff_expr = rlang::parse_expr(paste0(".data$", spec_diffs, "_diff", collapse = " | "))
    df_all <-
      reduce(spec_diffs, cols_differ, .init = df_all) %>%
      mutate(any_diff = !!any_diff_expr)
  }


  # if (col_groups == "index") {
  #   # This sorts the dataframe columns by ascending suffixes 1, ..., length(l):
  #   match_exprs <- parse_exprs(paste0("matches('", c(1:length(l), "_diff"), "$')"))
  # }
  # else if (col_groups == "spec") {
  #   # This sorts the dataframe columns by according to the sequence in spec_diffs:
  #   match_exprs <- parse_exprs(paste0("matches('", c(spec_diffs, "any"), "(\\\\d+|_diff)$')"))
  # }
  # print(spec_diffs)
  cols <- rearrange_cols(spec_diffs = spec_diffs,
                         inds = 1:length(l),
                         include_diffs = include_diffs,
                         col_groups = col_groups,
                         include_ids = include_ids)
  # print(cols)
  df_all %>%
    # mutate(var = factor(.data$var, levels = unique(.data$var))) %>%
    # arrange(.data$var) %>%
    # mutate(var = as.character(.data$var)) %>%
    factor_arrange(levels = unique(.data$var)) %>%
    select(cols)

}




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





rearrange_cols <- function(spec_diffs, inds, include_diffs, col_groups, include_ids) {
  if (include_diffs) {
    inds <- c(inds, "_diff")
  }
  df_colnames <-
    tibble(spec = as_factor(spec_diffs),
         inds = list(inds)) %>%
    unnest(inds) %>%
    unite("col_i", sep = "", remove = F)
  if (col_groups == "index") {
    df_colnames <-
      df_colnames %>% arrange(inds)
  }
  c("var",
    "n",
    df_colnames %>%
      pull(.data$col_i),
    c("any_diff")[include_diffs],
    c("ids")[include_ids])
}

join_labs2long <- function(df, id) {
  df_long <-
    df %>% longen(id)

  df_long %>%
    full_join(df %>% tab_vallabs(), by = c("var", "nv", "cv")) %>%
    full_join(df %>% tab_varlabs(), by = "var") %>%
    mutate(nv_join = ifelse(is.na(!!ensym(id)), .data$nv, NA_real_),
           cv_join = ifelse(is.na(!!ensym(id)), .data$cv, NA_character_))
}
