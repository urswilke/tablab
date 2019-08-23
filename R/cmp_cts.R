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
    factor_arrange(levels = unique(.data$var))
    # mutate(var = as.character(.data$var))
  if (include_ids == FALSE){
    df_cts <- df_cts %>% select(-.data$ids)
  }
  df_cts

}


# list_longed_ex <- function(l, id) {
#   l %>% imap(~longen(.x, id = {{ id }}) %>% mutate(!!paste0("ex", .y) := TRUE))
# }




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
#   df <- df %>% unattr()
#   # print(str(df))
#
#   df_types <- df %>% select(-{{ id }}) %>% tab_types()
#   res <-
#     df_types %>%
#     split(.$type) %>% # %T>% print()  %>%
#     map(~pull(.x, var)) %>%
#     map(~select(df, {{ id }}, .x)) %>%
#     imap_dfr(~.x %>% gather(var, !!.y, -!!ensym(id), convert = T), .id ="type")
#   # print((res))
#   if (is.null(res$character)){
#     res$character <- NA_character_
#   }
#   if (is.null(res$numeric)){
#     res$numeric <- NA_real_
#   }
#   res %>%
#     mutate(character = as.character(character)) %>%
#     # group_by(type) %>%
#     mutate(val = case_when(type == "character" ~ as.list(character),
#                            type == "numeric" ~ as.list(numeric))) #%>%
#     # # # mutate(character = ifelse(is.null(.$character), NA_character_, character),
#     # # #        numeric = ifelse(is.null(.$numeric), NA_real_, numeric)) %>%
#     # # rowwise() %>%
#     # group_by(type, var, character, numeric) %>%
#     # # mutate(listval = coalesce(list(character), list(numeric)))%T>% print() %>%
#     # # mutate(listval = case_when(type == "character" ~ as.list(character),
#     # #                            type == "numeric" ~ as.list(numeric))) %>%
#     # # unite(val, character, numeric, remove = F) %>%
#     # # summarise(val = listval[1], ids = list(!!ensym(id))) %>%
#     # summarise(val = val[1],
#     #           ids = list(!!ensym(id))) %>%
#     # ungroup() %>%
#     # mutate(var = factor(var, levels = names(df))) %>%
#     # arrange(var) %>%
#     # mutate(var = as.character(var))
# }
