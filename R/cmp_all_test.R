cmp_all_test <- function(l, id = "id", include_ids = FALSE){
  # argument checks
  assert_that(length(l) >= 2)
  walk(l, ~ assert_that(is.data.frame(.x)))
  walk(l, ~ assert_that(id %in% names(.x), msg = "At least one data.frame doesn't have the specified id."))
  walk(l, ~ assert_that(ncol(.x) >= 2, msg = "At least one data.frame has less than two columns."))
  walk(l, ~ assert_that(length(.x[[id]]) == length(unique(.x[[id]])), msg = "The key in the data.frame is not unique."))
  # assert_that(is.data.frame(df1))
  # assert_that(is.data.frame(df2))
  walk(l, ~ assert_that(not_empty(.x)))
  # not_empty(df1)
  # not_empty(df2)
  is.string(id)

  l <- unname(l)

  id_var <- rlang::as_name(id)
  longed_list <-
    l %>% imap(~longen(.x, id = {{ id }}) %>% mutate(!!paste0("ex", .y) := TRUE))

  df_cmp <-
    longed_list %>%
    # map2(l %>% imap(~longen(.x, id = {{ id }}) %>% mutate(!!paste0("ex", .y) := TRUE)),
    #      l %>% map( ~ tab_labs(.x, rm_non_labelled = TRUE)),
    #      ~ full_join(.x, .y, by = c("var", "val"))) %>%
    # map(~mutate(.x, !!id_var := ifelse(is.na(!!ensym(id_var)), 1e9 + val, !!ensym(id_var)))) %>%
    imap(~rename_at(.x, vars(c("val")), ~paste0(., !!.y))) %>%
    reduce(full_join, by = c(id, "var")) %>%
    mutate(var = factor(.data$var, levels = unique(.data$var))) %>%
    group_by_at(vars("var", matches("\\d+$"))) %>%
    summarise(n = n(), ids = list(!!ensym(id))) %>%
    ungroup()
  print(df_cmp)
  df_vallab <-
    l %>% map( ~ tab_vallabs(.x)) %>%
    imap(~rename_at(.x, vars(c("val", "vallab")), ~paste0(., !!.y))) %>%
    reduce(left_join, .init = df_cmp)

  df_all <-
    l %>% map( ~ tab_varlabs(.x)) %>%
    imap(~rename_at(.x, vars(c("varlab")), ~paste0(., !!.y))) %>%
    reduce(left_join, .init = df_vallab)

  df_non_ex_vallabs <-
    l %>% map(tab_vallabs) %>%
    map2(.,
         longed_list %>%
           map(~distinct(.x, var, val)),
         ~anti_join(.x, .y)) %>%
    imap(~rename_at(.x, vars(c("val")), ~paste0(., !!.y))) %>%
    reduce(full_join)

  df_non_ex_vallabs <-
    # df_non_ex_vallabs %>%
    reduce(l %>% map(tab_vallabs) %>% imap(~rename_at(.x, vars(c("val", "vallab")), ~paste0(., !!.y))),
           left_join,
           .init = df_non_ex_vallabs) %>%
    select(-vallab)

    # bind_cols(rep(df_non_ex_vallabs %>% select(vallab), length(l)) %>%
    #             set_names(paste0("vallab", 1:length(l))) %>% as_tibble()) %>%

  df_cmp <-
    df_all %>% bind_rows(df_non_ex_vallabs)

  # %>%
  #   group_by(.data$var) %>%
  #   fill(matches("^varlab\\d+$")) %>%
  #   ungroup()

  if (include_ids == FALSE){
    df_cmp <- df_cmp %>% select(-.data$ids)
  }

  suppressWarnings(
    df_cmp <-
      df_cmp %>%
      # group_by(.data$var) %>%
      # the ifelse function puts NAs first (in order to make the variable labels
      # appear first):
      # arrange_at(vars(matches("^val\\d+$")), ~ifelse(is.na(.), is.na(.), .), .by_group = TRUE) %>%
      # ungroup()  %>%
      select(var, n, names(df_cmp) %>% parse_number() %>% order()) %>%
      mutate(var = factor(var, levels = l %>% map( ~ names(.x[-1])) %>% unlist() %>% unique())) %>%
      arrange(var))

  # create the logical columns indicating differences:
  col_specs <- c("ex", "varlab", "val", "vallab")
  # The reduce command creates logical columns in the dataframe for each element
  # in col_spec, indicating if there are differences in the rows in df_cmp,
  # e.g., for the element "val" in col_specs, it creates:
  # df_cmp %>% mutate(val_diff = select(df_cmp, matches("^val\\d+$")) %>% t %>% as.data.frame() %>% map_int(n_distinct) > 1)
  reduce(col_specs, cols_differ, .init = df_cmp) %>%
    group_by(var) %>%
    # set the logical to false for each entry that's not the first of the grouping:
    mutate(varlab_diff = ifelse(row_number() == 1, .data$varlab_diff, FALSE)) %>%
    mutate(any_diff = .data$ex_diff |
             .data$val_diff | .data$varlab_diff | .data$vallab_diff)


}

cols_differ <- function(df_cmp, col_spec) {
  match_str <- paste0("^", col_spec, "\\d+$")
  col_name <- paste0(col_spec, "_diff")
  diff_lgl <- df_cmp %>% select(matches(match_str)) %>% is_diff_in_cols()
  df_cmp %>% mutate(!!col_name := diff_lgl)
}

is_diff_in_cols <- function(df) {
  res <- df %>% t %>% as.data.frame()%>% map_int(n_distinct)
  print(res)
  res  > 1
}




# testing -----------------------------------------------------------------

list(df2, df_mod) %>% cmp_all_test("fbnr", T) #%>% filter(any_diff) %>% View
