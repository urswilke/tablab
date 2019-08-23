#' #' Produce a summary of a dataframe in long format
#' #'
#' #' @param df Dataframe
#' #' @param id ID column in \code{df}
#' #'
#' #' @return Returns a dataframe with 5 columns: Variable \code{var}, \code{val},
#' #'   \code{n}, \code{ids} & \code{class}.
#' #'   \code{ids} &  showing all the values
#' #'   \code{val} (type indicated in \code{class}) each variable \code{var} of
#' #'   \code{df} takes.
#' #'   The list column \code{ids} (contains all the
#' #'   corresponding valus of \code{id}) and has \code{n} entries.
#' #' @importFrom dplyr case_when
#' #' @importFrom purrr transpose
#' #' @export
#' #'
#' #' @examples
#' #' df <-
#' #'   tibble::tibble(
#' #'     fbnr = 1:10,
#' #'     sex = haven::labelled(c(2, 1, 2, 1, 1, 2, 2, 1, 2, 1),
#' #'                           label = "sex",
#' #'                           labels = c(MALES = 1, FEMALES = 2)),
#' #'     age = c(24, 23, 23, 41, 23, 39, 30, 18, 31, 48),
#' #'     marital = haven::labelled(
#' #'       c(1, 7, 2, 6, 4, 5, 3, 8, 4, 2),
#' #'       label = "marital status",
#' #'       labels = c(
#' #'         "single" = 1,
#' #'         "steady relationship" = 2,
#' #'         "living with partner" = 3,
#' #'         "married first time" = 4,
#' #'         "remarried" = 5,
#' #'         "separated" = 6,
#' #'         "divorced" = 7,
#' #'         "non-occuring label" = 9
#' #'       )
#' #'     )
#' #'   )
#' #' longen_id_sum(df, "fbnr")
#'
#' longen_id_sum <- function(df, id = "id") {
#'   df %>%
#'     tab_cts(id, include_ids = TRUE) %>%
#'     left_join(df %>% unattr() %>% tab_attrs() %>% select(.data$var, .data$class)) %>%
#'     group_by(.data$class) %>%
#'     mutate(val = case_when(class == "numeric" ~ as.numeric(.data$val) %>% as.list(),
#'                            class == "character" ~ .data$val %>% as.list()))
#' }



#' Set variable of a dataframe to a value in the rows of a list of ids.

#' @param df Dataframe with an id column named "id".
#' @param l list which must contain 3 elements: \code{l$var} the variable name, and
#'    \code{l$ids} a list of ids where the value \code{l$val} is to be assigned.
#'
#' @return Copy of \code{df} with the overwritten cells explained above
#' @export
#'
#' @examples
#' df <- data.frame(id = 1:10,
#'                  sex = c(2, 1, 2, 1, 1, 2, 2, 1, 2, 1),
#'                  age = c(24, 23, 23, 41, 23, 39, 30, 18, 31, 48))
#' # Produce data summary:
#' df_cts <- tab_cts(df, include_ids = TRUE)
#' # Take the id column of df and reassign the first row in df_cts:
#' df_copy <-
#'   var_to_val_at_ids(
#'     df["id"],
#'     df_cts[1,] %>%
#'       make_type_list() %>%
#'       purrr::transpose() %>%
#'       .[[1]]
#'    )
#' df_copy
var_to_val_at_ids <- function(df, l) {
  # id_in_list_str <- paste0(id, " %in% l$ids")
  # asgn_expr <- rlang::parse_expr(id_in_list_str) %>% rlang::eval_tidy(data = df)
  # stopifnot(is.logical(asgn_expr))
  # print(df)
  df[df$id %in% l$ids, l$var] <- l$val
  df
}


#' Successively assign values to variables in a dataframe at specified ids
#'
#' @param df The dataframe to be modified
#' @param id The name of the id column (character vector)
#' @param df_cts A dataframe including the following columns: "var", "val" &
#'   "ids", such as generated, e.g., by tab_cts(df, include_ids = TRUE)
#'
#' @return The variables \code{var} in the dataframe df are set to val for all
#'   the ids in \code{ids}. This done for every tripple of these variables
#'   contained in the rows of df_cts
#' @export
#' @importFrom purrr transpose
#'
#' @examples
#' df <- data.frame(fbnr = 1:10,
#'              sex = c(2, 1, 2, 1, 1, 2, 2, 1, 2, 1),
#'              age = c(24, 23, 23, 41, 23, 39, 30, 18, 31, 48))
#' # Produce data summary:
#' df_cts <- tab_cts(df, "fbnr", include_ids = TRUE)
#' # Take the id column of df and reassign variables with all the values given in df_cts:
#' df_copy <- mult_assign(df["fbnr"], "fbnr", df_cts)
#' # Now both should be equal
#' all.equal(df, df_copy)
mult_assign <- function(df, id, df_cts) {
  reduce(df_cts %>% make_type_list() %>% transpose(),
         var_to_val_at_ids,
         # The 2 following renames are a HACK to provide the id column called
         # "id" to var_to_val_at_rows and restore the name afterwards
         .init = df %>% rename_at({{ id }}, ~ "id")) %>%
    rename_at("id", ~ {{ id }})
}

#' Coalesce the nv and cv columns to a new list column conserving the type
#'
#' @param df_cts Data frame
#'
#' @return list column conserving the type
#' @export
#'
#' @examples
make_type_list <- function(df_cts) {
  df_cts %>%
    mutate_at(c("nv", "cv"), as.list) %>%
    mutate(val = ifelse(is.na(.data$nv), .data$cv, .data$nv))

}




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
