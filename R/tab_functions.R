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
    longen(df, id) %>%
    group_by(.data$var, .data$nv, .data$cv) %>%
    summarise(n = n(), ids = list(!!rlang::ensym(id))) %>%
    ungroup() %>%
    factor_arrange(levels = names(df))
  if (include_ids == FALSE) {
    res <-
      res %>% select(-.data$ids)
  }
  res
}



#' Create a variable label dataframe
#'
#' @param df Labelled dataframe, e.g., resulting of the read_sav / read_dta
#'   functions of the package haven.
#' @return Dataframe consisting of two columns \code{var} and \code{varlab},
#'   showing all variable labels in \code{df}.
#' @export
#' @importFrom assertthat assert_that not_empty has_attr
#' @importFrom purrr map_lgl map_dfr
#' @importFrom tibble enframe
#' @importFrom dplyr select select_if
#' @examples
#' # load spss data
#' path <- system.file("examples", "iris.sav", package = "haven")
#' df <- haven::read_sav(path)
#' attr(df$Sepal.Width, "label") <- "width of the sepal"
#' tab_varlabs(df)
tab_varlabs <- function(df) {
  # argument checks
  assert_that(is.data.frame(df))
  not_empty(df)

  # check if dataframe has labelled variables:
  if (any(map_lgl(df, ~has_attr(.x, "label")))) {
    # df %>%
    #   map_dfr(~attr(.x, "label", exact = TRUE) %>% enframe(name = NULL, value = "varlab"), .id = "var")
    df %>%
      map(~attr(.x, "label", exact = TRUE) ) %>%
      enframe("var", "varlab") %>%
      unnest(cols = "varlab")
  }
  else {
    message("No variable in the data.frame has a variable label")
    tibble(var = character(), varlab = character())
  }

}




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
      imap_dfr(~rename(.x, !!.y := val) %>% unnest(cols = c(!!.y, vallab)))
    if (!"cv" %in% names(res)) {
      res["cv"] <- NA_character_
    }
    if (!"nv" %in% names(res)) {
      res["nv"] <- NA_real_
    }
    res %>%
      # reorder columns
      select(-.data$type, .data$var, .data$nv, .data$cv, .data$vallab) %>%
      factor_arrange(levels = names(df))
    # mutate(var = factor(.data$var, levels = names(df))) %>%
    # arrange(.data$var) %>%
    # mutate(var = as.character(.data$var))
  }
  # if no labelled variables exist, return empty tibble:
  else {
    message("no variable in data.frame of type haven::labelled")
    tibble(var = character(), nv = double(), cv = character(), vallab = character())
  }
}



#' Create a variable & value label dataframe
#'
#' @param df Labelled dataframe, e.g., resulting of the read_sav / read_dta
#'   functions  of the package haven.
#' @param rm_non_labelled Logical indicating whether the result should contain
#'   non-labelled variables (neither variable nor value labels).
#' @return Dataframe consisting of 4 columns \code{var}, \code{varlab},
#'   \code{val}  and \code{vallab}, showing all variable & value labels in
#'   \code{df}.
#' @export
#' @importFrom assertthat assert_that not_empty is.string
#' @importFrom dplyr full_join filter
#' @importFrom tibble tibble
#' @importFrom rlang := .data
#' @examples
#' # load spss data
#' path <- system.file("examples", "iris.sav", package = "haven")
#' df <- haven::read_sav(path)
#' tab_labs(df)
tab_labs <- function(df, rm_non_labelled = FALSE){
  # argument checks
  assert_that(is.data.frame(df))
  not_empty(df)

  result <-
    tibble(var = names(df)) %>%
    full_join(df %>% tab_varlabs(), by = "var") %>%
    full_join(df %>% tab_vallabs(), by = "var")
  if (rm_non_labelled == TRUE){
    result <-
      result %>% filter(!(is.na(.data$varlab) & is.na(.data$nv) & is.na(.data$cv)))
  }
  result
}



#' Tabulate a dataframe's variables' attributes
#'
#' @param df Dataframe
#'
#' @return Dataframe with one row per variable in \code{df}, containing the
#'   following columns: "var", "varlab", "val", "vallab", "format.spss",
#'   "labels", "display_width" & "class". In contrast to \code{tab_types()} the
#'   class shows the original type as it is imported by haven. This means
#'   labelled numerics or characters belong to the class haven_labelled.
#' @export
#' @importFrom rlang .data
#' @importFrom purrr map_chr
#'
#' @examples
#' path <- system.file("examples", "iris.sav", package = "haven")
#' df <- haven::read_sav(path) %>%
#'   # add id column
#'   tibble::rownames_to_column("id")
#'
#' df %>% tab_attrs()
#' # Only show variables with a variable label:
#' df %>% tab_attrs() %>% dplyr::filter(!sapply(varlab, is.null))

tab_attrs <- function(df) {
  df %>% map(attributes) %>%
    map(~{
      .x[["vallab"]] <- list(names(.x[["labels"]]));
      .x[["val"]] <- list(unname(.x[["labels"]]));
      .x[["varlab"]] <- list((.x[["label"]]));
      .x[["labels"]] <- list((.x[["labels"]]));
      .x[["class"]] <- NULL;
      .x}) %>%
    map_dfr(~tibble(!!!.x), .id = "var") %>%
    full_join(df %>% map_chr(class) %>% enframe("var", "class"), by = "var") %>%
    # filter(!sapply(varlab, is.null)) %>%
    select(.data$var, .data$varlab, .data$val, .data$vallab, .data$labels, everything())
}




#' Tabulate a dataframe's variables' value counts, and labels
#'
#' @param df Dataframe
#' @param id Character string of the id variable
#' @param include_ids Logical denoting whether a list column \code{ids} should
#'   be included in the results.
#'   The ids in each list show at which values of
#'   \code{id} the variable \code{var} contains the value \code{val}.
#'
#'
#' @return Dataframe containing a tabulation of the counts \code{n} of
#'   variables' values (\code{var} & \code{nv} / \code{cv}), as well as their labels
#'   \code{vallab} & \code{varlab}.
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
#'         "non-occuring label" = 9
#'       )
#'     )
#'   )
#' tab_all(df, "fbnr")
tab_all <- function(df, id, include_ids = FALSE) {
  df %>%
    tab_cts(id, include_ids) %>%
    full_join(df %>% tab_vallabs(), by = c("var", "nv", "cv")) %>%
    full_join(df %>% tab_varlabs(), by = "var")
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
    factor_arrange(levels = names(df))
  # mutate(var = factor(.data$var, levels = names(df))) %>%
  # arrange(.data$var) %>%
  # mutate(var = as.character(.data$var))
}
