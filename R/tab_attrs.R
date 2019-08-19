#' Tabulate a dataframe's variables' attributes
#'
#' @param df Dataframe
#'
#' @return Dataframe with one row per variable in \code{df}, containing the
#'   following columns: "var", "varlab", "val", "vallab", "format.spss",
#'   "labels", "display_width" & "class"
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
