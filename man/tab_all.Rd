% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tab_functions.R
\name{tab_all}
\alias{tab_all}
\title{Tabulate a dataframe's variables' value counts, and labels}
\usage{
tab_all(df, id, include_ids = FALSE)
}
\arguments{
\item{df}{Dataframe}

\item{id}{Character string of the id variable}

\item{include_ids}{Logical denoting whether a list column \code{ids} should
be included in the results.
The ids in each list show at which values of
\code{id} the variable \code{var} contains the value \code{val}.}
}
\value{
Dataframe containing a tabulation of the counts \code{n} of
variables' values (\code{var} & \code{nv} / \code{cv}), as well as their labels
\code{vallab} & \code{varlab}.
}
\description{
Tabulate a dataframe's variables' value counts, and labels
}
\examples{
df <-
  tibble::tibble(
    fbnr = 1:10,
    sex = haven::labelled(c(2, 1, 2, 1, 1, 2, 2, 1, 2, 1),
                          label = "sex",
                          labels = c(MALES = 1, FEMALES = 2)),
    age = c(24, 23, 23, 41, 23, 39, 30, 18, 31, 48),
    marital = haven::labelled(
      c(1, 7, 2, 6, 4, 5, 3, 8, 4, 2),
      label = "marital status",
      labels = c(
        "single" = 1,
        "steady relationship" = 2,
        "living with partner" = 3,
        "married first time" = 4,
        "remarried" = 5,
        "separated" = 6,
        "divorced" = 7,
        "non-occuring label" = 9
      )
    )
  )
tab_all(df, "fbnr")
}
