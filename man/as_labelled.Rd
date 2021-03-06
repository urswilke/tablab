% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{as_labelled}
\alias{as_labelled}
\title{Convert a factor to a haven::labelled variable}
\usage{
as_labelled(x)
}
\arguments{
\item{x}{Factor.}
}
\value{
Returns a labelled variable with values and labels according
to the levels in the factor. This should roundtrip with forcats::as_factor.
Additionally, a variable label can be set.
}
\description{
Convert a factor to a haven::labelled variable
}
\examples{
x <- factor(c("b", "b", "a"))
attr(x, "label") <- "variable label"
x
as_labelled(x)
all.equal(x,
          x \%>\%
            as_labelled() \%>\%
            forcats::as_factor())
}
