% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funcs.R
\name{chainRoundProjectData}
\alias{chainRoundProjectData}
\title{Project Contribution Data for a given Round and Chain Index}
\usage{
chainRoundProjectData(cidx, ridx)
}
\arguments{
\item{cidx}{Chain Index out of 1, 10, 137, 250, 42161, 421613, 424}

\item{ridx}{RoundId}
}
\value{
Project Contribution Data
}
\description{
Project Contribution Data for a given Round and Chain Index
}
\examples{
\dontrun{
chainRoundProjectData(42161,"0x302Dbc8eB3bf73565A1205648B61b23CB3f72Ff7")
}
}
