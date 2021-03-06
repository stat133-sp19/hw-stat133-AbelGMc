% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mainfunc.R
\name{bin_variable}
\alias{bin_variable}
\title{Binomial Variable}
\usage{
bin_variable(trials, prob)
}
\arguments{
\item{trials}{number of trials}

\item{prob}{Probability that a success occurs}
}
\value{
a list of binomial variable with trials and probability of success
}
\description{
generate a binomial variable
}
\examples{
bin_variable(trials=5,prob=0.5)
}
