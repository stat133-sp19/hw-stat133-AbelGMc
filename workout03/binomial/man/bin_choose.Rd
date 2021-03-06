% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mainfunc.R
\name{bin_choose}
\alias{bin_choose}
\title{choose function}
\usage{
bin_choose(n, k)
}
\arguments{
\item{n}{positive integer, n for trials}

\item{k}{integer, k for success, 0<=k<=n}
}
\value{
how many cases of choosing such success from given trials
}
\description{
calculate n!/(k!(n-k)!)
}
\examples{
bin_choose(n=5,k=2)
bin_choose(5,0)
bin_choose(5,1:3)
}
