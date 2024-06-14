\name{Hilbe}
\alias{Hilbe}
\docType{data}
\title{
Replication data for regression models with a count dependent variable. 
}
\description{
Data analyzed by Hilbe (2011), and used here to illustrate model visualization and coefficient decomposition for count models. 
}
\usage{data("Hilbe")}
\format{
  A data frame with 601 observations on the following 9 variables.
  \describe{
    \item{\code{naffairs}}{a numeric vector}
    \item{\code{avgmarr}}{a numeric vector}
    \item{\code{hapavg}}{a numeric vector}
    \item{\code{vryhap}}{a numeric vector}
    \item{\code{smerel}}{a numeric vector}
    \item{\code{vryrel}}{a numeric vector}
    \item{\code{yrsmarr4}}{a numeric vector}
    \item{\code{yrsmarr5}}{a numeric vector}
    \item{\code{yrsmarr6}}{a numeric vector}
  }
}
\source{
Hilbe, Joseph M., 2011. Negative binomial regression. NY: Cambridge University Press.
}
\examples{
data(Hilbe)
head(Hilbe)
}
\keyword{datasets}
