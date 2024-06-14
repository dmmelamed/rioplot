\name{RaginData}
\alias{RaginData}
\docType{data}
\title{
Subset of replication data from Ragin and Fiss (2017).
}
\description{
Subset of replication data from Ragin and Fiss (2017). Data were analyzed in Schoon, Melamed, and Breiger (2024). Full details on the variable selection and source information is available therein.}
\usage{data("RaginData")}
\format{
  A data frame with 4185 observations on the following 10 variables.
  \describe{
    \item{\code{incrat}}{a numeric}
    \item{\code{pinc}}{a numeric}
    \item{\code{ped}}{a numeric}
    \item{\code{resp_ed}}{a numeric}
    \item{\code{afqt}}{a numeric}
    \item{\code{kids}}{a numeric}
    \item{\code{married}}{a numeric}
    \item{\code{black}}{a numeric}
    \item{\code{male}}{a numeric}
    \item{\code{povd}}{a numeric}
  }
}
\references{
Ragin, Charles C. and Peer C. Fiss. 2017. Intersectional inequality: Race, class, test scores, and poverty. Chicago, IL: University of Chicago Press.
Schoon, Eric W., David Melamed, and Ronald L. Breiger. 2024. Regression Inside Out. NY: Cambridge University Press.
}
\examples{
data(RaginData)
head(RaginData)
}
\keyword{datasets}
