\name{Kenworthy99}
\alias{Kenworthy99}
\docType{data}
\title{
Data to replicate OLS regression models reported in Kenworthy (1999). 
}
\description{
Data to replicate OLS regression models reported in Kenworthy (1999). Data were analyzed in Schoon, Melamed, and Breiger (2024). Full details on the variable selection and source information is available therein.}
\usage{data("Kenworthy99")}
\format{
  A data frame with 15 observations on the following 6 variables.
  \describe{
    \item{\code{dv}}{a numeric vector}
    \item{\code{gdp}}{a numeric vector}
    \item{\code{pov}}{a numeric vector}
    \item{\code{tran}}{a numeric vector}
    \item{\code{ISO3}}{a character vector}
    \item{\code{nation.long}}{a character vector}
  }
}
\references{
Kenworthy, Lane. 1999. "Do social-welfare policies reduce poverty? A cross-national assessment."" Social Forces 77(3): 1119-1139.
Schoon, Eric W., David Melamed, and Ronald L. Breiger. 2024. Regression Inside Out. NY: Cambridge University Press.
}
\examples{
data(Kenworthy99)
head(Kenworthy99)
}
\keyword{datasets}
