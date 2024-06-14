\name{Beckfield06}
\alias{Beckfield06}
\docType{data}
\title{
Replication data for Beckfield (2006) as re-analyzed by Schoon, Melamed, and Breiger (2024)
}
\description{
Beckfield (2006) analyzed these data using fixed and random effects regression models. He showed that regional economic and political integregation is associated with increased economic inequality. Schoon, Melamed, and Breiger (2024) turned these models inside out and decomposed the model coefficients. 
}
\usage{data("Beckfield06")}
\format{
  A data frame with 48 observations on the following 9 variables.
  \describe{
    \item{\code{year}}{a numeric vector}
    \item{\code{polint}}{a numeric vector}
    \item{\code{ecoint}}{a numeric vector}
    \item{\code{ecoints}}{a numeric vector}
    \item{\code{gdp}}{a numeric vector}
    \item{\code{trans}}{a numeric vector}
    \item{\code{outflo}}{a numeric vector}
    \item{\code{gini}}{a numeric vector}
    \item{\code{countryid}}{a character vector}
  }
}
\references{
Beckfield, Jason. 2006. "European integration and income inequality."" American Sociological Review 71(6): 964-985.
Schoon, Eric W., David Melamed, and Ronald L. Breiger. 2024. Regression Inside Out. NY: Cambridge University Press.
}
\examples{
data(Beckfield06)
head(Beckfield06)
}
\keyword{datasets}
