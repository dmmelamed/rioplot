\name{GSS2018}
\alias{GSS2018}
\docType{data}
\title{
Subset of the General Social Survey analyzed by Schoon, Melamed, and Breiger (2024)
}
\description{
Subset of the General Social Survey analyzed by Schoon, Melamed, and Breiger (2024). Full details on the variable selection and source information is available therein.
}
\usage{data("GSS2018")}
\format{
  A data frame with 558 observations on the following 7 variables.
  \describe{
    \item{\code{dog}}{a numeric vector}
    \item{\code{race}}{a numeric vector}
    \item{\code{sex}}{a numeric vector}
    \item{\code{children}}{a numeric vector}
    \item{\code{married}}{a numeric vector}
    \item{\code{age}}{a numeric vector}
    \item{\code{income}}{a numeric vector}
  }
}
\references{
Schoon, Eric W., David Melamed, and Ronald L. Breiger. 2024. Regression Inside Out. NY: Cambridge University Press.
}
\examples{
data(GSS2018)
head(GSS2018)
}
\keyword{datasets}