\name{decompose.model}
\alias{decompose.model}
\title{Decompose the Results of a Regression Model by Cases
}
\description{
This function takes a regression model object and a vector of case assignments to groups (note, cases can be in their own group) and computes each cases' contribution to the overall regression coefficients.
}
\usage{
decompose.model(m1,group.by=group.by,include.int="yes",model.type="OLS")
}
\arguments{
  \item{m1}{A regression model object. OLS, logistic, Poisson and negative binomial regression are supported.}
  \item{group.by}{A numeric vector denoting group membership. Should be the same length as the number of cases.}
  \item{include.int}{Whether the regression model included an intercept. Default is "yes."}
  \item{model.type}{Type of model to be decomposed. OLS via lm, logistic via glm ("logit"), Poisson via glm ("poisson"), and negative binomial via MASS ("nb") are supported.}
}
\value{
  \item{decomp.coef}{Each case's or subset of cases' contribution to the estimated slope or regression coefficient.}
  \item{decomp.var}{Each case's or subset of cases' contribution to the variance of the estimated slope or regression coefficient.}
}
\references{
Schoon, Eric, David Melamed, and Ronald L. Breiger. 2024. Regression Inside Out. NY: Cambridge University Press.
}
\author{
David Melamed, Ronald L. Breiger, and Eric Schoon
}
\examples{
data(Kenworthy99)
m1 <- lm(scale(dv) ~ scale(gdp) + scale(pov) + scale(tran) -1,data=Kenworthy99)
decompose.model(m1,group.by=c("Liberal","Corp","Liberal",
"SocDem","SocDem","Corp","Corp","Corp","Corp","Corp","SocDem",
"SocDem","Liberal","Liberal","Liberal"),include.int="no")
}
