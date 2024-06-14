\name{rio.plot}
\alias{rio.plot}
\title{ Regression Inside Out: Plotting Regression Models
}
\description{ rio.plot is used to generate a reduced rank image of a regression model. The function computes row and column dimensions for both cases and variables, and generates an image of the model based on those scores.
}
\usage{
rio.plot(m1,exclude.vars="no",r1="none",case.names="",col.names="no",
h.just=-.2,v.just=0,case.col="blue",var.name.col="black",
include.int="yes",group.cases=1,model.type="OLS")
}
\arguments{
  \item{m1}{a regression model object. Supported models include OLS, Logistic, Poisson, and Negative Binomial Regression.
}
  \item{exclude.vars}{an optional numerical vector indicating variables from the model to exclude from the plot of the model.}
  \item{r1}{an optional numerical vector indicating cases to include in the plot. By default, all cases are excluded from the plot.}
  \item{case.names}{a character string of names to label the cases. Should be the same length as 'r1.'}
  \item{col.names}{whether to include the variable names in the plot. Default is "no"}
  \item{h.just}{horizontal justification in the plot. Default is -.2}
  \item{v.just}{vertical justification in the plot. Default is 0}
  \item{case.col}{if cases are added to the plot, this is their color. Default is "blue"}
  \item{var.name.col}{Color of the names of variables in the plot. Default is "black"}
  \item{include.int}{Whether the underlying model included a model intercept. Default is "yes"}
  \item{group.cases}{Whether to aggregate cases into clusters or subsets. If yes, provide a numeric vector of memberships. It will aggregate over them by summing.}
  \item{model.type}{The type of regression model. OLS is supported via the lm function. Logistic and Poisson regression are supported via the glm function. Negative Binomial regression is supported via the MASS package. Default is "OLS." For logistic regression, use "logit." For Poisson regression, use "poisson." For negative binomial regression, use "nb."}}

\details{The function take a regression model object (OLS, logistic, Poisson, or negative binomial) and computes the corresponding row (case) and column (variables) scores. The scores are part of the output, as is a ggplot object of the model.
}
\value{
rio.plot returns several objects.
  \item{p1}{a ggplot object of the model space, given the terms in the function}
  \item{row.dimensions}{the scores assigned to each case, or each subset of cases if they were aggregated using the 'group.cases' option. These are the co-ordinates in the plot.}
  \item{col.dimensions}{the scores assigned to each variable. These are the co-ordinates in the plot.}
  \item{case.variances}{each cases' contribution (or each subsets' contribution) to the variance of the estimated regression coefficient}
  \item{U}{The orthogonalized column space matrix from the Singular Value Decomposition of the predictor matrix and fitted values.}
  \item{UUt}{The orthogonalized column space matrix from the Singular Value Decomposition of the predictor matrix and fitted values, post-multiplied by its transpose.}
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
rp1 <- rio.plot(m1,include.int="no")
names(rp1)
rp1$gg.obj 
# rp1$gg.obj + ggplot2::scale_x_continuous(limits=c(-.55,1)) # useful option

rp2 <- rio.plot(m1,r1=1:15,case.names=paste(1:15),include.int="no")
rp2$gg.obj

Kenworthy99 <- data.frame(Kenworthy99,type=c("Liberal","Corp","Liberal",
"SocDem","SocDem","Corp","Corp","Corp","Corp","Corp","SocDem","SocDem",
"Liberal","Liberal","Liberal"))

rp3 <- rio.plot(m1,r1=1:15,group.cases=Kenworthy99$type,include.int="no")
rp3$gg.obj 
# rp3$gg.obj + ggplot2::scale_x_continuous(limits=c(-1,20))
}
