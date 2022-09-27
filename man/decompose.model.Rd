\name{decompose.model}
\alias{decompose.model}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Decompose the Results of a Regression Model by Cases
%%  ~~function to do ... ~~
}
\description{
This function takes a regression model object and a vector of case assignments to groups (note, cases can be in their own group) and computes each cases' contribution to the overall regression coefficients.
}
\usage{
decompose.model(m1,group.by=group.by,include.int="yes",model.type="OLS")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m1}{A regression model object. OLS, logistic, Poisson and negative binomial regression are supported.}
  \item{group.by}{A numeric vector denoting group membership. Should be the same length as the number of cases.}
  \item{include.int}{Whether the regression model included an intercept. Default is "yes."}
  \item{model.type}{Type of model to be decomposed. OLS via lm, logistic via glm, Poisson via glm, and negative binomial via MASS are supported.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{decomp.coef}{Each case's or subset of cases' contribution to the estimated slope or regression coefficient.}
  \item{decomp.var}{Each case's or subset of cases' contribution to the variance of the estimated slope or regression coefficient.}
%% ...
}
\references{
Schoon, Eric, David Melamed, and Ronald L. Breiger. 2023. Regression Inside Out. NY: Cambridge University Press.
}
\author{
David Melamed, Ronald L. Breiger, and Eric Schoon
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(Kenworthy99)
m1 <- lm(scale(dv) ~ scale(gdp) + scale(pov) + scale(tran) -1,data=Kenworthy99)
decompose.model(m1,group.by=c("Liberal","Corp","Liberal","SocDem","SocDem","Corp","Corp","Corp","Corp","Corp","SocDem","SocDem","Liberal","Liberal","Liberal"),
                include.int="no")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
