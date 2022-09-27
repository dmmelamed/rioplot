\name{cosine}
\alias{cosine}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Compute the Cosine similarity between two points.
%%  ~~function to do ... ~~
}
\description{
Given two points, the function computes the cosine similarity between them.}
\usage{
cosine(x,y) }
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ Point 1}
  \item{y}{Point 2}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The cosine similarity, ranging between -1 and +1.}
\references{
Schoon, Eric, David Melamed, and Ronald L. Breiger. 2023. Regression Inside Out. NY: Cambridge University Press.
}
\author{
Ronald L. Breiger, David Melamed and Eric Schoon}
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
rp1 <- rio.plot(m1,include.int="no",r1=1:15)
cosine(rp1$row.dimensions[15,],rp1$row.dimensions[8,]) # cosine similarity between USA and Ireland
cosine(rp1$row.dimensions[15,],rp1$row.dimensions[14,]) # cosine similarity between USA and United Kingdom
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
