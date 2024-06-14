\name{cosine}
\alias{cosine}
\title{Compute the Cosine similarity between two points.
}
\description{
Given two points, the function computes the cosine similarity between them.}
\usage{
cosine(x,y) }
\arguments{
  \item{x}{ Point 1}
  \item{y}{Point 2}
}
\value{
The cosine similarity, ranging between -1 and +1.}
\references{
Schoon, Eric, David Melamed, and Ronald L. Breiger. 2023. Regression Inside Out. NY: Cambridge University Press.
}
\author{
Ronald L. Breiger, David Melamed and Eric Schoon}
\examples{
data(Kenworthy99)
m1 <- lm(scale(dv) ~ scale(gdp) + scale(pov) + scale(tran) -1,data=Kenworthy99)
rp1 <- rio.plot(m1,include.int="no",r1=1:15)
cosine(rp1$row.dimensions[15,],rp1$row.dimensions[8,]) 
# cosine similarity between USA and Ireland

cosine(rp1$row.dimensions[15,],rp1$row.dimensions[14,]) 
# cosine similarity between USA and United Kingdom
}