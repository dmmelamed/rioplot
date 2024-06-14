\name{project.point}
\alias{project.point}
\title{ Project point 1 onto the line (at 90 degress) running through point 2 and the origin (0,0).
}
\description{
Given two points, p1 and p2, this function identifies the point at which p1 is projected onto the line connecting p2 and the origin (0,0). The projection occurs at a right angle.}
\usage{
project.point(p1,p2)}
\arguments{
  \item{p1}{
First point, the one that is to be projected onto point 2.}
  \item{p2}{Second point, the one that is projected to the origin. This is the outcome or dependent variable in our book. See reference below.}
}
\details{The output is just a single point. This is implemented as the point to which lines are drawn in many graphs.}
\value{
Two values which correspond to the x and y co-ordinates in the graph.
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
rp1 <- rio.plot(m1,include.int="no",r1=1:15)
project.point(as.numeric(rp1$col.dimensions[1,]),as.numeric(rp1$row.dimensions[1,]))
}