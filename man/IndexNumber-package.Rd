\name{IndexNumber-package}
\alias{IndexNumber-package}
\alias{IndexNumber}
\docType{package}
\title{
\packageTitle{IndexNumber}
}
\description{
\packageDescription{IndexNumber}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{IndexNumber}
\packageIndices{IndexNumber}
Once we have defined a magnitude for a product (of several products), we can analyse how it (they) evolves along the time. Index Numbers model this effect in Social Science. In this sense, several approaches may be considered. We include in this package several options of analysing this problem.
}
\author{
\packageAuthor{IndexNumber}

Maintainer: \packageMaintainer{IndexNumber}
}
\keyword{IndexNUMBER}

\references{
Index Number (2008) In: The Concise Encyclopedia of Statistics. Springer, New York, NY. <doi:https://doi.org/10.1007/978-0-387-32833-1>.

Laspeyres, E. (1871) Die Berechnung einer mittleren Waarenpreissteigerung. Jahrb. Natl. Stat. 16, 296--314.

Paasche, H. (1874) Uber die Preisentwicklung der letzten Jahre nach den Hamburger Borsen-notirungen. Jahrb. Natl. Stat. 23, 168--178.
}

\examples{
prices<-c(70,75,77,77,85,90,85,75,77,87,85,90,70,75,77,77,85,90)
index.number.serie(prices,name="Prices",opt.plot=TRUE,opt.summary=TRUE)
}
