\name{aggregated.index.number}
\alias{aggregated.index.number}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate an aggregate index number
}
\description{
This function determines index numbers without weights for those cases in which there exist more than an only product (in chain or in serie)
}
\usage{
aggregated.index.number(x, base, type, name,opt.plot=FALSE, opt.summary=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
It is a matrix containing that magnitude to be studied. In each column, it contains the magnitud of a different product. Thus, we have \code{nrow(x)} values of a magnitud for \code{ncol(x)} products.
}
  \item{base}{
Chain of characters that indicates the nature of the index number. If we introduce \code{base="serie"}, we compare each value with respect to the initial one. In this case, it is said to be an index number in serie. Otherwise, if we introduce \code{base="chain"}, we obtain the index number in chain, by comparing each value with the immediately previous value.
}
  \item{type}{
Chain of characters to indicate the type of non-weighted index number to evaluate the evolution of a set of magnitudes (even for different products).

By considering \code{base="serie"}, if we introduce \code{type="arithmetic"}, we obtain the Sauerbeck index number. If we introduce \code{type="geometric"}, we obtain the Geometric index. If we choose \code{type="harmonic"}, we obtain the Harmonic mean index. If we write \code{type="BDutot"}, we will obtain the Bradstreet-Dutot index.

Otherwise, if we take \code{base="chain"} and \code{type="Carli"}, we obtain the Carli index number. If we introduce \code{type="Jevons"}, we obtain the Jevons index and if we choose \code{type="Dutot"}, we obtain the Dutot index.
}
  \item{name}{
Chain of characters to indicate the name of the variable under study.
}
  \item{opt.plot}{
Logical option to indicate if a graphical description of the index number along the different stages is required. It takes the value \code{TRUE} or \code{FALSE}.
}
  \item{opt.summary}{
Logical option to indicate if a statistical summary of the index number is required. It takes the value \code{TRUE} or \code{FALSE}.
}
}
\value{
\item{Summary}{Statistical summary (optional) of the index number along the considered period.}
\item{Agg. index number}{Table containing the values of the index number for the considered stages and the aggregate value.}
\item{Graphical}{Graphical summary (optional) of the index number along the considered period.}
}
\author{
A. Saavedra-Nieves, P. Saavedra-Nieves
}
\references{
CPI Manual (2004). Consumer Price Index Manual: Theory and Practice. OECD, UN, Eurostat, and TheWorld Bank by ILO, Geneva.

Index Number (2008). In: The Concise Encyclopedia of Statistics. Springer, New York, NY. <doi:https://doi.org/10.1007/978-0-387-32833-1>
}
\examples{
prices<-matrix(c(70,75,77,77,85,90,85,75,77,87,85,90,70,75,77,77,85,90),ncol=3)
aggregated.index.number(prices,"chain","geometric","Price",opt.plot=TRUE,opt.summary=TRUE)
}
\keyword{aggregated}
