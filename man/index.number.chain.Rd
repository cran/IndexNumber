\name{index.number.chain}
\alias{index.number.chain}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate an index number in chain
}
\description{
This function determines index numbers  ``in chain'' for those cases with an only product.
}
\usage{
index.number.chain(x, name, opt.plot = FALSE, opt.summary = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
It is a vector containing that magnitude to be studied for a product. Thus, we have \code{length(x)} values of it.
}
 \item{name}{
Chain of characters to indicate the name of the variable under study.
}
  \item{opt.plot}{
Logical option to indicate if a graphical descriptiony of the index number along the different stages is required. It takes the value \code{TRUE} or \code{FALSE}.
}
  \item{opt.summary}{
Logical option to indicate if a statistical summary of the index number is required. It takes the value \code{TRUE} or \code{FALSE}.
}
}
\value{
\item{Summary}{Statistical summary (optional) of the index number along the considered period.}
\item{Index number}{Table containing the values of the index number for the considered stages.}
\item{Graphical}{Graphical summary (optional) of the index number along the considered period.}
}
\author{
A. Saavedra-Nieves, P. Saavedra-Nieves
}
\references{
Index Number (2008) In: The Concise Encyclopedia of Statistics. Springer, New York, NY. <doi:https://doi.org/10.1007/978-0-387-32833-1>.
}
\examples{
prices<-c(70,75,77,77,85,90,85,75,77,87,85,90,70,75,77,77,85,90)
index.number.chain(prices,"Prices",opt.plot=TRUE,opt.summary=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{indexnumberchain}% use one of  RShowDoc("KEYWORDS")
\keyword{indexnumberchain}% __ONLY ONE__ keyword per line
