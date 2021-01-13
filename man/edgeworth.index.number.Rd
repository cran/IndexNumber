\name{edgeworth.index.number}
\alias{edgeworth.index.number}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the Edgeworth index number
}
\description{
This function determines the Marshall-Edgeworth index number for those cases in which there exist more than an only product.
}
\usage{
edgeworth.index.number(x, y, name, opt.plot = FALSE, opt.summary = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Matrix that contains the magnitude(s) under study. In each column, it contains the magnitude of a different product considered. Thus, we have \code{nrow(x)} values of a magnitude for \code{ncol(x)} products.
}
  \item{y}{
Matrix that contains that magnitude used as weight. In each column, it contains another magnitude associated to each different product along the time. Thus, we have \code{nrow(x)} values of  magnitudes for the set of \code{ncol(x)} products.
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
\item{Agg. index number}{Table containing the values of the index number for the considered stages and the aggregate value.}
\item{Graphical}{Graphical summary (optional) of the index number along the considered period.}
}
\author{
A. Saavedra-Nieves, P. Saavedra-Nieves
}
\references{
Edgeworth, F. (1887) Measurement of change in value of money i. First Memorandum presented to the
British Association for the Advancement of Science. Reprinted in his Papers Relating to Political Economy, 1, 198--259.

 Marshall, A. (1887) Remedies for fluctuations of general prices. The Contemporary review, 1866-1900, 51, 355--375.
}
\examples{
prices<-matrix(c(70,75,77,77,85,90,85,75,77,87,85,90,70,75,77,77,85,90),ncol=3)
weights<-matrix(c(1,1.5,0.8,1.1,1,0.9,0.7,0.8,0.6,1,1.1,0.9,1,1,0.9,1.1,0.6,0.8),ncol=3)
edgeworth.index.number(prices,weights,"Price",opt.plot=TRUE,opt.summary=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{edgeworth}% use one of  RShowDoc("KEYWORDS")
\keyword{edgeworth}% __ONLY ONE__ keyword per line
