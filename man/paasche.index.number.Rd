\name{paasche.index.number}
\alias{paasche.index.number}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the Paasche index number
}
\description{
This function determines the Paasche index number for those cases in which there exist more than an only product.
}
\usage{
paasche.index.number(x, y, name, opt.plot = FALSE, opt.summary = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
It is a matrix containing that magnitude to be studied. In each column, it contains the magnitud of a different product. Thus, we have \code{nrow(x)} values of a magnitud for \code{ncol(x)} products.
}
  \item{y}{
It is a matrix containing that magnitude used as weight. In each column, it contains another magnitud of the different products along the time. Thus, we have \code{nrow(x)} values of a magnitud for \code{ncol(x)} products.
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
Paasche, H. (1874) Uber die Preisentwicklung der letzten Jahre nach den Hamburger Borsen-notirungen. Jahrb. Natl. Stat. 23, 168--178.
}
\examples{
prices<-matrix(c(70,75,77,77,85,90,85,75,77,87,85,90,70,75,77,77,85,90),ncol=3)
weights<-matrix(c(1,1.5,0.8,1.1,1,0.9,0.7,0.8,0.6,1,1.1,0.9,1,1,0.9,1.1,0.6,0.8),ncol=3)
paasche.index.number(prices,weights,"Price",opt.plot=TRUE,opt.summary=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{paasche}% use one of  RShowDoc("KEYWORDS")
\keyword{paasche}% __ONLY ONE__ keyword per line
