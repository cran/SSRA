% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.ssra.R
\name{print.ssra}
\alias{print.ssra}
\title{Sakai Sequential Relation Analysis Print}
\usage{
\method{print}{ssra}(x, digits = 3, ...)
}
\arguments{
\item{x}{requires the result object of hssr function}

\item{digits}{integer indicating the number of decimal places to be used}

\item{...}{further arguments passed to or from other methods}
}
\description{
\code{print} function for the \code{ssra} object
}
\examples{
# Example data based on Takeya (1991)

# Sakai Sequential Relation Analysis
# ordering assesed according to the correlation coefficient and mean difference
exdat.ssra <- SSRA(exdat, output = FALSE)
print(exdat.ssra)
}
\author{
Takuya Yanagida \email{takuya.yanagida@univie.ac.at},
Keiko Sakai \email{keiko.sakai@oit.ac.jp}
}
\references{
Takeya, M. (1991). \emph{A new test theory: Structural analyses for educational information}.
Tokyo: Waseda University Press.
}
\seealso{
\code{\link{seqtable}}
}

