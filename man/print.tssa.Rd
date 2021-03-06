% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.tssa.R
\name{print.tssa}
\alias{print.tssa}
\title{Semantric Structure Analysis Print}
\usage{
\method{print}{tssa}(x, digits = 3, ...)
}
\arguments{
\item{x}{requires the result object of hssr function}

\item{digits}{integer indicating the number of decimal places to be used}

\item{...}{further arguments passed to or from other methods}
}
\description{
\code{print} function for the \code{tssa} object
}
\examples{
# Example data based on Takeya (1991)

# Takeya Semantic Structure Analysis
# ordering assesed according to the ordering coefficient
exdat.tssa <- TSSA(exdat, m = 5, output = FALSE)
print(exdat.tssa)

# Takeya Semantic Structure Analysis including statistical testing
# ordering assesed according to the ordering coefficient and statistical significance
exdat.tssa <- TSSA(exdat, m = 5, sig = TRUE, output = FALSE)
print(exdat.tssa)
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

