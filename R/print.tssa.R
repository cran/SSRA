#' Semantric Structure Analysis Print
#'
#' \code{print} function for the \code{tssa} object
#'
#' Takea Semantic Structure Analysis (TSSA) and Sakai Sequential Relation Analysis (SSRA) are graphical approaches
#'
#' @param x        requires the result object of hssr function
#' @param digits   integer indicating the number of decimal places to be used
#' @param ...      further arguments passed to or from other methods
#'
#' @author
#' Takuya Yanagida
#' Keiko Sakai
#'
#' @seealso
#' \code{\link{seqtable}}
#'
#' @references
#' Takeya, M. (1991). \emph{A new test theory: Structural analyses for educational information}.
#' Tokyo: Waseda University Press.
#'
#' @export
#'
#' @examples
#' # Example data based on Takeya (1991)
#'
#' # Takea Semantic Structure Analysis
#' # ordering assesed according to the ordering coefficient
#' exdat.tssa <- TSSA(exdat, m = 5, output = FALSE)
#' print(exdat.tssa)
#'
#' # Takea Semantic Structure Analysis including statistical testing
#' # ordering assesed according to the ordering coefficient and statistical significance
#' exdat.tssa <- TSSA(exdat, m = 5, sig = TRUE, output = FALSE)
#' print(exdat.tssa)
print.tssa <- function(x, digits = 3, ...) {

  #--------------------------------------------------------------------------------------------------------------------#

  dat <- x$dat

  # Check for missing values
  if (any(is.na(dat))) {

    if (x$args$pairwise == TRUE) {

      warning("Data matrix contains missing values. Pairwise deletion applied for missing values")

    } else {

      # Listwise deletion
      dat <- na.omit(dat)

      warning("Data matrix contains missing values. Listwise deletion applied for missing values")

    }

  }

  cat("--------------------------------------------------------------------------\n")
  cat(" Call:    "); print(x$call)
  cat(" Time:   ", x$time, "\n")
  cat(" R:      ", x$R, "\n")
  cat(" Package:", x$package, "\n")
  cat("--------------------------------------------------------------------------\n\n")

  ###

  cat("Sequential Relationship Analysis", "\n\n",
      "  Number of response categories:", x$args$m, "\n",
      "  Criteria for ordering coefficient:", x$args$crit, "\n")

  if (x$args$sig == TRUE) {

    if (x$args$exact == TRUE) {

      cat("\n", "  Exact Binomial Test", "\n",
          "    Sig. level =", x$args$alpha * 100, "%", "\n")

    } else {

      cat("\n", "  Single-Sample Proportion Test", "\n",
          "    Sig. level =", x$args$alpha * 100, "%", "\n")

    }

    if (x$args$p.adjust.method != "none") {

      cat("     Correction method for multiple comparisons:", x$args$p.adjust.method, "\n\n",
          "  Ordering based on ordering coefficient and statistical significance", "\n\n")

    } else {

      cat("     No alpha protection for multiple testing", "\n\n",
          "  Ordering based on ordering coefficient and statistical significance", "\n\n")

    }

  } else{

    cat("\n", "Ordering based on ordering coefficient", "\n\n")

  }

  #------------------------------------------------#
  # Create object result.d

  result <- x$restab

  if (x$args$sig == TRUE) {

    result.d <- cbind(result[, c("j", "k", "n")], round(result[, c("j.mean", "j.sd", "k.mean", "k.sd", "c.jk")], digits = digits),
                      p.jk = formatC(result[, "p.jk"], format = "f", digits = 3), sig.jk = result[, "sig.jk"],
                      c.kj = round(result[, "c.kj"], digits = digits),
                      p.kj = formatC(result[, "p.kj"], format = "f", digits = 3),
                      result[, c("sig.kj", "crt.jk", "crt.kj", "order")])

  } else {

    result.d <- cbind(result[, c("j", "k", "n")], round(result[, c("j.mean", "j.sd", "k.mean", "k.sd", "c.jk")], digits = x$args$digits),
                      c.kj = round(result[, "c.kj"], digits = x$args$digits),
                      result[, c("crt.jk", "crt.kj", "order")])

  }

  #------------------------------------------------#
  # Exclude paths with no relationship

  if (x$args$exclude == TRUE) {

    result.d <- result.d[result.d$order != "", ]
    row.names(result.d) <- NULL

  }

  print(result.d)

}
