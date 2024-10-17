#' Sakai Sequential Relation Analysis Print
#'
#' \code{print} function for the \code{ssra} object
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
#' # Sakai Sequential Relation Analysis
#' # ordering assesed according to the correlation coefficient and mean difference
#' exdat.ssra <- SSRA(exdat, output = FALSE)
#' print(exdat.ssra)
print.ssra <- function(x, digits = 3, ...) {

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

  cat("\n Sakai Sequential Relation Analysis", "\n\n ",

      ifelse(x$args$method == "pearson", "Product-moment correlation coefficient",
             ifelse(x$args$method == "spearman", "Spearman correlation", "Kendall-Tau")), "\n",
      "   Correlation coefficient criterion to be judged 'sequential' or 'equivalent:", x$args$r.crt, "\n\n",

      "  Paired t-test", "\n",
      "   Absolute mean difference criterion to be judged 'sequential':", x$args$mu.sq, "\n",
      "   Maximal absolute mean difference to be judged 'equivalent':", x$args$mu.eq, "\n",
      "   Effect size for mean difference criterion to be judged 'sequential':", x$args$d.sq, "\n",
      "   Maximal effect size for mean difference to be judged 'equivalent':", x$args$d.eq, "\n\n",

      "  Significance level:", x$args$alpha * 100, "%", "\n")

  if (x$args$p.adjust.method != "none") {

    cat("    Correction method for multiple comparisons:", paste0(toupper(substr(x$args$p.adjust.method, 1, 1)),
                                                                  substr(x$args$p.adjust.method, 2, nchar(x$args$p.adjust.method))), "\n\n")

  } else {

    cat("    No alpha protection for multiple testing", "\n\n")

  }

  #------------------------------------------------#
  # Create object result.d

  result <- x$restab

  #------------------------------------------------#
  # Object result.d

  result.d <- data.frame(result[, c("j", "k", "n")],
                         round(result[, c("j.mean", "j.sd", "k.mean", "k.sd", "r", "r.t")], digits = digits),
                         r.p = formatC(result[, "r.p"], format = "f", digits = 3),
                         r.sig = result[, "r.sig"], r.crt = result[, "r.crt"],
                         round(result[, c("m.diff", "sd.diff", "m.diff.eff", "m.diff.t")], digits = digits),
                         m.diff.p = formatC(result[, "m.diff.p"], format = "f", digits = 3),
                         m.diff.sig = result[, "m.diff.sig"],
                         result[, c("m.diff.crt.sq", "m.diff.crt.eq")],
                         result[, c("seq", "eq", "order")],
                         stringsAsFactors = FALSE)

  # Exclude paths with no relationship
  if (x$args$exclude == TRUE) {

    result.d <- result.d[result.d$order != "", ]
    row.names(result.d) <- NULL

  }

  print(result.d)

}
