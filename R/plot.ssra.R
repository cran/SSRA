#' Plot ssra
#'
#' Function for plotting the ssra object
#'
#' Takea Semantic Structure Analysis (TSSA) and Sakai Sequential Relation Analysis (SSRA) are graphical approaches
#'
#' @param x         requires the return object from the SSRA function
#' @param r.crt     minimal absolute correlation to be judged 'sequential'
#' @param r.sig     plot statistically significant correlations
#' @param d.sq      minimal effect size Cohen's d to be judged 'sequential'
#' @param m.sig     plot statistically significant mean difference
#' @param sig.col   significance in different colors
#' @param col       color code or name
#' @param pch       plotting character
#' @param mar       number of lines of margin to be specified on the four sides of the plot
#' @param ...       further arguments passed to or from other methods
#'
#' @author
#' Takuya Yanagida
#' Keiko Sakai
#'
#' @seealso
#' \code{\link{SSRA}}, \code{\link{treegram}}, \code{\link{scatterplot}}
#'
#' @references
#' Takeya, M. (1991). \emph{A new test theory: Structural analyses for educational information}.
#' Tokyo: Waseda University Press.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example data based on Takeya (1991)
#'
#' # Sakai Sequential Relation Analysis
#' # ordering assesed according to the correlation coefficient and mean difference
#' exdat.ssra <- SSRA(exdat, output = FALSE)
#' plot(exdat.ssra)
#' }
plot.ssra <- function(x, r.crt = NULL, r.sig = TRUE, d.sq = NULL, m.sig = TRUE,
                      sig.col = TRUE, col = c("red2", "green4", "blue3", "black"),
                      pch = c(1, 2, 0, 4), mar = c(3.5, 3.5, 1.5, 1), ...) {

  #--------------------------------------------------------------------------------------------------------------------#

  # Check class of object
  if (!inherits(x, what = "ssra")) {

    stop("Input is not a ssra object")

  }

  #------------------------------------------------#

  if (is.null(r.crt)) { r.crt <- x$args$r.crt }
  if (is.null(d.sq)) { d.sq <- x$args$d.sq }

  object <- x$restab

  if (sig.col == TRUE) {

    object$col <- ifelse(object$r.sig == 1 & object$m.diff.sig == 1, col[1],
                         ifelse(object$r.sig == 1 & object$m.diff.sig == 0, col[2],
                         ifelse(object$r.sig == 0 & object$m.diff.sig == 1, col[3], col[4])))

    object$pch <- ifelse(object$r >= r.crt & object$m.diff.eff >= d.sq, pch[1],
                         ifelse(object$r >= r.crt & object$m.diff.eff < d.sq, pch[2],
                         ifelse(object$r < r.crt & object$m.diff.eff >= d.sq, pch[3], pch[4])))

  } else {

    object$pch <- ifelse(object$r.sig == 1 & object$m.sig == 1, pch[1],
                         ifelse(object$r.sig == 1 & object$m.sig == 0, pch[2],
                                ifelse(object$r.sig == 0 & object$m.sig == 1, pch[3], pch[4])))

    object$col <- ifelse(object$r >= r.crt & object$m.eff >= d.sq, col[1],
                         ifelse(object$r >= r.crt & object$m.eff < d.sq, col[2],
                                ifelse(object$r < r.crt & object$m.eff >= d.sq, col[3], col[4])))

  }

  ###

  lim.x <- c(floor(min(object$r)*10)/10, ceiling(max(object$r)*10)/10)
  lim.y <- c(0, ceiling(max(object$m.diff.eff)*10)/10)

  dev.par <- par()$mar
  par(mar = mar)

  plot(object$r, object$m.diff.eff, axes = FALSE, col = object$col, pch = object$pch,
       xlim = lim.x, ylim = lim.y, xlab = "", ylab = "")

  axis(1, at = seq(lim.x[1], lim.x[2], by = 0.1), pos = 0)
  axis(2, at = seq(lim.y[1], lim.y[2], by = 0.1), pos = lim.x[1])

  lines(c(r.crt, r.crt), lim.y)
  lines(lim.x, c(d.sq, d.sq))

  mtext("Correlation coefficient", 1, line = 1.75)
  mtext(expression(paste("Mean difference (Cohen's  ", italic(d), ")")), 2, line = 1.75)

  par(mar = dev.par)

  ###

  seq.rel <- sum(object$r.sig == 1 & object$m.diff.sig == 1 & object$r >= r.crt & object$m.diff.eff >= d.sq)
  cat("\n Number of sequential relations: ",
      seq.rel, "/", nrow(object), " (", round(seq.rel/nrow(object), digits = 2) * 100, "%)\n\n", sep = "")

}
