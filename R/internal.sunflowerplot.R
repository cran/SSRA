##########################################################################################################
#
# SSRA: Sakai Squential Relation Analysis
#
# Internal function: internal.sizeplot
#
# Takuya Yanagida <akuya.yanagida@univie.ac.at>
# Keiko Sakai <keiko.sakai@oit.ac.jp>
#
internal.sunflowerplot <- function (x, y = NULL, number, log = "", digits = 6L, xlab = NULL,
          ylab = NULL, xlim = NULL, ylim = NULL, add = TRUE, rotate = FALSE,
          pch = 16, cex = 0.8, cex.fact = 1.5, col = par("col"), bg = NA,
          size = 1/8, seg.col = 2, seg.lwd = 1.5, ...) {

  xlabel <- if (!missing(x))
    deparse(substitute(x))
  ylabel <- if (!missing(y))
    deparse(substitute(y))
  is.xyn <- (is.list(x) && all(c("x", "y", "number") %in% names(x)))
  xy <- if (is.xyn) {
    number <- x$number
    x
  } else xy.coords(x, y, xlabel, ylabel, log)
  if (!add) {
    xlab <- if (is.null(xlab))
      xy$xlab
    else xlab
    ylab <- if (is.null(ylab))
      xy$ylab
    else ylab
    xlim <- if (is.null(xlim))
      range(xy$x[is.finite(xy$x)])
    else xlim
    ylim <- if (is.null(ylim))
      range(xy$y[is.finite(xy$y)])
    else ylim
  }
  n <- length(xy$x)
  if (missing(number)) {
    tt <- xyTable(xy, digits = digits)
    x <- tt$x
    y <- tt$y
    number <- tt$number
  } else {
    if (length(number) != n)
      stop("'number' must have same length as 'x' and 'y'")
    np <- number > 0
    x <- xy$x[np]
    y <- xy$y[np]
    number <- number[np]
  }
  n <- length(x)
  dev.hold()
  on.exit(dev.flush())
  if (!add)
    plot(x, y, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim,
         log = log, type = "n", ...)
  n.is1 <- number == 1
  if (any(n.is1))
    points(x[n.is1], y[n.is1], pch = pch, col = col, bg = bg, cex = cex)
  if (any(!n.is1)) {
    points(x[!n.is1], y[!n.is1], pch = pch, col = col, bg = bg,
           cex = cex/cex.fact)
    i.multi <- (1L:n)[number > 1]
    ppin <- par("pin")
    pusr <- par("usr")
    xr <- size * abs(pusr[2L] - pusr[1L])/ppin[1L]
    yr <- size * abs(pusr[4L] - pusr[3L])/ppin[2L]
    i.rep <- rep.int(i.multi, number[number > 1])
    z <- numeric()
    for (i in i.multi) z <- c(z, 1L:number[i] + if (rotate) stats::runif(1) else 0)
    deg <- (2 * pi * z)/number[i.rep]
    segments(x[i.rep], y[i.rep], x[i.rep] + xr * sin(deg),
             y[i.rep] + yr * cos(deg), col = seg.col, lwd = seg.lwd)
  }
  invisible(list(x = x, y = y, number = number))
}
