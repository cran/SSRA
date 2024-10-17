#' Treegram
#'
#' This function draws a treegram for the Takea Semantic Structure Analysis (TSSA) and Sakai Sequential Relation Analysis (SSRA)
#'
#' Takea Semantic Structure Analysis (TSSA) and Sakai Sequential Relation Analysis (SSRA) are graphical approaches
#'
#' @param object     requires the result object of seqtab function
#' @param select     select items to be plotted
#' @param pos  		 	 position of items on the x-axis
#' @param col        color code or name for paths
#' @param mai        numeric vector of the form c(bottom, left, top, right)
#'                   which gives the margin size specified in inches
#' @param print.pos  display x/y-position as legend
#' @param cex.text   text expansion factor relative to current par("cex")
#' @param x.factor   shift factor of legend position
#' @param x.digits   decimal places of x-position
#' @param y.digits   decimal places of y-position
#' @param y.intersp  legend character interspacing factor for vertical (y) line distances
#' @param cex.legend legend character expansion factor relative to current par("cex)
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
#' exdat.seqtab <- seqtable(exdat.ssra, output = FALSE)
#' treegram(exdat.seqtab)
#'
#' # Select items to be plotted
#' exdat.ssra <- SSRA(exdat, output = FALSE)
#' exdat.seqtab <- seqtable(exdat.ssra, output = FALSE)
#' treegram(exdat.seqtab, select = c("Item2", "Item3", "Item4"))
#'
#' # Define position for each item on the x-axis
#' exdat.ssra <- SSRA(exdat, output = FALSE)
#' exdat.seqtab <- seqtable(exdat.ssra, output = FALSE)
#' treegram(exdat.seqtab, pos = c(Item5 = 1, Item4 = 3,
#'                                Item3 = 5, Item2 = 2, Item1 = 4))
#'
#' # Change colors for each path of an item
#' exdat.ssra <- SSRA(exdat, output = FALSE)
#' exdat.seqtab <- seqtable(exdat.ssra, output = FALSE)
#' treegram(exdat.seqtab,
#'          col = c(Item5 = "red3", Item4 = "blue3",
#'                  Item3 = "gray99", Item2 = "darkgreen", Item1 = "darkorange2"))
treegram <- function(object, select = NULL,
                     pos = NULL, col = NULL, mai = c(0.2, 0, 0.2, 0.2), print.pos = TRUE,
                     cex.text = 0.95, x.factor = 1.7, x.digits = 0, y.digits = 2, y.intersp = 1.45,
                     cex.legend = 0.9) {

  #------------------------------------------------------------------------------------------------------#

  # Check class of object
  if (!inherits(object, what = "seqtable")) {

    stop("Input is not a seqtable object")

  }

  #------------------------------------------------#

  object <- object$restab

  #------------------------------------------------#

  if (!is.null(select)) {

    if (any(!select %in% names(object))) {

      x <- select[!select %in% names(object)]
      stop(paste0("Items selected which are not part of the seqtable object: ", x))

    }

    select <- object$item[(object$item %in% select)]

    row.names(object) <- object$item
    object <- object[select, c("item", "mean", select)]

  }

  #------------------------------------------------#

  object[, -c(1:2)][lower.tri(object[, -c(1:2)])] <- ""
  object$mean <- object$mean*-1

  items.all <- object$item

  items.path <- items.all[apply(object[, -c(1:2)], 1, function(x) all(x == ""))]
  items.null <- items.all[apply(object[, -c(1:2)], 1, function(x) all(x == "")) &
                          apply(object[, -c(1:2)], 2, function(x) all(x == ""))]
  items.select <- items.all[!items.all %in% items.null]

  ###

  if (length(items.select) == 0) {

    stop("There are no relations between items to plot")

  } else {

    # Position
    if (is.null(pos)) {

      if (length(items.null) != 0) {

          x.pos <- rbind(data.frame(item = items.select,
                                    x.pos = sample(1:length(items.select), replace = FALSE)),
                         data.frame(item = items.null, x.pos = 0))

      } else {

        x.pos <- data.frame(item = items.select,
                            x.pos = sample(1:length(items.select), replace = FALSE))

      }

      object <- merge(object, x.pos, by = "item")

    } else {

      if (!is.null(names(pos))) {

          object$x.pos <- pos[match(object$item, names(pos))]

      } else {

        object$x.pos <- pos

      }

    }

    ###

    # Color
    if (is.null(col)) {

        object$col <- NA

        object[object$item %in% union(items.path, items.null), "col"] <- "#000000"
        col.exclude <- c(1:5, 13:15, 18:20, 24:25, 138, 149, 141, 53:253, 261:364, 377:379,
                         387:390, 394:396, 443:445, 449, 478, 492, 578:581, 605:607, 652:654)
        object[object$item %in% setdiff(items.select, items.path), "col"] <- sample(colors()[-col.exclude], length(setdiff(items.select, items.path)), replace = FALSE)

    } else {

      if (!is.null(names(col))) {

        object$col <- col[match(object$item, names(col))]

      } else {

        object$col <- col

      }

    }

    ###

    lim <- c(min(object$mean), max(object$mean) + 0.05)

    dev.par <- par()$mai
    par(mai = mai)

    if (print.pos == TRUE) {

      plot(1, type = "n", axes = FALSE,
           xlim = c(min(object$x.pos) - 0.5, max(object$x.pos)*x.factor),
           ylim = c(lim[1], (lim[2] + 0.05)),
           xlab = "", ylab = "")

      object$item.leg <- object$item

      nchar.d <- max(nchar(object$item)) - nchar(object$item)
      for (i in which(nchar.d != 0)) {

          object$item.leg[i] <- paste0(object$item.leg[i], rep("  ", times = nchar.d[i]))

      }

      ord <- order(object$mean, decreasing = TRUE)

      x <- 1:length(object$item.leg)
      x <- formatC(x, format = "f", width = max(nchar(x)), digits = 0)

      legend("topright", col = object$col[ord], lty = rep(1, times = 12),
             legend = paste0(paste0("Item ", x, ": ", object$item.leg[ord]), " (",
                             formatC(object$x.pos[ord], format = "f", width = max(nchar(object$x.pos)), digits = x.digits), ", ",
                             formatC(object$mean[ord] * -1, format = "f", digits = y.digits), ")" ),
             bty = "n", y.intersp = y.intersp, cex = cex.legend)

    } else {

      plot(1, type = "n", axes = FALSE,
           xlim = c(min(object$x.pos) - 0.5, max(object$x.pos)),
           ylim = c(lim[1], (lim[2] + 0.05)),
           xlab = "", ylab = "")

    }

    text(object$x.pos, object[, 2], object[, 1], cex = cex.text)

    ###

    move.l <- diff(lim)/30
    move.u <- diff(lim)/20

    # Sequential relations
    for (i in items.select) {

      temp <- object[object$item == i, items.select]

      rela.p <- which(temp == "+")
      for (j in names(temp[rela.p])) {

          coord.x <- unlist(object[object$item == i, c("x.pos", "mean")])
          coord.y <- unlist(object[object$item == j, c("x.pos", "mean")])

          shape::Arrows(coord.x[1], coord.x[2] - move.u, coord.y[1], coord.y[2] + move.l,
                        code = 1, col = object[object$item == i, "col"],
                        arr.type = "triangle")

      }

      # Equal relations
      rela.e <- which(temp == "=")
      for (j in names(temp[rela.e])) {

          coord.x <- unlist(object[object$item == i, c("x.pos", "mean")])
          coord.y <- unlist(object[object$item == j, c("x.pos", "mean")])

          lines(c(coord.x[1], coord.y[1]),
                c(coord.x[2] - move.l, coord.y[2] + move.l),
                  lwd = 1, lty = 2, col = 1)

      }

    }

    par(mai = dev.par)

  }

}
