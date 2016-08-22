#' Scatterplot Matrices
#'
#' This function produces a scatterplot matrix
#'
#' Using a scatterplot matrix, an overview of the answer patterns for the pairs of items can be taken.
#'
#' @param data    a data frame
#' @param select  select items to be plotted
#' @param type    type of plot, i.e., 'jitter', 'size', 'count', and 'sun'
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at},
#' Keiko Sakai \email{keiko.sakai@@oit.ac.jp}
#'
#' @seealso
#' \code{\link{TSSA}}, \code{\link{SSRA}}
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
#' # Select items to be plotted
#' scatterplot(exdat, select = c("Item2", "Item3", "Item4"))
#'
#' # Scatterplot matrix: jitter
#' scatterplot(exdat)
#'
#' # Scatterplot matrix: size
#' scatterplot(exdat, type = "size")
#'
#' # Scatterplot matrix: count
#' scatterplot(exdat, type = "count")
#'
#' # Scatterplot matrix: sun
#' scatterplot(exdat, type = "sun")
scatterplot <- function(data, select = NULL,
                        type = c("jitter", "size", "count", "sun")) {

  #--------------------------------------------------------------------------------------------------------------------#

  data <- na.omit(data)

  ###

  if (!is.null(select)) {

    if (length(select) == 1) {

      stop("Select at least two items")

    }

    data <- data[, select]

  }

  ###

  type <- ifelse(all(c("jitter", "size", "count", "sun") %in% type), "jitter", type)

  if (type == "jitter") {

    pairs(apply(data, 2, jitter))

  }

  if (type == "size") {

    pairs(data, panel = internal.sizeplot)

  }

  if (type == "count") {

    pairs(data, panel = internal.count.overplot)

  }

  if (type == "sun") {

    pairs(data, panel = internal.sunflowerplot)

  }

}
