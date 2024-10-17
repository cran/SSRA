#' Scatterplot Matrices
#'
#' This function produces a scatterplot matrix
#'
#' Takea Semantic Structure Analysis (TSSA) and Sakai Sequential Relation Analysis (SSRA) are graphical approaches
#'
#' @param data    a data frame
#' @param type    type of plot, i.e., 'jitter', 'size', 'count', and 'sun'
#'
#' @author
#' Takuya Yanagida
#' Keiko Sakai
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
scatterplot <- function(data, type = c("jitter", "size", "count", "sun")) {

  #--------------------------------------------------------------------------------------------------------------------#

  data <- na.omit(data)

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

  #------------------------------------------------#

}
