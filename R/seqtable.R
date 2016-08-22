#' Sequential Relation Table
#'
#' This function builds a table for the tssa and ssra object used to create a treegram
#'
#' In this table, we can see how many 'sequential' or 'equal' relations each of items has
#' with the other items.
#'
#' @param object      requires the return object from the TSSA or SSRA function
#' @param order       sort by item mean of j?
#' @param digits      integer indicating the number of decimal places to be used
#' @param output      print result table?
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at},
#' Keiko Sakai \email{keiko.sakai@@oit.ac.jp}
#'
#' @seealso
#' \code{\link{TSSA}}, \code{\link{SSRA}}, \code{\link{treegram}}, \code{\link{summary.seqtable}}
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
#' # Takeya Semantic Structure Analysis
#' # ordering assesed according to the correlation coefficient and mean difference
#' exdat.tssa <- TSSA(exdat, m = 5, output = FALSE)
#' seqtable(exdat.tssa)
#'
#' # Sakai Sequential Relation Analysis
#' # ordering assesed according to the correlation coefficient and mean difference
#' exdat.ssra <- SSRA(exdat, output = FALSE)
#' seqtable(exdat.ssra)
seqtable <- function(object, order = c("no", "decreasing", "increasing"), digits = 3, output = TRUE) {

  #--------------------------------------------------------------------------------------------------------------------#

  # Check class of object
  if (!class(object) %in% c("tssa", "ssra")) {

    stop("Input is not a tssa or ssra object")

  }

  #------------------------------------------------#

  object <- object$restab

  temp <- rbind(data.frame(x = object$j, m = object$j.mean, stringsAsFactors = FALSE),
                data.frame(x = object$k, m = object$k.mean, stringsAsFactors = FALSE))

  items <- unique(temp$x)

  tab <- data.frame(item = items, mean = temp[match(items, temp$x), "m"],
                    matrix("", ncol = length(items), dimnames = list(NULL, items)),
                    stringsAsFactors = FALSE)

  #------------------------------------------------#

  object <- data.frame(j = object$j, k = object$k, order = object$order,
                       stringsAsFactors = FALSE)

  for (i in items) {

       temp <- object[object$j == i, c("k", "order")]

       tab[tab$item == i, na.omit(match(temp$k, names(tab)))] <- temp$order

  }

  ###

  object$order <- ifelse(object$order == "+", "-", ifelse(object$order == "-", "+", object$order))

  for (i in items) {

    temp <- object[object$k == i, c("j", "order")]

    tab[tab$item == i, na.omit(match(temp$j, names(tab)))] <- temp$order

  }

  # Order by item means
  order <- ifelse(all(c("no", "decreasing", "increasing") %in% order), order <- "increasing", order)

  if(order %in% c("decreasing", "increasing")){

     o <- order(tab$mean, decreasing = ifelse(order == "decreasing", TRUE, FALSE))
     tab <- tab[o, c("item", "mean", names(tab)[o + 2])]

  }

  if (output == TRUE) {

    tab.d <- tab
    tab.d[, "mean"] <- round(tab.d[, "mean"], digits = digits)
    row.names(tab.d) <- NULL

    print(tab.d)

  }

  # Return object
  object <- list(restab = tab)
  class(object) <- "seqtable"

  return(invisible(object))

}
