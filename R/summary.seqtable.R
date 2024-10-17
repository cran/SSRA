#' Sequential Relationship Table Summary
#'
#' \code{summary} function for the \code{seqtab} object
#'
#' Takea Semantic Structure Analysis (TSSA) and Sakai Sequential Relation Analysis (SSRA) are graphical approaches
#'
#' @param object  requires the result object of seqtable function
#' @param exclude exclude lower-order paths (i.e., paths included in higher order paths)?
#' @param ...     additional arguments affecting the summary produced
#'
#' @author
#' Takuya Yanagida
#' Keiko Sakai
#'
#' @return
#' \tabular{ll}{
#'    \code{rel}  \tab relationship: sq = sequential / eq = equal \cr
#'    \code{var}  \tab variables involved in the sequential/equal paths \cr
#' }
#'
#' @seealso
#' \code{\link{SSRA}}, \code{\link{TSSA}}
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
#' exdat.seqtab<- seqtable(exdat.ssra, output = FALSE)
#' summary(exdat.seqtab)
summary.seqtable <- function(object, exclude = TRUE, ...) {

  #------------------------------------------------------------------------------------------------------#

  # Check class of object
  if (!inherits(object, what = "seqtable")) {

    stop("Input is not a seqtable object")

  }

  #------------------------------------------------#

  pos <- which(apply(object$restab[, -c(1:2)], 1, function(x) !all(x == "")))

  object <- data.frame(object$restab[pos, 1:2], object$restab[pos, -c(1:2)][, pos],
                       stringsAsFactors = FALSE)

  object <- object[order(object$mean, decreasing = TRUE), ]
  itemnam <- object$item

  for (i in length(itemnam):2) {

    if (i == length(itemnam)) {

    comb <- combn(itemnam, i)
    temp <- sapply(paste0(paste0("object[object$item == ", "\"", comb[-length(itemnam)], "\""), ", ", "\"", comb[-1], "\"", "]"),
                   function(x) eval(parse(text = x)))

    assign(paste0("seq_", i), if(all(temp == "-")) { paste(comb, collapse = " -> ") } )

    } else

      if (i > 2) {

      eval(parse(text = paste0("seq_", i, " <- NULL")))
      comb <- combn(itemnam, i)
      for (j in 1:ncol(comb)) {

         comb.temp <- comb[, j]
         temp <- sapply(paste0(paste0("object[object$item == ", "\"", comb.temp[-length(comb.temp)], "\""), ", ", "\"", comb.temp[-1], "\"", "]"),
                        function(x) eval(parse(text = x)))

         eval(parse(text = "assign(paste0(\"seq_\", i), c(eval(parse(text = paste0(\"seq_\", i))), if(all(temp == \"-\")) { paste(comb.temp, collapse = \" -> \") }))"))

      }

    } else

      if (i == 2) {

      eval(parse(text = paste0("seq_", i, " <- NULL")))
      comb <- combn(itemnam, i)
      for (j in 1:ncol(comb)) {

        comb.temp <- comb[, j]
        temp <- sapply(paste0(paste0("object[object$item == ", "\"", comb.temp[-length(comb.temp)], "\""), ", ", "\"", comb.temp[-1], "\"", "]"),
                       function(x) eval(parse(text = x)))

        eval(parse(text = "assign(paste0(\"seq_\", i), c(eval(parse(text = paste0(\"seq_\", i))), if(all(temp == \"-\")) { paste(comb.temp, collapse = \" -> \") }))"))

      }

    }

  }

  eq_2 <- NULL
  comb <- combn(itemnam, 2)
  for(j in 1:ncol(comb)) {

    comb.temp <- comb[, j]
    temp <- sapply(paste0(paste0("object[object$item == ", "\"", comb.temp[-length(comb.temp)], "\""), ", ", "\"", comb.temp[-1], "\"", "]"),
                   function(x) eval(parse(text = x)))

    eval(parse(text = "assign(\"eq_2\", c(eval(parse(text = \"eq_2\")), if(all(temp == \"=\")) { paste(comb.temp, collapse = \" = \") }))"))

  }

  ###

  seq <- names(which(unlist(lapply(sapply(paste0("seq_", length(itemnam):2), function(x) eval(parse(text = x))), function(y) !is.null(y)))))
  seq.n <- matrix(unlist(strsplit(seq, "_")), ncol = 2, byrow = TRUE)[, 2]

  ###

  if (exclude == TRUE) {

    for (i in (length(seq)):2) {

       x.seq <- eval(parse(text = seq[i]))
       y.seq <- eval(parse(text = seq[i - 1]))

       temp <- sapply(x.seq, function(x) grep(x, y.seq))
       assign(seq[i], x.seq[!x.seq %in% names(temp[unlist(lapply(temp, function(x) length(x) != 0))])])

    }

  }

  ###

  for (i in seq.n) {

      cat("\n Sequential path: ", i, " variables (n = ", eval(parse(text = paste0("length(seq_", i, ")"))), ")\n", sep = "",
          sapply(eval(parse(text = paste0("seq_", i))), function(x) paste("\t", x, "\n")))

  }

  if (!is.null(eq_2)) {

     cat("\n Equal path: 2 variables (n = ", length(eq_2), ")\n", sep = "",
         sapply(eq_2, function(x) paste("\t", x, "\n")))

    } else {

      cat("\n")

    }

  if (!is.null(eq_2)) { cat("\n") }

  ###

  seq.max <- max(as.numeric(seq.n))

  output <- NULL
  if (seq.max != 2) {

    for (i in length(seq):2) {

        output <- rbind(output, cbind(seq[i],
                        matrix(unlist(lapply(strsplit(eval(parse(text = seq[i])), "->"), function(x) c(x, rep(NA, times = seq.max - length(x))))),
                        ncol = seq.max, byrow = TRUE)))

    }

  }

  output <- rbind(output, cbind(seq[1], matrix(unlist(strsplit(eval(parse(text = seq[1])), "->")),
                                               ncol = seq.max, byrow = TRUE)))

  if (!is.null(eq_2)) {

     output <- rbind(output, cbind("eq", matrix(unlist(lapply(strsplit(eq_2, "="), function(x) c(x, rep(NA, times = seq.max - length(x))))),
                                                 ncol = seq.max, byrow = TRUE)))
  }

  output <- data.frame(matrix(sapply(output, stringr::str_trim), ncol = seq.max + 1,
                       dimnames = list(NULL, c("rel", paste0("var", 1:seq.max)))),
                       stringsAsFactors = FALSE)

  return(invisible(output))

}
