#' Takeya Semantic Structure Analysis
#'
#' This function conducts Takeya Semantic Structure Analysis (TSSA) for polytomous items based on
#' Takeya 1991.
#'
#' In Takeya Semantic Structure Analysis (TSSA), a pair of items (e.g., Item1 and Item2) is judged 'sequential',
#' if exceptional answer patterns are less than a defined criterion. If we suppose Item1 to be the item with higher
#' item mean than Item2 (i.e., 'Item1 -> Item2' in the treegram), 'exceptional answer patter' means that somone
#' gets a lower score on Item1 and a higher score on Item2. If this kind of sequential relation is bi-directional
#' (i.e., not only 'Item1 -> Item2' but also 'Item2 -> Item1'), the relation of the two items is judged 'equal'.
#'
#' @param dat		 	         requires a data frame with polytomous data,
#' 							           all items need to have the same numbers of response categories
#' @param m  		     		   requires the number of item response categories
#' @param crit			       criteria for ordering coefficient
#' @param pairwise         pairwise deletion of missing data,
#'                         if pairwise = FALSE listwise deletion if applied
#' @param sig  			       if sig = TRUE, ordering will be assesed according to
#'                         ordering coefficient and statistical significance
#' @param exact		         if exact = TRUE, exact binomial test will be applied
#'								         otherwise single-sample proportion test will be applied
#' @param alpha			       significance level
#' @param p.adjust.method  p-value correction method for multiple comparisons, see: ?p.adjust (default = holm)
#' @param digits           integer indicating the number of decimal places to be used
#' @param vnames		       use variable names for labeling?
#' @param order   		     sort by item mean of j and k?
#' @param exclude    	     exclude paths with no relationship?
#' @param output           print result table?
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#' Keiko Sakai \email{keiko.sakai@@oit.ac.jp}
#'
#' @return
#' Returns an object of class \code{tssa}, to be used for the \code{seqtable} function. The object is a list with
#' following entries: 'dat' (data frame), 'call" (function call), 'args' (specification of arguments),
#' 'time' (time of analysis), 'R' (R version), 'package' (package version), and 'restab' (result table).
#' The 'restab' entry has following entries:
#' \tabular{ll}{
#'    \code{j}       	\tab item j \cr
#'    \code{k}     		\tab item k \cr
#'    \code{n}				\tab sample size \cr
#'    \code{j.mean}	  \tab mean of item j \cr
#'    \code{j.sd}     \tab standard devication of item j \cr
#'    \code{k.mean}   \tab mean of item k \cr
#'    \code{k.sd}     \tab standard devication of item k \cr
#'    \code{c.jk}	   	\tab ordering coefficient j -> k \cr
#'    \code{p.jk}     \tab p-value j -> k  (available if \code{sig = TRUE}) \cr
#'    \code{sig.jk}   \tab statistical significane p-value j -> k  (0 = no / 1 = yes; available if sig = TRUE) \cr
#'    \code{c.kj}     \tab ordering coefficient k -> j \cr
#'    \code{p.kj}     \tab p-value k -> j  (0 = no / 1 = yes; available if \code{sig = TRUE}) \cr
#'    \code{sig.kj}   \tab statistical significane p-value k -> j (available if \code{sig = TRUE}) \cr
#'    \code{crt.jk}   \tab ordering j -> k \cr
#'    \code{crt.kj}   \tab ordering k -> j \cr
#'    \code{order}    \tab order structure of item pairs ("=", "+","-") \cr
#'  }
#'
#' @seealso
#' \code{\link{SSRA}}, \code{\link{seqtable}}, \code{\link{scatterplot}}
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
#' # ordering assesed according to the ordering coefficient
#' TSSA(exdat, m = 5)
#'
#' # Takeya Semantic Structure Analysis including statistical testing
#' # ordering assesed according to the ordering coefficient and statistical significance
#' TSSA(exdat, m = 5, sig = TRUE)
TSSA <- function(dat, m, crit = .93, pairwise = TRUE, sig = FALSE, exact = TRUE, alpha = 0.05,
                 p.adjust.method = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"),
                 digits = 3, vnames = TRUE, order = c("no", "decreasing", "increasing"),
                 exclude = TRUE, output = TRUE) {

  #------------------------------------------------------------------------------------------------------#

  # Check dat frame
  if (!is.data.frame(dat)) {

    stop("TSSA function requires a data frame")

  }

  # Check for missing values
  if (any(is.na(dat))) {

	   if (pairwise == TRUE) {

	      warning("Data matrix contains missing values. Pairwise deletion applied for missing values")

	   } else {

	    # Listwise deletion
	    dat <- na.omit(dat)

	    warning("Data matrix contains missing values. Listwise deletion applied for missing values")

	   }

  }

  # Number of observations
	if (nrow(dat) <= 1) {

	   stop("Not enough observations")

	}

	#------------------------------------------------#
	# Order data by item means

	order <- ifelse(all(c("no", "decreasing", "increasing") %in% order), order <- "increasing", order)

	if (order %in% c("decreasing", "increasing")) {

	   item.m <- data.frame(nr = 1:ncol(dat), i.mean = apply(dat, 2, mean, na.rm = TRUE), stringsAsFactors = FALSE)
	   item.m <- item.m[order(item.m$i.mean, decreasing = ifelse(order == "decreasing", TRUE, FALSE)), ]

	   dat <- dat[, item.m$nr]

	}

	#------------------------------------------------#
	# Generate all pairwise combinations

	comb <- combn(ncol(dat), m = 2)

	# Convert data in combination list and compute differences
	dat.list <- apply(comb, 2, function(x) dat[, x])

	# In case of missing values delete pairwise
	if (any(is.na(dat)) & pairwise == TRUE) {

	 	 dat.diff <- lapply(dat.list, function(x) na.omit(x[, 1] - x[, 2]))
	 	 mN <- unlist(lapply(dat.diff, function(x) length(x) * (m - 1)))

	} else {

		 dat.diff <- lapply(dat.list, function(x) x[, 1] - x[, 2])
		 mN <- unlist(lapply(dat.diff, function(x) length(x) * (m - 1)))

	}

	#------------------------------------------------#
	# Compute ordering coefficients

	jk.kj <- lapply(dat.diff, function(x) c(sum(x[x > 0]), sum(abs(x[x < 0]))))

	temp <- cbind(matrix(unlist(jk.kj), ncol = 2, byrow = TRUE), mN)
	c.jk.kj <- data.frame(jk = apply(comb, 2, paste, collapse = "->"), c.jk = 1 - temp[, 1]/temp[, "mN"], c.kj =  1 - temp[, 2]/temp[, "mN"],
	                      stringsAsFactors = FALSE)

  #------------------------------------------------#
	# Significance test

	p.adjust.method <- ifelse(all(c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none") %in% p.adjust.method),
	                          "holm", p.adjust.method)

	if (sig == TRUE) {

     if (exact == TRUE) {

    		 # Exact Binomial Test
    		 s.jk.kj <- data.frame(jk = apply(comb, 2, paste, collapse = "->"),
                               matrix(sapply(paste0("c(", mN - unlist(jk.kj), ", ", unlist(jk.kj), ")"),
  	 	 			 					                       function(x) binom.test(eval(parse(text = x)), p = crit, alternative = "greater")$p.value),
  										                ncol = 2, byrow = TRUE, dimnames = list(NULL, c("p.jk", "p.kj"))), stringsAsFactors = FALSE)

  	 } else {

   		   # Single-Sample Proportion Test
  		   s.jk.kj <- data.frame(jk = apply(comb, 2, paste, collapse = "->"),
                               matrix(sapply(paste0("cbind(", mN - unlist(jk.kj), ", ", unlist(jk.kj), ")"),
                                             function(x) prop.test(eval(parse(text = x)), p = crit, alternative = "greater")$p.value),
                                      ncol = 2, byrow = TRUE, dimnames = list(NULL, c("p.jk", "p.kj"))), stringsAsFactors = FALSE)

  	 }

     #------------------------------------------------#
   	 # p-value adjustment for multiple comparisons

  	 if (p.adjust.method != "none") {

  	    s.jk.kj$p.jk <-  p.adjust(s.jk.kj$p.jk, method = p.adjust.method)
  	    s.jk.kj$p.kj <-  p.adjust(s.jk.kj$p.kj, method = p.adjust.method)

  	 }

	}

	#------------------------------------------------#
	# Sample size

	n <- unlist(lapply(lapply(dat.list, na.omit), nrow))

	#------------------------------------------------#
  # Results

  temp <- data.frame(jk = apply(comb, 2, paste, collapse = "->"), j = comb[1, ], k = comb[2, ],
                     n = unlist(lapply(lapply(dat.list, na.omit), nrow)), stringsAsFactors = FALSE)

	# Item means and sd
	j.mean <- data.frame(nr = 1:ncol(dat),
                       j.mean = apply(dat, 2, mean, na.rm = TRUE),
	                     j.sd = apply(dat, 2, sd, na.rm = TRUE), stringsAsFactors = FALSE)

	k.mean <- data.frame(nr = 1:ncol(dat),
                       k.mean = apply(dat, 2, mean, na.rm = TRUE),
	                     k.sd = apply(dat, 2, sd, na.rm = TRUE), stringsAsFactors = FALSE)

	temp <- merge(temp, j.mean, by.x = "j", by.y = "nr")
	temp <- merge(temp, k.mean, by.x = "k", by.y = "nr")

  # Ordering coefficient
  temp <- merge(temp, c.jk.kj, by = "jk")

	#------------------------------------------------#
	# Statistical test and statistical significance

  if (sig == TRUE) {

      result <- merge(temp, s.jk.kj, by = "jk")

	    result <- data.frame(result, sig.jk = as.numeric(result[, "p.jk"] < alpha),
	                                 sig.kj = as.numeric(result[, "p.kj"] < alpha), stringsAsFactors = FALSE)

	#------------------------------------------------#

   	  # Ordering TRUE/FALSE according to statistical significance and ordering coefficient
      result <- data.frame(result, crt.jk = as.numeric(result$sig.jk == 1 & result[, "c.jk"] > crit),
                                   crt.kj = as.numeric(result$sig.kj == 1 & result[, "c.kj"] > crit), stringsAsFactors = FALSE)

  } else {

    result <- temp

	  # Ordering TRUE/FALSE according to ordering coefficient only
	  result <- data.frame(result, crt.jk = as.numeric(result[, "c.jk"] > crit),
	                               crt.kj = as.numeric(result[, "c.kj"] > crit), stringsAsFactors = FALSE)

	}

	#------------------------------------------------#
	# Variablenames

	if (vnames & !is.null(colnames(dat))) {

 		 dat.nam <- data.frame(nr = 1:ncol(dat), nam = colnames(dat), stringsAsFactors = FALSE)

		 result <- merge(result, dat.nam, by.x = "k", by.y = "nr", all.x = TRUE, sort = FALSE)
		 result <- merge(result, dat.nam, by.x = "j", by.y = "nr", all.x = TRUE, sort = FALSE)

     result <- result[, -c(1:2)]
     names(result)[grep("nam", names(result))] <- c("k", "j")

	}

	#---
	# Order +, -, =
  result <- data.frame(result, order = sapply(apply(result[, c("crt.jk", "crt.kj")], 1, function(x) paste(x, collapse = "")),
                                              switch, "00" = "", "10" = "+", "01" = "-", "11" = "="), stringsAsFactors = FALSE)

	#---
  # Sort columns

  if (sig == TRUE) {

     result <- result[, c("j", "k", "n", "j.mean", "j.sd", "k.mean", "k.sd", "c.jk", "p.jk", "sig.jk", "c.kj", "p.kj", "sig.kj", "crt.jk", "crt.kj", "order")]

  } else {

     result <- result[, c("j", "k", "n", "j.mean", "j.sd", "k.mean", "k.sd", "c.jk", "c.kj", "crt.jk", "crt.kj", "order")]

  }

	#------------------------------------------------#
  # Order by item means

	if (order %in% c("decreasing", "increasing")) {

	   result <- result[order(result[, "j.mean"], result[, "k.mean"],
	                          decreasing = ifelse(order == "decreasing", TRUE, FALSE)), ]

	}

	#------------------------------------------------#
	# Create object result.d

  if (sig == TRUE) {

     result.d <- cbind(result[, c("j", "k", "n")], round(result[, c("j.mean", "j.sd", "k.mean", "k.sd", "c.jk")], digits = digits),
                       p.jk = formatC(result[, "p.jk"], format = "f", digits = 3), sig.jk = result[, "sig.jk"],
    	                 c.kj = round(result[, "c.kj"], digits = digits),
    	                 p.kj = formatC(result[, "p.kj"], format = "f", digits = 3),
    	                 result[, c("sig.kj", "crt.jk", "crt.kj", "order")])

  } else {

    result.d <- cbind(result[, c("j", "k", "n")], round(result[, c("j.mean", "j.sd", "k.mean", "k.sd", "c.jk")], digits = digits),
                      c.kj = round(result[, "c.kj"], digits = digits),
                      result[, c("crt.jk", "crt.kj", "order")])

  }

	#------------------------------------------------#
	# Exclude paths with no relationship

	if (exclude == TRUE) {

	 	 result.d <- result.d[result.d$order != "", ]
	 	 row.names(result.d) <- NULL

	}

	#------------------------------------------------#
	# Output

  # Last call
  l.call <- match.call()

  time <- paste(Sys.time())
  pkg.version <- paste0("SSRA version ", packageDescription("SSRA")$Version,
                        " (", packageDescription("SSRA")$Date, ")")

  if (output == TRUE) {

    cat("--------------------------------------------------------------------------\n")
    cat(" Call:    "); print(l.call)
    cat(" Time:   ", time, "\n")
    cat(" R:      ", R.version$version.string, "\n")
    cat(" Package:", pkg.version, "\n")
    cat("--------------------------------------------------------------------------\n")

    ###

    cat("\n Takeya Semantic Structure Analysis", "\n\n",
    		"  Number of response categories:", m, "\n",
    		"  Criteria for ordering coefficient:", crit, "\n")

    if (sig == TRUE) {

       if (exact == TRUE) {

    		  cat("\n", "  Exact Binomial Test", "\n",
    			        	"    Sig. level =", alpha * 100, "%", "\n")

    	 } else {

       	  cat("\n", "  Single-Sample Proportion Test", "\n",
    	     	        "    Sig. level =", alpha * 100, "%", "\n")

    	 }

      if (p.adjust.method != "none") {

        p.adjust.method <- paste0(toupper(substr(p.adjust.method, 1, 1)), substr(p.adjust.method, 2, nchar(p.adjust.method)))

    	  cat("     Correction method for multiple comparisons:", p.adjust.method, "\n\n",
            "  Ordering based on ordering coefficient and statistical significance", "\n\n")

    	} else {

    	  cat("     No alpha protection for multiple testing", "\n\n",
            "  Ordering based on ordering coefficient and statistical significance", "\n\n")

      }

    } else{

      cat("\n", "Ordering based on ordering coefficient", "\n\n")

  	}

    print(result.d)

  }

  # Check number of response categories
  if (!all(apply(dat, 2, function(x) length(unique(na.omit(x))) == m)) |
      !length(na.omit(unique(unlist(dat)))) == m) {

    cat("\nFrequency distribution:\n")
    print(apply(dat, 2, function(x) table(x <- factor(x, levels = sort(unique(unlist(dat)))))))

    warning(paste0("All items need to have m = ", m, " response categories. Please check the input data"))

  }

	# Return object
	object <- list(dat = dat, call = l.call,
	               args = data.frame(m = m, crit = crit, pairwise = pairwise, sig = sig, exact = exact,
	                                 alpha = alpha, p.adjust.method = p.adjust.method, digits = digits,
	                                 vnames = vnames, order = order, exclude = exclude, stringsAsFactors = FALSE),
	               time = time, R = R.version$version.string, package = pkg.version,
	               restab = result)
	class(object) <- "tssa"

	return(invisible(object))

}
