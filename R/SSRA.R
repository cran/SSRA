#' Sakai Sequential Relation Analysis
#'
#' This function conducts the Sequential Relation Analysis based on Sakai 2016
#'
#' Takea Semantic Structure Analysis (TSSA) and Sakai Sequential Relation Analysis (SSRA) are graphical approaches
#'
#' @param dat		 	    	   requires a data frame with polytomous data
#' @param r.crt		    	   correlation coefficient criterion to be judged 'sequential' or 'equivalent
#' @param mu.sq	    	 	   Absolute mean difference criterion to be judged 'sequential'
#' @param mu.eq  	     	   maximal absolute mean difference to be judged 'equivalent'
#' @param d.sq  		     	 effect size for mean difference criterion to be judged 'sequential'
#' @param d.eq    	     	 maximal effect size Cohen's d to be judged 'equivalent'
#' @param pairwise         pairwise deletion of missing data,
#'                         if pairwise = FALSE listwise deletion is applied
#' @param method	         character string indicating which correlation coefficient to be used,
#'							           'pearson' = Pearson's product moment correlation coefficien
#'						             'spearman' = Spearman's rho statistic
#'						             'kendall' = Kendall's tau (default)
#' @param alpha	  		     significance level
#' @param p.adjust.method  p-value correction method for multiple comparisons, see: ?p.adjust (default = holm)
#' @param digits           integer indicating the number of decimal places to be used
#' @param vnames		       use variable names for labeling?
#' @param order	 	         sort by item mean of j and k?
#' @param exclude		       exclude paths with no relationship?
#' @param output           print result table?
#'
#' @author
#' Takuya Yanagida
#' Keiko Sakai
#'
#' @return
#' Returns an object of class \code{ssra}, to be used for the \code{seqtable} function. The object is a list with
#' following entries: 'dat' (data frame), 'call" (function call), 'args' (specification of arguments),
#' 'time' (time of analysis), 'R' (R version), 'package' (package version), and 'restab' (result table).
#' The 'restab' entry has following entries:
#' \tabular{ll}{
#'     \code{j}     	    \tab item j \cr
#'     \code{k}     	    \tab item k \cr
#'     \code{n}				    \tab sample size \cr
#'     \code{j.mean}	    \tab mean of item j \cr
#'     \code{j.sd}        \tab standard deviation of item j \cr
#'     \code{k.mean}      \tab mean of item k \cr
#'     \code{k.sd}        \tab standard deviation of item k \cr
#'     \code{r}	   	      \tab correlation coefficient \cr
#'     \code{r.t}	  	    \tab test statistic of the statistical significanc test for the correlation coefficient \cr
#'     \code{r.p}         \tab statistical significance value of the correlation \cr
#'     \code{r.sig}       \tab statistical significance of the correlation (0 = not significant / 1 = significant) \cr
#'     \code{r.crt}       \tab correlation criterion for judging 'sequential' or 'equal': 'r.p < alpha' and 'r > r.crt' (0 = no / 1 = yes) \cr
#'     \code{m.diff}	    \tab mean difference \cr
#'     \code{sd.diff}     \tab standard deviation difference \cr
#'     \code{m.diff.eff}  \tab effect size Cohen's d for dependent samples \cr
#'     \code{m.t}			    \tab test statistic of the statistical significanc test for mean difference \cr
#'     \code{m.p}    	    \tab statistical significance value of the mean difference \cr
#'     \code{m.sig} 	    \tab statistical significance of the mean difference (0 = not significant / 1 = significant) \cr
#'     \code{m.crt.sq}    \tab mean difference criteria for judging 'sequential': 'm.diff.p < alpha', 'm.diff > mu.sq' and 'm.diff.eff > d.sq'
#'                             (0 = no / -1 = yes negative / 1 = yes postive) \cr
#'     \code{m.crt.eq}    \tab mean difference criteria for judging 'equivalence':
#'                             statistical significant and 'm <= mu.eq' 'd <= d.sq'
#'                             (0 = no 1 = yes) \cr
#'     \code{seq}         \tab sequential relation of item pairs ("+","-", "") \cr
#'     \code{eq}          \tab equivalence of item pairs ("=" or "") \cr
#'     \code{order}       \tab order structure of item pairs ("=", "+","-") \cr
#'  }
#'
#' @seealso
#' \code{\link{seqtable}}, \code{\link{TSSA}}, \code{\link{plot.ssra}}, \code{\link{scatterplot}}
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
#' SSRA(exdat)
SSRA <- function(dat, r.crt = 0.3, mu.sq = 0, mu.eq = Inf, d.sq = 0.2, d.eq = 0.2,
                 pairwise = TRUE, method = c("pearson", "kendall", "spearman"), alpha = 0.05,
                 p.adjust.method = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"),
                 digits = 3, vnames = TRUE, order = c("no", "decreasing", "increasing"),
                 exclude = TRUE, output = TRUE) {

	#------------------------------------------------------------------------------------------------------#

  # Check data frame
  if (!is.data.frame(dat)) {

    stop("SSRA function requires a data frame")

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

	if (nrow(dat) <= 1) {

		 stop("Not enough observations")

	}

	#------------------------------------------------#

	# Order data by item means
	order <- ifelse(all(c("no", "decreasing", "increasing") %in% order), "increasing", order)

	if (order %in% c("decreasing", "increasing")) {

 	   item.m <- data.frame(nr = 1:ncol(dat), i.mean = apply(dat, 2, mean, na.rm = TRUE))
   	 item.m <- item.m[order(item.m$i.mean, decreasing = ifelse(order == "decreasing", TRUE, FALSE)), ]

     dat <- dat[, item.m$nr]

	}

	#------------------------------------------------#

	# Generate all pairwise combinations
	comb <- combn(ncol(dat), m = 2)

	# Convert data in combination list
	dat.list <- apply(comb, 2, function(x) dat[, x])

	#------------------------------------------------#
	# Correlation

  method <- ifelse(all(c("pearson", "kendall", "spearman") %in% method), "kendall", method)

	res.cor <- matrix(unlist(lapply(lapply(dat.list, function(x) cor.test(x[, 1], x[, 2],
                                                                        method = method, exact = FALSE, continuity = TRUE)),
													 function(x) c(x$estimate, x$statistic, x$p.value))), ncol = 3, byrow = TRUE)

	colnames(res.cor) <- c("r", "r.t", "r.p")

	#------------------------------------------------#
	# Paired samples t-test

	res.mean <- matrix(unlist(lapply(lapply(dat.list, function(x) t.test(x[, 2], x[, 1], paired = TRUE)),
				 				 		 			  function(x) c(x$estimate, x$statistic, x$p.value))), ncol = 3, byrow = TRUE)

	colnames(res.mean) <- c("m.diff", "m.diff.t", "m.diff.p")

	###

	# Effect size Cohens's d
	m.diff.eff <- unlist(lapply(dat.list,
	                            function(x) abs(mean(x[, 2] - x[, 1], na.rm = TRUE )) / sd(x[, 2] - x[, 1], na.rm = TRUE)))

	# Standard deviation difference
	sd.diff <- unlist(lapply(dat.list, function(x) sd(x[, 2] - x[, 1], na.rm = TRUE)))

	#------------------------------------------------#
	# Statistical significance

	p.adjust.method <- ifelse(all(c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none") %in% p.adjust.method),
                            "holm", p.adjust.method)

	res.cor[, "r.p"] <- p.adjust(res.cor[, "r.p"], method = p.adjust.method)
	res.mean[, "m.diff.p"] <- p.adjust(res.mean[, "m.diff.p"], method = p.adjust.method)

  r.sig <- as.numeric(res.cor[, "r.p"] < alpha)
	m.sig <- as.numeric(res.mean[, "m.diff.p"] < alpha)

	#------------------------------------------------#
	# Sample size

	n <- unlist(lapply(lapply(dat.list, na.omit), nrow))

	#------------------------------------------------#
	# Results

	result <- data.frame(t(comb), n = n, res.cor, r.sig = r.sig, r.crt = ifelse(res.cor[, 1] > r.crt, 1, 0),
	                     m.diff = res.mean[, "m.diff"], sd.diff = sd.diff, m.diff.eff, res.mean[, c("m.diff.t", "m.diff.p")], m.diff.sig = m.sig,
	                     m.diff.crt.sq = ifelse(m.sig == 1 & abs(res.mean[, 1]) >= mu.sq & m.diff.eff >= d.sq & res.mean[, 1] < 0, -1,
	                                            ifelse(m.sig == 1 & abs(res.mean[, 1]) >= mu.sq & m.diff.eff >= d.sq & res.mean[, 1] >= 0, 1, 0)),
	                     m.diff.crt.eq = ifelse((m.sig == 0 | (abs(res.mean[, 1]) <= mu.eq & m.diff.eff <= d.eq)), 1, 0))

  result <- data.frame(result,
                       seq = ifelse(result$r.crt == 1 & result$m.diff.crt.sq == 1, "+",
                                    ifelse(result$r.crt == 1 & result$m.diff.crt.sq == -1, "-", "")),
                       eq = ifelse(result$r.crt == 1 & result$m.diff.crt.eq == 1, "=", ""),
                       stringsAsFactors = FALSE)

  result <- data.frame(result,
                       order = ifelse(result$seq %in% c("+", "-"), result$seq,
                                      ifelse(result$eq == "=", "=", "")), stringsAsFactors = FALSE)

	colnames(result) <- c("j", "k", "n", "r", "r.t", "r.p", "r.sig", "r.crt", "m.diff", "sd.diff", "m.diff.eff", "m.diff.t", "m.diff.p", "m.diff.sig",
	                      "m.diff.crt.sq", "m.diff.crt.eq",
	                      "seq", "eq", "order")

	#------------------------------------------------#
	# Item mean and sd

	item.m <- data.frame(nr = 1:ncol(dat), i.mean = apply(dat, 2, mean, na.rm = TRUE),
	                     i.sd = apply(dat, 2, sd, na.rm = TRUE))
	result <- merge(result, item.m, by.x = "j", by.y = "nr", all.x = TRUE, sort = FALSE)
	result <- merge(result, item.m, by.x = "k", by.y = "nr", all.x = TRUE, sort = FALSE)

	result <- data.frame(result[, c(2, 1, 3)],
	                     j.mean = result[, "i.mean.x"], j.sd = result[, "i.sd.x"],
	                     k.mean = result[, "i.mean.y"], k.sd = result[, "i.sd.y"],
	                     result[, 4:grep("order", names(result))])

	#------------------------------------------------#
	# Variablenames

	if (vnames == TRUE & !is.null(colnames(dat))) {

 		 dat.nam <- data.frame(nr = 1:ncol(dat), nam = colnames(dat), stringsAsFactors = FALSE)

		 result <- merge(result, dat.nam, by.x = "k", by.y = "nr", all.x = TRUE, sort = FALSE)
		 result <- merge(result, dat.nam, by.x = "j", by.y = "nr", all.x = TRUE, sort = FALSE)

		 result <- data.frame(result[, c("nam.y", "nam.x")], result[, 3:grep("order", names(result))])
		 colnames(result)[1:2] <- c("j", "k")

	}

	#------------------------------------------------#

	# Order by item means

	if (order %in% c("decreasing", "increasing")) {

 	   result <- result[order(result[, "j.mean"], result[, "k.mean"],
	                          decreasing = ifelse(order == "decreasing", TRUE, FALSE)), ]

	}

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
	if (exclude == TRUE) {

	   result.d <- result.d[result.d$order != "", ]
	   row.names(result.d) <- NULL

	}

	#------------------------------------------------#
  # Output

  # Last call
  l.call <- match.call()

  time <- paste(Sys.time())
  pkg.version <- paste0("SSRA version ", packageDescription("SSRA")$Version)

  if (output == TRUE) {

  	cat("--------------------------------------------------------------------------\n")
  	cat(" Call:    "); print(l.call)
  	cat(" Time:   ", time, "\n")
  	cat(" R:      ", R.version$version.string, "\n")
  	cat(" Package:", pkg.version, " (", packageDescription("SSRA")$Date, ")\n")
  	cat("--------------------------------------------------------------------------\n")

    ###

  	cat("\n Sakai Sequential Relation Analysis", "\n\n ",

  	    ifelse(method == "pearson", "Product-moment correlation coefficient",
  	           ifelse(method == "spearman", "Spearman correlation", "Kendall-Tau")), "\n",
  	    "   Correlation coefficient criterion to be judged 'sequential' or 'equivalent:", r.crt, "\n\n",

  	    "  Paired t-test", "\n",
  			"   Absolute mean difference criterion to be judged 'sequential':", mu.sq, "\n",
  			"   Maximal absolute mean difference to be judged 'equivalent':", mu.eq, "\n",
  			"   Effect size for mean difference criterion to be judged 'sequential':", d.sq, "\n",
  			"   Maximal effect size for mean difference to be judged 'equivalent':", d.eq, "\n\n",

  			"  Significance level:", alpha * 100, "%", "\n")

  	if (p.adjust.method != "none") {

  	  cat("    Correction method for multiple comparisons:", paste0(toupper(substr(p.adjust.method, 1, 1)),
  	                                                                substr(p.adjust.method, 2, nchar(p.adjust.method))), "\n\n")

  	} else {

  		cat("    No alpha protection for multiple testing", "\n\n")

  	}

    # Display result table
  	print(result.d)

	}

  # Return object
	object <- list(dat = dat, call = l.call,
                 args = data.frame(r.crt = r.crt, mu.sq = mu.sq, mu.eq = mu.eq,
                                   d.sq = d.sq, d.eq = d.eq,
                                   pairwise = pairwise, method = method,
	                                 alpha = alpha, p.adjust.method = p.adjust.method,
	                                 digits = digits, vnames = vnames, order = order, exclude = exclude,
                                   stringsAsFactors = FALSE),
	               time = time, R = R.version$version.string, package = pkg.version,
	               restab = result)
  class(object) <- "ssra"

  return(invisible(object))

}
