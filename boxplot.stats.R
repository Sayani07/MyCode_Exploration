getAnywhere(boxplot.stats)


#This function is typically called by another function to gather the statistics necessary for producing box plots, but may be invoked separately.

#gives the 5 important stas - extreme of the lower whisker, the lower hinge, the median, the upper hinge and the extreme of the upper whisker 


function (x, coef = 1.5, do.conf = TRUE, do.out = TRUE) 
{
  if (coef < 0) 
    stop("'coef' must not be negative")
  nna <- !is.na(x)
  n <- sum(nna)
  stats <- stats::fivenum(x, na.rm = TRUE)
  iqr <- diff(stats[c(2, 4)])
  if (coef == 0) 
    do.out <- FALSE
  else {
    out <- if (!is.na(iqr)) {
      x < (stats[2L] - coef * iqr) | x > (stats[4L] + coef * 
                                            iqr)
    }
    else !is.finite(x)
    if (any(out[nna], na.rm = TRUE)) 
      stats[c(1, 5)] <- range(x[!out], na.rm = TRUE)
  }
  conf <- if (do.conf) 
    stats[3L] + c(-1.58, 1.58) * iqr/sqrt(n)
  list(stats = stats, n = n, conf = conf, out = if (do.out) x[out & 
                                                                nna] else numeric())
}
x=runif(100,0,1)
MYFUNCTION(x)