

StatBoxplot.me <- ggproto("StatBoxplot.me", Stat,
                          
                          required_aes = "y",
                          aesthetics = function (self) 
                          {
                            c(union(self$required_aes, names(self$default_aes)), "group")
                          },
                        
                       compute_group = function (data, scales, width = NULL, na.rm = FALSE, coef = 1.5) 
  {
    qs <- c(0,0.05, 0.25, 0.5, 0.75,0.95,1)
    #qs <- c(0,0.05,0.5,0.95,1)
                         
                         if (!is.null(data$weight)) {
      mod <- quantreg::rq(y ~ 1, weights = weight, data = data, 
                          tau = qs)
      stats <- as.numeric(stats::coef(mod))
    }
    else {
      stats <- as.numeric(stats::quantile(data$y, qs))
    }
    names(stats) <- c("ymin","lower5","lower", "middle", "upper", "upper95", "ymax")
    #names(stats) <- c("ymin","lower", "middle", "upper", "ymax")
    iqr <- diff(stats[c(3, 5)])
    outliers <- data$y < (stats[3] - coef * iqr) | data$y > (stats[5] + 
                                                               coef * iqr)
    if (any(outliers)) {
      stats[c(1, 7)] <- range(c(stats[3:5], data$y[!outliers]), 
                              na.rm = TRUE)
    }
    if (length(unique(data$x)) > 1) 
      width <- diff(range(data$x)) * 0.9
    df <- as.data.frame(as.list(stats))
    df$outliers <- list(data$y[outliers])
    if (is.null(data$weight)) {
      n <- sum(!is.na(data$y))
    }
    else {
      n <- sum(data$weight[!is.na(data$y) & !is.na(data$weight)])
    }
    df$notchupper <- df$middle + 1.58 * iqr/sqrt(n)
    df$notchlower <- df$middle - 1.58 * iqr/sqrt(n)
    df$x <- if (is.factor(data$x)) 
      data$x[1]
    else mean(range(data$x))
    df$width <- width
    df$relvarwidth <- sqrt(n)
    df 
    }
)



stat_boxplot.me <- function (mapping = NULL, data = NULL, geom = "boxplot.me", position = "dodge2", 
                             ..., coef = 1.5, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
  layer(data = data, mapping = mapping, stat = StatBoxplot.me, 
        geom = geom, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = list(na.rm = na.rm, 
                                                 coef = coef, ...))
}
geom_boxplot.me <- function (mapping = NULL, data = NULL, stat = "boxplot.me", position = "dodge2", 
          ..., outlier.colour = "Red", outlier.color = NULL, outlier.fill = NULL, 
          outlier.shape = 21, outlier.size = 1.5, outlier.stroke = 0.5, 
          outlier.alpha = NULL, notch = FALSE, notchwidth = 0.5, varwidth = FALSE, 
          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
  if (is.character(position)) {
    if (varwidth == TRUE) 
      position <- position_dodge2(preserve = "single")
  }
  else {
    if (identical(position$preserve, "total") & varwidth == 
        TRUE) {
      warning("Can't preserve total widths when varwidth = TRUE.", 
              call. = FALSE)
      position$preserve <- "single"
    }
  }
  layer(data = data, mapping = mapping, stat = stat, geom = GeomBoxplot.me, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(outlier.colour = outlier.color %||% outlier.colour, 
                      outlier.fill = outlier.fill, outlier.shape = outlier.shape, 
                      outlier.size = outlier.size, outlier.stroke = outlier.stroke, 
                      outlier.alpha = outlier.alpha, notch = notch, notchwidth = notchwidth, 
                      varwidth = varwidth, na.rm = na.rm, ...))
}

p <- ggplot(mpg, aes(class, hwy))
p + geom_boxplot.me()


