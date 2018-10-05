# Name: stat_boxplot

# Function: Creates a new layer which is a combination of data, stat and geom with a potential position adjustment

# Other than default arguments : geom:boxplot, position = "dodge2" and stat="boxplot"

###**** LAYER FUNCTION ARGUMENTS ****####
#layer(geom = NULL, stat = NULL, data = NULL, mapping = NULL,
#position = NULL, params = list(), inherit.aes = TRUE,
#check.aes = TRUE, check.param = TRUE, show.legend = NA)
#geom: geometrical object for displaying the data
#stat : statistical transformation to use on the data for this layer as a string
#inheri\t.aes	If FALSE, overrides the default aesthetics, rather than combining with them.


#stat_boxplot
function (mapping = NULL, data = NULL, geom = "boxplot", position = "dodge2", 
          ..., coef = 1.5, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
  layer(data = data, mapping = mapping, stat = StatBoxplot, 
        geom = geom, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = list(na.rm = na.rm, 
                                                 coef = coef, ...))
}


#StatBoxplot

StatBoxplot
ggproto object: Class StatBoxplot, Stat, gg>
aesthetics: function
compute_group: function
compute_layer: function
compute_panel: function
default_aes: uneval
extra_params: na.rm
finish_layer: function
non_missing_aes: weight
parameters: function
required_aes: y
retransform: TRUE
setup_data: function
setup_params: function
super:  <ggproto object: Class Stat, gg>

#   ggproto implements a protype based OO system which blurs the lines between classes and instances. It is inspired by the proto package, but it has some important differences. Notably, it cleanly supports cross-package inheritance, and has faster performance.
# 
# In most cases, creating a new OO system to be used by a single package is not a good idea. However, it was the least-bad solution for ggplot2 because it required the fewest changes to an already complex code base.
  
  
#StatBoxplot$compute_group


# Calculates  ymin lower middle upper ymax outliers notchupper notchlower x width relvarwidth given data(x,y)
#So find out the stats needed for hdrcde  

<ggproto method>
  <Wrapper function>
  function (...) 
    f(...)

<Inner function (f)>
  function (data, scales, width = NULL, na.rm = FALSE, coef = 1.5) 
  {
    qs <- c(0, 0.25, 0.5, 0.75, 1)
    if (!is.null(data$weight)) {
      mod <- quantreg::rq(y ~ 1, weights = weight, data = data, 
                          tau = qs)
      stats <- as.numeric(stats::coef(mod))
    }
    else {
      stats <- as.numeric(stats::quantile(data$y, qs))
    }
    names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
    iqr <- diff(stats[c(2, 4)])
    outliers <- data$y < (stats[2] - coef * iqr) | data$y > (stats[4] + 
                                                               coef * iqr)
    if (any(outliers)) {
      stats[c(1, 5)] <- range(c(stats[2:4], data$y[!outliers]), 
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

StatBoxplot$non_missing_aes
#[1] "weight"

StatBoxplot$required_aes 

#[1] "y"

StatBoxplot$setup_data

#<ggproto method>
<Wrapper function>
  function (...) 
    f(...)

#Question: Why null has been  replaced with zero using %||% operator and then remove_missing used on x to remove rows??
<Inner function (f)>
  function (data, params) 
  {
    data$x <- data$x %||% 0 
    # 
    # This infix function makes it easy to replace NULLs with a default value. It's inspired by the way that Ruby's or operation (||) works.
    
    # Usage
    # x %||% y
    # Arguments
    # x, y	
    # If x is NULL, will return y; otherwise returns x.
    data <- remove_missing(data, na.rm = FALSE, vars = "x", name = "stat_boxplot")
    #Remove all non-complete rows, with a warning if na.rm = FALSE.
    data
  }


StatBoxplot$setup_params

StatBoxplot$aesthetics

ggproto method>
  <Wrapper function>
  function (...) 
    f(..., self = self)

<Inner function (f)>
  function (self) 
  {
    c(union(self$required_aes, names(self$default_aes)), "group") #Question: What is the function if
  #of the group
  }



StatBoxplot$compute_layer

StatBoxplot$compute_panel

    
    
    