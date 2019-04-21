#' @export qbeeswarm
#' @import ggplot2
#' @import ggbeeswarm
#' @title Quick beeswarm-boxplots
#' @description convenience function for making beeswarmplots with overlaid boxplots.
#' @param df a data frame
#' @param x A column name within df, the variable to be displayed on the x axis.
#' @param y A column name within df, the variable to be displayed on the y axis.
#' @param box Should the boxplot be displayed? Default is TRUE.
#' @param notch Should the boxplot be displayed with a notch? Default is TRUE.
#' @param fill_box fill colour (or value) of the boxplot.
#' @param alpha alpha level of the boxplot color. Default is .3.
#' @param cex_axes font size of tick labels and axis text. Default is 18. Set to FALSE to add manual controls.
#' @param xlab (optional) add x axis label manually.
#' @param ylab (optional) add y axis label manually.
#' @param grid Should a grid be displayed? Default is FALSE.
#' @param colour colour of the points in the beeswarm plot
#' @param bw black-and-white: If set to TRUE, the plot will be printed in black and white.
#' Does not work properly if colour is specified. Overrides fill_box.
#' @param ... further arguments to be passed on to ggplot.
#'
#' @return a ggplot2 beeswarm-and-boxplot.
#'
#' @examples
#' set.seed(200)
#' df <- data.frame(A = c(rep("high", 50), rep("lo", 50)),
#'                 B = round(runif(n = 100)*100))
#' qbeeswarm(df, x = A, y = B)
#'
#'


# main function
qbeeswarm <- function(df, x, y,
                      xlab,
                      ylab,
                      box = TRUE,
                      notch = TRUE,
                      fill_box = "lightblue",
                      alpha = 0.3,
                      cex_axes = 18,
                      grid = FALSE,
                      colour,
                      bw = FALSE,
                      ...) {

  # substitute variables
  x <- substitute(x)
  if(is.character(x)) {
    x <- as.symbol(x)
  }

  y <- substitute(y)
  if(is.character(y)) {
    y <- as.symbol(y)
  }

  if(!missing(colour)) {
    if(!is.vector(colour)) {
      colour <- substitute(colour)
      if(is.character(colour)) {
        colour <- as.symbol(colour)
      }
    }
  }

  # make plot skeleton
  if(!missing(colour)) {
    if(as.character(colour[1]) %in% c(as.character(x), as.character(y))) {
      p <- ggplot2::ggplot(df, aes_string(x = as.character(x), y = as.character(y),
                                          colour = as.character(colour)), ...)
    } else {
      p <- ggplot2::ggplot(df, aes_string(x = as.character(x), y = as.character(y),
                                          colour = as.character(x)), ...)
    }

  } else {
    if(bw) {
      p <- ggplot2::ggplot(df, aes_string(x = as.character(x), y = as.character(y)), ...)
    } else {
      p <- ggplot2::ggplot(df, aes_string(x = as.character(x), y = as.character(y),
                                          colour = as.character(x)), ...)
    }


  }

  # add boxplot
  if(box) {

    if(bw) {
      p <- p +
        ggplot2::stat_boxplot(geom = "errorbar", width = .2, colour = "grey90") +
        ggplot2::geom_boxplot(outlier.shape = NA,
                              fill = "lightgrey",
                              alpha = alpha,
                              notch = notch,
                              colour = "grey90")
    } else {
      p <- p +
        ggplot2::stat_boxplot(geom = "errorbar", width = .2, colour = "grey90", alpha = .3) +
        ggplot2::geom_boxplot(outlier.shape = NA,
                              fill = fill_box,
                              alpha = alpha,
                              notch = notch,
                              colour = "grey90")
    }


  }


  # beeswarm function call
  if(!missing(colour)) {
    if(as.character(colour[1]) %in% c(as.character(x), as.character(y))) {
      p <- p +
        ggbeeswarm::geom_beeswarm() + ggplot2::theme_bw() +
        ggplot2::theme(panel.grid = element_blank())
    } else {
      p <- p +
        ggbeeswarm::geom_beeswarm() + ggplot2::theme_bw() +
        ggplot2::theme(panel.grid = element_blank()) +
        ggplot2::scale_colour_manual(values = colour)
    }


  }

  if(missing(colour)) {

    if(!bw) {
      # define color
      colour <- rep("darkblue", length(levels(factor(df[[which(colnames(df)==as.character(x))]]))))
    }

    # make plot...

    # ...in black-and-white
    if(bw) {
      p <- p +
        ggbeeswarm::geom_beeswarm() + ggplot2::theme_bw() +
        ggplot2::theme(panel.grid = element_blank())
    } else {

      # in colour
      p <- p +
        ggbeeswarm::geom_beeswarm() + ggplot2::theme_bw() +
        ggplot2::theme(panel.grid = element_blank()) +
        ggplot2::scale_colour_manual(values = colour)
    }


  }


  # customize axes
  if(cex_axes!=FALSE) {
    p <- p +
      ggplot2::theme(axis.text = element_text(size = cex_axes)) +
      ggplot2::theme(axis.title = element_text(size = cex_axes)) +
      ggplot2::theme(strip.text = element_text(size = cex_axes)) +
      ggplot2::theme(text = element_text(size = cex_axes))
  }



  # add x and y axis labels
  if(!missing(xlab)) {
    p <- p + ggplot2::xlab(label = xlab)
  }

  if(!missing(ylab)) {
    p <- p + ggplot2::ylab(label = ylab)
  }

  # omit legend
  p <- p + guides(colour = FALSE)


  return(p)

}

