#' @export qbarplot
#' @import ggplot2
#' @title Quick barplots
#' @description Convenience function for generating ggplot2 barplots from raw datasets.
#' @param df a data frame with raw data
#' @param x variable to be displayed on the x axis. Must be a column name of df. Can be unquoted.
#' @param fill variable that defines the fill of the bars. Must be a column name of df. Can be unquoted.
#' @param wrap variable along which the data should be split in different panels. See ?ggplot2::facet_wrap and ?ggplot2::facet_grid for more information. Default is NULL, i.e. no wrap.
#' @param raw if TRUE, the function returns the raw data, i.e. a data frame with cross-tabulated absolute and relative frequencies.
#' @param text if TRUE (the default), absolute frequencies are displayed on the bars using geom_text.
#' @param cex size of the text displaying the absolute frequencies.
#' @param percent If TRUE (the default), the y axis displays proportions in percent format. Requires package "scales". If FALSE (or if "scales" is not installed), it is displayed as a fraction between 0 and 1.
#' @param dodge If FALSE (the default), a stacked barplot if produced. If TRUE, qbarplot() returns a dodged barplot, i.e. the bars are displayed side-by-side.
#' @param xlab x axis label. By default, identical to x.
#' @param ylab y axis label. By default, "Relative Frequency".
#' @param wrap100 If wrap != NULL and wrap100 = TRUE, percentages are calculated separately for each category. In other words, if wrap100 = TRUE, the bars sum up to 100 percent in each panel. If wrap100 = FALSE (the default), the percentages sum up to 100 across panels. For illustration, compare e.g. \code{qbarplot(ggplot2::diamonds, color, clarity, cut, wrap100 = FALSE)} vs. \code{qbarplot(ggplot2::diamonds, color, clarity, cut, wrap100 = TRUE)}.
#' @param tibble If raw = TRUE and tibble = TRUE (the default), the raw data are returned as a tibble. Only works if the "tibble" package is installed. See ?tibble::tibble for more information.
#' @param vjust vertical adjustment of the text, if text = TRUE. Behaves differently for stacked and dodged barplots. Under the hood, vjust is part of the position_stacked() call in stacked barplots (default: .5, i.e. in the middle of each portion of the bar) and part of the geom_text() call in dodged barplots (-.5, i.e. slightly above each bar).
#' @param display0 If text = TRUE, display0 = TRUE (the default) prevents 0 values (if there are any) from being shown on the text display.
#' @param ... Further arguments to be passed on to ggplot().
#'
#' @return a ggplot2 barplot or, if raw = TRUE, a data frame with the cross-tabulated raw and relative frequencies.
#'
#' @examples
#' qbarplot(ggplot2::diamonds, color, cut)
#' qbarplot(ggplot2::diamonds, color, clarity, cut)



# function qbarplot -------------------------------------------
qbarplot <- function(df, x, fill, wrap=NULL,
                     raw = FALSE,
                     text = TRUE,
                     cex = 3,
                     percent = TRUE,
                     dodge = FALSE,
                     xlab, ylab,
                     wrap100 = FALSE,
                     tibble = TRUE,
                     vjust,
                     display0 = FALSE,
                     ...) {

  # define vjust variable if missing
  if(missing(vjust)) {
    if(dodge) {
      vjust = -.5
    } else {
      vjust = .5
    }
  }

  # make sure that df is a data.frame
  df <- as.data.frame(df)

  # substitute variables, get list of relevant colnames -----------
  x <- substitute(x)

  if(is.character(x)) {
    x <- as.symbol(x)
  }

  fill <- substitute(fill)

  if(is.character(fill)) {
    fill <- as.symbol(fill)
  }


  if(!missing(wrap)) {
    wrap <- substitute(wrap)
    if(is.character(wrap)) {
      wrap <- as.symbol(wrap)
    }


    l <- c(x, fill, wrap)
  } else {
    l <- c(x, fill)
  }

  # get relevant colnames
  cols <- which(colnames(df) %in% l)

  # make sure the colnames are in the right order
  cols <- unlist(lapply(1:length(l), function(i) grep(paste("^", l[[i]], "$", sep=""), colnames(df))))


  if(wrap100) {
    # column number of wrap variable
    wn <- which(colnames(df) == wrap)

    # factor levels of wrap variable
    lv <- levels(factor(df[,wn]))

    # create a table in which the percentages
    # sum up to 100 % for each facet_wrap window
    for(i in 1:length(lv)) {
      sub1 <- df[which(df[,wn] %in% lv[i]),]
      t1a <- table(sub1[,cols])
      t1b <- prop.table(t1a, margin = 1)
      t1a <- as.data.frame(t1a)
      t1b <- as.data.frame(t1b)
      colnames(t1b)[which(colnames(t1b)=="Freq")] <- "RelFreq"
      t1a <- base::merge(t1a, t1b)


      if(i == 1) {
        t1 <- t1a
      } else {
        t1 <- rbind(t1, t1a)
      }


    }

  } else {

    # create table of absolute and relative frequencies

    t1 <- table(df[,cols])
    t2 <- prop.table(t1, margin = 1)
    t1 <- as.data.frame(t1)
    t2 <- as.data.frame(t2)
    colnames(t2)[which(colnames(t2)=="Freq")] <- "RelFreq"
    t1 <- base::merge(t1, t2)
  }



  if(raw) {
    if(tibble) {
      if(!requireNamespace("tibble", quietly = TRUE)) {
        warning("Package 'tibble' not installed.
                Install it using install.packages(\"tibble\")
                if you want qbarplot() to return raw data in tibble format.
                If you're ok with the standard dataframe format,
                you can safely ignore this warning.")
      } else {
        return(tibble::as_tibble(t1))
      }

    }

    return(t1)
  } else {

    if(missing(xlab)) {
      xlab <- as.character(l[[1]])
    }

    if(missing(ylab)) {
      ylab <- "Relative Frequency"
    }

    # modify Freq column to make sure that zeros are not dispayed if dodge == T
    if(dodge) {
      if(!display0) {
        t1$Freq <- ifelse(t1$Freq==0, "", t1$Freq)
      }
    }

    # save x and fill value as character
    # so that they can be used as input to aes_

    x.value = as.character(l[[1]])
    fill.value = as.character(l[[2]])

    p <- ggplot2::ggplot(t1, ggplot2::aes_string(x = x.value,
                                 y = "RelFreq",
                                 fill = fill.value,
                                 group = fill.value,
                                 label = "Freq"), ...)

    if(!missing(wrap)) {
        p <- p + ggplot2::facet_wrap(~eval(l[[3]]))
    }

    if(dodge) {

      p <- p + ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
        ggplot2::geom_col(position = ggplot2::position_dodge()) +
        ggplot2::guides(fill = ggplot2::guide_legend(title = l[[2]]))

      if(text) {

        p <- p  + ggplot2::geom_text(size = cex,
                                     vjust = vjust,
                                     position = ggplot2::position_dodge(width=0.9))

      }

    } else {
      p <- p + ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
        ggplot2::geom_col() +
        ggplot2::guides(fill = ggplot2::guide_legend(title = l[[2]]))


      if(text) {
        if(!display0) {
          p <- p + ggplot2::geom_text(data = t1[t1$Freq>0,], size = cex,
                                      position = ggplot2::position_stack(vjust=vjust))
        } else {
          p <- p + ggplot2::geom_text(size = cex,
                                      position = ggplot2::position_stack(vjust=vjust))
        }
      }


    }


    if(percent) {
      if(!requireNamespace("scales", quietly = TRUE)) {
        warning("Package 'scales' not installed. Please install it using install.packages(\"scales\") if you want to show percentages on the y axis.")
      } else {
        p <- p + ggplot2::scale_y_continuous(labels = scales::percent)
      }
    }

    return(p)


  }



}


