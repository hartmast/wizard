#' @export format_age
#' @title Format age in child language data
#' @description Convenience function for formatting age values:
#' In child language acquisition studies, age values ofte come in
#' formats like 2;1.10 (year;month.day), which causes problems when
#' sorting the data because they will typically be recognized as
#' character strings, and 2;10.5 will precede 2;1.10 because
#' of the 0 in the first character string. This function helps
#' to circumvent this problem by adding trailing zeros to all
#' one-digit month and day values. Its input can be either a vector
#' or a dataframe. Its output is a vector of formatted numbers
#' in the former case or a dataframe with an added column
#' age_formatted in the latter case. Alternatively, if you use
#' a dataframe as input, you can specify factors = TRUE, in which
#' case the original formatting will be kept but the factor levels
#' will be rearranged in such a way that the temporal order
#' is accurately reflected.
#' @param x A vector or dataframe.
#' @param col If x is a dataframe: The column containing the
#' age values.
#' @param month_separator sign that separates months from years.
#' Defaults to ; as in 2;4.10
#' @param day_separator sign that separates days from months.
#' Defaults to . as in 2;4.10
#' @param factors If TRUE, the original formatting will be retained
#' but the factor levels will be rearranged to reflect the
#' correct temporal order (if x is a vector). If x is a dataframe,
#' the factor levels will be rearranged in the original column
#' but an age_formatted column will still be appended to the
#' original dataframe. To get rid of the column, use
#' x[,!names(x)=="age_formatted"]. Default is FALSE.
#' @return a dataframe or a vector, depending on the input.
#' @importFrom stats setNames
#' @examples
#' df <- data.frame(a = c("a", "b", "c"),
#'                  age = c("1;2,10, "1;2.11", "1;2.12"))
#' format_age(df)

# main function
format_age <- function(x, col, month_separator = ";",
                       day_separator = ".", factors = FALSE) {


  # specify search string from the two
  # separators

  seps <- paste0("\\", month_separator, "|", "\\",  day_separator)


  if(is.data.frame(x)) {

    # if column name is not specified,
    # try "Age"
    if(missing(col)) {
      if(is.element("Age", colnames(x))) {
        col <- "Age"
      } else if(is.element("age", colnames(x))) {
        col <- "age"
      } else if(length(grep("age", colnames(x), ignore.case = T))==1) {
        col <- colnames(x)[grep("age", colnames(x), ignore.case = T)]
      }

    }




    df <- x
    df$age_formatted <- NA
    for(j in 1:nrow(df)) {
      x1 <- unlist(strsplit(as.character(df[j, which(colnames(df)==as.character(col))]), seps))

      # replace scarequotes if present
      x1 <- gsub("\"", "", x1)

      if(length(x1)>1) {
        if(as.numeric(x1[2])<10) { x1[2] <- paste(c("0", as.numeric(x1[2])), sep="", collapse="") }
        if(length(x1)<3) { x1[3] <- 0 }
        if(x1[3]=="")    { x1[3] <- 0 }
        if(as.numeric(x1[3])<10) { x1[3] <- paste(c("0", as.numeric(x1[3])), sep="", collapse="") }

        df$age_formatted[j] <- paste(x1[1], ";", x1[2], ".", x1[3],
                                     sep="", collapse="")
      } else {
        df$age_formatted[j] <- paste0(c(x1, month_separator, "00", day_separator, "00"), collapse = "")
      }


    }

    if(!factors) {
      return(df)
    } else {

      # add helper column
      df$f_levels <- as.numeric(factor(df$age_formatted))

      # get factor levels and apply them
      f_levels <- df[,names(df) %in% c(col, "f_levels")]
      f_levels <- f_levels[order(df$f_levels),]
      f_levels <- unique(f_levels)

      # make sure that df is not a tibble,
      # otherwise reults will be NA
      df <- as.data.frame(df)
      f_levels <- as.data.frame(f_levels)

      # apply levels
      df[,names(df)==col] <- factor(df[,names(df)==col], levels = f_levels[,names(f_levels)==col])

      # remove helper column
      df <- df[,names(df)!="f_levels"]

      #return df
      return(df)


    }



  } else {

    if(missing(col)) {
      col = NULL
    }

    # backup copy in case it is needed because factors=T
    y <- x

    for(j in 1:length(x)) {
      x1 <- unlist(strsplit(as.character(x[j]), seps))

      # replace scarequotes if present
      x1 <- gsub("\"", "", x1)

      if(length(x1)>1) {
        if(as.numeric(x1[2])<10) { x1[2] <- paste(c("0", as.numeric(x1[2])), sep="", collapse="") }
        if(length(x1)<3) { x1[3] <- 0 }
        if(x1[3]=="")    { x1[3] <- 0 }
        if(as.numeric(x1[3])<10) { x1[3] <- paste(c("0", as.numeric(x1[3])), sep="", collapse="") }

        x[j] <- paste(x1[1], ";", x1[2], ".", x1[3],
                                     sep="", collapse="")
      } else {
        x[j] <- paste0(c(x1, month_separator, "00", day_separator, "00"), collapse = "")
      }


    }

    if(!factors) {
      return(x)
    } else {

      # get order of factor levels
      y <- as.data.frame(y)
      y <- setNames(y, "age")
      y$f_levels <- as.numeric(factor(y$age))
      y <- unique(y)

      # add order of factor levels to original data
      x01 <- unique(x)
      x01 <- x01[order(y$age)]
      x <- factor(x, levels = x01)

      # return vector
      return(x)

    }






    }

}



