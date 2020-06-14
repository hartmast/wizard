#' @export pretty_df
#' @title Pretty dataframes
#' @description Convenience function for nice dataframe output
#' @param df A dataframe.
#' @return a dataframe.
#' @example
#' df <- data.frame(a = c("a", "b", "c"),
#'                  b = c(0.86965965, 989687.89068, 278.9058574))
#' pretty_df(df)

# main function
pretty_df <- function(df) {

  # function for rounding
  round_this <- function(x, digits = 2) ifelse(x < 1, signif(x, digits = digits), round(x, digits = 2))

  # function for getting prettyfied dataframe
  df_pretty <- as.data.frame(lapply(1:length(df),
                                    function(i) if(!class(df[[i]]) %in% c("character", "factor"))
                                    {
                                      round_this(df[[i]])
                                    } else {
                                      return(df[[i]])
                                    })
  )

  # set names to original names
  colnames(df_pretty) <- colnames(df)
  return(df_pretty)


}
