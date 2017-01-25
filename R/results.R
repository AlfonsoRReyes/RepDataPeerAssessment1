#' Gets a summary statistics for a data frame
#'
#' It could be before or after imputation
#'
#' @param x data frame
#' @param uniVar variable to analyze
#' @param colName name of the column in the report table
#' @param decs number of decimals. Default is 3.
#'
#' @return data frame with one column
#'
#' @importFrom stats median sd
#'
#' @export
#'
info.imp <- function(x, uniVar, colName, decs = 3) {
  if (anyNA(x[, uniVar])) {
    indNA <- which(is.na(x[, uniVar]))             # which rows are NA
    indNA.not <- which(!((1:nrow(x)) %in% indNA))  # which rows are not NA
    count.NA <- length(indNA)

    x <- x[indNA.not, ]    # get the observations only; not NAs

  }

  tmp <- data.frame(nrows = nrow(x),
                    median = median(x[, uniVar]),
                    mean = mean(x[, uniVar]),
                    sd = sd(x[, uniVar]),
                    ses = sd(x[, uniVar]) / sqrt(sum(!is.na(x[, uniVar])))
  )
  tmp <- data.frame(round(t(tmp), decs))
  names(tmp) <- colName
  return(tmp)

}

#' Get a complete data frame including the imputed values
#'
get.imp.df <- function(x, mice.imp, uniVar) {
  imp.df <- x
  indNA <- which(is.na(x[, uniVar]))            # which rows are NA
  indNA.not <- which(!((1:nrow(x)) %in% indNA)) # rows that are not NA

  steps.imp <- mice.imp$imp[uniVar][[1]]   # all imputed values ONLY; no observations

  imp.df[indNA, uniVar] <- steps.imp
  return(imp.df)

}
