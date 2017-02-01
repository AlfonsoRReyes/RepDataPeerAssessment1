# A sample function to randomly impute the missing data
library(imputeTS)
library(mice)
library(RepDataPeerAssessment1)

exit = FALSE
out <- 0

sss <- function(In){
  out <- na.random(In)
  out <- as.numeric(out)
  return(out)
}


run.mice.pmm <- function() {
  df  <- activity[, c("steps", "interval")]
  res.mice <- mice(df, method = "pmm")
  mice.imp.1 <- mice::complete(res.mice, 1)
  assign("exit", "TRUE", envir = .GlobalEnv)
  assign("out", mice.imp.1$steps, envir = .GlobalEnv)
  #return(mice.imp.1$steps)
}

pmm <- function(In) {
  # this function will only work with dataset activity
  if (exit == FALSE) {
    run.mice.pmm()
  }
  out <- as.numeric(out)
  return(out)
}


