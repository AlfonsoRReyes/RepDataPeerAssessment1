# A sample function to randomly impute the missing data
library(imputeTS)

sss <- function(In){
  out <- na.random(In)
  out <- as.numeric(out)
  return(out)
}

pmm <- function(In) {
  # this function will only work with dataset activity
  library(mice)
  library(RepDataPeerAssessment1)

  df  <- activity[, c("steps", "interval")]
  res.mice <- mice(df, method = "pmm")
  mice.imp.1 <- mice::complete(res.mice, 1)
  out <- mice.imp.1$steps
  out <- as.numeric(out)
  return(out)
}
