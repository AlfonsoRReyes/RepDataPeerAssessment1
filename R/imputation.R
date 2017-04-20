# A sample function to randomly impute the missing data
# library(imputeTS)
# library(mice)
# library(RepDataPeerAssessment1)


#' ImputeTS function
#' @param In input param
#' @importFrom imputeTS na.random
sss <- function(In){
  out <- na.random(In)
  out <- as.numeric(out)
  return(out)
}

#' Run mice
#' @importFrom mice mice
run.mice.pmm <- function() {
  df  <- activity[, c("steps", "interval")]
  res.mice <- mice(df, method = "pmm")
  mice.imp.1 <- mice::complete(res.mice, 1)
  assign("exit", "TRUE", envir = myEnv)
  assign("out", mice.imp.1$steps, envir = myEnv)
  #return(mice.imp.1$steps)
}

#' pmm
#' @param In input param
pmm <- function(In) {
  # this function will only work with dataset activity
  if (myEnv$exit == FALSE) {
    run.mice.pmm()
  }
  myEnv$out <- as.numeric(out)
  return(out)
}


