# A sample function to randomly impute the missing data
library(imputeTS)

sss <- function(In){
  out <- na.random(In)
  out <- as.numeric(out)
  return(out)
}
