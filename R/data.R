#' Activity data (raw)
#'
#' Steps monitoring device data from 2012 as read from CSV file
#'
#' @docType data
#'
#' @usage data(activity.raw)
#'
#' @format A data frame with 17568 rows and 3 variables:
#' \describe{
#'   \item{steps}{ Number of steps taking in a 5-minute interval (missing values are coded as NA)}
#'   \item{date}{The date on which the measurement was taken in YYYY-MM-DD format}
#'   \item{interval}{Identifier for the 5-minute interval in which measurement was taken}
#' }
#'
#'
#' @keywords datasets
#'
#'
"activity.raw"



#' Activity data processed.
#'
#' Steps monitoring device data from 2012. date column converted from factor
#'
#' @docType data
#'
#' @usage data(activity)
#'
#' @format A data frame with 17568 rows and 3 variables:
#' \describe{
#'   \item{steps}{ Number of steps taking in a 5-minute interval (missing values are coded as NA)}
#'   \item{date}{The date on which the measurement was taken in YYYY-MM-DD format}
#'   \item{interval}{Identifier for the 5-minute interval in which measurement was taken}
#' }
#'
#'
#' @keywords datasets
#'
#'
"activity"



#' Activity data by intervals
#'
#' Intervals are the columns, dates (rows), steps as values.
#' Data frame of 61x289
#'
#' @docType data
#'
#' @usage data(intervals)
#'
#' @format a data frame derivative of activity
#'
#' @keywords datasets
#'
#'
"intervals"


#' Activity data by intervals with date as rownames
#'
#' Intervals are the columns, steps are values. The date is rownames.
#' Data frame of 61x288. The date as rownames.
#'
#' @docType data
#'
#' @usage data(intervals)
#'
#' @format a data frame derivative of activity
#'
#' @keywords datasets
#'
#'
"intervals.only"
