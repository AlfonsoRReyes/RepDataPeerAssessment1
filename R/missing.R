utils::suppressForeignCheck(c("x.values", "y.values", "value", "."))



#' Gets the number of NAs
#'
#' @param x vector
#' @return None
#' @export
#'
nas.count <- function(x) {
  length(x[is.na(x) == TRUE])
}


#' Get only non-NAs elements
#'
#' @param x vector with NAs
#' @importFrom stats complete.cases
#' @export
#'
nas.complete <- function(x) {
  complete <- complete.cases(x)
  return(x[complete])
}


#' Get only NA elements
#'
#' @param x vector with NAs
#' @importFrom stats complete.cases
#' @export
#'
nas.missing <- function(x) {
  complete <- complete.cases(x)
  x[!complete]
}



#' Plot NAs variable vs frequency
#' @param df dataframe
#' @param na.var variable with NA
#' @param step increment in the breaks
#' @param ylim limit y-axis
#'
#' @importFrom graphics hist matplot
#' @importFrom mice mdc
#'
#' @export
#'
nas.plot.frequency <- function(df, na.var, step = 30, ylim = 0) {

  # par(mfrow = c(1,2))

  # library(mice)
  # var.name <- eval(substitute(na.var), df, parent.frame())
  # var.name <- deparse(substitute(na.var))
  var.name <- eval(substitute(na.var), df)

  lwd <- 1.5
  nudge <- 1
  fac <- 1.1

  vec <- df[[var.name]]

  max <- max(nas.complete(vec)) * fac
  min <- min(nas.complete(vec))

  breaks <- seq(min, max, step)

  x <- matrix(c(breaks-nudge, breaks+nudge), ncol=2)

  obs <- df[, var.name]
  # mis  <- imp$imp[[var.name]][,1]
  mis <- nas.missing(vec)


  fobs <- c(hist(obs, breaks, plot=FALSE)$counts, 0)
  fmis <- c(hist(mis, breaks, plot=FALSE)$counts, 0)

  y <- matrix(c(fobs, fmis), ncol=2)

  if (length(ylim) < 2) {    # provide temporariry y-axis limits
    ylim1 <- min(fobs)
    ylim2 <- max(fobs)
    ylim <- c(ylim1, ylim2)
    }

  matplot(x, y, type="s",
          col = c(mice::mdc(4), mice::mdc(5)),
          lwd=2, lty=1,
          #xlim = c(0, 170),
          ylim = ylim,
          yaxs = "i",
          xlab="Steps",
          ylab="Frequency")

  list(breaks = breaks, obs = obs, mis = mis, max = max, min = min, len_obs = length(obs), len_mis = length(mis))

}



# ## Put this in the same source file (to remind you that you did it) add:
# if(getRversion() >= "2.15.1")  utils::globalVariables(
#   # needed by function ggplot_missing()
#   c("x.values", "y.values", ".", "value"
#   )
# )

#' A function that plots missingness.
#'    Works only for dataframes with more than 1 variable
#'
#' @param x data.frame
#'
#' @import magrittr
#' @import ggplot2
#' @import reshape2
#' @export
#'
ggplot_missing <- function(x){

  # library(reshape2)
  # library(ggplot2)

  x %>%
    is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = Var2,
               y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present","Missing")) +
    theme_minimal() +
    theme(axis.text.x  = element_text(angle=45, vjust=0.5)) +
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
}

