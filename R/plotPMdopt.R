#' @title Plot Pmetrics D-optimal Times
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Plot PMdopt objects
#' @details
#' This function will plot the output of the \code{\link{Dopt}} function.  A histogram is generated
#' with the probability distribution of each optimal time for each support point in the model,
#' and the weighted mean for that time.
#'
#' @method plot PMdopt
#' @param x The name of an \emph{PMdopt} data object generated by \code{\link{Dopt}}

#' @param col.mean This parameter will be applied to the tick mark indicating the weighted mean
#' optimal time
#' @param lwd.mean This parameter will be applied to the tick mark indicating the weighted mean
#' optimal time
#' @param ticksize.mean This parameter will be applied to the tick mark indicating the weighted mean
#' optimal time
#' @param xlab Define x-axis label.  Default is \dQuote{Time}.
#' @param ylab Define y-axis label.  Default \dQuote{Probability}.
#' @param layout This parameter specifies the number of rows and columns per page, e.g. layout=c(2,2).
#' @param \dots Other parameters as found in \code{\link{plot.default}}.
#' @return Plots the object.
#' @author Michael Neely
#' @seealso \code{\link{Dopt}}, \code{\link{summary.PMdopt}}
#' @export


plot.PMdopt <- function(x, col.mean = "red", lwd.mean = 4, ticksize.mean = 0.1, xlab = "Time", ylab = "Probability", layout = c(1, 1), ...) {
  .par <- par("mfrow") # save current layout
  ntimes <- length(x$mean)
  if (missing(layout)) {
    if (ntimes > 4) {
      par(mfrow = c(2, 2))
      devAskNewPage(T)
    } else {
      par(mfrow = c(ceiling(ntimes / 2), ifelse(ntimes > 2, 2, ntimes)))
    }
  } else {
    par(mfrow = layout)
    if (ntimes > sum(layout)) {
      devAskNewPage(T)
    }
  }
  par(mar = c(5, 5, 4, 2) + 0.1)


  for (i in 1:ntimes) {
    probs <- tapply(x$all$prob, x$all$gridpoint, function(x) x[i])
    times <- tapply(x$all$time, x$all$gridpoint, function(x) x[i])
    plot(x = times, y = probs, type = "h", xlab = xlab, ylab = ylab, main = paste("Mean = ", round(x$mean[i], 3), sep = ""))
    rug(x$mean[i], col = col.mean, lwd = lwd.mean, ticksize = ticksize.mean)
  }
  par(.par)
  devAskNewPage(F)
}
