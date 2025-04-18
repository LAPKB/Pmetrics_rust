#' @title Print PMdopt
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Print a Pmetrics PMdopt Object made by [Dopt].
#'
#' @method print PMdopt
#' @param x A PMdopt object made by [Dopt].
#' @param ... Not used.
#' @return A printed object.
#' @author Michael Neely
#' @seealso [Dopt]
#' @export

print.PMdopt <- function(x, ...) {
  cat("\nThe mean of the two optimization runs for each optimal sample are:\n\n")
  means <- apply(x$means, 1, mean)
  for (i in 1:length(means)) {
    cat(paste("Time ", i, ": ", sprintf("%.3f", means[i]), "\n", sep = ""))
  }

  cat(paste("\nThe average times between Optimizations 1 and 2 are  ", sprintf("%.3g", mean(100 * (1 - x$all$time1 / x$all$time2))), "% different (P=", sprintf("%.3g", t.test(x$all$time1, x$all$time2)$p.value), ")", sep = ""))
}
