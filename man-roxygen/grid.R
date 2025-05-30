#' Controls grid display.
#' This argument maps to the xaxis and yaxis layout objects in plotly. 
#' Use the plotly `plotly::schema()` command in the console and navigate
#' to layout > layoutAttributes > xaxis or yaxis > gridcolor or gridwidth.
#' It is a Boolean operator. If `FALSE`, no grid is plotted. 
#' If `TRUE`, the default color *grey* and width 1 will be plotted at 
#' major tick marks. If a list, color and width can be customized.<br>
#' <br>
#' Examples:
#' \itemize{
#' \item{`grid = F`}
#' \item{`grid = list(gridcolor = "black", gridwidth = 2)`}
#' }

