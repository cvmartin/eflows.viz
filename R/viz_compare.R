
#' Compare two or more dygraphs
#'
#' @param dylist List of dygraphs to compare. Normally two.
#' @param colnames vector with the names to display in dygraph legend
#'
#' @return A dygraph
#' @export
#'
#' @examples
#' 1+1
viz_compare <- function(dylist, colnames) {

  stack_depend <- function(q){
    datafrom <- ifelse(isTRUE(q$x$attrs$axes$y2$usedCap), 3, 2)
    data <- q$x$data[datafrom:length(q$x$data)]
    Reduce(`+`, data)
    }

  mtx <- do.call(cbind, lapply(dylist, stack_depend))
  colnames(mtx) <- colnames

  pal <- c(col$gray_dull, col$green_success)

  d <- xts(x = mtx, order.by = as_datetime(dylist[[1]]$x$data[[1]]))

  dy_style(dygraph(d),
           units = dylist[[1]]$x$attrs$ylabel,
           fillGraph = TRUE,
           includeZero = TRUE,
           colors = pal)
}
