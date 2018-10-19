
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
  stack_all <- function(q){Reduce(`+`, q$x$data[2:length(q$x$data)])}

  mtx <- do.call(cbind, lapply(dylist, stack_all))
  colnames(mtx) <- colnames

  pal <- c(col$gray_dull, col$green_success)

  d <- xts(x = mtx, order.by = as_datetime(dylist[[1]]$x$data[[1]]))

  dy_style(dygraph(d),
           units = dylist[[1]]$x$attrs$ylabel,
           fillGraph = TRUE,
           colors = pal)
}
