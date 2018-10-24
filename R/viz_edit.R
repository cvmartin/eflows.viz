
#' Especify the randge of the Y axis in a dygraph
#'
#' @param dy Dygraph
#' @param n Double
#'
#' @return A dygraph, with the especification of the Y axis modified
#' @export
#'
#' @examples
#' 1+1
set_yaxis <- function(dy, n){
  dy$x$attrs$axes$y$valueRange <- c(0, n)
  dy
}

set_group <- function(dy, groupname){
  dy$x$group <- groupname
  dy
}


#' Find the maximum drawing in the Y axis
#'
#' @param list_stacked Dygraph lists that is intended to show stacked
#' @param list_unstacked Dygraph lists that is intended to show unstacked
#'
#' @return a double value
#' @export
#'
#' @examples
#' 1+1
max_yaxis <- function(list_stacked = NULL, list_unstacked = NULL) {
  if (!is.null(list_stacked)){
    stackedmax <- function(q){max(Reduce(`+`, q$x$data[2:length(q$x$data)]))}
    themax_s <- max(sapply(list_stacked, stackedmax)) * 1.05
  } else {
    themax_s <- 0
  }

  if (!is.null(list_unstacked)){
    unstackedmax <- function(q){max(sapply(q$x$data[2:length(q$x$data)], max))}
    themax_u <- max(sapply(list_unstacked, unstackedmax)) * 1.05
  } else {
    themax_u <- 0
  }
  themax <- ifelse(themax_s > themax_u, themax_s, themax_u)
  themax
}


#' Bundle several dygraphs
#'
#' @param ... dygraphs
#' @param ymax if supplied, the y randge of all graphs is et between 0 and `ymax`
#' @param names names to give to the graphs
#'
#' @return list of graphs
#' @export
#'
#' @examples
#' 1+1
viz_bundle <- function(..., ymax = NULL, names = NULL, group = NULL) {
  dylist <- list(...)

  if (!is.null(ymax)) {
    dylist <- lapply(dylist, set_yaxis, n = ymax)
  }
  if (!is.null(names)){
    names(dylist) <- names
  }

  dylist <- lapply(dylist, set_group, groupname = group)

  dylist
}

