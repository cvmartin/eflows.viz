
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
  dy$x$attrs$axes$y2$valueRange <- c(0, n)
  dy
}

#' Give the same group name to a list of dygraphs
#'
#' @param dy Dygraph
#' @param n Double
#'
#' @return A dygraph, with the especification of the Y axis modified
#' @export
#'
#' @examples
#' 1+1
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
max_yaxis <- function(list_stacked = list(),
                      list_unstacked = list(),
                      update = FALSE) {
  # Assert that are lists
  stackedmax <- function(q){
    if (!is.null(q$x$attrs$axes$y$valueRange) & update == FALSE) {
      return(q$x$attrs$axes$y$valueRange)
    }
    data <- q$x$data[2:length(q$x$data)]
    max(Reduce(`+`, data))
  }
  unstackedmax <- function(q){
    if (!is.null(q$x$attrs$axes$y$valueRange) & update == FALSE) {
      return(q$x$attrs$axes$y$valueRange)
    }
    data <- q$x$data[2:length(q$x$data)]
    max(sapply(data, max))
  }

  themax_s <- ifelse(length(list_stacked) > 0,
                     max(sapply(list_stacked, stackedmax)) * 1.05,
                     0)
  themax_u <- ifelse(length(list_unstacked) > 0,
                     max(sapply(list_unstacked, unstackedmax)) * 1.05,
                     0)
  ifelse(themax_s > themax_u,
         themax_s,
         themax_u)
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

