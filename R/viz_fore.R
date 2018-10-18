#' Visualize foreshift
#'
#' @param obj an `e_frame` object
#' @param show_fixed display the original fixed demand
#'
#' @return
#' dygraph with the results
#' @export
#'
#' @examples
#' 1+1
viz_fore_input <- function(obj, show_fixed = TRUE){

  val$is_e_frame(obj)
  val$has_demand_input(obj)

  x <- obj$demand$input

  data <-  mapply(mtx_tag_col,
                  lapply(x$flex, function(x){x[["data"]]}),
                  lapply(x$flex, function(x){x[["name"]]}),
                  SIMPLIFY = FALSE)

  data <-  lapply(data, mtx_reverse)
  data <- lapply(data, mtx_rm_zerocol)
  data <- do.call(cbind, rev(data))

  pal <- c(foregrad(ncol(data)))

  if (show_fixed == TRUE){
    f <- as.matrix(x$fixed)
    colnames(f) <- "fixed"
    data <- cbind(data, f)
    pal <- c(pal, neutral)
  }

  xdata <- mtx_dyprepare(data, obj$time$series)

  dy <- dyOptions(dygraph(xdata),
                  stackedGraph = TRUE,
                  colors = pal,
                  fillAlpha = 0.7)
  dy

}

#' @export
viz_fore_output <- function(obj, show_fixed = TRUE){

  val$is_e_frame(obj)
  val$has_demand_output(obj)

  x <- obj$demand$output

  data <-  mapply(mtx_tag_col,
                  x$demand_flex,
                  as.list(names(x$demand_flex)),
                  SIMPLIFY = FALSE)

  data <-  lapply(data, mtx_reverse)
  data <- lapply(data, mtx_rm_zerocol)
  data <- do.call(cbind, rev(data))

  pal <- c(foregrad(ncol(data)))

  if (show_fixed == TRUE){
    f <- as.matrix(x$demand_fixed)
    colnames(f) <- "fixed"
    data <- cbind(data, f)
    pal <- c(pal, neutral)
  }

  xdata <- mtx_dyprepare(data, obj$time$series)

  dy <- dyOptions(dygraph(xdata),
                  stackedGraph = TRUE,
                  colors = pal,
                  fillAlpha = 0.7)
  dy
}