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
viz_fore_input <- function(obj,
                           show_fixed = TRUE,
                           aggregate = c("none", "object", "flex", "all")){

  agg <- match.arg(aggregate)

  val$is_e_frame(obj)
  val$has_demand_input(obj)

  x <- obj$demand$input

  if (agg == "none"){
    data <-  mapply(mtx_tag_col,
                    matrix = lapply(x$flex, function(x){x[["data"]]}),
                    name = lapply(x$flex, function(x){x[["name"]]}),
                    vector = lapply(x$flex, function(x){x[["steps"]]}),
                    SIMPLIFY = FALSE)
  }

  if (agg == "object"){
    summed <- do.call(cbind,
                      lapply(lapply(x$flex, function(x){x[["data"]]}),
                             function(x){apply(x, 1, sum)}
                      ))
    colnames(summed) <- lapply(x$flex, function(x){x[["name"]]})
    data <- list(summed)
  }

  if (agg == "flex"){
    data <- list(
      mtx_tag_col(
        matrix = Reduce('+', lapply(x$flex, function(x){x[["data"]]})),
        name = "flex")
    )
  }

  if (agg == "all"){
    summed <- as.matrix(apply(Reduce('+', lapply(x$flex, function(x){x[["data"]]})), 1, sum))
    colnames(summed) <- "flex"
    data <- list(summed)
  }



  data <-  lapply(data, mtx_reverse)
  if (length(data) > 1){
    data <- list(mtx_interleave(data))
  }
  data <- lapply(data, mtx_rm_zerocol)

  data <- do.call(cbind, rev(data))

  pal <- c(foregrad(ncol(data)))

  if (show_fixed == TRUE){
    f <- as.matrix(x$fixed)
    colnames(f) <- "fixed"
    data <- cbind(data, f)
    pal <- c(pal, col$neutral)
  }

  xdata <- mtx_dyprepare(data, obj$setup$time$series)

  dy_style(dygraph(xdata), stackedGraph = TRUE, colors = pal)
}


#' @export
viz_fore_output <- function(obj,
                            show_fixed = TRUE,
                            aggregate = c("none", "object", "flex", "all")){

  agg <- match.arg(aggregate)

  val$is_e_frame(obj)
  val$has_demand_output(obj)

  x <- obj$demand$output

  if (agg == "none"){
    data <-  mapply(mtx_tag_col,
                    matrix = x$flex,
                    name = as.list(names(x$flex)),
                    SIMPLIFY = FALSE)
  }

  if (agg == "object"){
    data <- list(
      do.call(cbind,
              lapply(x$flex, function(x){apply(x, 1, sum)}))
    )
  }

  if (agg == "flex"){
    data <- list(
      mtx_tag_col(
      matrix = Reduce('+', x$flex),
      name = "flex")
    )
  }

  if (agg == "all"){
    summed <- as.matrix(apply(Reduce('+', x$flex), 1, sum))
    colnames(summed) <- "flex"
    data <- list(summed)
  }


  data <-  lapply(data, mtx_reverse)
  if (length(data) > 1){
    data <- list(mtx_interleave(data))
  }
  data <- lapply(data, mtx_rm_zerocol)

  data <- do.call(cbind, rev(data))

  pal <- c(foregrad(ncol(data)))

  if (show_fixed == TRUE){
    f <- as.matrix(x$fixed)
    colnames(f) <- "fixed"
    data <- cbind(data, f)
    pal <- c(pal, col$neutral)
  }

  xdata <- mtx_dyprepare(data, obj$setup$time$series)

  dy_style(dygraph(xdata), stackedGraph = TRUE, colors = pal)

}
