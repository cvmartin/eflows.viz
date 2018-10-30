#' Visualize foreshift
#'
#' @param obj an `e_frame` object
#' @param show_fixed display the original fixed demand
#' @param aggregate level of aggregation.
#'
#' @return
#' dygraph with the results
#' @export
#'
#' @examples
#' 1+1
viz_fore_input <- function(obj,
                           show_fixed = TRUE, stacked = TRUE,
                           aggregate = c("none", "object", "flex", "all")){

  agg <- match.arg(aggregate)
  palette_function <- foregrad

  val$is_e_frame(obj)
  val$has_demand_input(obj)

  route <- obj$demand$input
  local <- list(data = lapply(route$flex, function(x){x[["data"]]}),
                name = lapply(route$flex, function(x){x[["name"]]}),
                steps = lapply(route$flex, function(x){x[["steps"]]}))

  if (agg == "none"){
    data <-  mapply(mtx_tag_col,
                    matrix = local$data,
                    name = local$name,
                    vector = local$steps,
                    SIMPLIFY = FALSE)
  }

  if (agg == "object"){
    summed <- do.call(cbind,
                      lapply(local$data, mtx_rsum))
    colnames(summed) <- local$name
    data <- list(summed)
    palette_function <- gg_palette
  }

  if (agg == "flex"){
      summed <- mtx_tag_col(
        matrix = Reduce('+', local$data),
        name = "flex")
      data <- list(summed)
  }

  if (agg == "all"){
    summed <- as.matrix(mtx_rsum(Reduce('+', local$data)))
    colnames(summed) <- "flex"
    data <- list(summed)
  }

  data <-  lapply(data, mtx_reverse)
  if (length(data) > 1) data <- list(mtx_interleave(data))
  data <- lapply(data, mtx_rm_zerocol)
  data <- do.call(cbind, rev(data))

  pal <- c(palette_function(ncol(data)))

  if (show_fixed == TRUE){
    f <- as.matrix(route$fixed)
    colnames(f) <- "fixed"
    data <- cbind(data, f)
    pal <- c(pal, col$neutral)
  }

  xdata <- mtx_dyprepare(data, obj$setup$time$series)

  dy_style(dygraph(xdata),
           units = obj$setup$units$energy,
           stackedGraph = stacked,
           fillAlpha = 0.8,
           colors = pal)
}


#' @export
viz_fore_output <- function(obj,
                            show_fixed = TRUE, stacked = TRUE,
                            aggregate = c("none", "object", "flex", "all")){

  agg <- match.arg(aggregate)
  palette_function <- foregrad

  val$is_e_frame(obj)
  val$has_demand_output(obj)

  route <- obj$demand$output
  local <- list(data = route$flex,
                name = as.list(names(route$flex))
                )

  if (agg == "none"){
    data <-  mapply(mtx_tag_col,
                    matrix = local$data,
                    name = local$name,
                    SIMPLIFY = FALSE)
  }

  if (agg == "object"){
     summed <- do.call(cbind,
                       lapply(local$data, mtx_rsum))
     data <- list(summed)
     palette_function <- gg_palette
  }

  if (agg == "flex"){
    summed <- mtx_tag_col(
      matrix = Reduce('+', local$data),
      name = "flex")
    data <- list(summed)
  }

  if (agg == "all"){
    summed <- as.matrix(mtx_rsum(Reduce('+', local$data)))
    colnames(summed) <- "flex"
    data <- list(summed)
  }

  data <-  lapply(data, mtx_reverse)
  if (length(data) > 1) data <- list(mtx_interleave(data))
  data <- lapply(data, mtx_rm_zerocol)
  data <- do.call(cbind, rev(data))

  pal <- c(palette_function(ncol(data)))

  if (show_fixed == TRUE){
    f <- as.matrix(route$fixed)
    colnames(f) <- "fixed"
    data <- cbind(data, f)
    pal <- c(pal, col$neutral)
  }

  xdata <- mtx_dyprepare(data, obj$setup$time$series)

  dy_style(dygraph(xdata),
           units = obj$setup$units$energy,
           stackedGraph = stacked,
           fillAlpha = 0.8,
           colors = pal)
}
