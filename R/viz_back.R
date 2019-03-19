#' Visualize backshift
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
viz_back_potential <- function(obj,
                           show_fixed = TRUE,
                           show_cap = TRUE,
                           zoom_cap = FALSE,
                           stacked = TRUE,
                           aggregate = c("none", "object", "flex", "all")){


  agg <- match.arg(aggregate)

  val$is_e_frame(obj)

  data <- obj$demand$output$bsh_pot
  data <-  mtx_reverse(data)
  colnames(data) <- c("Potential | max", "Potential | high",
                      "Potential | avg", "Potential | low",
                      "Potential | min")


  pal <- rev(backgrad(5))

  if (show_fixed == TRUE){
    f <- as.matrix(obj$demand$input$fixed) - apply(data,1,sum)
    colnames(f) <- "fixed demand"
    data <- cbind(data, f)
    pal <- c(pal, col$neutral)
  }

  xdata <- mtx_dyprepare(data, obj$setup$time$series)
  xdygraph <- dy_style(dygraph(xdata),
                       units = obj$setup$units$energy,
                       stackedGraph = stacked,
                       fillAlpha = 0.8,
                       colors = pal)

  if (show_cap == TRUE){
    cap_data <- obj$infrastructure$input$grid$capacity %||% NULL
    if (!is.null(cap_data)){
      xdygraph <- add_cap(xdygraph, cap_data, zoom = zoom_cap)
    }
  }

  xdygraph

}

#' @export
viz_back_output <- function(obj,
                               show_fixed = TRUE,
                               show_cap = TRUE,
                               zoom_cap = FALSE,
                               stacked = TRUE,
                               aggregate = c("none", "object", "flex", "all")){


  agg <- match.arg(aggregate)

  val$is_e_frame(obj)

  data <- obj$demand$output$backshifted
  # data <-  mtx_reverse(data)
  colnames(data) <- rev(c("Backshifted | max", "Backshifted | high",
                      "Backshifted | avg", "Backshifted | low",
                      "Backshifted | min"))


  pal <- backgrad(5)

  if (show_fixed == TRUE){
    f <- as.matrix(obj$demand$output$fixed - apply(data,1,sum))

    # unused <- obj$demand$output$fixed - apply(obj$demand$output$bsh_pot,1,sum)
    # unused[unused < 0] <- 0
    # unused <- as.matrix(unused)
    # colnames(unused) <- "unused"
    # f <- f - unused

    colnames(f) <- "fixed demand"
    data <- cbind(data, f)
    pal <- c(pal, col$neutral)
  }

  xdata <- mtx_dyprepare(data, obj$setup$time$series)
  xdygraph <- dy_style(dygraph(xdata),
                       units = obj$setup$units$energy,
                       stackedGraph = stacked,
                       fillAlpha = 0.8,
                       colors = pal)

  if (show_cap == TRUE){
    cap_data <- obj$infrastructure$input$grid$capacity %||% NULL
    if (!is.null(cap_data)){
      xdygraph <- add_cap(xdygraph, cap_data, zoom = zoom_cap)
    }
  }

  xdygraph

}
