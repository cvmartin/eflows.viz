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
                           show_fixed = TRUE,
                           show_cap = TRUE,
                           zoom_cap = FALSE,
                           stacked = TRUE,
                           aggregate = c("none", "object", "flex", "all")){
  viz_blank(obj = obj,
            route = obj$demand$input,
            show_fixed = show_fixed,
            show_cap = show_cap,
            zoom_cap = zoom_cap,
            stacked = stacked,
            aggregate = aggregate,
            palette_function = foregrad)
}


#' @export
viz_fore_output <- function(obj,
                            show_fixed = TRUE,
                            show_cap = TRUE,
                            zoom_cap = FALSE,
                            stacked = TRUE,
                            aggregate = c("none", "object", "flex", "all")){

  val$has_demand_output(obj)

  viz_blank(obj = obj,
            route = obj$demand$output,
            show_fixed = show_fixed,
            show_cap = show_cap,
            zoom_cap = zoom_cap,
            stacked = stacked,
            aggregate = aggregate,
            palette_function = foregrad)
}



