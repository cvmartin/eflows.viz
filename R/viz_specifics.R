viz_vector <- function(obj, path_data, path_unit, name){

  data <- as.matrix(path_data)
  colnames(data) <- c(name)

  xdata <- mtx_dyprepare(data, obj$setup$time$series)

  dy_style(dygraph(xdata),
           units = path_unit,
           fillGraph = TRUE,
           includeZero = TRUE)
}



# Graph of the price

#' Compare two or more dygraphs
#'
#' @param obj e_frame object.
#'
#' @return A dygraph
#' @export
#'
#' @examples
#' 1+1
viz_price <- function(obj) {
  viz_vector(obj,
             path_data = obj$utility$input$price,
             path_unit = obj$setup$units$price,
             name = "price")
}

# viz_price <- function(obj){
#   data <- as.matrix(obj$utility$input$price)
#   colnames(data) <- c("price")
#
#   xdata <- mtx_dyprepare(data, obj$setup$time$series)
#
#   dy_style(dygraph(xdata),
#            units = obj$setup$units$price,
#            fillGraph = TRUE)
# }


# Graph of the fixed production

#' Compare two or more dygraphs
#'
#' @param obj e_frame object.
#'
#' @return A dygraph
#' @export
#'
#' @examples
#' 1+1
viz_production_fixed <- function(obj) {
  viz_vector(obj,
             path_data = obj$production$sum_fixed,
             path_unit = obj$setup$units$energy,
             name = "fixed production")
}


# Graph of the fixed demand

#' Compare two or more dygraphs
#'
#' @param obj e_frame object.
#'
#' @return A dygraph
#' @export
#'
#' @examples
#' 1+1
viz_demand_fixed <- function(obj) {
  viz_vector(obj,
             path_data = obj$demand$input$fixed,
             path_unit = obj$setup$units$energy,
             name = "fixed demand")
}



#' See the capacity
#'
#' @param obj e_frame object.
#'
#' @return A dygraph
#' @export
#'
#' @examples
#' 1+1
viz_cap <- function(obj) {
  # viz_vector(obj,
  #            path_data = obj$infrastructure$input$grid$capacity,
  #            path_unit = obj$setup$units$energy,
  #            name = "grid_capacity")
  data <- as.matrix(obj$infrastructure$input$grid$capacity)
  colnames(data) <- c("grid capacity")

  xdata <- mtx_dyprepare(data, obj$setup$time$series)

  dy_style(dygraph(xdata),
           units = obj$setup$units$energy,
           colors = "red",
           strokePattern = c(7,3),
           includeZero = TRUE
           )
}

# Graph of the fitting curve

#' Compare two or more dygraphs
#'
#' @param obj e_frame object.
#'
#' @return A dygraph
#' @export
#'
#' @examples
#' 1+1
viz_fit <- function(obj) {
  fit_intial <- viz_vector(obj,
                           path_data = obj$utility$input$fit$curve,
                           path_unit = "utility",
                           name = "initial fitting curve")

  fit_final <- viz_vector(obj,
                           path_data = obj$utility$output$fit$curve,
                           path_unit = "utility",
                           name = "final fitting curve")

  viz_compare(list(fit_intial,fit_final), colnames = c("initial fitting curve",
                                                       "final fitting curve"))
}



