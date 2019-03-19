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
viz_storage_flows <- function(obj){
  data.frame(datetime = obj$setup$time$series,
             flow =  obj$demand$output$v_soc) %>%
    df_to_ts() %>%
    dygraph() %>%
    dyAxis("y", label = "kWh") %>%
    dyOptions(fillGraph = TRUE)

}

#' @export
viz_storage_soc <- function(obj){
  data.frame(datetime = obj$setup$time$series,
             `state of charge` =  cumsum(obj$demand$output$v_soc)) %>%
    df_to_ts() %>%
    dygraph() %>%
    dyAxis("y", label = "kWh") %>%
    dyOptions(fillGraph = TRUE, colors = c("orange"))
}
