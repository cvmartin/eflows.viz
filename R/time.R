
#' Convert a data frame in a time series
#'
#' @param df
#'
#' @return time series
#' @import zoo
#' @import xts
#' @import dygraphs
#' @import lubridate
#' @import shiny
#' @import shinyWidgets
#' @export
#'
#' @examples
#' 1+1
df_to_ts <- function(df) {
  new_ts <- xts::xts(x = df[, 2:(length(colnames(df)))], order.by = df[[1]])
  if (is.null(colnames(new_ts))) colnames(new_ts) <- colnames(df[2])
  new_ts
}

ts_to_df <- function(tseries){
  data.frame(datetime=index(tseries), coredata(tseries))
}


# validation --------------------------------------------------------------
is_POSIXt <- function(x) inherits(x, "POSIXt")
