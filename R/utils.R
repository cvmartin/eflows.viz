


# set the names of the columns from low to high, preceded by "name".
mtx_tag_col <- function(matrix, name){
  names <- sapply(seq(1:ncol(matrix)),
                  function(x){paste0(name, "_", x)}
  )
  colnames(matrix) <- names
  matrix
}

# swap the order of the columns (last columns are displayed below in dygraphs)
mtx_reverse <- function(matrix){
  matrix[, rev(seq_len(ncol(matrix)))]
}

# remove columns whose sum is zero
mtx_rm_zerocol <- function(matrix){
  matrix[,apply(matrix, 2, sum) != 0]
}

# Prepare the data for dygraph display (returns either a ts or a df)
mtx_dyprepare <- function(matrix, vector){
  if (is_POSIXt(vector)) {
    new_ts <- xts(x = matrix, order.by = vector)
    if (is.null(colnames(new_ts))) colnames(new_ts) <- colnames(df[2])
    return(new_ts)
  }
  as.data.frame(
    cbind(vector, matrix)
  )
}

# display -----------------------------------------------------------------
display_object <- function(obj, show_fixed = TRUE){
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

display_original <- function(obj, show_fixed = TRUE){
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


# combine dygraphs --------------------------------------------------------
dy_equalize_y <- function(stacked, unstacked) {
  stackedmax <- function(q){max(Reduce(`+`, q$x$data[2:length(q$x$data)]))}
  themax_s <- max(sapply(stacked, stackedmax)) * 1.05

  unstackedmax <- function(q){max(sapply(q$x$data[2:length(q$x$data)], max))}
  themax_u <- max(sapply(unstacked, unstackedmax)) * 1.05

  themax <- ifelse(themax_s > themax_u, themax_s, themax_u)

  set_y_range <- function(q){
    q$x$attrs$axes$y$valueRange <- c(0, themax)
    q
  }

  lapply(c(stacked, unstacked), set_y_range)
}

dy_compare_total <- function(list, colnames) {
  stack_all <- function(q){Reduce(`+`, q$x$data[2:length(q$x$data)])}

  mtx <- do.call(cbind, lapply(list, stack_all))
  colnames(mtx) <- colnames

  d <- xts(x = mtx, order.by = as_datetime(list[[1]]$x$data[[1]]))
  dyOptions(dygraph(d), fillGraph = TRUE)
}
