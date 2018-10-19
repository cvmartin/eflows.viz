

mtx_tag_col <- function(matrix, name, vector = seq(1:ncol(matrix))){
  names <- sapply(vector,
                  function(x){paste0(name, "_", x)}
  )
  colnames(matrix) <- names
  matrix
}

# swap the order of the columns (last columns are displayed below in dygraphs)
mtx_reverse <- function(matrix){
  matrix[,rev(seq_len(ncol(matrix))), drop = FALSE]
}

# remove columns whose sum is zero
mtx_rm_zerocol <- function(matrix){
  matrix[,apply(matrix, 2, sum) != 0, drop = FALSE]
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

# interleave matrices
mtx_to_list <- function(mtx){
  l <- split(mtx, rep(1:ncol(mtx), each = nrow(mtx)))
  names(l) <- colnames(mtx)
  l
}
mtx_interleave <- function(mtx_list){
  list_vectorized <- lapply(mtx_list, mtx_to_list)
  list_grouped <- do.call(function(...){c(rbind(...))}, list_vectorized)
  names <- as.vector(
    do.call(rbind,
            lapply(mtx_list, function(x){colnames(x)}))
  )
  names(list_grouped) <- names
  do.call(cbind, list_grouped)
}

# validate -----------------------------------------------------------------

val <- list(
  is_e_frame = function(x){stopifnot(inherits(x, "e_frame"))},
  has_demand_input = function(x){stopifnot(!is.null(x$demand$input))},
  has_demand_output = function(x){stopifnot(!is.null(x$demand$output))}
)



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
