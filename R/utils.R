

# utils -------------------------------------------------------------------

listify <- function(input) {
  if (is.list(input)) {return(input)}
  list(input)
}

`%||%` <- function (x, y){
  if (is.null(x)) y else x
}

# matrix manipulation -----------------------------------------------------
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
    if (is.null(colnames(new_ts))) colnames(new_ts) <- colnames(matrix)
    return(new_ts)
  }

  as.data.frame(cbind(vector, matrix))
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

mtx_rsum <- function(mtx){
  apply(mtx, 1, sum)
}

# validate -----------------------------------------------------------------

val <- list(
  is_e_frame = function(x){stopifnot(inherits(x, "e_frame"))},
  has_demand_input = function(x){stopifnot(!is.null(x$demand$input))},
  has_demand_output = function(x){stopifnot(!is.null(x$demand$output))}
)


# edit (cap) ----------------------------------------------------------------
add_cap <- function(dy, newseries, label = "grid capacity", color = "red", zoom = FALSE){

  # max_y will be the stored maximum, first
  max_y <- ifelse(!is.null(post$x$attrs$axes$y$valueRange),
                  post$x$attrs$axes$y$valueRange,
                  max_yaxis(list_stacked = list(dy)))

  dy$x$data <- append(dy$x$data, list(newseries), 1)
  dy$x$attrs$labels <- append(dy$x$attrs$labels, label, 1)
  dy$x$attrs$colors <- c(color, dy$x$attrs$colors)

  dy$x$attrs$series[[label]] <- list(axis = "y2", fillGraph = FALSE, stackedGraph = FALSE, strokePattern = c(7,3))
  dy$x$attrs$axes$y2$axisLabelWidth <- 0

  if (zoom == TRUE) {
    max_y2 <- max_yaxis(list_unstacked = list(dy))
    if (max_y2 > max_y) max_y <- max_y2
  }

  dy$x$attrs$axes$y$valueRange <- c(0, max_y)
  dy$x$attrs$axes$y2$valueRange <- c(0, max_y)

  dy
}


# viz ---------------------------------------------------------------------

dyUnzoom <-function(dygraph) {
  dyPlugin(
    dygraph = dygraph,
    name = "Unzoom",
    path = system.file("plugins/unzoom.js", package = "dygraphs")
  )
}


viz_blank <- function(obj,
                      route,
                      show_fixed = TRUE,
                      show_cap = TRUE,
                      zoom_cap = FALSE,
                      stacked = TRUE,
                      aggregate = c("none", "object", "flex", "all"),
                      palette_function){

  agg <- match.arg(aggregate)
  palette_function <- palette_function

  val$is_e_frame(obj)

  route <- route

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
  xdygraph <- dy_style(dygraph(xdata),
           units = obj$setup$units$energy,
           stackedGraph = stacked,
           fillAlpha = 0.8,
           colors = pal)

  # the cap is added manipulating the dygraph after it is rendered
  if (show_cap == TRUE){
    cap_data <- obj$infrastructure$input$grid$capacity %||% NULL
    if (!is.null(cap_data)){
      xdygraph <- add_cap(xdygraph, cap_data, zoom = zoom_cap)
    }
  }

  xdygraph
}
