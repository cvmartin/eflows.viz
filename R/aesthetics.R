

# color -------------------------------------------------------------------

col = list(
  neutral = "#90B4D2",
  green_success = "green", #green4
  gray_dull = "gray",
  cap = "red"
)

gg_palette <- function(n) {
  # if (n == 0 | is.null(n)) return(NULL)
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

foregrad <- function(n){
  # if (n == 0 | is.null(n)) return(NULL)
  if (n == 1) return("darkgreen")
  colorRampPalette(c("orange","olivedrab", "darkgreen"))(n)
}

backgrad <- function(n){
  # if (n == 0 | is.null(n)) return(NULL)
  if (n == 1) return("indianred3")
  colorRampPalette(c("darksalmon","indianred3", "purple4"))(n)
}


# dygraphs ----------------------------------------------------------------

dy_style <- function(dygraph, units, ...) {
  dy <- dyHighlight(dygraph,
                    highlightSeriesBackgroundAlpha = 0.6,
                    highlightSeriesOpts = list(strokeWidth = 2))

  dy <- dyAxis(dy, "y", label = units)

  dy <- dyLegend(dy, show = "onmouseover", width = 150)

  dy <- dyOptions(dy,
                  mobileDisableYTouch = TRUE,
                  retainDateWindow = TRUE,
                  ...)

  dy <- dyUnzoom(dy)

  dy <- dyCSS(dy, system.file("css", "dygraph_style.css", package = "eflows.viz"))
  dy
}



