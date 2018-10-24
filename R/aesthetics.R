

# color -------------------------------------------------------------------

col = list(
  neutral = "#90B4D2",
  green_success = "green", #green4
  gray_dull = "gray"
)

foregrad <- function(x){
  if (x == 1) return("darkgreen")
  colorRampPalette(c("orange","olivedrab", "darkgreen"))(x)
}

backgrad <- function(x){
  if (x == 1) return("indianred3")
  colorRampPalette(c("darksalmon","indianred3", "purple4"))(x)
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

  dy <- dyCSS(dy, system.file("css", "dygraph_style.css", package = "eflows.viz"))
  dy
}



