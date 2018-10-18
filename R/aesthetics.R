

# color -------------------------------------------------------------------

col = list(
  neutral = "skyblue"
)

foregrad <- function(x){
  if (x == 1) return("olivedrab")
  colorRampPalette(c("orange","olivedrab",  "darkgreen"))(x)
}

backgrad <- function(x){
  if (x == 1) return("indianred3")
  colorRampPalette(c("darksalmon","indianred3", "purple4"))(x)
}


# dygraphs ----------------------------------------------------------------

dy_style <- function(dygraph, ...) {
  dy <- dyHighlight(dygraph,
                    highlightSeriesBackgroundAlpha = 0.6,
                    highlightSeriesOpts = list(strokeWidth = 2))

  dy <- dyLegend(dy, show = "onmouseover", width = 150)

  dy <- dyOptions(dy,
                  fillAlpha = 0.8,
                  mobileDisableYTouch = TRUE,
                  animatedZooms = TRUE,
                  ...)

  dy <- dyCSS(dy, system.file("css", "dygraph_style.css", package = "eflows.viz"))
  dy
}
