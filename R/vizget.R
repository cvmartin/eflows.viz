
#' Visualization gadget for bundles of graphs
#'
#' @param dybundle Object generated with viz_bundle
#'
#' @return Shiny gadget
#' @export
#'
#' @examples
#' 1+1
vizget <- function(dybundle) {
  ui <- miniPage(
    gadgetTitleBar(deparse(substitute(dybundle))),
    miniContentPanel(
      fillCol(flex = c(NA, 1),
              div(style = "text-align: center;",
                  uiOutput("rstripe")),
              fillRow(
                dygraphOutput("thegraph", height = 250)
              )
      )
    )
  )
  server <- function(input, output, session) {
    output$thegraph <- renderDygraph({
      dybundle[[input$rbutton]]
    })
    output$rstripe <- renderUI({
      radioButtons("rbutton", NULL, names(dybundle), inline = TRUE)
    })
    observeEvent(input$done, {
      stopApp()
    })
  }
  runGadget(ui, server)
}
