library(shiny)

shinyServer(function(input, output) {
  # MA-plot
  # output$plotma <- renderPlot({
  #   par( mar=c(5,5,3,2), cex.main=1.5, cex.lab=1.35 )
  #   plotMA( res, ylim=c(-ymax, ymax) )
  # })
})