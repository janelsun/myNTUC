library(shiny)
library(ggplot2)

function(input, output) {
  
  dataset <- reactive({diamonds[sample(nrow(diamonds), input$sampleSize),]})
  
  output$plot <- renderPlot({
    p <- ggplot(dataset(), aes(x = carat)) + geom_histogram()
    print(p)
  }, height=700)
  
}