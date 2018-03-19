library(shiny)
library(ggplot2)

dataset <- diamonds

fluidPage(
  
  titlePanel("MyNTUC App Dashboard"),
  
  sidebarPanel(
    sliderInput('sampleSize', 'Sample Size', min=1, max=nrow(dataset),
                value=min(1000, nrow(dataset)), step=500, round=0)
  ),
  
  mainPanel(
    plotOutput('plot')
  )
)
