library(shiny)

ui <- fluidPage(
  titlePanel("Demand-Capacity Balancing Heatmap"),
  # Input: Simple integer interval ----
  sliderInput("integer", "Resolution Step:",
              min = 1, max = 498, 
              value = 1),
  htmlOutput("heatmap_output")
)