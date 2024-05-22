library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Plots of UserFollowers"),
  sidebarLayout(
    sidebarPanel(
      numericInput("bins", "Number of bins for histogram:", 30, min = 1, max = 100)
    ),
    mainPanel(
      plotOutput("histPlot"),
      plotOutput("scatterPlot")
    )
  )
)
