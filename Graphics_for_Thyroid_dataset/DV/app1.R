library(shiny)
library(plotly)
library(bslib)

# Load data
thyroid <- read.csv("new-thyroid.csv", header = FALSE)
names(thyroid) <- c("Class", "T3_resin", "Serum_thyroxin", "Serum_triiodothyronine", "TSH", "Max_diff_TSH")

# Define UI
ui <- bootstrapPage(
  tags$style(HTML(".sidebar { width: 300px; }")), # Adjust sidebar width if needed
  sidebarPanel(
    selectInput("x_axis", "Select X-axis Feature:", choices = colnames(thyroid)[-1]),
    selectInput("y_axis", "Select Y-axis Feature:", choices = colnames(thyroid)[-1]),
    selectInput("color_by", "Color by Class:", choices = c("Class", "T3_resin", "Serum_thyroxin", "Serum_triiodothyronine", "TSH", "Max_diff_TSH")),
    sliderInput("point_size", "Point Size:", min = 1, max = 10, value = 5),
    selectInput("point_shape", "Point Shape:", choices = c("circle", "square", "diamond"), selected = "circle")
  ),
  mainPanel(
    plotlyOutput("scatter_plot")
  )
)

# Define server logic
server <- function(input, output) {
  output$scatter_plot <- renderPlotly({
    p <- plot_ly(data = thyroid, x = ~get(input$x_axis), y = ~get(input$y_axis), color = ~get(input$color_by)) %>%
      add_trace(type = "scatter", mode = "markers", marker = list(size = input$point_size, symbol = input$point_shape)) %>%
      layout(title = "Scatter Plot",
             xaxis = list(title = input$x_axis),
             yaxis = list(title = input$y_axis),
             showlegend = TRUE) %>%
      config(displayModeBar = TRUE) # Show the toolbar for Pan and Zoom
    p
  })
}

# Run the application
shinyApp(ui = ui, server = server)
