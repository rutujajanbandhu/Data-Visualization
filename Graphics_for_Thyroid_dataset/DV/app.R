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
    selectInput("feature", "Select Feature:", choices = colnames(thyroid)[-1]),
    sliderInput("bins", "Number of Bins:", min = 1, max = 30, value = 10)
  ),
  mainPanel(
    plotlyOutput("histogram")
  )
)

# Define server logic
server <- function(input, output) {
  output$histogram <- renderPlotly({
    p <- ggplot(data = thyroid, aes_string(x = input$feature)) +
      geom_histogram(bins = input$bins, fill = "blue") +
      labs(x = input$feature, y = "Frequency") +
      theme_minimal()
    
    # Convert ggplot to plotly for interactivity
    ggplotly(p) %>%
      layout(dragmode = "zoom", autosize = TRUE) %>%
      config(displayModeBar = TRUE) # Show the toolbar for Pan and Zoom
  })
}

# Run the application
shinyApp(ui = ui, server = server)
