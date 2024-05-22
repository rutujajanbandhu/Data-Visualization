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
    radioButtons("plot_type", "Plot Type:", choices = c("Violin", "Box"), selected = "Violin")
  ),
  mainPanel(
    plotlyOutput("plot")
  )
)

# Define server logic
server <- function(input, output) {
  output$plot <- renderPlotly({
    if (input$plot_type == "Violin") {
      p <- plot_ly(data = thyroid, x = ~Class, y = ~get(input$feature), type = "violin", box = list(visible = TRUE)) %>%
        layout(title = paste(input$plot_type, "Plot of", input$feature),
               xaxis = list(title = "Class"),
               yaxis = list(title = input$feature)) %>%
        config(displayModeBar = TRUE) # Show the toolbar for Pan and Zoom
    } else {
      p <- plot_ly(data = thyroid, x = ~Class, y = ~get(input$feature), type = "box") %>%
        layout(title = paste(input$plot_type, "Plot of", input$feature),
               xaxis = list(title = "Class"),
               yaxis = list(title = input$feature)) %>%
        config(displayModeBar = TRUE) # Show the toolbar for Pan and Zoom
    }
    p
  })
}

# Run the application
shinyApp(ui = ui, server = server)
