# Load required libraries
library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Plots of KernelTags"),
  sidebarLayout(
    sidebarPanel(
      numericInput("bins", "Number of bins for histogram:", 30, min = 1, max = 100)
    ),
    mainPanel(
      plotOutput("histPlot"),
      plotOutput("barPlot"),
      plotOutput("linePlot"),  # Placeholder for line plot
      plotOutput("scatterPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Load the KernelTags dataset from a CSV file
  dataset <- read.csv("C:/Users/HP/Downloads/KernelTags.csv")
  
  # Histogram of TagIds
  output$histPlot <- renderPlot({
    ggplot(data = dataset, aes(x = TagId)) +
      geom_histogram(binwidth = input$bins, fill = "#007bc2", color = "blue") +
      labs(x = "TagId", y = "Frequency", title = "Histogram of TagIds in KernelTags") +
      theme_minimal()  # Use minimal theme for cleaner appearance
  })
  
  # Bar plot of TagId frequencies
  output$barPlot <- renderPlot({
    tag_freq <- table(dataset$TagId)
    tag_freq_df <- data.frame(TagId = names(tag_freq), Frequency = as.vector(tag_freq))
    
    ggplot(data = tag_freq_df, aes(x = TagId, y = Frequency, fill = TagId)) +
      geom_bar(stat = "identity") +
      labs(x = "TagId", y = "Frequency", title = "Bar Plot of TagIds in KernelTags") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for better readability
  })
  
  # Line plot of TagId frequencies
  output$linePlot <- renderPlot({
    tag_freq <- table(dataset$TagId)
    tag_freq_df <- data.frame(TagId = names(tag_freq), Frequency = as.vector(tag_freq)) %>%
      arrange(TagId)
    
    ggplot(data = tag_freq_df, aes(x = TagId, y = Frequency, group = 1)) +
      geom_line(color = "#ff5733") +
      labs(x = "TagId", y = "Frequency", title = "Line Plot of TagIds in KernelTags") +
      theme_minimal()  # Use minimal theme for cleaner appearance
  })
  
  # Scatter plot of two features (e.g., Id and KernelId)
  output$scatterPlot <- renderPlot({
    ggplot(data = dataset, aes(x = Id, y = KernelId)) +
      geom_point() +
      labs(x = "Id", y = "KernelId", title = "Scatter Plot of Id vs. KernelId in KernelTags")
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
