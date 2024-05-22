# Load required libraries
library(shiny)
library(ggplot2)

# Define server logic
server <- function(input, output) {
  
  # Load the DatasetTags dataset from a CSV file
  dataset <- read.csv("C:/Users/HP/Downloads/DatasetTags.csv")
  
  # Histogram of TagIds
  output$histPlot <- renderPlot({
    ggplot(data = dataset, aes(x = TagId)) +
      geom_histogram(binwidth = input$bins, fill = "#007bc2", color = "blue") +
      labs(x = "TagId", y = "Frequency", title = "Histogram of TagIds in DatasetTags") +
      theme_minimal()  # Use minimal theme for cleaner appearance
  })
  
  # Bar plot of TagId frequencies
  output$barPlot <- renderPlot({
    tag_freq <- table(dataset$TagId)
    tag_freq_df <- data.frame(TagId = names(tag_freq), Frequency = as.vector(tag_freq))
    
    ggplot(data = tag_freq_df, aes(x = TagId, y = Frequency, fill = TagId)) +
      geom_bar(stat = "identity") +
      labs(x = "TagId", y = "Frequency", title = "Bar Plot of TagIds in DatasetTags") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for better readability
  })
  
  # Scatter plot of two features (e.g., Id and DatasetId)
  output$scatterPlot <- renderPlot({
    ggplot(data = dataset, aes(x = Id, y = DatasetId)) +
      geom_point() +
      labs(x = "Id", y = "DatasetId", title = "Scatter Plot of Id vs. DatasetId in DatasetTags")
  })
  
}

