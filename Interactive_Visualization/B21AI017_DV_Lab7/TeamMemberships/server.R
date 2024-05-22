library(shiny)
library(ggplot2)

# Define server logic
server <- function(input, output) {
  # Load the UserFollowers dataset from a CSV file
  dataset <- read.csv("C:/Users/HP/Downloads/UserFollowers.csv")
  
  # Histogram of UserId
  output$histPlot <- renderPlot({
    ggplot(data = dataset, aes(x = UserId)) +
      geom_histogram(binwidth = input$bins, fill = "#007bc2", color = "blue") +
      labs(x = "UserId", y = "Frequency", title = "Histogram of UserId in UserFollowers") +
      theme_minimal()  # Use minimal theme for cleaner appearance
  })
  
  # Scatter plot of Id vs FollowingUserId
  output$scatterPlot <- renderPlot({
    ggplot(data = dataset, aes(x = Id, y = FollowingUserId)) +
      geom_point() +
      labs(x = "Id", y = "FollowingUserId", title = "Scatter Plot of Id vs. FollowingUserId in UserFollowers")
  })
}
