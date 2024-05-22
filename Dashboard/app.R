library(shiny)
library(shinydashboard)
library(dygraphs)
library(quantmod)
library(ggplot2)
library(dplyr)

# Define UI ----
ui <- dashboardPage(
  dashboardHeader(title = "Stock Market Analysis Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Stock Analysis", tabName = "stock_analysis", icon = icon("line-chart")),
      selectInput("tickerInput", "Select Ticker:", 
                  choices = c("AAPL", "GOOGL", "MSFT", "AMZN", "NFLX"),  # Add more ticker options here
                  selected = "AAPL"),
      dateRangeInput("dateRangeInputSidebar", "Date Range:", start = Sys.Date() - 365, end = Sys.Date()),
      selectInput("colorInput", "Price Distribution Color:", choices = c("LightBlue", "LightGreen", "Orange"), selected = "Blue")
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(
        HTML("
          .content-wrapper { background-color: black; }
          .content-wrapper { overflow: hidden; } /* Hide overflow to prevent scrolling */
          .content-wrapper, .sidebar { height: auto !important; } /* Auto height for content and sidebar */
          .sidebar {
            background-color: black; /* Change background color to black */
            border-right: 1px solid #e0e0e0; /* Add right border */
          }
          .tab-content { padding: 0; } /* Remove padding for tab content */
          .output-chart { max-height: 350px; } /* Set maximum height for output charts */
        ")
      )
    ),
    tabItems(
      tabItem(
        tabName = "stock_analysis",
        fluidRow(
          box(
            background = 'black',
            title = "Price Change Distribution",
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 6,
            height = "300px",
            div(style = "overflow-x: auto;",
                plotOutput("priceDistributionPlot", height = "250px"))  # Adjust height as needed
          ),
          box(
            background = 'black',
            title = "Stock Price Chart",
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 6,
            height = "300px",
            div(style = "overflow-x: auto;",
                dygraphOutput("stockChart", height = "250px"))  # Adjust height as needed
          )
        ),
        fluidRow(
          splitLayout(
            cellWidths = c("33%", "33%", "34%"),
            box(
              background = 'black',
              title = "Last 7 Days",
              status = "warning",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = "100%",
              height = "450px",
              div(style = "overflow-x: auto;",
                  plotOutput("last7DaysPlot", height = "400px"))  # Adjust height as needed
            ),
            box(
              background = 'black',
              title = "Last 30 Days",
              status = "danger",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = "100%",
              height = "450px",
              div(style = "overflow-x: auto;",
                  plotOutput("last30DaysPlot", height = "400px"))  # Adjust height as needed
            ),
            box(
              background = 'black',
              title = "Selected Days",
              status = "success",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = "100%",
              height = "450px",
              div(style = "overflow-x: auto;",
                  plotOutput("selectedDaysPlot", height = "400px"))  # Adjust height as needed
            )
          )
        )
      )
    )
  ),
  tags$style(".content-wrapper { background-color: #f5f5f5; }")  # Add background color to content wrapper
)


# Define server logic ----
server <- function(input, output) {
  # Reactive function to fetch stock data based on inputs
  stock_data <- reactive({
    ticker <- input$tickerInput
    start_date <- input$dateRangeInputSidebar[1]
    end_date <- input$dateRangeInputSidebar[2]
    
    getSymbols(ticker, from = start_date, to = end_date, auto.assign = FALSE)
  })
  
  # Render stock chart with improved axis visibility
  output$stockChart <- renderDygraph({
    stock_data <- stock_data()
    dygraph(stock_data, main = paste("Stock Price Chart -", input$tickerInput)) %>%
      dyLegend(labelsSeparateLines = TRUE) %>%
      dyOptions(axisLabelFontSize = 12, axisLabelColor = "white", digitsAfterDecimal = 2)
  })
  
  
  output$priceDistributionPlot <- renderPlot({
    stock_data <- stock_data()
    color <- switch(input$colorInput,
                    "LightBlue" = "lightblue",
                    "LightGreen" = "lightgreen",
                    "Orange" = "orange")
    
    ggplot(data = stock_data, aes(x = index(stock_data), y = Cl(stock_data))) +
      geom_line(color = color) +
      labs(x = "Date", y = "Closing Price", title = "Price Change Distribution") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "black"),  # Set plot background to black
        panel.grid.major = element_line(color = "lightgrey"),  # Add major grid lines in grey color
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.text = element_text(color = "white"),  # Set axis text color to white
        axis.title = element_text(color = "white")  # Set axis title color to white
      )
  })
  
  
  # Render plots for last 7 days, last 30 days, and selected days
  output$last7DaysPlot <- renderPlot({
    stock_data <- stock_data()
    par(bg = "black", col.axis = "white", col.lab = "white")
    plot(index(tail(stock_data, 7)), Cl(tail(stock_data, 7)), type = "l", main = "Last 7 Days", col = "yellow")
  })
  
  output$last7DaysBarChart <- renderPlot({
    stock_data <- stock_data()
    par(bg = "black", col.axis = "white", col.lab = "white")
    barplot(Vo(tail(stock_data, 7)), main = "Stock Volume - Last 7 Days", col = "yellow")
  })
  
  output$last30DaysPlot <- renderPlot({
    stock_data <- stock_data()
    par(bg = "black", col.axis = "white", col.lab = "white")
    plot(index(tail(stock_data, 30)), Cl(tail(stock_data, 30)), type = "l", main = "Last 30 Days", col = "red")
  })
  
  output$last30DaysBarChart <- renderPlot({
    stock_data <- stock_data()
    par(bg = "black", col.axis = "white", col.lab = "white")
    barplot(Vo(tail(stock_data, 30)), main = "Stock Volume - Last 30 Days", col = "red")
  })
  
  output$selectedDaysPlot <- renderPlot({
    stock_data <- stock_data()
    par(bg = "black", col.axis = "white", col.lab = "white")
    plot(index(stock_data), Cl(stock_data), type = "l", main = "Selected Days", col = "green")
  })
  
  output$selectedDaysBarChart <- renderPlot({
    stock_data <- stock_data()
    par(bg = "black", col.axis = "white", col.lab = "white")
    barplot(Vo(stock_data), main = "Stock Volume - Selected Days", col = "green")
  })
  
  outputOptions(output, "stockChart", suspendWhenHidden = FALSE)
}


# Run the app ----
shinyApp(ui = ui, server = server)
