# app.R
library(shiny)
library(ggplot2)
library(dplyr)

# Load dataset
sales <- read.csv("sales_dataset.csv")

ui <- fluidPage(
  titlePanel("Sales Dashboard - R Shiny"),
  sidebarLayout(
    sidebarPanel(
      selectInput("region", "Select Region:", choices = c("All", unique(sales$Region))),
      selectInput("product", "Select Product:", choices = c("All", unique(sales$Product)))
    ),
    mainPanel(
      plotOutput("revenueByProduct"),
      plotOutput("revenueOverTime"),
      plotOutput("revenueByRegion")
    )
  )
)

server <- function(input, output) {
  
  filteredData <- reactive({
    data <- sales
    if (input$region != "All") {
      data <- data %>% filter(Region == input$region)
    }
    if (input$product != "All") {
      data <- data %>% filter(Product == input$product)
    }
    return(data)
  })
  
  output$revenueByProduct <- renderPlot({
    ggplot(filteredData(), aes(x = Product, y = Revenue, fill = Product)) +
      geom_bar(stat = "summary", fun = "sum") +
      theme_minimal() +
      labs(title = "Revenue by Product")
  })
  
  output$revenueOverTime <- renderPlot({
    ggplot(filteredData(), aes(x = as.Date(Date), y = Revenue)) +
      geom_line(stat = "summary", fun = "sum", color = "blue") +
      theme_minimal() +
      labs(title = "Revenue Over Time")
  })
  
  output$revenueByRegion <- renderPlot({
    ggplot(filteredData(), aes(x = Region, y = Revenue, fill = Region)) +
      geom_bar(stat = "summary", fun = "sum") +
      theme_minimal() +
      labs(title = "Revenue by Region")
  })
}

shinyApp(ui = ui, server = server)
