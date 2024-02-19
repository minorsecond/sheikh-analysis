library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(mgcv)

# Load your data
# Ensure to change the path to where your actual data is stored
workout_data <- read.csv('backupDB.csv', stringsAsFactors = FALSE)

# Make sure the Date column is in Date format and Weight is numeric
workout_data$Date <- as.Date(workout_data$Date, format="%Y-%m-%d")
workout_data$Weight <- as.numeric(as.character(workout_data$Weight_Used))

# Filter out rows where Date is NA
workout_data <- workout_data %>% filter(!is.na(Date))

# User Interface
ui <- fluidPage(
  titlePanel("Workout Progress Over Time"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("exerciseInput", "Choose an Exercise:", 
                  choices = unique(workout_data$Exercise))
    ),
    
    mainPanel(
      plotOutput("weightProgressPlot")
    )
  )
)

server <- function(input, output) {
  
  output$weightProgressPlot <- renderPlot({
    selected_data <- workout_data %>%
      filter(Exercise == input$exerciseInput) %>%
      group_by(Date) %>%
      summarise(Average_Weight = mean(Weight_Used, na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(Date)
    
    # Prepare the plot with a GAM smooth line
    ggplot(selected_data, aes(x = Date, y = Average_Weight)) +
      geom_line() +
      geom_point() +
      geom_smooth(method = "gam", formula = y ~ s(x), se = FALSE, color = "blue") + # GAM smooth line
      theme_minimal() +
      labs(title = paste("Average Weight Progress for", input$exerciseInput),
           x = "Date",
           y = "Average Weight Lifted (lb)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Run the application
shinyApp(ui = ui, server = server)