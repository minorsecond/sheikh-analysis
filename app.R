library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(mgcv)

workout_data <- read.csv('backupDB.csv', stringsAsFactors = FALSE)

workout_data$Date <- as.Date(workout_data$Date, format="%Y-%m-%d")
workout_data$Weight <- as.numeric(as.character(workout_data$Weight_Used))

# Filter out rows where Date is NA
workout_data <- workout_data %>% filter(!is.na(Date))

ui <- fluidPage(
  titlePanel("Workout Progress Over Time"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("fileUpload", "Upload your CSV file", 
                accept = c("text/csv", 
                           "text/comma-separated-values,text/plain", 
                           ".csv")),
      selectInput("exerciseInput", "Choose an Exercise:", choices = NULL) # Choices will be updated based on uploaded file
    ),
    
    mainPanel(
      plotOutput("weightProgressPlot"), # Average weight plot
      plotOutput("topWeightPlot") # Top weight plot
    )
  )
)

server <- function(input, output, session) {
  processedData <- reactive({
    inFile <- input$fileUpload
    
    if (is.null(inFile))
      return(NULL)
  
    read.csv(inFile$datapath, stringsAsFactors = FALSE) %>%
      mutate(Date = as.Date(Date, format = "%Y-%m-%d"),
             Weight = as.numeric(as.character(Weight_Used))) %>%
      filter(!is.na(Date))
  })
  
  # Update 'selectInput' choices based on uploaded file
  observe({
    data <- processedData()
    updateSelectInput(session, "exerciseInput", choices = unique(data$Exercise))
  })
  
  output$weightProgressPlot <- renderPlot({
    data <- processedData()
    if (is.null(data)) return()
    
    selected_data <- data %>%
      filter(Exercise == input$exerciseInput) %>%
      group_by(Date) %>%
      summarise(Average_Weight = mean(Weight, na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(Date)
    
    if (nrow(selected_data) == 0) return()
    
    ggplot(selected_data, aes(x = Date, y = Average_Weight)) +
      geom_line() +
      geom_point() +
      geom_smooth(method = "gam", formula = y ~ s(x), se = FALSE, color = "blue") +
      theme_minimal() +
      labs(title = paste("Average Weight Progress for", input$exerciseInput),
           x = "Date", y = "Average Weight Lifted (lb)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$topWeightPlot <- renderPlot({
    data <- processedData()
    if (is.null(data)) return()
    
    top_weight_data <- data %>%
      filter(Exercise == input$exerciseInput) %>%
      group_by(Date) %>%
      summarise(Top_Weight = max(Weight, na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(Date)
    
    if (nrow(top_weight_data) == 0) return()
    
    ggplot(top_weight_data, aes(x = Date, y = Top_Weight)) +
      geom_line() +
      geom_point() +
      geom_smooth(method = "gam", formula = y ~ s(x), se = FALSE, color = "blue") +
      theme_minimal() +
      labs(title = paste("Top Weight Lifted for", input$exerciseInput),
           x = "Date", y = "Top Weight Lifted (lb)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

shinyApp(ui = ui, server = server)