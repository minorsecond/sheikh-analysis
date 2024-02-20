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
  
  # Reactive expression for data processing
  processedData <- reactive({
    inFile <- input$fileUpload
    
    if (is.null(inFile)) {
      return(NULL)
    }
    
    read.csv(inFile$datapath, stringsAsFactors = FALSE) %>%
      mutate(Date = as.Date(Date, format = "%Y-%m-%d"),
             Weight_Used = as.numeric(as.character(Weight_Used)),
             RIR = as.numeric(as.character(RIR))) %>%
      filter(!is.na(Date))
  })
  
  # Dynamically update exercise selection input
  observe({
    data <- processedData()
    updateSelectInput(session, "exerciseInput", choices = unique(data$Exercise))
  })
  
  # Plot for average weight lifted over time
  output$weightProgressPlot <- renderPlot({
    data <- processedData()
    if (is.null(data)) return()
    
    selected_data <- data %>%
      filter(Exercise == input$exerciseInput) %>%
      group_by(Date) %>%
      summarise(Average_Weight = mean(Weight_Used, na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(Date)
    
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
    if (is.null(data) || nrow(data) == 0) return()
    
    # Step 1: Identify the top weight for each day and join this information back to the original dataset
    top_weights <- data %>%
      filter(Exercise == input$exerciseInput) %>%
      group_by(Date) %>%
      summarise(Top_Weight = max(Weight_Used, na.rm = TRUE)) %>%
      ungroup()
    
    # Ensuring that only records with Weight_Used equal to Top_Weight are selected
    filtered_data <- data %>%
      inner_join(top_weights, by = "Date") %>%
      filter(Weight_Used == Top_Weight)
    
    # Step 2: Calculate the median RIR for these filtered records
    median_rir_data <- filtered_data %>%
      group_by(Date) %>%
      summarise(Top_Weight = first(Top_Weight),  # Ensuring Top_Weight is available
                Median_RIR = median(RIR, na.rm = TRUE)) %>%
      ungroup()
    
    # Plotting the data
    ggplot(median_rir_data, aes(x = Date, y = Top_Weight)) +
      geom_line(color = "gray") +
      geom_point(aes(color = Median_RIR), size = 3) +
      scale_color_gradient(low = "red", high = "blue", 
                           limits = c(0, 5), 
                           na.value = "black", 
                           oob = scales::oob_squish) +
      geom_smooth(method = "gam", formula = y ~ s(x), se = FALSE, color = "darkred") +
      theme_minimal() +
      labs(title = paste("Top Weight Lifted with Median RIR for", input$exerciseInput),
           x = "Date", y = "Top Weight Lifted (lb)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

shinyApp(ui = ui, server = server)