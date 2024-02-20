library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(mgcv)

ui <- fluidPage(
  titlePanel("Workout Progress Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("fileUpload", "Upload your SG Backup CSV file", 
                accept = c("text/csv", 
                           "text/comma-separated-values,text/plain", 
                           ".csv")),
      selectInput("exerciseInput", "Choose an Exercise:", choices = NULL), # Choices will be updated based on uploaded file
      selectInput("weightUnit", "Select Weight Units:", choices = c("LB", "KG"), selected = "KG")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Graphs", 
                 plotOutput("weightProgressPlot"),
                 plotOutput("topWeightPlot"),
                 plotOutput("volumeLoadPlot"),
                 plotOutput("velocityBoxPlot"),
                 plotOutput("velocityOverTimePlot")
        ),
        tabPanel("Explanations",
                 tags$div(
                   h3("Average Weight Progress for Selected Exercise"),
                   p("This graph shows the average weight lifted over time for the selected exercise."),
                   h3("Top Weight Lifted with Median RIR"),
                   p("This graph displays the top weight lifted each day for the selected exercise, with the color indicating the median RIR (Reps in Reserve)."),
                   h3("Volume Load Over Time"),
                   p("This graph illustrates the total volume load over time for the selected exercise, with separate lines for each main muscle group involved."),
                   h3("Average Velocity by RIR"),
                   p("This box plot shows the distribution of average velocity across different levels of RIR for the selected exercise. The box represents the interquartile range (IQR), with the horizontal line inside the box indicating the median velocity. The whiskers extend to the minimum and maximum velocities within 1.5 times the IQR from the lower and upper quartiles, respectively. Any points beyond the whiskers are considered outliers."),
                   h3("Median Velocity Over Time by RIR"),
                   p("This graph illustrates the median velocity over time for sets performed at Reps in Reserve (RIR) values of 0, 1, and 2 for the selected exercise. A decrease in velocity over time may indicate an improvement in mental toughness and the ability to overcome challenging lifts with reduced RIR.")
                 )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  selectedExercise <- reactiveVal(NULL)
  
  processedData <- reactive({
    req(input$fileUpload, input$weightUnit)  # Wait for file upload and unit selection
    
    df <- read.csv(input$fileUpload$datapath, stringsAsFactors = FALSE) %>%
      mutate(Date = as.Date(Date, format = "%Y-%m-%d"),
             Weight_Used = as.numeric(as.character(Weight_Used)),
             Reps_Done = as.numeric(as.character(Reps_Done)),
             RIR = as.numeric(as.character(RIR))) %>%
      filter(!is.na(Date))
    
    valid_units <- c("LB", "lb", "KG", "kg")
    if (!all(df$Weight_Units %in% valid_units)) {
      stop("Invalid or missing weight units detected in the input data.")
    }
    
    if (input$weightUnit == "KG") {
      df$Weight_Used <- ifelse(toupper(df$Weight_Units) == "LB", df$Weight_Used * 0.453592, df$Weight_Used)
      df$Weight_Units <- "KG"
    } else {
      df$Weight_Used <- ifelse(toupper(df$Weight_Units) == "KG", df$Weight_Used / 0.453592, df$Weight_Used)
      df$Weight_Units <- "LB"
    }
    
    return(df)
  })
  
  yAxisLabel <- reactive({
    paste("Weight Lifted (", input$weightUnit, ")")
  })
  
  observe({
    data <- processedData()
    if (!is.null(data)) {
      updateSelectInput(session, "exerciseInput", choices = unique(data$Exercise))
      
      if (!is.null(selectedExercise())) {
        updateSelectInput(session, "exerciseInput", selected = selectedExercise())
      }
    }
  })
  
  observeEvent(input$exerciseInput, {
    selectedExercise(input$exerciseInput)
  })
  
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
           x = "Date", y = paste("Average Weight Lifted (", input$weightUnit, ")")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$topWeightPlot <- renderPlot({
    data <- processedData()
    if (is.null(data) || nrow(data) == 0) return()
    
    top_weights <- data %>%
      filter(Exercise == input$exerciseInput) %>%
      group_by(Date) %>%
      summarise(Top_Weight = ifelse(all(is.na(Weight_Used)), NA, max(Weight_Used, na.rm = TRUE))) %>%
      ungroup()
    
    filtered_data <- data %>%
      inner_join(top_weights, by = "Date") %>%
      filter(Weight_Used == Top_Weight)
    
    median_rir_data <- filtered_data %>%
      group_by(Date) %>%
      summarise(Top_Weight = first(Top_Weight),
                Median_RIR = median(RIR, na.rm = TRUE)) %>%
      ungroup()
    
    ggplot(median_rir_data, aes(x = Date, y = Top_Weight)) +
      geom_line(color = "gray") +
      geom_point(aes(color = 5 - Median_RIR), size = 3) +  # Invert the RIR value for color mapping
      scale_color_gradient(low = "blue", high = "red",
                           limits = c(0, 5),
                           breaks = c(0, 2.5, 5),
                           labels = c("5+", "2.5", "0"),  # Adjust labels to reflect inverted RIR values
                           na.value = "black",
                           guide = guide_colorbar(title = "RIR", title.position = "top", title.hjust = 0.5)) +
      geom_smooth(method = "gam", formula = y ~ s(x), se = FALSE, color = "blue") +
      theme_minimal() +
      labs(title = paste("Top Weight Lifted with Median RIR for", input$exerciseInput),
           x = "Date", y = paste("Top Weight Lifted (", input$weightUnit, ")")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$volumeLoadPlot <- renderPlot({
    data <- processedData()
    if (is.null(data) || nrow(data) == 0) return()
    
    exercise_data <- data %>%
      filter(Exercise == input$exerciseInput)
    
    volume_load_data <- exercise_data %>%
      mutate(Volume_Load = Weight_Used * Reps_Done) %>%
      group_by(Date, Main_Muscle) %>%
      summarise(Total_Volume_Load = sum(Volume_Load, na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(Date, Main_Muscle)
    
    ggplot(volume_load_data, aes(x = Date, y = Total_Volume_Load, color = Main_Muscle, group = Main_Muscle)) +
      geom_line() +
      labs(title = paste("Volume Load Over Time for", input$exerciseInput),
           x = "Date",
           y = paste("Total Volume (", input$weightUnit, ")")) +
      geom_smooth(method = "gam", formula = y ~ s(x), se = FALSE, color = "blue") +
      theme_minimal() +
      theme(legend.position = "right") +
      guides(color = guide_legend(title = "Main Muscle Group"))
    
  })
  
  output$velocityBoxPlot <- renderPlot({
    data <- processedData()
    if (is.null(data) || nrow(data) == 0) return()
    
    filtered_data <- data %>%
      filter(Avg_velocity != 0, RIR != 6, Exercise == input$exerciseInput)
    if (nrow(filtered_data) == 0) return(NULL) # Graph won't appear without velos data
    
    ggplot(filtered_data, aes(x = factor(RIR), y = Avg_velocity, fill = factor(RIR))) +
      geom_boxplot() +
      labs(title = paste("Average Velocity by RIR for", input$exerciseInput),
           x = "RIR",
           y = "Average Velocity") +
      theme_minimal() +
      scale_fill_brewer(palette = "Set3") +
      labs(fill = "RIR")
  })
  
  output$velocityOverTimePlot <- renderPlot({
    data <- processedData()
    if (is.null(data) || nrow(data) == 0) return()
    
    # Filter data for the selected exercise and RIR values of 0, 1, and 2
    filtered_data <- data %>%
      filter(Exercise == input$exerciseInput, RIR %in% c(0, 1, 2), Avg_velocity != 0)  # Exclude Avg_velocity of 0
    
    # Calculate the median velocity for each RIR per workout
    median_velocity <- filtered_data %>%
      group_by(Date, RIR) %>%
      summarise(Median_Avg_velocity = median(Avg_velocity, na.rm = TRUE)) %>%
      filter(!is.na(Median_Avg_velocity))  # Remove any NA values
    
    # Check if there are any records with valid median velocity data
    if (nrow(median_velocity) == 0) return(NULL)
    
    # Plot median velocity over time by RIR
    ggplot(median_velocity, aes(x = Date, y = Median_Avg_velocity, color = factor(RIR))) +
      geom_line() +
      labs(title = paste("Median Velocity Over Time by RIR for", input$exerciseInput),
           x = "Date", y = "Median Velocity",
           color = "RIR") +
      scale_color_brewer(palette = "Set1", name = "RIR") +  # Change palette to a brighter one
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)