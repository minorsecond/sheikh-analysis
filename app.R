library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(mgcv)

ui <- fluidPage(
  titlePanel("Workout Progress Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("fileUpload", "Upload your CSV file", 
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
                 plotOutput("volumeLoadPlot")
        ),
        tabPanel("Explanations",
                 tags$div(
                   h3("Average Weight Progress for Selected Exercise"),
                   p("This graph shows the average weight lifted over time for the selected exercise."),
                   h3("Top Weight Lifted with Median RIR"),
                   p("This graph displays the top weight lifted each day for the selected exercise, with the color indicating the median RIR (Rate of Perceived Exertion)."),
                   h3("Volume Load Over Time"),
                   p("This graph illustrates the total volume load over time for the selected exercise, with separate lines for each main muscle group involved.")
                 )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
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
    }
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
           x = "Date", y = yAxisLabel()) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$topWeightPlot <- renderPlot({
    data <- processedData()
    if (is.null(data) || nrow(data) == 0) return()
    
    top_weights <- data %>%
      filter(Exercise == input$exerciseInput) %>%
      group_by(Date) %>%
      summarise(Top_Weight = max(Weight_Used, na.rm = TRUE)) %>%
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
                           labels = c("5", "2.5", "0"),  # Adjust labels to reflect inverted RIR values
                           na.value = "black",
                           guide = guide_colorbar(title = "RIR", title.position = "top", title.hjust = 0.5)) +
      geom_smooth(method = "gam", formula = y ~ s(x), se = FALSE, color = "blue") +
      theme_minimal() +
      labs(title = paste("Top Weight Lifted with Median RIR for", input$exerciseInput),
           x = "Date", y = yAxisLabel()) +
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
           y = "Total Volume Load") +
      geom_smooth(method = "gam", formula = y ~ s(x), se = FALSE, color = "blue") +
      theme_minimal() +
      theme(legend.position = "right") +
      guides(color = guide_legend(title = "Main Muscle Group"))
  })
}

shinyApp(ui = ui, server = server)