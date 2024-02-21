library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(mgcv)

ui <- fluidPage( 
  titlePanel("Sheiko Gold Training Insights"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("fileUpload", "Upload your SG Backup CSV file", 
                accept = c("text/csv", 
                           "text/comma-separated-values,text/plain", 
                           ".csv")),
      selectInput("exerciseInput", "Choose an Exercise:", choices = NULL), # Choices will be updated based on uploaded file
      selectInput("weightUnit", "Select Weight Units:", choices = c("LB", "KG"), selected = "KG"),
      fluidRow(
        column(12, h4("Velocity Over Time Graph Settings")),
        column(12, checkboxInput("lowerRepRange", "Show Lower Rep Range (1-3 reps)", value = FALSE))
      ),
      tags$style(HTML(".thick-hr {border-top: 2px solid black; /* Adjust thickness as needed */}")),
      tags$div(
        class = "thick-hr",
        p("Disclaimer: This web application is independently developed and is not affiliated with Sheiko Gold or its developer. It is intended for analytical purposes only and should not be considered as official software associated with Sheiko Gold or its developer. All data and information provided within this application are generated and processed independently."),
        p("For any inquiries or concerns, please contact the developer at minorsecond@gmail.com."),
        p("Find the project's GitHub repository ", tags$a(href = "https://github.com/minorsecond/sheiko-analysis", "here"), ".")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Graphs", 
                 plotOutput("weightProgressPlot"),
                 plotOutput("topWeightPlot"),
                 plotOutput("volumeLoadPlot"),
                 plotOutput("estimatedOneRepMax"),
                 plotOutput("lowestRIROverTime"),
                 plotOutput("velocityBoxPlot"),
                 plotOutput("velocityOverTimePlot"),
                 plotOutput("velocityLossPlot"),
                 plotOutput("powerLossPlot")
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
                   p("This graph illustrates the median velocity over time for sets performed at Reps in Reserve (RIR) values of 0, 1, and 2 for the selected exercise. A decrease in velocity over time may indicate an improvement in mental toughness and the ability to overcome challenging lifts with reduced RIR."),
                   h3("Estimated 1RM Over Time"),
                   p("This graph displays the estimated 1RM (One Repetition Maximum) over time for the selected exercise. The red line represents the estimated 1RM, and the blue line represents the smoothed estimate with a 95% confidence interval (CI). The grey band around the smoothed line represents the range within which we are 95% confident that the true 1RM falls. 1RMs are only calculated for sets with fewer than 4 reps with less than 3 RIR."),
                   h3("Average Velocity Loss Over Time"),
                   p("This graph illustrates the total velocity loss over time for the selected exercise. Velocity loss is calculated as the difference between the velocity of the first set and the subsequent sets within each workout. A decreasing trend in velocity loss may indicate an improvement in strength or fatigue resistance, while an increasing trend may suggest fatigue or a decline in performance."),
                   p("Interpretations:"),
                   HTML("<ul>
                    <li>No Velocity Loss: If the graph shows minimal or no velocity loss over time, it may indicate that the athlete is maintaining consistent performance throughout their workouts, suggesting good fatigue management and readiness to perform at a high level.</li>
                    <li>Velocity Loss: On the other hand, if the graph shows increasing velocity loss over time, it could suggest accumulating fatigue or potential overreaching, indicating the need for adjustments in training volume, intensity, or recovery strategies.</li>
                  </ul>"),
                   h3("Average Power Loss Over Time"),
                   p("This graph illustrates the average power loss over time for the selected exercise. Power loss is calculated as the percentage difference between the average power of the first set at the heaviest weight lifted and the subsequent sets at the same weight. A decreasing trend in power loss may indicate improved performance or fatigue resistance, while an increasing trend may suggest accumulating fatigue or declining performance."),
                   p("Interpretations:"),
                   HTML("<ul>
                    <li>Decreasing Power Loss: A decreasing trend in power loss over time may indicate improved performance, fatigue resistance, or adaptation to training stimuli.</li>
                    <li>Increasing Power Loss: Conversely, an increasing trend in power loss may suggest accumulating fatigue, overreaching, or declining performance, highlighting the potential need for adjustments in training volume, intensity, or recovery strategies.</li>
                  </ul>")
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
           y = "Average Velocity (m/s)") +
      theme_minimal() +
      scale_fill_brewer(palette = "Set3") +
      labs(fill = "RIR")
  })
  
  output$velocityOverTimePlot <- renderPlot({
    data <- processedData()
    if (is.null(data) || nrow(data) == 0) return()
    
    filtered_data <- data %>%
      filter(Exercise == input$exerciseInput, RIR %in% c(0, 1, 2), Avg_velocity != 0)  # Exclude Avg_velocity of 0
    
    if (input$lowerRepRange) {
      filtered_data <- filtered_data %>%
        filter(Reps_Done >= 1, Reps_Done <= 3)
    } else {
      filtered_data <- filtered_data
    }
    
    median_velocity <- filtered_data %>%
      group_by(Date, RIR) %>%
      summarise(Median_Avg_velocity = median(Avg_velocity, na.rm = TRUE)) %>%
      filter(!is.na(Median_Avg_velocity))  # Remove any NA values
    
    if (nrow(median_velocity) == 0) return(NULL)
    
    ggplot(median_velocity, aes(x = Date, y = Median_Avg_velocity, color = factor(RIR))) +
      geom_line() +
      labs(title = paste("Median Velocity Over Time by RIR for", input$exerciseInput),
           x = "Date", y = "Median Velocity (m/s)",
           color = "RIR") +
      scale_color_brewer(palette = "Set1", name = "RIR") +
      theme_minimal()
  })
  
  output$estimatedOneRepMax <- renderPlot({
    data <- processedData()
    if (is.null(data) || nrow(data) == 0) return()
    
    selected_data <- data %>%
      filter(Exercise == input$exerciseInput)
    
    if (nrow(selected_data) == 0) return(NULL)
    
    one_rep_max_data <- selected_data %>%
      filter(Reps_Done < 4, RIR < 3) %>%
      group_by(Date) %>%
      arrange(Date, desc(Weight_Used)) %>%
      slice(1) %>%
      mutate(Estimated_1RM = Weight_Used * (1 + ((Reps_Done + RIR) / 30)))
    
    if (nrow(one_rep_max_data) == 0) return(NULL)
    
    p <- ggplot() +
      geom_line(data = one_rep_max_data, aes(x = Date, y = Estimated_1RM, color = "Estimated 1RM"), linetype = "solid", size = 1) +
      geom_smooth(data = one_rep_max_data, aes(x = Date, y = Estimated_1RM), method = "gam", se = TRUE, color = "blue") +  # Smoothed line with 95% CI
      geom_text(aes(label = "The grey 95% confidence band represents the range within which we are 95% confident that the true 1RM falls."),
                x = Inf, y = -Inf, hjust = 1, vjust = -1, color = "blue", size = 4) +
      labs(title = paste("Estimated 1RM Over Time for", input$exerciseInput),
           x = "Date",
           y = paste("Estimated 1RM (", input$weightUnit, ")"),
           color = "Metric") +
      scale_color_manual(values = c("Estimated 1RM" = "red"),
                         labels = c("Estimated 1RM")) + 
      theme_minimal()
    
    p + theme(legend.position = "bottom")
  })
  
  output$lowestRIROverTime <- renderPlot({
    data <- processedData()
    if (is.null(data) || nrow(data) == 0) return()
    
    # Filter the data for the selected exercise
    selected_data <- data %>%
      filter(Exercise == input$exerciseInput)
    
    if (nrow(selected_data) == 0) return(NULL)
    
    # Calculate the lowest RIR per workout
    lowest_rir_data <- selected_data %>%
      group_by(Date) %>%
      summarise(Lowest_RIR = min(RIR)) %>%
      filter(Lowest_RIR <= 5)  # Exclude RIR values greater than 5
    
    # Plot the lowest RIR over time
    ggplot(lowest_rir_data, aes(x = Date, y = Lowest_RIR)) +
      geom_line() +
      geom_point() +
      labs(title = paste("Lowest RIR Over Time for", input$exerciseInput),
           x = "Date",
           y = "Lowest RIR") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_reverse()
  })
  
  output$velocityLossPlot <- renderPlot({
    data <- processedData()
    if (is.null(data) || nrow(data) == 0) return()
    
    selected_data <- data %>%
      filter(Exercise == input$exerciseInput)
    
    if (nrow(selected_data) == 0) return(NULL)
    
    selected_data <- selected_data %>%
      mutate(Volume = Weight_Used * Reps_Done)
    
    if (!"Avg_velocity" %in% colnames(selected_data)) return(NULL)
    
    top_sets <- selected_data %>%
      group_by(Date) %>%
      filter(Weight_Used == max(Weight_Used))
    
    # Filter out records with avg_velocity = 0
    velocity_loss_data <- selected_data %>%
      filter(Avg_velocity != 0) %>%
      left_join(top_sets, by = "Date", suffix = c("", "_top")) %>%
      mutate(Velocity_Loss = (Avg_velocity_top - Avg_velocity) / Avg_velocity_top * 100)
    
    # Check if velocity_loss_data is empty
    if (nrow(velocity_loss_data) == 0) return(NULL)
    
    # Calculate average velocity loss per workout
    velocity_loss_per_workout <- velocity_loss_data %>%
      group_by(Date) %>%
      summarise(Avg_Velocity_Loss = mean(Velocity_Loss, na.rm = TRUE))
    
    # Plot velocity loss over time
    ggplot(velocity_loss_per_workout, aes(x = Date, y = Avg_Velocity_Loss)) +
      geom_line() +
      geom_point() +
      labs(title = paste("Average Velocity Loss Across Top Sets for", input$exerciseInput),
           x = "Date",
           y = "Velocity Loss (%)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$powerLossPlot <- renderPlot({
    data <- processedData()
    if (is.null(data) || nrow(data) == 0) return()
    
    selected_data <- data %>%
      filter(Exercise == input$exerciseInput)
    
    if (nrow(selected_data) == 0) return(NULL)
    
    if (!"Avg_power" %in% colnames(selected_data)) return(NULL)
    
    top_sets <- selected_data %>%
      group_by(Date) %>%
      filter(Weight_Used == max(Weight_Used))
    
    power_loss_data <- selected_data %>%
      left_join(top_sets, by = "Date", suffix = c("", "_top")) %>%
      filter(!is.na(Avg_power) & Avg_power != 0) %>%
      mutate(Power_Loss = (Avg_power_top - Avg_power) / Avg_power_top * 100)
    
    power_loss_per_workout <- power_loss_data %>%
      group_by(Date) %>%
      summarise(Avg_Power_Loss = mean(Power_Loss, na.rm = TRUE))
    
    ggplot(power_loss_per_workout, aes(x = Date, y = Avg_Power_Loss)) +
      geom_line() +
      geom_point() +
      labs(title = paste("Average Power Loss Across Top Sets for", input$exerciseInput),
           x = "Date",
           y = "Power Loss (%)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
}

shinyApp(ui = ui, server = server)