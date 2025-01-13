# Load required libraries
library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(stringr)
library(plotly)

# Read in data
nyuclasses <- read_csv("nyuclasses.csv")

# Filter and clean data
nyuclasses <- nyuclasses %>%
  filter(str_detect(assessment, "Assignment")) %>%
  drop_na(score)

# Convert due_date to Date type
nyuclasses$due_date <- as.Date(nyuclasses$due_date, format = "%m/%d/%Y")

# Define UI ----
ui <- fluidPage(
  titlePanel("Assessment Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("assessment", "Assessment:", choices = c("All", unique(nyuclasses$assessment)))
    ),
    mainPanel(
      fluidRow(
        column(
          width = 4,
          # Title above the box
          tags$p("Minimum Score", style = "text-align: center; font-size: 16px; font-weight: bold; margin-bottom: 0;"),
          wellPanel(
            style = "background-color: #4cbea3; border: 2px solid #4cbea3; color: #ffffff; padding: 10px; text-align: center;",
            textOutput("minBox")
          )
        ),
        column(
          width = 4,
          # Title above the box
          tags$p("Median Score", style = "text-align: center; font-size: 16px; font-weight: bold; margin-bottom: 0;"),
          wellPanel(
            style = "background-color: #4cbea3; border: 2px solid #4cbea3; color: #ffffff; padding: 10px; text-align: center;",
            textOutput("medianBox")
          )
        ),
        column(
          width = 4,
          # Title above the box
          tags$p("Maximum Score", style = "text-align: center; font-size: 16px; font-weight: bold; margin-bottom: 0;"),
          wellPanel(
            style = "background-color: #4cbea3; border: 2px solid #4cbea3; color: #ffffff; padding: 10px; text-align: center;",
            textOutput("maxBox")
          )
        )
      ),
      fluidRow(
        column(
          width = 6, # Increased width
          plotlyOutput("gradePlot", height = "600px") # Increased height
        ),
        column(
          width = 6,
          plotlyOutput("doughnutChart", height = "600px") # Increased height
        )
      )
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  # Default to "All" for assessments
  updateSelectInput(session, "assessment", selected = "All")
  
  # Reactive expression to filter data based on inputs
  filtered_data <- reactive({
    nyuclasses %>%
      filter(input$assessment == "All" | assessment == input$assessment)
  })
  
  # Render maximum score box
  output$maxBox <- renderText({
    data <- filtered_data()
    if (nrow(data) == 0) {
      "No Data Available"
    } else {
      max_score <- max(data$score, na.rm = TRUE)
      paste(max_score)
    }
  })
  
  # Render minimum score box
  output$minBox <- renderText({
    data <- filtered_data()
    if (nrow(data) == 0) {
      "No Data Available"
    } else {
      min_score <- min(data$score, na.rm = TRUE)
      paste(min_score)
    }
  })
  
  # Render median score box
  output$medianBox <- renderText({
    data <- filtered_data()
    if (nrow(data) == 0) {
      "No Data Available"
    } else {
      median_score <- median(data$score, na.rm = TRUE)
      paste(median_score)
    }
  })
  
  # Render grade histogram
  output$gradePlot <- renderPlotly({
    data <- filtered_data()
    
    p <- ggplot(data, aes(x = score)) +
      geom_histogram(binwidth = 1, color = "#4cbea3") +
      labs(title = if (input$assessment == "All") "Assignment Scores Histogram" else input$assessment,
           x = "Score") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 12, face = "bold"), # Adjusted size and style
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10)
      )
    
    ggplotly(p) %>%
      layout(
        title = list(
          text = if (input$assessment == "All") "Assignment Scores Histogram" else input$assessment,
          font = list(size = 12, family = "Arial") # Consistent title font size
        ),
        margin = list(l = 50, r = 50, t = 50, b = 50) # Consistent margins
      )
  })
  
  # Render doughnut chart
  output$doughnutChart <- renderPlotly({
    tryCatch({
      data <- filtered_data()
      
      # Define all possible statuses
      all_statuses <- c("Completed", "Late")
      
      # Check if filtered data is empty
      if (nrow(data) == 0) {
        p <- ggplot() +
          labs(title = "No Data Available") +
          theme_void()
        ggplotly(p) %>%
          layout(
            title = list(
              text = "No Data Available",
              font = list(size = 12, family = "Arial") # Consistent title font size
            ),
            margin = list(l = 50, r = 50, t = 50, b = 50) # Consistent margins
          )
      } else {
        # Calculate counts of completion statuses
        status_count <- data %>%
          mutate(status = case_when(
            !is.na(sub_date) & as.Date(sub_date, format = "%m/%d/%Y") <= due_date ~ "Completed",
            !is.na(sub_date) & as.Date(sub_date, format = "%m/%d/%Y") > due_date ~ "Late"
          )) %>%
          group_by(status) %>%
          summarise(count = n(), .groups = 'drop')
        
        # Ensure all statuses are included in the data frame
        status_count <- status_count %>%
          complete(status = all_statuses, fill = list(count = 0))
        
        # Create doughnut chart with annotation for total submissions
        plot_ly(status_count, labels = ~status, values = ~count, type = 'pie', hole = 0.5,
                textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF'),
                hoverinfo = 'label+percent+value', marker = list(colors = rep('#4cbea3', length(status_count$status)))) %>%
          layout(
            title = list(
              text = "Assessment Status Overview",
              font = list(size = 12, family = "Arial") # Consistent title font size
            ),
            showlegend = FALSE, # Remove legends
            margin = list(l = 50, r = 50, t = 50, b = 50), # Consistent margins
            annotations = list(
              list(
                text = paste("Total Submitted\n", sum(status_count$count)),
                font = list(size = 12, color = '#000000'),
                showarrow = FALSE,
                x = 0.5,
                y = 0.5,
                xref = 'paper',
                yref = 'paper',
                yshift = 10
              )
            )
          )
      }
    }, error = function(e) {
      message("Error in renderPlotly: ", e$message)
      plot_ly(data.frame()) %>%
        layout(
          title = list(
            text = "Error: Check data format",
            font = list(size = 12, family = "Arial") # Consistent title font size
          ),
          margin = list(l = 50, r = 50, t = 50, b = 50) # Consistent margins
        )
    })
  })
}

# Run the Shiny app ----
shinyApp(ui, server)
