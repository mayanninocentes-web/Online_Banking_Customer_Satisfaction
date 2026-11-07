
library(shiny)
library(ggplot2)

# Load dataset
bank_data <- read.csv("online_banking_satisfaction.csv")

# Feminine-inspired color palette
colors <- list(
  pink = "#D78EAD",
  mauve = "#A65F82",
  lavender = "#8979B9",
  gold = "#C58B75",
  plum = "#75445F",
  cream = "#FFF9FB",
  pale = "#F8E8EF"
)

# Dashboard UI
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #FFF9FB;
        font-family: 'Segoe UI', sans-serif;
        color: #75445F;
      }

      .main-header {
        background: linear-gradient(120deg, #D78EAD, #8979B9);
        color: white;
        padding: 25px;
        border-radius: 16px;
        margin-bottom: 22px;
        box-shadow: 0 4px 12px rgba(117,68,95,0.15);
      }

      .main-header h2 {
        font-weight: 700;
        margin-top: 5px;
      }

      .sidebar {
        background-color: #F8E8EF;
        border-radius: 15px;
        padding: 18px;
      }

      .well {
        background-color: #F8E8EF;
        border: none;
        border-radius: 15px;
      }

      .filter-title {
        color: #75445F;
        font-weight: bold;
        margin-bottom: 15px;
      }

      .stat-card {
        background: white;
        border-radius: 15px;
        padding: 20px;
        margin-bottom: 18px;
        text-align: center;
        min-height: 115px;
        box-shadow: 0 3px 12px rgba(117,68,95,0.10);
        border-top: 5px solid #D78EAD;
      }

      .stat-card h4 {
        color: #A65F82;
        font-size: 14px;
        font-weight: 600;
      }

      .stat-card h2 {
        color: #75445F;
        font-size: 27px;
        font-weight: bold;
        margin-top: 8px;
      }

      .section-card {
        background: white;
        padding: 18px;
        border-radius: 15px;
        margin-bottom: 20px;
        box-shadow: 0 3px 12px rgba(117,68,95,0.08);
      }

      .section-card h4 {
        color: #75445F;
        font-weight: bold;
      }

      .btn {
        background-color: #A65F82;
        color: white;
        border: none;
        border-radius: 9px;
        font-weight: bold;
      }

      .btn:hover {
        background-color: #75445F;
        color: white;
      }

      .control-label {
        color: #75445F;
        font-weight: 600;
      }

      table {
        background-color: white;
      }
    "))
  ),
  
  # Dashboard heading
  div(
    class = "main-header",
    h2("Online Banking Usage & Customer Satisfaction"),
    p("An Interactive Dashboard for Exploring Customer Banking Behavior")
  ),
  
  sidebarLayout(
    
    # Sidebar filters
    sidebarPanel(
      class = "sidebar",
      
      h4(class = "filter-title", "Dashboard Filters"),
      
      selectInput(
        "gender",
        "Gender:",
        choices = c("All", sort(unique(bank_data$Gender)))
      ),
      
      selectInput(
        "employment",
        "Employment Status:",
        choices = c("All", sort(unique(bank_data$Employment_Status)))
      ),
      
      selectInput(
        "device",
        "Primary Banking Device:",
        choices = c("All", sort(unique(bank_data$Primary_Banking_Device)))
      ),
      
      selectInput(
        "satisfaction",
        "Overall Satisfaction:",
        choices = c("All", "1", "2", "3", "4", "5")
      ),
      
      actionButton("reset", "Reset All Filters"),
      
      br(), br(),
      p("Use the filters to explore different customer groups.")
    ),
    
    # Main dashboard
    mainPanel(
      
      # Summary cards
      fluidRow(
        column(
          4,
          div(
            class = "stat-card",
            h4("TOTAL CUSTOMERS"),
            h2(textOutput("total_customers"))
          )
        ),
        
        column(
          4,
          div(
            class = "stat-card",
            h4("AVERAGE MONTHLY LOGINS"),
            h2(textOutput("average_logins"))
          )
        ),
        
        column(
          4,
          div(
            class = "stat-card",
            h4("AVERAGE SATISFACTION"),
            h2(textOutput("average_satisfaction"))
          )
        )
      ),
      
      # Graph 1
      div(
        class = "section-card",
        h4("1. Monthly Login Frequency by Banking Device"),
        plotOutput("login_plot", height = "320px")
      ),
      
      # Graph 2
      div(
        class = "section-card",
        h4("2. Customer Satisfaction Ratings"),
        plotOutput("satisfaction_plot", height = "320px")
      ),
      
      # Graph 3
      div(
        class = "section-card",
        h4("3. Overall Satisfaction by Employment Status"),
        plotOutput("employment_plot", height = "320px")
      ),
      
      # Customer table
      div(
        class = "section-card",
        h4("Customer Information Table"),
        p("Filtered customer records are displayed below."),
        tableOutput("customer_table")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Filtered dataset
  filtered_data <- reactive({
    
    data <- bank_data
    
    if (input$gender != "All") {
      data <- data[data$Gender == input$gender, ]
    }
    
    if (input$employment != "All") {
      data <- data[data$Employment_Status == input$employment, ]
    }
    
    if (input$device != "All") {
      data <- data[data$Primary_Banking_Device == input$device, ]
    }
    
    if (input$satisfaction != "All") {
      data <- data[
        data$Overall_Satisfaction == as.numeric(input$satisfaction), ]
    }
    
    data
  })
  
  # Reset filters
  observeEvent(input$reset, {
    updateSelectInput(session, "gender", selected = "All")
    updateSelectInput(session, "employment", selected = "All")
    updateSelectInput(session, "device", selected = "All")
    updateSelectInput(session, "satisfaction", selected = "All")
  })
  
  # Summary cards
  output$total_customers <- renderText({
    nrow(filtered_data())
  })
  
  output$average_logins <- renderText({
    data <- filtered_data()
    if (nrow(data) == 0) return("N/A")
    round(mean(data$Monthly_Login_Frequency), 1)
  })
  
  output$average_satisfaction <- renderText({
    data <- filtered_data()
    if (nrow(data) == 0) return("N/A")
    paste0(round(mean(data$Overall_Satisfaction), 2), " / 5")
  })
  
  # Graph 1
  output$login_plot <- renderPlot({
    
    data <- filtered_data()
    validate(need(nrow(data) > 0, "No customer records match these filters."))
    
    ggplot(
      data,
      aes(
        x = Primary_Banking_Device,
        y = Monthly_Login_Frequency,
        fill = Primary_Banking_Device
      )
    ) +
      geom_boxplot(alpha = 0.85, width = 0.65) +
      scale_fill_manual(values = c(
        "#D78EAD", "#8979B9", "#C58B75", "#A65F82"
      )) +
      labs(
        x = "Banking Device",
        y = "Monthly Login Frequency"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 15, hjust = 1),
        plot.background = element_rect(fill = "white", color = NA)
      )
  })
  
  # Graph 2
  output$satisfaction_plot <- renderPlot({
    
    data <- filtered_data()
    validate(need(nrow(data) > 0, "No customer records match these filters."))
    
    satisfaction_data <- data.frame(
      Rating = c(
        rep("Security", nrow(data)),
        rep("Service", nrow(data)),
        rep("Overall", nrow(data))
      ),
      Score = c(
        data$Security_Satisfaction,
        data$Service_Satisfaction,
        data$Overall_Satisfaction
      )
    )
    
    ggplot(
      satisfaction_data,
      aes(x = factor(Score), fill = Rating)
    ) +
      geom_bar(position = "dodge", width = 0.7) +
      scale_fill_manual(values = c(
        "Security" = "#D78EAD",
        "Service" = "#8979B9",
        "Overall" = "#C58B75"
      )) +
      labs(
        x = "Satisfaction Rating (1 to 5)",
        y = "Number of Ratings",
        fill = "Satisfaction Type"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        legend.position = "bottom"
      )
  })
  
  # Graph 3
  output$employment_plot <- renderPlot({
    
    data <- filtered_data()
    validate(need(nrow(data) > 0, "No customer records match these filters."))
    
    ggplot(
      data,
      aes(
        x = Employment_Status,
        y = Overall_Satisfaction,
        fill = Employment_Status
      )
    ) +
      geom_boxplot(alpha = 0.85, width = 0.65) +
      scale_fill_manual(values = c(
        "#D78EAD", "#8979B9", "#C58B75", "#A65F82"
      )) +
      labs(
        x = "Employment Status",
        y = "Overall Satisfaction"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 15, hjust = 1),
        plot.background = element_rect(fill = "white", color = NA)
      )
  })
  
  # Customer information table
  output$customer_table <- renderTable({
    
    data <- filtered_data()
    validate(need(nrow(data) > 0, "No customer records match these filters."))
    
    data[, c(
      "Customer_ID",
      "Gender",
      "Employment_Status",
      "Primary_Banking_Device",
      "Monthly_Login_Frequency",
      "Overall_Satisfaction"
    )]
    
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
}

# Run dashboard
shinyApp(ui = ui, server = server)
