library(shiny)
library(bslib)
library(fontawesome)

ui <- fluidPage(
  theme = bs_theme(
    preset = "default",
    primary = "#003366",
    "navbar-bg" = "white",
    "card-border-color" = "#ddd",
    "card-bg" = "#f9f9f9"
  ),
  
  # Header with title
  div(
    class = "navbar navbar-expand-lg navbar-light bg-white border-bottom shadow-sm py-3",
    div(
      class = "container-fluid",
      span("Economic Dashboard 2025", style = "font-size: 24px; color: #333;"),
      
      # Main Tabs as tabsetPanel
      div(
        class = "ms-auto",
        tabsetPanel(
          id = "main_tabs",
          tabPanel("Overview", value = "overview"),
          tabPanel("Map", value = "map"),
          tabPanel("Stock", value = "stock"),
          type = "pills"
        )
      )
    )
  ),
  
  # Main content
  div(
    class = "container-fluid mt-3",
    fluidRow(
      # Left Sidebar
      column(
        width = 3,
        div(
          class = "card h-100",
          div(
            class = "card-body",
            p("Additional content here.", style = "font-size: 14px; color: #666;")
          )
        )
      ),
      
      # Content Area
      column(
        width = 9,
        div(
          class = "card h-100",
          
          # Sub-header with subtabs and filters
          div(
            class = "card-header",
            div(
              class = "d-flex justify-content-between align-items-center",
              
              # Sub-tabs for Map
              conditionalPanel(
                condition = "input.main_tabs == 'map'",
                tabsetPanel(
                  id = "map_subtabs",
                  tabPanel("Static", value = "static"),
                  tabPanel("Interactive", value = "interactive"),
                  type = "pills"
                )
              ),
              
              # Filters
              div(
                class = "filters d-flex gap-2 justify-content-end",
                conditionalPanel(
                  condition = "input.main_tabs == 'map'",
                  div(
                    id = "yearFilterPlaceholder",
                    uiOutput("selectYear_ui")
                  )
                ),
                conditionalPanel(
                  condition = "input.main_tabs == 'stock'",
                  div(
                    id = "companyFilterPlaceholder",
                    uiOutput("selectCompany_ui")
                  )
                )
              )
            )
          ),
          
          # Content for each tab
          div(
            class = "card-body",
            conditionalPanel(
              condition = "input.main_tabs == 'overview'",
              h4("Overview", class = "chart-header"),
              div(
                class = "chart-container",
                uiOutput("line_ui")
              )
            ),
            
            conditionalPanel(
              condition = "input.main_tabs == 'map' && input.map_subtabs == 'static'",
              h4("Static Map", class = "chart-header"),
              div(
                class = "chart-container",
                uiOutput("staticmap_ui")
              )
            ),
            
            conditionalPanel(
              condition = "input.main_tabs == 'map' && input.map_subtabs == 'interactive'",
              h4("Interactive Map", class = "chart-header"),
              div(
                class = "chart-container",
                uiOutput("interactive_ui")
              )
            ),
            
            conditionalPanel(
              condition = "input.main_tabs == 'stock'",
              h4("Stock", class = "chart-header"),
              div(
                class = "chart-container",
                uiOutput("stock_ui")
              ),
              br(),
              actionButton(
                "showTableBtn", 
                "Show Table", 
                class = "btn btn-outline-primary"
              )
            )
          )
        )
      )
    )
  ),
  
  # Footer
  div(
    class = "footer mt-4 py-3 bg-white border-top",
    div(
      class = "container-fluid d-flex justify-content-between align-items-center",
      div(
        class = "footer-icons",
        tags$a(
          href = "https://github.com", 
          target = "_blank",
          fa_i("github"), " GitHub",
          style = "color: #003366; margin: 0 10px;"
        ),
        tags$a(
          href = "https://facebook.com", 
          target = "_blank",
          fa_i("facebook"), " Facebook",
          style = "color: #003366; margin: 0 10px;"
        )
      ),
      div(
        class = "footer-text",
        HTML("&copy; Created by Loccx78. All rights reserved."),
        style = "font-size: 14px; color: #666;"
      )
    )
  ),
  
  # Modal dialog for the data table
  uiOutput("tableModal")
)

server <- function(input, output, session) {
  # Output UI elements
  output$selectYear_ui <- renderUI({
    # Placeholder for year selection dropdown
    selectInput("year", "Select Year", choices = 2015:2024, selected = 2024)
  })
  
  output$selectCompany_ui <- renderUI({
    # Placeholder for company selection dropdown
    selectInput("company", "Select Company", 
                choices = c("Apple", "Google", "Microsoft", "Amazon"), 
                selected = "Apple")
  })
  
  output$line_ui <- renderUI({
    # Placeholder for line chart
    plotOutput("line_chart", height = "350px")
  })
  
  output$staticmap_ui <- renderUI({
    # Placeholder for static map
    plotOutput("static_map", height = "350px")
  })
  
  output$interactive_ui <- renderUI({
    # Placeholder for interactive map
    plotOutput("interactive_map", height = "350px")
  })
  
  output$stock_ui <- renderUI({
    # Placeholder for stock chart
    plotOutput("stock_chart", height = "350px")
  })
  
  output$datastock_ui <- renderUI({
    # Placeholder for stock data table
    tableOutput("stock_table")
  })
  
  # Modal for data table
  output$tableModal <- renderUI({
    modalDialog(
      title = "Stock Data",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      tableOutput("stock_table_modal")
    )
  })
  
  # Show table modal on button click
  observeEvent(input$showTableBtn, {
    showModal(
      modalDialog(
        title = "Stock Data",
        size = "lg",
        easyClose = TRUE,
        footer = modalButton("Close"),
        tableOutput("stock_table_modal")
      )
    )
  })
  
  # Example outputs
  output$line_chart <- renderPlot({
    plot(1:10, main = "Economic Overview", type = "l")
  })
  
  output$static_map <- renderPlot({
    plot(1:10, main = paste("Static Map for Year", input$year), type = "p")
  })
  
  output$interactive_map <- renderPlot({
    plot(10:1, main = paste("Interactive Map for Year", input$year), type = "p")
  })
  
  output$stock_chart <- renderPlot({
    plot(rnorm(10), main = paste("Stock Chart for", input$company), type = "l")
  })
  
  output$stock_table <- renderTable({
    data.frame(
      Date = seq(as.Date("2023-01-01"), by = "month", length.out = 5),
      Price = round(rnorm(5, 100, 10), 2),
      Volume = sample(1000:10000, 5)
    )
  })
  
  output$stock_table_modal <- renderTable({
    data.frame(
      Date = seq(as.Date("2023-01-01"), by = "day", length.out = 20),
      Price = round(rnorm(20, 100, 10), 2),
      Volume = sample(1000:10000, 20),
      Change = paste0(sample(c("+", "-"), 20, replace = TRUE), 
                      round(runif(20, 0, 5), 2), "%")
    )
  })
}

# Add custom CSS to match your original styling
add_css <- tags$style(HTML("
  .chart-header {
    font-size: 18px;
    font-weight: bold;
    margin-bottom: 10px;
  }
  
  .chart-container {
    height: 100%;
    overflow: auto;
  }
  
  .nav-pills .nav-link {
    color: #003366;
  }
  
  .nav-pills .nav-link.active {
    background-color: #003366;
    color: white;
    font-weight: bold;
  }
  
  .nav-pills .nav-link:hover:not(.active) {
    color: white;
    background-color: #003366;
  }
  
  .shadow-sm {
    box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1) !important;
  }
  
  .card {
    border-color: #ddd;
    background-color: #f9f9f9;
  }
"))

# Run the app with custom CSS
shinyApp(
  ui = tagList(add_css, ui),
  server = server
)