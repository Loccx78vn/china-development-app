
library(shiny)
library(bslib)
library(plotly)
library(leaflet)
library(DT)
library(bsicons)
library(shinyjs)

ui <- fluidPage(
  # Apply custom theme
  theme = bs_theme(
    version = 5, 
    bootswatch = "flatly", 
    primary = "#1b9e77", 
    "navbar-bg" = "#081D58"
  ) %>%
    bs_add_rules(
      ".bg-gradient { background: linear-gradient(135deg, #ffffcc, #a1dab4, #41b6c4, #2c7fb8, #253494); } 
      .card-header { background-color: #41b6c4; color: white; font-weight: bold; }"
    ),
  
  # Custom CSS
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css"),
    tags$style(HTML("
                   html, body {
  height: 100%;
  width: 100%;
  margin: 0;
  padding: 0;
  font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
}

.container-fluid {
  min-height: 100%;
  position: relative;
  padding: 0 !important;
  margin: 0 !important;
  width: 100% !important;
  max-width: 100% !important;
  overflow-y: visible;
}

/* Fix for page_fluid container */
.page-fill > .container-fluid {
  overflow-y: visible;
  padding: 0 !important;
  margin: 0 !important;
}

.header { 
  background-color: #253494; 
  color: white;
  padding: 15px 20px; 
  display: flex; 
  justify-content: space-between; 
  align-items: center;
  flex-wrap: wrap;
  box-shadow: 0 2px 5px rgba(0,0,0,0.2);
  margin-bottom: 20px;
  width: 100%;
}

.header-buttons {
  display: flex;
  gap: 10px;
}

.sidebar-left { 
  background-color: #f8f9fa; 
  padding: 15px; 
  box-shadow: 0 2px 5px rgba(0,0,0,0.1);
  height: 100%;
}

.footer { 
  background-color: #253494; 
  color: white;
  padding: 15px; 
  display: flex; 
  justify-content: space-between; 
  align-items: center; 
  margin-top: 20px;
  width: 100%;
  flex-wrap: wrap;
  left: 0;
  right: 0;
}

.footer-icons { 
  display: flex; 
  gap: 15px; 
  flex-wrap: wrap;
}

.footer-icons a {
  color: white;
  text-decoration: none;
}

.footer-icons a:hover {
  color: #a1dab4;
}

.chart-header { 
  font-size: 18px; 
  font-weight: bold; 
  margin-bottom: 15px; 
  color: #2c7fb8;
}

.modal-overlay {
  display: none;
  position: fixed;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  background: rgba(0,0,0,0.7);
  z-index: 1000;
  justify-content: center;
  align-items: center;
}

.modal-content {
  background: white;
  border-radius: 8px;
  box-shadow: 0 4px 20px rgba(0,0,0,0.3);
  width: 85%;
  max-height: 85%;
  overflow: auto;
  padding: 20px;
  position: relative;
}

.close-modal {
  position: absolute;
  top: 10px;
  right: 15px;
  font-size: 24px;
  cursor: pointer;
  color: #666;
}

.close-modal:hover {
  color: #000;
}

.card {
  box-shadow: 0 2px 8px rgba(0,0,0,0.1);
  border: none;
  margin-bottom: 20px;
  overflow: auto;
  width: 100%;
  box-sizing: border-box;
}

.btn-primary {
  background-color: #2c7fb8;
  border-color: #2c7fb8;
}

.btn-primary:hover {
  background-color: #253494;
  border-color: #253494;
}

.badge {
  background-color: #41b6c4;
}

.nav-link { 
  font-weight: 500; 
}

/* Remove tab titles */
.nav-tabs .nav-link, .nav-pills .nav-link {
  border-radius: 0.25rem 0.25rem 0 0;
  padding: 0.5rem 1rem;
}

/* Center align content */
.center-content {
  display: flex;
  justify-content: center;
  align-items: center;
}

/* Responsive adjustments */
@media (max-width: 768px) {
  .header {
    flex-direction: column;
    align-items: flex-start;
  }
  .header-buttons {
    margin-top: 10px;
  }
  .footer {
    flex-direction: column;
    gap: 10px;
  }
  .footer-text {
    text-align: center;
    font-size: 0.9em;
  }
}

/* Fix for Bootstrap row margins which can cause whitespace */
.row {
  margin-right: 0 !important;
  margin-left: 0 !important;
  width: 100% !important;
}

/* Fix for bslib row and column containers */
.bslib-page-fill, .bslib-row, .bslib-column, .row, .col {
  padding: 0 !important;
  margin: 0 !important;
  max-width: 100% !important;
  overflow: visible;
}

/* Ensure no extra whitespace in the layout */
.page-fluid, .page-fill {
  padding: 0 !important;
  margin: 0 !important;
  max-width: 100% !important;
  width: 100% !important;
}

/* Ensure charts scale properly */
.plotly, .leaflet-container {
  width: 100% !important;
  max-width: 100%;
}

/* Ensure content scales */
img, svg {
  max-width: 100%;
  height: auto;
}

/* Improve scrolling for tables */
.dataTables_wrapper {
  width: 100%;
  overflow-x: auto;
}

/* Fix for value boxes */
.value-box {
  width: 100%;
  overflow: hidden;
}

/* Fix for tab content */
.tab-content, .tab-pane {
  overflow: visible;
  width: 100%;
}

/* Adjust paddings for inner content */
.card-body {
  width: 100%;
  box-sizing: border-box;
} 
                    
                    ")
    )
  ),

# Header
div(class = "header",
    h2("Economic Dashboard 2025"),
    div(class = "header-buttons", 
        actionButton("showTableBtn", HTML("<i class='fas fa-table'></i> Show Stock Data"), class = "btn btn-light btn-sm")
    )
),

# Main content with navset
layout_columns(
  # Left sidebar
  col_widths = c(3, 9),
  card(class = "sidebar-left h-100",
       card_image(src = "https://via.placeholder.com/300x150?text=Dashboard+Info", class = "card-img-top"),
       tags$h4("Dashboard Information", class = "mt-3"),
       tags$p("This dashboard provides economic insights across different perspectives:"),
       tags$ul(
         tags$li(tags$strong("Overview:"), " Key economic indicators over time"),
         tags$li(tags$strong("Map:"), " Geographic visualization of economic data"),
         tags$li(tags$strong("Stock:"), " Market performance of major companies")
       ),
       tags$hr(),
       tags$p(class = "text-muted fst-italic", "Last updated: March 2023"),
       tags$div(class = "d-grid gap-2", downloadButton("downloadData", "Download Data", class = "btn-outline-primary"))
  ),
  
  # Main content area with tabs (no titles)
  card(
    navset_card_underline(
      # Overview Tab
      nav_panel(
        title = "Overview",
        div(class = "chart-header", "Economic Growth Overview"),
        card(class = "bg-light p-3", plotlyOutput("lineChart", height = "400px")),
        layout_column_wrap(
          width = 1/3,
          card(card_header("GDP Growth"), "4.2%", tags$span(class = "text-success", HTML("<i class='fas fa-arrow-up'></i> 0.3%"))),
          card(card_header("Inflation"), "2.1%", tags$span(class = "text-danger", HTML("<i class='fas fa-arrow-up'></i> 0.5%"))),
          card(card_header("Unemployment"), "3.6%", tags$span(class = "text-success", HTML("<i class='fas fa-arrow-down'></i> 0.2%")))
        )
      ),
      
      # Map Tab
      nav_panel(
        title = "Map",
        navset_underline(
          # Static Map
          nav_panel(
            title = "Static",
            div(class = "chart-header", "Economic Indicators by Region"),
            layout_column_wrap(
              width = 1/4,
              selectInput("selectYear", "Select Year:", choices = 2020:2023, width = "100%"),
              selectInput("selectIndicator", "Indicator:", choices = c("GDP Growth", "Inflation", "Unemployment"), width = "100%")
            ),
            card(class = "p-3 bg-light", plotOutput("staticMap", height = "500px"))
          ),
          # Interactive Map
          nav_panel(
            title = "Interactive",
            div(class = "chart-header", "Interactive Economic Map"),
            layout_column_wrap(
              width = 1/4,
              selectInput("selectYear2", "Select Year:", choices = 2020:2023, width = "100%"),
              selectInput("selectView", "View Mode:", choices = c("Markers", "Heatmap", "Choropleth"), width = "100%")
            ),
            card(class = "p-3 bg-light", leafletOutput("interactiveMap", height = "500px"))
          )
        )
      ),
      
      # Stock Tab
      nav_panel(
        title = "Stock",
        div(class = "chart-header", "Stock Market Performance"),
        layout_column_wrap(
          width = 1/4,
          selectInput("selectCompany", "Select Company:", choices = c("Apple", "Google", "Microsoft", "Amazon"), width = "100%"),
          selectInput("timeRange", "Time Range:", choices = c("1 Month", "3 Months", "6 Months", "1 Year"), selected = "3 Months", width = "100%")
        ),
        card(class = "p-3 bg-light", plotlyOutput("stockChart", height = "400px")),
        layout_column_wrap(
          width = 1/3,
          value_box(title = "Opening Price", value = textOutput("openPrice"), showcase = bsicons::bs_icon("arrow-up-right")),
          value_box(title = "Closing Price", value = textOutput("closePrice"), showcase = bsicons::bs_icon("arrow-down-right")),
          value_box(title = "Trading Volume", value = textOutput("volume"), showcase = bsicons::bs_icon("bar-chart"))
        )
      )
    )
  )
),

# Stock data modal
div(id = "stockTableModal", class = "modal-overlay", 
    div(class = "modal-content", 
        span(id = "closeModal", class = "close-modal", HTML("×")), 
        h3("Stock Data Table"),
        div(id = "stockTableContainer", DTOutput("stockTable"))
    )
),

# Footer that stays at the bottom
div(class = "footer", 
    div(class = "footer-icons", 
        a(href = "https://github.com", target = "_blank", HTML("<i class='fab fa-github'></i> GitHub")),
        a(href = "https://linkedin.com", target = "_blank", HTML("<i class='fab fa-linkedin'></i> LinkedIn")),
        a(href = "mailto:contact@example.com", HTML("<i class='fas fa-envelope'></i> Contact"))
    ), 
    div(class = "footer-text", HTML("© 2023 Economic Dashboard by Loccx78. All rights reserved."))
),

# JavaScript for modal operation
tags$script(HTML("
    $(document).ready(function() {
      $('#showTableBtn').click(function() {
        $('#stockTableModal').fadeIn(300);
      });
      
      $('#closeModal').click(function() {
        $('#stockTableModal').fadeOut(300);
      });
      
      $('#stockTableModal').click(function(e) {
        if (e.target == this) {
          $(this).fadeOut(300);
        }
      });
      
      $(window).on('resize', function() {
        Plotly.Plots.resize($('.plotly').get(0));
      });
      
      function adjustFooter() {
        var docHeight = $(window).height();
        var footerHeight = $('.footer').height();
        var footerTop = $('.footer').position().top + footerHeight;
        
        if (footerTop < docHeight) {
          $('.footer').css('margin-top', (docHeight - footerTop) + 'px');
        }
      }
      
      adjustFooter();
      $(window).resize(adjustFooter);
    });
  "))
)




server <- function(input, output, session) {
  observeEvent(input$showTableBtn, {
    # This runs when the button is clicked
    runjs('
    document.querySelector(".modal-overlay").style.display = "flex";
  ')
  })
  
  # Also make sure you have a handler to close the modal
  observeEvent(input$closeModal, {
    runjs('
    document.querySelector(".modal-overlay").style.display = "none";
  ')
  })
  # Sample data for line chart (Overview tab)
  gdp_data <- data.frame(
    Year = 2015:2023,
    GDP = c(18.2, 18.7, 19.5, 20.5, 21.4, 20.9, 22.9, 25.0, 26.9)
  )
  
  # Overview tab output
  output$lineChart <- renderPlotly({
    plot_ly(gdp_data, x = ~Year, y = ~GDP, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Annual GDP Growth",
             xaxis = list(title = "Year"),
             yaxis = list(title = "GDP in Trillion USD"))
  })
  
  # Map tab - Static output
  output$staticMap <- renderPlot({
    selected_year <- input$selectYear
    # For demonstration, just showing different colors for different years
    par(mar = c(0,0,2,0))
    # Simple placeholder map
    plot(1:10, 1:10, type = "n", xlab = "", ylab = "", 
         main = paste("Static Economic Map for Year", selected_year))
    text(5, 5, paste("Economic Data for", selected_year), cex = 1.5)
  })
  
  # Map tab - Interactive output
  output$interactiveMap <- renderLeaflet({
    selected_year <- input$selectYear2
    
    # Create a basic interactive map with sample points
    leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 30, zoom = 2) %>%
      addMarkers(lng = c(-74, -0.1, 116.4, 151.2), 
                 lat = c(40.7, 51.5, 39.9, -33.9),
                 popup = c(paste("New York -", selected_year),
                           paste("London -", selected_year),
                           paste("Beijing -", selected_year),
                           paste("Sydney -", selected_year)))
  })
  
  # Stock tab outputs
  output$stockChart <- renderPlotly({
    selected_company <- input$selectCompany
    
    # Generate sample stock data based on selected company
    companies <- c("Apple", "Google", "Microsoft", "Amazon")
    set.seed(which(companies == selected_company))
    days <- 90
    dates <- seq(as.Date("2023-01-01"), by = "day", length.out = days)
    
    stock_price <- cumsum(rnorm(days, mean = 0.1, sd = 1)) + 100
    
    stock_data <- data.frame(
      Date = dates,
      Price = stock_price
    )
    
    plot_ly(stock_data, x = ~Date, y = ~Price, type = 'scatter', mode = 'lines') %>%
      layout(title = paste(selected_company, "Stock Performance"),
             xaxis = list(title = "Date"),
             yaxis = list(title = "Stock Price ($)"))
  })
  
  # Stock table data
  output$stockTable <- renderDT({
    selected_company <- input$selectCompany
    companies <- c("Apple", "Google", "Microsoft", "Amazon")
    
    # Generate sample stock data for table
    set.seed(which(companies == selected_company))
    days <- 30
    dates <- seq(as.Date("2023-01-01"), by = "day", length.out = days)
    
    stock_price <- cumsum(rnorm(days, mean = 0.1, sd = 1)) + 100
    volume <- sample(100000:1000000, days)
    open_price <- stock_price - rnorm(days, mean = 0, sd = 0.5)
    close_price <- stock_price + rnorm(days, mean = 0, sd = 0.5)
    
    stock_table <- data.frame(
      Date = dates,
      Open = round(open_price, 2),
      High = round(pmax(stock_price, open_price, close_price), 2),
      Low = round(pmin(stock_price, open_price, close_price), 2),
      Close = round(close_price, 2),
      Volume = volume
    )
    
    datatable(stock_table, 
              options = list(pageLength = 10),
              caption = paste("Stock Data for", selected_company))
  })
}

shinyApp(ui = ui, server = server)