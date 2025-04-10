box::use(
  shiny[...],
  DT[...],
  bslib[...],
  fontawesome[fa]
)

# Import necessary libraries for data handling and visualization
box::use(
  app/logic/func_stock[prepare_stock_data],
  app/logic/func_table[process_stock_data,render_market_info,render_stock_table]
)
#' UI function for the stock data module
#' 
#' Creates the user interface for the stock data module
#' 
#' @return A UI definition
#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Include required CSS libraries
    tags$head(
      # Include flag-icon-css for country flags
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/flag-icon-css/3.5.0/css/flag-icon.min.css"),
      # Custom CSS for flag icons and table headers
      tags$style(HTML("
        .flag-icon {
          font-size: 1.5em !important;
          width: 1.5em !important;
          height: 1.5em !important;
          margin: 0 auto;
          display: block !important;
        }
        .dt-center {
          text-align: center !important;
        }
        /* Center the column headers and make them single-line */
        table.dataTable thead th {
          text-align: center !important;
          vertical-align: middle !important;
          white-space: nowrap !important;
        }
        /* Add space for icons in the header */
        table.dataTable thead th i {
          display: inline-block;
          margin-bottom: 5px;
          font-size: 1.2em;
        }
        /* Ensure all cell content is centered */
        table.dataTable tbody td {
          text-align: center !important;
          vertical-align: middle !important;
          white-space: nowrap !important;
        }
      "))
    ),
    
    page(
      title = tags$span(
        fa("chart-line", fill = "#41b6c4"), 
        "Financial Exchange Stock Data"
      ),
      theme = bs_theme(
        bootswatch = "flatly", 
        # Apply YlGnBu palette
        primary = "#225ea8",  # Dark blue
        secondary = "#1d91c0", # Medium blue
        success = "#41b6c4",  # Light blue
        info = "#7fcdbb",     # Teal
        warning = "#c7e9b4",  # Light green
        danger = "#ffffcc"    # Pale yellow
      ),
      
      # Main card with a header that includes the title and download button
      card(
        card_header(
          div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            h4(
              style = "margin: 0;",
              tags$span(
                fa("chart-line", fill = "#41b6c4"), 
                "Financial Exchange Stock Data"
              )
            ),
            # Download button in the header
            downloadButton(ns("download_data"), 
                           label = tags$span(fa("download"), "Download Data"),
                           style = "margin: 0;")
          )
        ),
        
        # Content cards with reduced padding/margins
        div(
          style = "padding: 0;",
          
          # 1. Market Summary - with reduced padding
          card(
            style = "margin-bottom: 10px;",
            card_header(
              style = "padding: 8px 15px;",
              tags$span(fa("globe", fill = "#7fcdbb"), "Market Overview")
            ),
            card_body(
              style = "padding: 10px;",
              uiOutput(ns("market_info"))
            )
          ),
          
          # 2. Data Table - with reduced padding
          card(
            card_header(
              style = "padding: 8px 15px;",
              tags$span(fa("table", fill = "#41b6c4"), "Stock Market Data")
            ),
            card_body(
              style = "padding: 10px;",
              DTOutput(ns("stock_table"))
            )
          )
        )
      )
    )
  )
}

#' Initialize the server function
#' 
#' This function prepares any objects that will be needed by the
#' server function.
#' 
#' @export
init_server <- function(id) {
  callModule(server, id)
}

#' Server function for the stock data module
#' 
#' @export
server <- function(input, output, session) {
    # Get the data
  filtered_data <- reactive({
    data<-prepare_stock_data()$stock_data
    stock_data<-process_stock_data(data)
    return(stock_data)
  })
    
    # Render market info
    output$market_info <- shiny::renderUI({
      render_market_info(filtered_data())
    })
    
    # Render the data table with financial formatting
    output$stock_table <- DT::renderDT({
      render_stock_table(filtered_data())
    })
    
    # Download handler
    output$download_data <- shiny::downloadHandler(
      filename = function() {
        paste("stock_data_", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        utils::write.csv(filtered_data(), file, row.names = FALSE)
      }
    )
}
