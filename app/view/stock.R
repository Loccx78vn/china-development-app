box::use(
  shiny[...],
  highcharter[...],
  app/logic/func_stock[prepare_stock_data],
  RColorBrewer[brewer.pal],
  utils[head]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$div(
      class = "chart-stock-container",
      highchartOutput(ns("stock"))
    )
  )
}

#' @export
server <- function(id, data_filter, company) {
  moduleServer(id, function(input, output, session) {
    
    
    filtered_data <- reactive({
      data <- prepare_stock_data()
      # Add debugging messages
      message("Class of prepare_stock_data(): ", paste(class(data), collapse = ", "))
      message("Names in prepare_stock_data(): ", paste(names(data), collapse = ", "))
      message("Current value of company(): ", company())
      
      req(company())
      
      if(company() == "All") {  
        return(data$stock_data)
      } else {
        return(data$individual_stock_data)  
      }
    })

    # Create the stock chart based on filter
    output$stock <- renderHighchart({
      req(filtered_data(), company(), data_filter) 
      
      data <- filtered_data()
      selected_company <- company()
      data_check <- data_filter
      
      if(is.null(data)) {
        return(highchart() %>% hc_title(text = "No data available in stock module"))
      }
      
      if(selected_company == "All") { 
        # Generate YlGnBu color palette
        # Get 9 colors from the YlGnBu palette
        ylgnbu_colors <- brewer.pal(9, "YlGnBu")
        
        highchart() %>%
          hc_title(
            text = "Stock Prices of Shipping Companies (2024 - Now) in USD"
          ) %>%
          hc_xAxis(type = "datetime") %>%
          hc_yAxis(
            title = list(text = "Stock Price (USD)"),
            opposite = FALSE
          ) %>%
          hc_add_series(
            data = data,
            type = "line",
            hcaes(x = date, y = (open + close) / 2 * exchange_rate, group = name),
            name = "Mean Price",
            yAxis = 0
          ) %>%
          # Apply the YlGnBu color palette
          hc_colors(ylgnbu_colors) %>%
          hc_tooltip(
            shared = FALSE,
            valueDecimals = 2,
            pointFormat = '<b style="color: black;">Stock Exchange:</b> {point.symbol}<br><b style="color: black;">Mean Price/Day:</b> {point.y}',
            headerFormat = '<span style="font-size: 12px; font-weight: bold; color:{point.series.color};">{point.key}</span><br>',
            followPointer = TRUE
          ) %>%
          hc_rangeSelector(
            enabled = TRUE,
            buttons = list(
              list(type = "day", count = 1, text = "1d"),
              list(type = "week", count = 1, text = "1w"),
              list(type = "month", count = 1, text = "1m"),
              list(type = "year", count = 1, text = "1y"),
              list(type = "all", text = "All")
            ),
            inputEnabled = FALSE
          ) %>%
          hc_legend(
            enabled = TRUE  # Enable legend to show colors by company
          ) %>%
          hc_chart(
            events = list(
              load = JS(
                "function() {
                  var chart = this;
                  chart.update({
                    navigation: {
                      menuItemStyle: {
                        fontSize: '12px'
                      }
                    },
                    exporting: {
                      enabled: true,
                      buttons: {
                        resetButton: {
                          text: 'Reset',
                          onclick: function() {
                            chart.yAxis[0].update({ min: null, max: null });
                          }
                        },
                        rangeButton1: {
                          text: 'Low Range',
                          onclick: function() {
                            chart.yAxis[0].update({ min: 0, max: 100 });
                          }
                        },
                        rangeButton2: {
                          text: 'Mid Range',
                          onclick: function() {
                            chart.yAxis[0].update({ min: 100, max: 300 });
                          }
                        },
                        rangeButton3: {
                          text: 'High Range',
                          onclick: function() {
                            chart.yAxis[0].update({ min: 1000, max: 2000 });
                          }
                        }
                      }
                    }
                  });
                }"
              )
            )
          )
      } else {
        # Individual company chart logic
        selected_ticker <- names(which(data_check == selected_company))
        
        if (length(selected_ticker) == 0 || is.null(data[[selected_ticker]])) {
          return(highchart() %>% hc_title(text = paste("No data available for", selected_company)))
        }
        
        stock_data <- data[[selected_ticker]]
        
        # For individual company, use specific colors from YlGnBu palette
        ylgnbu_color <- brewer.pal(9, "YlGnBu")[5]  # Middle color from palette
        
        highchart(type = "stock") %>% 
          hc_title(text = paste("Individual Stock Data -", selected_company)) %>%
          hc_add_series(
            stock_data, 
            name = selected_company, 
            type = "candlestick",
            color = ylgnbu_color,        # Down color
            lineColor = ylgnbu_color,    # Line color
            upColor = brewer.pal(9, "YlGnBu")[2]  # Up color (lighter shade)
          ) %>%
          hc_rangeSelector(
            enabled = TRUE,
            buttonPosition = list(
              align = "left",
              x = 0,
              y = 0
            ),
            buttonTheme = list(
              width = 60,  
              style = list(
                fontSize = "12px"
              )
            ),
            buttons = list(
              list(type = "day", count = 1, text = "1d"),
              list(type = "week", count = 1, text = "1w"),
              list(type = "month", count = 1, text = "1m"),
              list(type = "all", text = "All")
            ),
            inputEnabled = FALSE
          ) %>%
          hc_tooltip(
            valueDecimals = 2,
            pointFormat = '<b style="color: black;">{series.name}</b><br>Open: {point.open}<br>High: {point.high}<br>Low: {point.low}<br>Close: {point.close}',
            backgroundColor = "rgba(255, 255, 255, 0.95)",
            borderWidth = 0,
            borderRadius = 5,
            shadow = TRUE
          ) %>%
          hc_legend(enabled = FALSE) %>%  # Removed legend as requested
          hc_chart(
            backgroundColor = "#f8f9fa",
            borderRadius = 8,
            style = list(fontFamily = "Arial, Helvetica, sans-serif"),
            zoomType = "xy",
            panning = TRUE,
            panKey = "shift"
          ) %>% 
          hc_xAxis(
            gridLineWidth = 0.5,
            gridLineColor = "#e6e6e6",
            lineColor = "#ccc",
            tickColor = "#ccc",
            labels = list(
              style = list(
                color = "#666",
                fontSize = "12px"
              )
            ),
            title = list(
              text = "Date",
              style = list(
                fontSize = "14px",
                fontWeight = "bold",
                color = "#555"
              )
            )
          ) %>% 
          hc_yAxis(
            gridLineWidth = 0.5,
            gridLineColor = "#e6e6e6",
            lineColor = "#ccc",
            tickColor = "#ccc",
            labels = list(
              style = list(
                color = "#666",
                fontSize = "12px"
              )
            ),
            title = list(
              text = "Price",
              style = list(
                fontSize = "14px",
                fontWeight = "bold",
                color = "#555"
              )
            )
          ) %>%
          hc_plotOptions(
            candlestick = list(
              lineWidth = 1.5,
              states = list(
                hover = list(
                  lineWidth = 2
                )
              ),
              shadow = FALSE
            )
          ) %>%
          hc_navigator(
            enabled = TRUE,
            outlineColor = "#92a8d1",
            outlineWidth = 1,
            handles = list(
              backgroundColor = "#92a8d1",
              borderColor = "#92a8d1"
            )
          ) %>%
          hc_credits(enabled = FALSE)
      }
    })
  })
}