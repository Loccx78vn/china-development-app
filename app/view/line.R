box::use(
  plotly[...],
  shiny[...],
  ggplot2[...],
  ggstatsplot[ggbetweenstats],  
  scales[comma],
  bslib[...],
  grDevices[colorRampPalette]
)

box::use(
  app/constant
)

#' @export
#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$div(
      class = "chart-line-container",
      plotlyOutput(ns("line"))
    ),
    tags$hr(),
    tags$div(
      class = "chart-statistical_chart-container",
      plotOutput(ns("statistical_chart"))
    )
  )  
}


#' @export
init_server <- function(id, df, df_long) {
  callModule(server, id, df, df_long)
}

#' @export
server <- function(input, output, session, df, df_long) {
    
    # Reactive data for boxplot chart:
    long_filtered_data <- reactive({ df_long }) 
    
    # Reactive data for line chart:  
    filtered_data <- reactive({ df }) 
    
    ## Reactive value to track selected year
    rv <- reactiveValues(selected_year = NULL)
    
    ## Observer for clicks on the plot
    observeEvent(event_data("plotly_click"), {
      click_data <- event_data("plotly_click")
      if(!is.null(click_data) && click_data$curveNumber == 3) {
        rv$selected_year <- round(click_data$x)
      }
    })
    
    ## Observer for double-click to reset
    observeEvent(event_data("plotly_doubleclick"), {
      rv$selected_year <- NULL
    })
    
    output$line <- renderPlotly({
    # Check if year is selected
    selected_year <- rv$selected_year
    
    data <- filtered_data()
    
    bar_colors <- colorRampPalette(c(constant$colors[2], constant$colors[5]))(length(sort(unique(data$Year))))
    
  # Line plot:   
    fig <- plot_ly(data, 
                   x = ~Year) 
    
    if (!is.null(selected_year)) {
      # Filter data for selected year
      filtered_data <- data[data$Year == selected_year, ]
      
      # Main line (ghosted) for all years
      fig <- fig %>% 
        add_trace(
          y = ~Gov_Exp, 
          type = 'scatter', 
          mode = 'lines', 
          line = list(color = constant$colors[6], width = 3, dash = 'dot'),
          opacity = 0.3,
          name = "All Years"
        )
      
      # Highlight point for selected year
      fig <- fig %>%
        add_trace(
          data = filtered_data,
          x = ~Year,
          y = ~Gov_Exp,
          type = 'scatter',
          mode = 'markers',
          marker = list(color = constant$colors[7], size = 16, line = list(color = "white", width = 2)),
          name = paste("Year", selected_year),
          text = ~paste("<b>Year", Year, "</b><br>Gov Exp:", round(Gov_Exp, 2)),
          hoverinfo = "text"
        )
      
      # GDP bar for selected year only
      fig <- fig %>%
        add_trace(
          data = filtered_data,
          x = ~Year,
          y = ~GDP,
          type = 'bar',
          marker = list(
            color = bar_colors[which(sort(unique(data$Year)) == selected_year)],
            line = list(color = constant$colors[4], width = 1)
          ),
          name = "GDP",
          text = ~paste("<b>Year", Year, "</b><br>GDP:", round(GDP, 2)),
          hoverinfo = "text"
        )
      
      # Add title for filtered view
      title_text <- paste("Government Expenditure and GDP - Year", selected_year)
    } else {
      # Regular view with all years
      fig <- fig %>% 
      add_trace(y = ~Gov_Exp, 
                type = 'scatter', 
                mode = 'lines', 
                line = list(color = constant$colors[6], width = 4),
                name = "Gov Expenditure") %>% 
      add_trace(x = ~c(Year[1], Year[23]), 
                y = ~c(Gov_Exp[1], Gov_Exp[23]), 
                type = 'scatter', 
                mode = 'markers', 
                marker = list(color = constant$colors[7], size = 14),
                showlegend = FALSE) %>% 
      add_trace(x = ~Year, 
                y = ~GDP, 
                type = 'bar', 
                marker = list(color = bar_colors), 
                name = "GDP") 
      
      title_text <- "Government Expenditure and GDP Example (2000-2023)"
    }
    
    # Main layout with centered title
    fig <- fig %>% layout(
      title = list(
        text = title_text,
        xanchor = "center",
        x = 0.5
      ),
      font = list(size = 14, weight = "bold"),
      xaxis = constant$x_axis, 
      yaxis = constant$y_axis, 
      margin = constant$margin, 
      autosize = TRUE, # Changed to TRUE for responsiveness
      showlegend = FALSE
    )
    
    # Prepare annotations
    annotations_list <- list(constant$y_2000, constant$y_2022)
    
    # Add indicator for selected year if applicable
    if (!is.null(selected_year)) {
      selected_annotation <- list(
        x = 0.5,
        y = 1.05,
        text = paste("<b>Selected Year:", selected_year, "</b>"),
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        font = list(size = 16, color = constant$colors[8])
      )
      
      # Make sure there's a proper line break here
      annotations_list <- c(annotations_list, list(selected_annotation))
    }
    
    # Update layout with annotations (removed the inset chart title since it's now in the instruction panel)
    fig_with_inset <- fig %>% layout(
      annotations = annotations_list,
      barmode = 'stack', 
      xaxis2 = list(
        title = "Year", 
        overlaying = "x", 
        side = "bottom", 
        showgrid = FALSE, 
        zeroline = FALSE
      ), 
      yaxis2 = list(
        title = "Log(GDP in USD)", 
        side = "right", 
        showgrid = FALSE, 
        zeroline = FALSE
      )
    )
    
    # Add inset trace for % government expenditure
    fig_with_inset <- fig_with_inset %>%
      add_trace(
        data = data,
        x = ~Year, 
        y = ~round(Per_Gov_Exp,2),
        type = 'scatter',
        mode = 'lines+markers',
        line = list(color = constant$colors[5], width = 3),
        marker = list(
          size = ~ifelse(Year == selected_year, 10, 5),
          color = ~ifelse(Year == selected_year, constant$colors[8], constant$colors[5]),
          line = list(color = "white", width = 1)
        ),
        name = "% Government Expenditure",
        xaxis = "x3",
        yaxis = "y3",
        hoverinfo = "text",
        text = ~paste("Year:", Year, "<br>% of GDP:", round(Per_Gov_Exp, 2))
      ) %>%
      layout(
        xaxis3 = list(
          domain = c(0.03, 0.34),
          anchor = "y3",
          showticklabels = TRUE,
          tickfont = list(size = 9, color = "rgb(82, 82, 82)"),
          showgrid = TRUE,          gridcolor = "rgba(204, 204, 204, 0.5)",
          zeroline = FALSE,
          showline = TRUE,
          linecolor = "rgba(204, 204, 204, 1)",
          title = "",
          range = c(min(data$Year), max(data$Year)),
          automargin = TRUE
        ),
        yaxis3 = list(
          domain = c(0.69, 0.92),
          anchor = "x3",
          showticklabels = TRUE,
          tickfont = list(size = 9, color = "rgb(82, 82, 82)"),
          zeroline = TRUE,
          zerolinecolor = "rgba(204, 204, 204, 0.5)",
          showgrid = TRUE,
          gridcolor = "rgba(204, 204, 204, 0.5)",
          title = list(text = "% of GDP", font = list(size = 10)),
          range = c(min(data$Per_Gov_Exp) * 0.95, 
                    max(data$Per_Gov_Exp) * 1.05)
        )
      )
    
    # Add inset chart background
    fig_with_inset <- fig_with_inset %>%
      layout(
        shapes = list(
          list(
            type = "rect",
            fillcolor = "rgba(255, 255, 249, 0.8)",  # Translucent background
            line = list(color = constant$colors[5], width = 1.5),  # Medium blue border
            x0 = 0.02, x1 = 0.35,
            y0 = 0.68, y1 = 0.95,
            xref = "paper", yref = "paper",
            layer = "below"  
          )
        )
      )
    # Enable event handling for the plot
    fig_with_inset <- event_register(fig_with_inset, "plotly_click")
    fig_with_inset <- event_register(fig_with_inset, "plotly_doubleclick")
    
    # Configure the plot
    fig_with_inset %>% config(
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c(
        "sendDataToCloud", "editInChartStudio", "lasso2d", "select2d"
      ),
      displaylogo = FALSE
    )
  })
    
    # Create the statistical comparison chart
    output$statistical_chart <- renderPlot({
      data <- long_filtered_data()
     
      # Calculate percentage increase
      gdp_2000 <- data$GDP[data$Year == "2000"]
      gdp_2010 <- data$GDP[data$Year == "2010"]
      percent_increase <- round(mean(((gdp_2010 - gdp_2000) / gdp_2000) * 100), 0)
      
      # Create the ggstatsplot visualization
      p <- ggbetweenstats(
        data = data,
        x = Year,
        y = GDP,
        xlab = "Year",
        ylab = "GDP (Yuan)",
        ggtheme = ggplot2::theme_minimal(),
        package = "yarrr",
        palette = "info2",
        title = "China's GDP Growth by Year"
      ) +
        # Add a coord_cartesian to ensure plot dimensions
        coord_cartesian(
          ylim = c(min(data$GDP) * 0.9, 
                   max(data$GDP) * 1)
        ) +
        # Simplify the scale to avoid dimension issues
        scale_y_continuous(
          labels = scales::comma
        ) +
        # Customizations
        theme(
          # Base font settings
          text = element_text(family = "Roboto", size = 8, color = "black"),
          # Title customization
          plot.title = element_text(
            family = "Lobster Two", 
            size = 18,
            face = "bold",
            color = "#081D58"
          ),
          # Statistical annotations below the main title
          plot.subtitle = element_text(
            family = "Roboto", 
            size = 14, 
            face = "bold",
            color = "#225EA8"
          ),
          plot.title.position = "plot", 
          # Make axis text and titles bold
          axis.text = element_text(size = 10, color = "black"),
          axis.title = element_text(size = 12, face = "bold"),
          # Caption settings
          plot.caption = element_text(
            family = "Roboto",
            size = 10,
            color = "darkgray",
            face = "italic"
          )
        )+
        # Add custom annotation to highlight the percentage change
        annotate(
          "text",
          x = "2010",  # Position in the middle
          y = max(data$GDP) * 0.8,
          label = paste0(round(percent_increase, 1), "% GDP increase\nbetween 2000-2010"),
          color = "#253494",
          size = 4,
          fontface = "bold"
        )
      
      return(p)
    })
}