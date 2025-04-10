box::use(
  shiny[moduleServer, NS, tags, tagList, plotOutput, reactive, observe, 
        observeEvent, req, validate, need, callModule, renderPlot],
  ggplot2[...],
  dplyr[filter, arrange, mutate, select, summarise, group_by, n, desc],
  patchwork[plot_layout, plot_annotation,plot_spacer],
  RColorBrewer[brewer.pal],
  ggthemes[theme_map],
  ggrepel[geom_text_repel],
  sf[st_as_sf],
  stats[na.omit]
)

box::use(
  app/constant,
  app/logic/func_map[func_barchart, func_toptier, create_gdp_labels, plot_gdp_boxplot, plot_bar_chart]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$div(
      class = "chart-map-container",
      plotOutput(ns("map"))
    )
  )
}

#' @export
init_server <- function(id, selected_year, df) {
  callModule(server, id, selected_year, df)
}

#' @export
server <- function(input, output, session, selected_year, df) {
  # One-time validation of input data
  observe({
    validate(
      need(!is.null(selected_year()), "Year data is not loaded to map module."),
      need(!is.null(df), "Dataset is not loaded to map module.")
    )
    message("Module map loaded with year: ", selected_year(), " and data with ", nrow(df), " rows")
  })
  
  # Create a single reactive for all the data needed for plotting
  plot_data <- reactive({
    req(selected_year(), df)
    year <- selected_year()
    
    # Process all data at once to minimize redundant calculations
    result <- list(
      result_list = func_toptier(df, year),
      barchart_data = func_barchart(df, year),
      label_gdp_group = create_gdp_labels(df, year)
    )
    
    # Extract filtered data and top tiers from result_list
    if (!is.null(result$result_list)) {
      result$filtered_data <- result$result_list$filtered_df
      result$top_tier <- result$result_list$top_tiers
    }
    
    # Log any issues with data
    if (is.null(result$filtered_data)) {
      message("WARNING: filtered_data is NULL")
    }
    if (is.null(result$top_tier)) {
      message("WARNING: top_tier data is NULL")
    } else if (nrow(result$top_tier) == 0) {
      message("WARNING: top_tier data has 0 rows")
    }
    if (is.null(result$barchart_data)) {
      message("WARNING: barchart_data is NULL")
    }
    if (is.null(result$label_gdp_group)) {
      message("WARNING: label_gdp_group is NULL")
    }
    
    return(result)
  })
  
  # Render the combined plot
  output$map <- renderPlot({
    data <- plot_data()
    req(data$filtered_data)
    
    # Create base map
    map_plot <- ggplot() +
      geom_sf(data = data$filtered_data, 
              aes(fill = GDP), 
              lwd = 0.05, 
              color = "white") +
      scale_fill_gradientn(
        name = "",
        colors = brewer.pal(n = 9, name = "YlGnBu"),
        breaks = constant$seq_values
      ) +
      theme_map() +
      theme(
        legend.position = "none",
        plot.caption = element_text(size = 10, hjust = 1, face = "italic")
      )
    
    # Add text labels if top_tier data is available
    if (!is.null(data$top_tier) && nrow(data$top_tier) > 0 && 
        all(c("lon", "lat", "province_label") %in% names(data$top_tier))) {
      
      map_plot <- map_plot +
        geom_text_repel(
          data = data$top_tier, 
          aes(x = lon, y = lat, label = province_label),
          size = 3.5, fontface = "bold", color = "black",
          box.padding = 0.7, point.padding = 0.5,
          segment.color = "grey50", force = 2,
          min.segment.length = 0
        )
      
      # Add caption with total GDP percentage
      if ("gdp_percent" %in% names(data$top_tier)) {
        map_plot <- map_plot +
          labs(
            caption = paste0("Top provinces account for ", 
                             round(sum(data$top_tier$gdp_percent, na.rm = TRUE), 1), 
                             "% of total GDP")
          )
      }
    } else {
      map_plot <- map_plot +
        labs(caption = "GDP distribution across provinces")
    }
    
    # Create supporting plots with error handling
    boxplot <- tryCatch({
      plot_gdp_boxplot(data$filtered_data)
    }, error = function(e) {
      message("Error creating boxplot: ", e$message)
      ggplot() + annotate("text", x = 0, y = 0, label = "Error in boxplot generation") + theme_minimal()
    })
    
    barchart_plot <- tryCatch({
      if (!is.null(data$barchart_data) && !is.null(data$label_gdp_group)) {
        plot_bar_chart(data$barchart_data, data$label_gdp_group)
      } else {
        ggplot() + annotate("text", x = 0, y = 0, label = "No data available") + theme_minimal()
      }
    }, error = function(e) {
      message("Error creating bar chart: ", e$message)
      ggplot() + annotate("text", x = 0, y = 0, label = "Error in chart generation") + theme_minimal()
    })
    
    blank_space <- plot_spacer()
    
    # Create the final plot with patchwork
    map_plot + (boxplot / (blank_space + barchart_plot + plot_layout(widths = c(2, 8)))) + 
      plot_layout(ncol = 2, widths = c(4, 3)) +
      plot_annotation(
        title = paste0("China GDP Distribution (", selected_year(), ")"),
        subtitle = "Top GDP provinces labeled with share of national GDP",
        caption = "Data source: National Bureau of Statistics of China",
        theme = theme(
          plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 16, hjust = 0.5),
          plot.caption = element_text(size = 12, hjust = 1, face = "italic")
        )
      )
  })
}