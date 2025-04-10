box::use( 
  shiny[...], 
  ggplot2[ggsave], 
  dplyr[...], 
  patchwork[plot_layout, plot_annotation], 
  RColorBrewer[brewer.pal], 
  leaflet[...], 
  sf[st_drop_geometry, st_coordinates, st_centroid], 
  htmltools[...], 
  leaflet.extras[addResetMapButton],
  base64enc[base64encode] )

box::use( 
  app/constant, 
  app/logic/func_map[func_barchart, func_toptier, plot_bar_chart, create_gdp_labels])

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Removed the CSS for reset button
    tags$div(
      class = "chart-onl_map-container",
      leafletOutput(ns("onl_map"))
    ),
    # Toggle chart button (hidden)
    tags$div(
      style = "display: none;", 
      actionButton(ns("toggle_chart"), "Toggle Chart")
    )
    # Hidden reset map button is removed
  )
}

#' @export
server <- function(id, selected_year, df) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Track visibility state of the barchart
    chart_visible <- reactiveVal(TRUE)
    
    # Observer for toggle button
    observeEvent(input$toggle_chart, {
      message("Toggle button clicked, current visibility: ", chart_visible())
      chart_visible(!chart_visible())
      message("New visibility: ", chart_visible())
    })
    
    # Validation at module initialization
    observe({
      req(selected_year(), df)
      message("Module onl_map loaded with year: ", selected_year(), 
              " and data with ", nrow(df), " rows",
              " | Chart visibility: ", chart_visible())
    })
    
    result_list <- reactive({
      req(selected_year(), df)
      
      data <- func_toptier(df, selected_year())
      
      if(is.null(data)) {
        message("Missing data: Top tier data is NULL for year ", selected_year())
        return(NULL)
      }
      
      return(data)
    })
    
    filtered_data <- reactive({
      result <- result_list()
      req(result)
      
      data <- result$filtered_df
      
      if(is.null(data)) {
        message("Missing data: filtered_df is NULL")
        return(NULL)
      }
      
      return(data)
    })
    
    barchart_data <- reactive({
      req(selected_year(), df)
      
      tryCatch({
        data <- func_barchart(df, selected_year())
        
        if(is.null(data)) {
          message("Missing data: Barchart data is NULL")
          return(NULL)
        }
        
        return(data)
      }, error = function(e) {
        message("Error in barchart_data: ", e$message)
        return(NULL)
      })
    })
    
    LABEL <- reactive({
      req(df, selected_year())
      
      tryCatch({
        label <- create_gdp_labels(df, selected_year())
        
        if(is.null(label)) {
          message("Missing data: GDP labels are NULL")
          return(NULL)
        }
        
        return(label)
      }, error = function(e) {
        message("Error in LABEL: ", e$message)
        return(NULL)
      })
    })
    
    # Create the barchart image
    barchart_image <- reactive({
      data <- barchart_data()
      label <- LABEL()
      
      req(data, label)
      message("Generating barchart image")
      
      tryCatch({
        tmp_file <- tempfile(fileext = ".png")
        
        p <- plot_bar_chart(data, label)
        if(is.null(p)) {
          message("ERROR: plot_bar_chart returned NULL")
          return(NULL)
        }
        
        ggsave(tmp_file, plot = p, width = 7, height = 5, dpi = 120)
        img_data <- base64encode(tmp_file)
        
        message("Barchart image created successfully")
        return(img_data)
      }, error = function(e) {
        message("ERROR in barchart_image: ", e$message)
        return(NULL)
      })
    })
    
    # Create the color palette
    map_palette <- reactive({
      data <- filtered_data()
      req(data)
      
      seq_vals <- seq(min(data$GDP), max(data$GDP), mean(data$GDP) / 2)
      max_GDP <- max(data$GDP)
      
      if (length(seq_vals) < 8) {
        seq_vals <- c(seq_vals, max_GDP)
      }
      
      colorBin(
        palette = brewer.pal(min(9, max(3, length(seq_vals) - 1)), name = "YlGnBu"),
        domain = data$GDP,
        bins = seq_vals
      )
    })
    
    # Create the map leaflet object
    output$onl_map <- renderLeaflet({
      message("Starting map render process")
      
      data <- filtered_data()
      req(data)
      message("Rendering map with ", nrow(data), " data points")
      
      pal <- map_palette()
      req(pal)
      
      # Base map
      map <- leaflet() |> 
        addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") |>
        addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") |>
        addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB Dark Matter") |>
        addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") |>
        setView(lng = 105, lat = 35, zoom = 4) |>
        addResetMapButton() |> 
        addPolygons(
          data = data,
          fillColor = ~pal(GDP),
          fillOpacity = 0.8,
          color = "white",
          weight = 1,
          opacity = 1,
          highlightOptions = highlightOptions(
            weight = 2,
            fillOpacity = 0.9,
            bringToFront = TRUE
          ),
          label = ~paste0(
            "<strong>", Province, "</strong><br>",
            "GDP: ", format(GDP, big.mark = ",", scientific = FALSE), " ¥<br>",
            "Population: ", format(Population, big.mark = ",", scientific = FALSE)
          )  |> lapply(HTML)
        ) |>
        addLegend(
          position = "bottomright",
          pal = pal,
          values = data$GDP,
          title = "GDP (¥)",
          opacity = 0.8,
          labFormat = labelFormat(prefix = "¥"),
          layerId = "gdpLegend"
        ) |>
        addLayersControl(
          baseGroups = c("CartoDB Positron", "OpenStreetMap", "CartoDB Dark Matter", "Esri World Imagery"),
          options = layersControlOptions(collapsed = FALSE)
        )
      
      # Removed the custom JavaScript for the reset button
      
      return(map)
    })
    
    # Removed the reset map button observer
    
    # Observe changes to chart_visible and update the map accordingly
    observe({
      is_visible <- chart_visible()
      img_data <- barchart_image()
      
      # Only proceed if we have a map and image data
      req(img_data)
      
      proxy <- leafletProxy(ns("onl_map"))
      
      # Remove any existing chart controls
      proxy |> removeControl("chart_control")
      
      if (is_visible) {
        # Create chart HTML
        chart_html <- div(
          id = "barchart_container",
          style = "padding: 6px 8px; background: white; background: rgba(255,255,255,0.9); box-shadow: 0 0 15px rgba(0,0,0,0.2); border-radius: 5px; position: relative;",
          tags$button(
            id = ns("toggle_chart"),
            class = "btn btn-sm",
            style = "position: absolute; top: 2px; right: 2px; padding: 0px 6px; background: transparent; color: #777; border: none; font-size: 16px; cursor: pointer;",
            onClick = paste0("Shiny.setInputValue('", ns("toggle_chart"), "', Math.random());"),
            HTML("&times;")
          ),
          h5(style = "margin: 0 0 5px; color: #777; padding-right: 20px;", "Population by GDP"), 
          img(src = paste0("data:image/png;base64,", img_data), width = 350)
        )
        
        # Add chart to map
        proxy |> addControl(
          html = renderTags(chart_html)$html,
          position = "topright",
          layerId = "chart_control"
        )
      } else {
        # Add show button
        show_button <- tags$button(
          id = ns("show_chart_btn"),
          class = "btn btn-sm btn-primary",
          style = "padding: 6px 10px;",
          onClick = paste0("Shiny.setInputValue('", ns("toggle_chart"), "', Math.random());"),
          "Show Chart"
        )
        
        proxy |> addControl(
          html = renderTags(show_button)$html,
          position = "topright",
          layerId = "chart_control"
        )
      }
      
      # Use a JavaScript approach to invalidate size
      session$sendCustomMessage(type = "leaflet-invalidateSize", 
                                message = list(id = ns("onl_map")))
    })
  })
}