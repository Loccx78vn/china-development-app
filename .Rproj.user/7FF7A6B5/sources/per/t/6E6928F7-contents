box::use(
  shiny[...],
  htmltools[htmlTemplate],
  dplyr[...]
)

box::use(
  app/view/line,
  app/view/map,
  app/view/onl_map,
  app/view/stock,
  app/view/table,
  app/view/filters[timelineInput,shipping_selector_ui],
  data/data[china_data_2000,china_gdp_sf,china_gdp_long],
  app/constant[COMPANIES]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  # Html template used to render UI
  htmlTemplate(
    "app/static/index.html",
    
    # Connect the UI placeholders to module UIs or input widgets
    selectYear_ui = selectInput(ns("year"), 
                                "Select Year", 
                                choices = c(sort(unique(china_gdp_sf$Year))), 
                                selected = min(unique(china_gdp_sf$Year))),
    
    selectCompany_ui = selectInput(
      ns("company"),
      "Select a Shipping Company:",
      choices = c("All",unname(COMPANIES$names)),
      selected = "All"
    ),
    line_ui = line$ui(ns("line")),
    staticmap_ui = map$ui(ns("map")),
    interactive_ui = onl_map$ui(ns("onl_map")),
    stock_ui = stock$ui(ns("stock")),
    datastock_ui = table$ui(ns("table"))
  )
}


#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ## Save selected value--------------------------------------------------------------------
    selectYear <- reactive({
      input$year
    })
    selectCompany <- reactive({
      input$company
    })
    
    ## Main content:
    line$init_server(
      "line",
      df = china_data_2000,
      df_long = china_gdp_long
    )
    map$init_server(
      "map",
      selected_year = selectYear,
      df = china_gdp_sf
    )
    onl_map$server(
      "onl_map",
      selected_year = selectYear,
      df = china_gdp_sf
    )
    stock$server(
      "stock",
      data_filter = COMPANIES$names,
      company = selectCompany
    )
    table$init_server(
      "table"
    )
  })
}
