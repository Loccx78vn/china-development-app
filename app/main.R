box::use(
  shiny[...],
  bslib[card, page_navbar, nav_panel, nav_menu, nav_spacer, nav_item, layout_sidebar, navset_card_tab],
  fontawesome[fa],
  dplyr[...],
  shinyjs,
  shinycssloaders[withSpinner]
)

box::use(
  app/view/line,
  app/view/map,
  app/view/onl_map,
  app/view/stock,
  app/view/table,
  app/view/select_box[timelineInput,styled_company_selector],
  data/data[china_data_2000,china_gdp_sf,china_gdp_long],
  app/constant[COMPANIES]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  # Define color palette for theming
  color_palette <- c("#FFFFD9", "#EDF8B1", "#C7E9B4", "#7FCDBB", "#41B6C4",
                     "#1D91C0", "#225EA8", "#253494", "#081D58")
  
  fluidPage(
    shinyjs$useShinyjs(),
    # Head section
    tags$head(
      # Font Awesome for icons
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"),
      # Add CSS directly in the head instead of external file
      tags$style(HTML("
        /* Base color variables */
        :root {
          --color-light-1: #FFFFD9;
          --color-light-2: #EDF8B1;
          --color-light-3: #C7E9B4;
          --color-mid-1: #7FCDBB;
          --color-mid-2: #41B6C4;
          --color-mid-3: #1D91C0;
          --color-dark-1: #225EA8;
          --color-dark-2: #253494;
          --color-dark-3: #081D58;
        }
        
        /* General styles */
        body {
          background-color: white;
        }
        
        /* Navbar styling */
        .navbar {
          background-color: var(--color-dark-2) !important;
          padding: 15px !important;
        }
        
        .navbar-brand {
          color: white !important;
          font-size: 1.8rem !important;
        }
        
        /* Position navigation items to the right */
        .navbar-nav {
          margin-left: auto !important;
        }
        /* Adjust sidebar width in the Stock tab */
        #navbar-stock .sidebar {
          width: 250px !important; /* Change this to your desired width */
          max-width: 250px !important;
          flex: 0 0 250px !important;
        }
        
        /* Adjust the main content area width for the Stock tab */
        #navbar-stock .main {
          width: calc(100% - 250px) !important;
          margin-left: 250px !important; /* Should match the sidebar width */
        }
        
        /* Make the left-sidebar class in the Stock tab match the new width */
        #navbar-stock .left-sidebar {
          width: 100% !important;
          max-width: 100% !important;
        }  
        
        /* Nav links styling - circular tabs */
        .navbar .nav-link {
          color: white !important;
          font-size: 1.2rem !important;
          padding: 10px 15px !important;
          border-radius: 8px !important; /* Circular tabs */
          margin: 0 5px !important;
          transition: background-color 0.3s ease;
        }
        
        .navbar .nav-link.active {
          border-radius: 8px !important;
          background-color: var(--color-dark-1) !important;
        }
        
        .navbar .nav-link:hover {
          background-color: var(--color-dark-1) !important;
        }
        
        /* Card styling */
        .card {
          border: 2px solid var(--color-mid-1) !important;
          border-radius: 8px !important;
          box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1) !important;
        }
        
        .card-header {
          background-color: var(--color-mid-2) !important;
          color: white !important;
          border-bottom: 1px solid var(--color-mid-1) !important;
          padding: 12px 20px !important;
          font-size: 1.3rem !important;
        }
        
        /* Sidebar styling */
        .sidebar {
          background-color: white !important;
          border-right: 2px solid var(--color-mid-1) !important;
        }
        
        .left-sidebar {
          background-color: white;
          border-radius: 8px;
          padding: 15px;
          border: 1px solid var(--color-mid-1);
        }
        /* Tabs styling */
        .nav-tabs .nav-link.active {
          background-color: var(--color-mid-1) !important;
          color: white !important;
        }
        
        .nav-tabs .nav-link {
          border-radius: 6px 6px 0 0; 
          background-color: var(--color-light-3);
          margin-right: 5px;
          border: 1px solid var(--color-mid-1);
        }
        
        /* Button styling */
        .btn-primary {
          background-color: var(--color-mid-3) !important;
          border-color: var(--color-dark-1) !important;
        }
        
        .btn-primary:hover {
          background-color: var(--color-dark-1) !important;
        }
        
        /* Modal styling */
        .modal-backdrop {
          background-color: rgba(0, 0, 0, 0.5);
          backdrop-filter: blur(5px);
        }
        
        .modal-dialog {
          max-width: 90%;
          margin: 1.75rem auto;
        }
        
        .modal-content {
          max-height: 85vh;
          overflow-y: auto;
          border: 2px solid var(--color-mid-1);
        }
        
        /* Footer styling */
        .app-footer {
          margin-top: 20px;
          padding: 10px 0;
          background-color: var(--color-dark-2);
          border-top: 2px solid var(--color-mid-1);
          color: white;
        }
        
        .footer-content {
          display: flex;
          justify-content: space-between;
          align-items: center;
          padding: 0 20px;
        }
        
        .social-links a {
          margin-right: 15px;
          color: white;
          text-decoration: none;
        }
        
        .social-links a:hover {
          color: var(--color-light-2);
        }
        
        .footer-credit {
          color: white;
          font-size: 0.9rem;
        }
        
        /* Form control styling */
        .selectize-input {
          border: 1px solid var(--color-mid-1) !important;
        }
        
        .action-button {
          border: 1px solid var(--color-mid-1);
        }
        .navbar-collapse {
          justify-content: flex-end;
        }
      "))
    ),
    
    # Main navigation
    page_navbar(
      title = span(icon("chart-line"), " Economic Dashboard 2025"),
      id = ns("navbar"),
      
      # Tab 1: Overview
      nav_panel(
        title = span(icon("home"), " Overview"),
        value = "overview",
        layout_sidebar(
          sidebar = tagList(
            h5(icon("info-circle"), " Summary"),
            p(
              style = "font-size: 0.9em;", 
              "There are two charts: a ", 
              tags$strong("bar + line chart"), 
              " and a ", 
              tags$strong("boxplot"), 
              ". The line chart includes a small line chart that shows the ", 
              tags$strong("percentage of public consumption in China/GDP each year"), 
              ". The bar chart displays the ", 
              tags$strong("GDP each year"), 
              ", with the line representing the ", 
              tags$strong("total amount of public consumption in actual figures"), 
              ". Below is a ", 
              tags$strong("boxplot of 31 provinces in China from 2000 to 2023"), 
              "."
            ),
            h6(icon("question-circle"), "Some details"),
            tags$ul(
              style = "font-size: 0.9em;",
              tags$li(HTML("<b>China's government spending</b> as a percentage of GDP fluctuated, but <span style='color:#007BFF'><b>actual spending consistently increased</b></span>.")),
              tags$li(HTML("This <b>steady rise</b> in expenditure contrasts with more volatile factors like <span style='color:#28A745'><b>FDI</b></span>.")),
              tags$li(HTML("A major turning point came in <span style='color:#FFC107'><b>2000</b></span> with the launch of the <b>10th Five-Year Plan</b>.")),
              tags$li(HTML("From <b>2000 to 2010</b>, China's GDP <span style='color:#DC3545'><b>nearly tripled</b></span>.")),
              tags$li(HTML("<b>Sustained government investment</b> was a key driver of <span style='color:#17A2B8'><b>strong economic growth</b></span> during this period."))
            )
          ),
          card(
            full_screen = TRUE,
            title = span(icon("chart-area"), " Economic Trends"),
            line$ui(ns("line"))
          )
        )
      ),
      
      # Tab 2: Map - Modified to have filter at top instead of sidebar
      nav_panel(
        title = span(icon("map"), " Map"),
        value = "map",
        # Filter at top instead of sidebar
        div(
          timelineInput(
            inputId = ns("year"), 
            label = "Select Year", 
            choices = c(sort(unique(china_gdp_sf$Year))),
            selected = min(unique(china_gdp_sf$Year))
          ),
          # Map content below the filter
          navset_card_tab(
            nav_panel(
              title = span(fa("map-marked"), " Static Map"), 
              map$ui(ns("map"))
            ),
            nav_panel(
              title = span(fa("map-marked-alt"), " Interactive Map"), 
              onl_map$ui(ns("onl_map"))
            )
          )
        )
      ),
      
      # Tab 3: Stock
      nav_panel(
        title = span(icon("chart-line"), " Stock"),
        value = "stock",
        layout_sidebar(
          sidebar = tagList(
            styled_company_selector(ns("company"),
                                    companies_data = COMPANIES,
                                    selected = "All"),
            hr(),
            p("Analyze stock performance for shipping companies.")
          ),
          card(
            full_screen = TRUE,
            title = span(icon("chart-bar"), " Stock Analysis"),
            stock$ui(ns("stock")),
            hr(),
            fluidRow(
              column(
                width = 12,
                actionButton(ns("showTable"), 
                             span(icon("table"), " Show Data Table"), 
                             class = "btn-primary")
              )
            )
          )
        )
      )
    ),
    
    # Hidden table overlay - Initially hidden with shinyjs
    shinyjs::hidden(
      div(id = ns("table_overlay"),
          style = "
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background-color: rgba(0, 0, 0, 0.5);
        backdrop-filter: blur(5px);
        z-index: 1000;
        display: flex;
        align-items: center;
        justify-content: center;
      ",
      div(id = ns("table_container"),
          style = "
            background-color: white;
            border-radius: 10px;
            z-index: 1001;
            width: 80%;
            max-height: 80vh;
            overflow: auto;
            position: relative;
            box-shadow: 0 4px 20px rgba(0, 0, 0, 0.2);
          ",
          div(id = ns("close_btn"),
              style = "
                position: absolute;
                top: 10px;
                right: 10px;
                cursor: pointer;
                font-size: 20px;
                font-weight: bold;
                color: var(--color-dark-2);
                width: 30px;
                height: 30px;
                display: flex;
                align-items: center;
                justify-content: center;
                border-radius: 50%;
                background-color: white;
                z-index: 1002;
              ",
              icon("times"),
              onclick = paste0("Shiny.setInputValue('", ns("closeBtn"), "', Math.random())")
          ),
          table$ui(ns("table")),
      ))
    ),
    
    # Footer
    div(
      class = "app-footer",
      div(
        class = "footer-content",
        div(
          class = "social-links",
          a(href = "https://www.facebook.com/cao.loc.9693", icon("facebook"), " Facebook"),
          a(href = "https://github.com/Loccx78vn", icon("github"), " GitHub")
        ),
        div(
          class = "footer-credit",
          "© 2025 Economic Data Dashboard | Created with Shiny for R"
        )
      )
    )
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
    table$init_server("table")
    
    # Show table when showTable button is clicked
    observeEvent(input$showTable, {
      shinyjs::show("table_overlay")
    })
    
    # Close when clicking the close button
    observeEvent(input$closeBtn, {
      shinyjs::hide("table_overlay")
    })
  })
}