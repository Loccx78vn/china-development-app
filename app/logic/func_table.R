box::use(
  magrittr[`%>%`],
  app/constant[company_icons,exchange_emoji_flags,country_mapping,country_flag_codes],
  dplyr[...]
)

#' Process stock data
#' 
#' Reorganize columns and add calculated metrics typically used in finance
#' 
#' @param data The stock data dataframe
#' @return A processed dataframe with additional metrics
#' @export
process_stock_data <- function(data) {
  data |>
    # Calculate additional financial metrics
    dplyr::mutate(
      # Round all numeric columns to 2 decimal places
      open = round(open, 2),
      high = round(high, 2),
      low = round(low, 2),
      close = round(close, 2),
      adjusted = round(adjusted, 2),
      change = round(close - open, 2),
      change_pct = round((close - open) / open * 100, 2),
      exchange_rate = round(exchange_rate, 4),
      country = recode(name, !!!country_mapping),
      exchange_rate_pct = paste0(format(round(exchange_rate * 100, 2), nsmall = 2), "%"),  # Format as percentage
      
      # Create a combined company column with icon, bolded name, and country flag
      company_display = mapply(function(n, country) {
        icon_name <- if (n %in% names(company_icons)) company_icons[[n]] else "building"
        icon_html <- as.character(fontawesome::fa(icon_name, fill = "#41b6c4"))
        
        paste0(icon_html, " <strong>", n)
      }, name, country, SIMPLIFY = TRUE),  
      
      # Add emoji flag to stock symbol based on exchange
      symbol_display = sapply(symbol, function(s) {
        # Extract exchange code from symbol
        exchange_code <- if (grepl("\\.", s)) {
          parts <- strsplit(s, "\\.")[[1]]
          parts[length(parts)]  # Get last part after dot
        } else {
          "default"  # For symbols without dots (NYSE/NASDAQ)
        }
        
        # Get appropriate emoji flag
        flag_emoji <- if (exchange_code %in% names(exchange_emoji_flags)) {
          exchange_emoji_flags[[exchange_code]]
        } else {
          exchange_emoji_flags[["default"]]
        }
        
        # Create display with icon and emoji flag
        paste0(s, " ", flag_emoji)
      }),
      flag_icon = sapply(country, function(country_name) {
        country_code <- if (country_name %in% names(country_flag_codes)) {
          country_flag_codes[[country_name]]
        } else {
          "unknown"
        }
        # Return HTML for flag icon
        sprintf('<span class="flag-icon flag-icon-%s"></span>', country_code)
      }),
      # Format volume with commas
      volume = scales::comma(volume),
      
      # Color and style the change_pct column
      change_pct = case_when(
        change_pct < 0 ~ paste0('<span style="color:red; font-weight:bold;">', change_pct, '%</span>'),
        change_pct > 0 ~ paste0('<span style="color:green; font-weight:bold;">', change_pct, '%</span>'),
        TRUE ~ paste0('<span style="color:blue; font-weight:bold;">', change_pct, '%</span>')
      )
    ) |>
    # Reorder columns to put display columns first
    dplyr::select(company_display, symbol_display, date, open, high, low, close, change, change_pct, 
                  volume, adjusted, exchange_rate, country, exchange_rate_pct, flag_icon, dplyr::everything())
}

#' Market info UI function
#' 
#' @param data The processed stock data
#' @return A UI element with market information
#' @export
render_market_info <- function(data) {
  # Use Sys.time() for the latest day instead of trying to extract it from data
  current_date <- Sys.Date()
  
  # Calculate some market metrics
  if (nrow(data) > 0) {
    latest_data <- data[which.max(data$date),]
    
    volume_change <- if ("volume" %in% names(data)) {
      last_two_days <- data |> 
        dplyr::arrange(desc(date)) |> 
        utils::head(2)
      
      if (nrow(last_two_days) == 2) {
        vol1 <- as.numeric(gsub(",", "", last_two_days$volume[1]))
        vol2 <- as.numeric(gsub(",", "", last_two_days$volume[2]))
        
        # Check if conversion was successful and vol2 is not zero
        if (!is.na(vol1) && !is.na(vol2) && vol2 != 0) {
          pct_change <- (vol1 - vol2) / vol2 * 100
          if(pct_change > 0) {
            sprintf('<span style="color:#41b6c4">%s %+.2f%%</span>', as.character(fontawesome::fa("arrow-up")), pct_change)
          } else {
            sprintf('<span style="color:#225ea8">%s %+.2f%%</span>', as.character(fontawesome::fa("arrow-down")), pct_change)
          }
        } else {
          "Volume change: N/A (invalid data)"
        }
      } else {
        "Volume change: N/A (insufficient data)"
      }
    } else {
      "Volume data not available"
    }
    
    htmltools::tagList(
      htmltools::div(
        style = "background-color: #ffffcc; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
        htmltools::h4(fontawesome::fa("info-circle", fill = "#1d91c0"), "Market Summary")
      ),
      htmltools::div(
        style = "display: flex; flex-wrap: wrap;",
        htmltools::div(
          style = "flex: 1; min-width: 200px; margin: 5px; padding: 10px; background-color: #f5f5f5; border-radius: 5px;",
          htmltools::h5(fontawesome::fa("calendar-day", fill = "#41b6c4"), "Latest Trading Day"),
          htmltools::p(format(current_date, "%B %d, %Y"))
        ),
        htmltools::div(
          style = "flex: 1; min-width: 200px; margin: 5px; padding: 10px; background-color: #f5f5f5; border-radius: 5px;",
          htmltools::h5(fontawesome::fa("calendar-day", fill = "#41b6c4"), "Latest Trading Day"),
          htmltools::p(format(current_date, "%B %d, %Y"))
        ),
        htmltools::div(
          style = "flex: 1; min-width: 200px; margin: 5px; padding: 10px; background-color: #f5f5f5; border-radius: 5px;",
          htmltools::h5(fontawesome::fa("chart-simple", fill = "#41b6c4"), "Trading Volume"),
          htmltools::p(if("volume" %in% names(latest_data)) 
            htmltools::HTML(paste0(fontawesome::fa("chart-line", fill = "#1d91c0"), " ", format(latest_data$volume, big.mark = ",")))
            else "N/A")
        )
      ),
      htmltools::div(
        style = "margin-top: 10px; padding: 10px; background-color: #f5f5f5; border-radius: 5px;",
        htmltools::h5(fontawesome::fa("chart-line", fill = "#41b6c4"), "Volume Change"),
        htmltools::HTML(volume_change)
      ),
      htmltools::hr(),
      htmltools::div(
        style = "background-color: #c7e9b4; padding: 10px; border-radius: 5px;",
        htmltools::HTML(paste0(fontawesome::fa("lightbulb", fill = "#225ea8"), " <strong>Tip:</strong> Use the table pagination and search to explore the data."))
      )
    )
  } else {
    htmltools::div(
      style = "background-color: #ffffcc; padding: 10px; border-radius: 5px;",
      htmltools::HTML(paste0(fontawesome::fa("exclamation-triangle", fill = "#225ea8"), " No data available."))
    )
  }
}

#' Render stock data table
#' 
#' @param data The processed stock data
#' @return A DT table object
#' @export
render_stock_table <- function(data) {
  # Get indices for financial columns
  price_cols <- which(names(data) %in% c("open", "high", "low", "close", "adjusted"))
  
  custom_names <- c(
    '<i class="fas fa-globe"></i> Country',
    '<i class="fas fa-building"></i> Company',
    '<i class="fas fa-tags"></i> Stock Code',
    '<i class="fas fa-calendar"></i> Date',
    '<i class="fas fa-door-open"></i> Open',
    '<i class="fas fa-arrow-up"></i> High',
    '<i class="fas fa-arrow-down"></i> Low',
    '<i class="fas fa-door-closed"></i> Close',
    '<i class="fas fa-chart-line"></i> Change %',
    '<i class="fas fa-chart-bar"></i> Volume',
    '<i class="fas fa-calculator"></i> Adjusted',
    '<i class="fas fa-exchange-alt"></i> Exchange Rate',
    '<i class="fas fa-percentage"></i> Exchange Rate %'
  )
  
  # Define conditional formatting for financial data
  DT::datatable(
    data |> 
      dplyr::select(flag_icon,company_display,symbol_display,date,open,high,low,close,change_pct,volume,adjusted,exchange_rate,exchange_rate_pct), 
    colnames = custom_names,
    options = list(
      pageLength = 15,
      scrollX = TRUE,
      autoWidth = TRUE,
      order = list(list(2, 'desc')),  # Sort by date by default (descending)
      columnDefs = list(
        list(targets = c(0, 1), className = "dt-left", orderable = TRUE),  # Company and symbol columns
        list(targets = "_all", className = "dt-center"),
        list(targets = price_cols, 
             render = DT::JS("function(data, type) { if(type === 'display') return '<span class=\"fa fa-dollar-sign\" style=\"color:#41b6c4\"></span> ' + data.toLocaleString('en-US', {minimumFractionDigits: 2, maximumFractionDigits: 2}); return data; }"))
      ),
      initComplete = DT::JS("
        function(settings, json) {
          $(this.api().table().header()).css({'background-color': '#c7e9b4', 'color': '#225ea8'});
          $('.dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_length').css('margin-bottom', '10px');
        }
      ")
    ),
    rownames = FALSE,
    escape = FALSE,  # Allow HTML for the icons and bold text
    # Apply special formatting to the change columns
    callback = DT::JS("
      table.on('draw.dt', function() {
        var api = this.api();
        var changeCol = api.column('Change:name').index();
        var changePctCol = api.column('Change %:name').index();
        
        if(changeCol !== undefined) {
          api.cells(null, changeCol).nodes().each(function(cell, i) {
            var value = api.cell(i, changeCol).data();
            if(value > 0) {
              $(cell).css('color', '#41b6c4').html('<i class=\"fas fa-caret-up\"></i> ' + value.toLocaleString('en-US', {minimumFractionDigits: 2, maximumFractionDigits: 2}));
            } else if(value < 0) {
              $(cell).css('color', '#225ea8').html('<i class=\"fas fa-caret-down\"></i> ' + value.toLocaleString('en-US', {minimumFractionDigits: 2, maximumFractionDigits: 2}));
            } else {
              $(cell).html(value.toLocaleString('en-US', {minimumFractionDigits: 2, maximumFractionDigits: 2}));
            }
          });
        }          
        if(changePctCol !== undefined) {
          api.cells(null, changePctCol).nodes().each(function(cell, i) {
            var value = api.cell(i, changePctCol).data();
            if(value > 0) {
              $(cell).css('color', '#41b6c4').html('<i class=\"fas fa-arrow-up\"></i> +' + value.toFixed(2) + '%');
            } else if(value < 0) {
              $(cell).css('color', '#225ea8').html('<i class=\"fas fa-arrow-down\"></i> ' + value.toFixed(2) + '%');
            } else {
              $(cell).html('0.00%');
            }
          });
        }
        
        var exchangeRateCol = api.column('Exchange Rate %:name').index();
        if(exchangeRateCol !== undefined) {
          api.cells(null, exchangeRateCol).nodes().each(function(cell, i) {
            $(cell).html('<i class=\"fas fa-percentage\" style=\"color:#7fcdbb\"></i> ' + $(cell).text());
          });
        }
      });
    ")
  )
}