box::use(
  tidyquant[tq_get],
  quantmod[...],
  dplyr[...],
  xts[xts],
  magrittr[`%>%`],
  highcharter[...],
  shiny[NS]
)

box::use(
  app/constant[COMPANIES, EXCHANGE_RATES, DATE_RANGE]
)

#' @export
get_stock_data <- function(ticker) {
  tryCatch({
    tidyquant::tq_get(ticker, from = DATE_RANGE$from, to = DATE_RANGE$to())
  }, error = function(e) {
    message(paste("Error fetching data for:", ticker))
    return(NULL)
  })
}

#' @export
get_exchange_rate <- function(symbol, company_name) {
  tryCatch({
    tidyquant::tq_get(symbol, from = DATE_RANGE$from, to = DATE_RANGE$to()) %>%
      select(date, adjusted) %>%
      mutate(symbol = company_name) %>%
      rename(exchange_rate = adjusted)
  }, error = function(e) {
    message(paste("Error fetching exchange rate for:", symbol))
    return(NULL)
  })
}

#' @export
prepare_stock_data <- function() {
  
  # Get stock data for all companies
  stocks_list <- lapply(COMPANIES$tickers, get_stock_data)
  stocks_list <- stocks_list[!sapply(stocks_list, is.null)]
  
  # Combine stock data and add company names
  stocks_combined <- bind_rows(stocks_list) %>%
    filter(!is.na(adjusted)) %>%
    mutate(name = recode(symbol, !!!COMPANIES$names))
  
  # Get exchange rate data for all companies
  exchange_list <- lapply(names(EXCHANGE_RATES), function(company) {
    get_exchange_rate(EXCHANGE_RATES[company], company)
  })
  
  exchange_combined <- bind_rows(exchange_list)
  
  # Join stock data with exchange rate data
  data <- left_join(stocks_combined, exchange_combined, by = c("symbol", "date"))
  
  # Handle missing exchange rates
  data <- data %>% 
    group_by(symbol) %>%  
    mutate(
      exchange_rate = ifelse(is.na(exchange_rate), mean(exchange_rate, na.rm = TRUE), exchange_rate),
      date = as.Date(date)
    ) %>% 
    ungroup()
  
  # Prepare data for individual stock chart
  data_selected <- data %>%
    select(date, symbol, open, high, low, close) 
  
  split_data <- split(data_selected, data_selected$symbol)
  
  xts_list <- lapply(split_data, function(df) {
    symbol_name <- unique(df$symbol)
    
    xts_data <- xts(
      select(df, open, high, low, close),
      order.by = df$date
    )
    
    colnames(xts_data) <- paste(symbol_name, colnames(xts_data), sep = ".")
    
    return(xts_data)
  })
  
  # Return all the prepared data
  list(
    stock_data = data,
    individual_stock_data = xts_list
  )
}


