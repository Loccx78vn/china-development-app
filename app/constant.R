#-------------------------------------------------------------------------------
### Constant objects for line chart
box::use(
  data/data[china_data_2000, china_data, china_gdp_sf]
)

COMPANIES <- list(
  tickers = c("MAERSK-B.CO", "1919.HK", "HLAG.DE", "2603.TW", 
              "2609.TW", "011200.KS", "ZIM"),
  
  names = c(
    "MAERSK-B.CO" = "Maersk Line", 
    "1919.HK" = "China COSCO Shipping Corporation",
    "HLAG.DE" = "Hapag-Lloyd",
    "2603.TW" = "Evergreen Marine",
    "2609.TW" = "Yang Ming Marine Transport",
    "011200.KS" = "Hyundai Merchant Marine",
    "ZIM" = "ZIM Integrated Shipping Services"
  )
)

EXCHANGE_RATES <- c(
  "DKKUSD=X",  # MAERSK-B.CO
  "HKDUSD=X",  # 1919.HK
  "EURUSD=X",  # HLAG.DE
  "TWDUSD=X",  # 2603.TW
  "TWDUSD=X",  # 2609.TW
  "KRWUSD=X",  # 011200.KS
  "ILSUSD=X"   # ZIM
)

names(EXCHANGE_RATES) <- COMPANIES$tickers

DATE_RANGE <- list(
  from = "2024-01-01",
  to = function() { Sys.Date() }
)

#-------------------------------------------------------------------------------
### Constant objects for table chart
company_icons <- list(
  "Maersk Line" = "ship",
  "China COSCO Shipping Corporation" = "building",
  "Hapag-Lloyd" = "anchor",
  "Evergreen Marine" = "water",
  "Yang Ming Marine Transport" = "truck",
  "Hyundai Merchant Marine" = "globe",
  "ZIM Integrated Shipping Services" = "box"
)

exchange_emoji_flags <- list(
  "CO" = "🇩🇰",  # Denmark flag
  "HK" = "🇭🇰",  # Hong Kong flag
  "DE" = "🇩🇪",  # Germany flag
  "TW" = "🇹🇼",  # Taiwan flag
  "KS" = "🇰🇷",  # South Korea flag
  "IL" = "🇮🇱",  # Israel flag
  "default" = "🇺🇸"  # US flag
)

country_mapping <- c(
  "Maersk Line" = "Denmark",
  "China COSCO Shipping Corporation" = "China",
  "Hapag-Lloyd" = "Germany",
  "Evergreen Marine" = "Taiwan",
  "Yang Ming Marine Transport" = "Taiwan",
  "Hyundai Merchant Marine" = "South Korea",
  "ZIM Integrated Shipping Services" = "Israel"
)

# Define country flag codes for flag-icon-css
country_flag_codes <- c(
  "USA" = "us",
  "Denmark" = "dk",
  "Hong Kong" = "hk",
  "Germany" = "de",
  "Taiwan" = "tw",
  "South Korea" = "kr",
  "China" = "cn",
  "Japan" = "jp",
  "United Kingdom" = "gb",
  "France" = "fr",
  "Canada" = "ca",
  "Australia" = "au",
  "Israel" = "il"  
)
#----------------------------------------------------------------------
### Constant objects for line chart
x_axis <- list(
  title = "",
  showline = TRUE,
  showgrid = FALSE,
  showticklabels = TRUE,
  linecolor = 'rgb(204, 204, 204)',
  linewidth = 2,
  autotick = FALSE,
  ticks = 'outside',
  tickcolor = 'rgb(204, 204, 204)',
  tickwidth = 2,
  ticklen = 5,
  tickfont = list(family = 'Arial',
                  size = 12,
                  color = 'rgb(82, 82, 82)')
)

y_axis <- list(
  title = "",
  showgrid = FALSE,
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE
)

margin <- list(
  autoexpand = FALSE,
  l = 100,
  r = 100,
  t = 110
)

y_2000 <- list(
  xref = "x", yref = "y", x = 2000, y = china_data_2000$Gov_Exp[china_data_2000$Year == 2000], 
  xanchor = 'left', yanchor = 'middle', 
  text = paste('11th Five-Year Plan: \n', round(china_data_2000$Per_Gov_Exp[china_data_2000$Year == 2000], 3), '% of GDP'), 
  font = list(family = 'Arial Black', size = 10, color = 'rgb(160,47,34)'), 
  showarrow = TRUE, arrowhead = 2
)

y_2022 <- list(
  xref = "x", yref = "y", x = 2022, y = china_data_2000$Gov_Exp[china_data_2000$Year == 2022], 
  xanchor = 'middle', 
  yanchor = 'middle', 
  text = paste('2022 Plan: ', round(china_data_2000$Per_Gov_Exp[china_data_2000$Year == 2022], 3), '%'), 
  font = list(family = 'Arial Black', size = 10, color = 'rgb(160,47,34)'), 
  showarrow = TRUE, arrowhead = 2
)


main_color <- "rgb(44, 127, 184)"  
secondary_color <- "rgba(44, 127, 184, 0.8)"
bar_color <- "rgba(44, 127, 184, 0.4)"
# Color palette
colors <- c("#FFFFD9", "#EDF8B1", "#C7E9B4", "#7FCDBB", "#41B6C4", "#1D91C0",
            "#225EA8", "#253494", "#081D58")
#-------------------------------------------------------------------------------
### Constant objects for map chart
seq <- seq(min(china_gdp_sf$GDP), max(china_gdp_sf$GDP), mean(china_gdp_sf$GDP) / 2)

if (length(seq) < 8) {
  seq <- c(seq, max(china_gdp_sf$GDP))
}

x_labels <- sapply(1:(length(seq) - 1), function(i) {
  if (i == length(seq) - 1) {
    paste0(format(round(seq[i], 0), big.mark = ","), " \u00A5", " to more")
  } else {
    paste0(format(round(seq[i], 0), big.mark = ","), " \u00A5", " to ", 
           format(round(seq[i+1], 0), big.mark = ","), " \u00A5")
  }
})

#-------------------------------------------------------------------------------
### Constant objects for func_onl_map
seq_values<-seq(min(china_gdp_sf$GDP), max(china_gdp_sf$GDP), mean(china_gdp_sf$GDP) / 2)
max_GDP<-max(china_gdp_sf$GDP)

#-------------------------------------------------------------------------------

