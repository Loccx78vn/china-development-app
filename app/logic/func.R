library(WDI)

# Function to get economic data (GDP, FDI, Balance of Trade) for China
get_china_economic_data <- function(start_year, end_year) {
  # Indicators for GDP, FDI, Exports, and Imports
  indicators <- c(
    "NY.GDP.MKTP.CD",   # GDP (current US$)
    "BX.KLT.DINV.CD.WD", # Foreign Direct Investment, net inflows (current US$)
    "NE.EXP.GNFS.CD",    # Exports of goods and services (current US$)
    "NE.IMP.GNFS.CD"     # Imports of goods and services (current US$)
  )
  
  # Retrieve the data from World Bank
  china_data <- WDI(country = "CN", indicator = indicators, start = start_year, end = end_year)
  
  # Calculate Balance of Trade (Exports - Imports)
  china_data$Balance_of_Trade <- china_data$NE.EXP.GNFS.CD - china_data$NE.IMP.GNFS.CD
  
  return(china_data)
}