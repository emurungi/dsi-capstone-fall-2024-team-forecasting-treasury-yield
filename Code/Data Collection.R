install.packages('fredr')

library(fredr)
library(dplyr)
library(tidyr)
library(purrr)

#API key to get data from FRED
FRED_api_key <- "1e3261d006a3b1782545cc1281beb157"

fredr_set_key(FRED_api_key)

#Get variable of interest
fred_data <- fredr(
  series_id = "DGS10"
)

fred_data <- fred_data %>% rename(tenyr_yield = value) %>% select(date, tenyr_yield)

metadata <- fredr_series(series_id = "DGS10")
metadata <- metadata[, !(names(metadata) %in% c('notes'))]


#Collect other potentially useful data from FRED
data_list <- list(
  list(series_id = "GDP", col_name = "GDP"), 
  list(series_id = "GDPC1", col_name = "real_GDP"), 
  list(series_id = "GNP", col_name = "GNP"),
  list(series_id = "GDPPOT", col_name = "real_potential_GDP(forecast)"), 
  list(series_id = "GFDGDPA188S", col_name = "gross_federal_debt_as_%_GDP"), 
  list(series_id = "EXPINF5YR", col_name = "5yr_expected_inflation"), #
  list(series_id = "EXPINF10YR", col_name = "10yr_expected_inflation"),
  list(series_id = "EXPINF30YR", col_name = "30yr_expected_inflation"),
  list(series_id = "CPIAUCSL", col_name = "CPI_all_urban_consumers_US_city_average"),
  list(series_id = "MICH", col_name = "umich_inflation_excpectation"),
  list(series_id = "DFF", col_name = "federal_funds_effective_rate"),
  list(series_id = "INTDSRUSM193N", col_name = "US_discount_rate"),
  list(series_id = "DFEDTARU", col_name = "fed_funds_target_upper_limit"),
  #list(series_id = "FEDTARCTM", col_name = "fomc_fed_funds_midpoint_projection"),
  list(series_id = "DGS30", col_name = "30yr_yield"),
  list(series_id = "DGS2",col_name = "2yr_yield"),
  list(series_id = "DGS5", col_name = "5yr_yield"),
  list(series_id = "DGS7", col_name = "7yr_yield"),
  list(series_id = "DGS6MO", col_name = "6month_yield"),
  #list(series_id = "PCECTPICTM", col_name = "FOMC_PCE_inflation_rate_midpoint_projection"),
  list(series_id = "PCEPI", col_name = "PCE_price_index"),
  #list(series_id = "GDPC1CTM", col_name = "FOMC_real_GDP_growth_rate_midpoint_projection"),
  list(series_id = "JHGDPBRINDX", col_name = "GDP_based_recession_indicator"),
  list(series_id = "RECPROUSM156N", col_name = "US_recession_probabilities"),
  list(series_id = "PINCOME", col_name = "personal_income"),
  list(series_id = "PCU44414441", col_name = "producer_price_index_building_materials&supplies"),
  list(series_id = "ACMSNO", col_name = "manufacturs_new_orders_construction_materials&supplies"),
  list(series_id = "IPB54100S", col_name = "industrial_production_construction_supplies"),
  list(series_id = "WPUSI012011", col_name = "producer_price_index_construction_materials"),
  list(series_id = "IB0000043Q086SBEA", col_name = "real_imports_industrial_supplies_materials"),
  list(series_id = "IPCONGD", col_name = "industrial_production_consumer_goods"),
  list(series_id = "IPBUSEQ", col_name = "industrial_production_business_equipment"),
  list(series_id = "WSHOTSL", col_name = "US_treasury_securities_held_outright"),
  list(series_id = "PAYEMS", col_name = "total_nonfarm_employees"),
  list(series_id = "DEXJPUS", col_name = "yen_dollar_exchange_rate"),
  list(series_id = "FYGFDPUN", col_name = "federal_debt_held_by_public"),
  list(series_id = "UNRATE", col_name = "unemployment_rate"),
  list(series_id = "M1V", col_name = "velocity_of_m1_money_stock"),
  list(series_id = "M2V", col_name = "velocity_of_m2_money_stock"),
  list(series_id = "BOPGEXP", col_name = "exports_of_goods"),
  list(series_id = "BOPGIMP", col_name = "imports_of_goods"),
  list(series_id = "EMVOVERALLEMV", col_name = "equity_market_volatility"),
  list(series_id = "WTISPLC", col_name = "crude_oil_price_west_texas")
)

#Go through each data source in FRED
for (data in data_list) {
  
  curr_metadata <- fredr_series(series_id = data$series_id)
  
  if ('notes' %in% colnames(curr_metadata)){
    curr_metadata <- curr_metadata[, !(names(curr_metadata) %in% c('notes'))]
  }
  
  metadata <- rbind(metadata, curr_metadata)
  
  #Change the name of value column to informative col_name
  curr_data <- curr_data %>% rename(!!data$col_name := value) %>% select(date, !!data$col_name)
  
  #Join data with already collected data
  fred_data <- reduce(list(fred_data, curr_data), full_join, by = "date")
}

#Order data by date
fred_data <- fred_data %>% arrange(date) %>% filter(fred_data$date >= as.Date("1970-01-01"))


#We'll start with data from 1985 only
#Then we'll start with data from 2000
#Then from 2005

metadata_1985 <- metadata %>% filter(observation_start <= as.Date("1985-01-01"))

#Starting with monthly data where we backfill quarterly data and average daily data

#monthly data, and daily data.


#Plan
#Train up to 2015 to predict 2020 to 2024 



#Got way too much data and some will definitely be excluded from being correlated, but some we might
#want to initially exclude based on it's first date (i.e. exclude those that don't have enough data like must be prior to 2000)

all_dates <- data.frame(date = seq(from = min(fred_data$date), to = max(fred_data$date), by = "day"))

fred_data_all_dates <- all_dates %>% left_join(fred_data, by = "date")
