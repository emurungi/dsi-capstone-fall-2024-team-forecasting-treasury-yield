install.packages('fredr')

library(fredr)
library(openxlsx)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(stringr)
library(ggplot2)

#API key to get data from FRED
FRED_api_key <- "1e3261d006a3b1782545cc1281beb157"

fredr_set_key(FRED_api_key)

#Get variable of interest
tenyr_yield <- fredr(
  series_id = "DGS10"
)

tenyr_yield <- tenyr_yield %>% rename(tenyr_yield = value) %>% select(date, tenyr_yield)

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

#Go through each data source in FRED and collect metadata
for (data in data_list) {
  
  #Get metadata for the current data series
  curr_metadata <- fredr_series(series_id = data$series_id)
  
  #remove the notes column because not all have it and not useful
  if ('notes' %in% colnames(curr_metadata)){
    curr_metadata <- curr_metadata[, !(names(curr_metadata) %in% c('notes'))]
  }
  
  #Add metadata to the metadata dataframe
  metadata <- rbind(metadata, curr_metadata)
}

#We'll start with data from 1985 only
#Then we'll start with data from 2003

metadata_1985 <- metadata %>% filter(observation_start <= as.Date("1985-01-01")
                                     & observation_end <= as.Date("2025-01-01") 
                                     & observation_end >= as.Date("2024-01-01"))

metadata_2003 <- metadata %>% filter(observation_start <= as.Date("2003-01-01") 
                                     & observation_end <= as.Date("2025-01-01") 
                                     & observation_end >= as.Date("2024-01-01"))

data_1985 <- tenyr_yield

data_2003 <- tenyr_yield

for (data in data_list) {
  
  if(data$series_id %in% metadata_1985$id){
    curr_data <- fredr(series_id = data$series_id)
    
    #Change the name of value column to informative col_name
    curr_data <- curr_data %>% rename(!!data$col_name := value) %>% select(date, !!data$col_name)
    
    #Join data with already collected data
    data_1985 <- reduce(list(data_1985, curr_data), full_join, by = "date")
  }
  
  if(data$series_id %in% metadata_2003$id){
    curr_data <- fredr(series_id = data$series_id)
    
    #Change the name of value column to informative col_name
    curr_data <- curr_data %>% rename(!!data$col_name := value) %>% select(date, !!data$col_name)
    
    #Join data with already collected data
    data_2003 <- reduce(list(data_2003, curr_data), full_join, by = "date")
  }
}

# CBO Data - first run the Python Script in Data/cbo-eval-projections/src/main.py
x_vec <- c('revenue', 'outlay', 'deficit', 'debt') 
rev_exps_outlay_projs <- lapply(x_vec[1:4], 
                                FUN = function(x){
                                  read.csv(paste0('Data/cbo-eval-projections/output_data/', x, '_projection_errors.csv') ) %>% 
                                    tibble() %>% 
                                    filter(category=="Total") %>% 
                                    select(component, category, fiscal_year=projected_fiscal_year, actual_value, GDP, projected_year_number, value) %>% 
                                    filter(fiscal_year > ((.) %>% group_by(component, category, fiscal_year, actual_value, GDP) %>% 
                                                            summarise(max_proj_years = max(projected_year_number)) %>% ungroup() %>% 
                                                            filter(max_proj_years<5) %>% pull(fiscal_year) %>% max() )) %>% 
                                    filter(projected_year_number <= 5) %>% 
                                    mutate(projected_year_number = paste0('projection_', projected_year_number, '_yrs_before')) %>% 
                                    pivot_wider(names_from = 'projected_year_number', values_from='value')
                                } ) %>% bind_rows()
# Hair Line Plots of CBO Data
rev_exps_outlay_projs %>% 
  mutate('projection_0_yrs_before' = actual_value) %>% 
  pivot_longer(cols = paste0('projection_', seq(0,5), '_yrs_before')) %>% 
  mutate(fiscal_year_end_date = as.Date(sprintf("%s-12-31", fiscal_year)), .before=actual_value ) %>% 
  mutate(projection_date_approx = as.Date(fiscal_year_end_date) %m-% months((as.numeric(substr(name,12,12)))*12) ) %>% 
  # pivot_wider(id_cols = c("component", "category", "fiscal_year_end_date", "actual_value", "GDP"), names_from="name",values_from = "value")
  # filter(component=="revenue") %>% 
  ggplot() +
  geom_line(aes(x=projection_date_approx, group = fiscal_year, y = value), size = 0.5, alpha = 0.5) +  # Projection lines
  geom_line(aes(x=fiscal_year_end_date, y = actual_value), color = "black", size = 1.0) +  # Actual value line
  labs(x = "Fiscal Year", y = "Value (in Billion $)") +
  theme_minimal() +
  facet_wrap(~component, scales="free_y")

# Treasury Quarterly Refunding https://home.treasury.gov/policy-issues/financing-the-government/quarterly-refunding/most-recent-quarterly-refunding-documents 
GET("https://home.treasury.gov/system/files/221/2024-4th-Quarter.xls"
    , write_disk(tf <- tempfile(fileext = ".xls")))
df <- read_excel(tf, sheet=4)
treasury_securities_outstanding <- read_excel(tf, sheet=4) %>% tibble()  

treasury_securities_by_maturity <- bind_cols(treasury_securities_outstanding %>% select(1) %>% slice(4:nrow(treasury_securities_outstanding)) %>% 
                                               set_names("year"),
                                             treasury_securities_outstanding %>% select(2:ncol(treasury_securities_outstanding)) %>% 
                                               janitor::row_to_names(row_number = 3, remove_rows_above = TRUE) %>% 
                                               select(-contains("%")) %>% 
                                               janitor::clean_names() %>% 
                                               rename(x1_year_or_less = one_year_or_less, total_treasuries_outstanding = total_outstanding_billions) %>% 
                                               rename_with(~ str_remove(., "^x_?")) %>%   # Remove 'x' or 'x_' prefixes
                                               rename_with(~ paste0("treasury_outstanding_maturities_", .), .cols = -c(end_of_month, total_treasuries_outstanding, average_length_months)) 
) %>% 
  mutate(across(-c(year, end_of_month), ~ as.numeric(.))) %>% 
  mutate(end_of_month_date = ymd(paste(year, end_of_month, "01")) %>% 
           ceiling_date(unit = "month") - days(1), 
         .after="end_of_month") %>% select(-c("year","end_of_month"))

data_1985 <- data_1985 %>% left_join(treasury_securities_by_maturity, by=c("date"="end_of_month_date"))

#Order data by date
#Will look at 2003 onward and 1985 onward
data_1985 <- data_1985 %>% filter(data_1985$date >= "1985-01-01") %>% arrange(date)

data_2003 <- data_2003 %>% filter(data_2003$date >= "2002-12-31") %>% arrange(date)

save(data_1985, data_2003, tenyr_yield, file = "Data/dataframes.RData")
