load("Data/dataframes.RData")


#Maybe add some more plots of data (i.e. trend decomposition etc.)

#Starting with monthly data where we forward fill quarterly data and average daily data
monthly_2003 <- data_2003 %>% fill(everything(), .direction = "down") %>% filter(data_2003$date >= "2003-01-01")

monthly_2003 <- monthly_2003 %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(across(-date, ~mean(.x, na.rm = TRUE)))

#The monthly average captures a majority of the variation in the 10 year yield
ggplot() +
  geom_line(data = monthly_2003, aes(x = month, y = tenyr_yield), color = "blue") +
  geom_line(data = tenyr_yield, aes(x = date, y = tenyr_yield), color = "red", alpha = 0.5) +
  xlim(c(as.Date("2003-01-01"), as.Date("2024-12-31"))) +
  ylim(c(0, 6))

#remove date and y-variable for correlation testing
numeric_data <- monthly_2003[, sapply(monthly_2003, is.numeric) & names(monthly_2003) != "tenyr_yield"]

#Make the data percent changes to test correlation
percent_change <- function(x) c(NA, diff(x) / head(x + 1e-10, -1))
change_data <- as.data.frame(lapply(numeric_data, percent_change))
change_data <- change_data[-1, ]

# Calculate the correlation matrix
correlation_matrix_2003 <- cor(change_data, use = "complete.obs")
cor_df_2003 <- as.data.frame(as.table(correlation_matrix_2003))

#Define high correlation as above 0.75, view and remove highly correlated values
high_corr <- subset(cor_df_2003, abs(Freq) > 0.7 & Var1 != Var2)

#Based on high correlations we remove GDP, GNP, X5yr_expected_inflation, X30yr_expected_inflation, PCE_price_index,
#X5yr_yield, X7yr_yield, industrial_production_business_equipment, total_nonfarm_employees, velocity_of_m2_money_stock
# and unemployment_rate

# List of variables to remove
remove_vars <- c("GDP", "GNP", "X5yr_expected_inflation", "X30yr_expected_inflation", "PCE_price_index", 
                 "X5yr_yield", "X7yr_yield", "industrial_production_business_equipment", 
                 "total_nonfarm_employees", "velocity_of_m2_money_stock", "unemployment_rate",
                 "5yr_yield", "7yr_yield", "30yr_expected_inflation", "5yr_expected_inflation")

# Subset the data to exclude these variables
test_change_data <- change_data[, !(names(change_data) %in% remove_vars)]

#testing new correlation to make sure the change in variables over time are no longer correlated
cor_matrix <- cor(test_change_data, use = "complete.obs")
cor_df <- as.data.frame(as.table(cor_matrix))
high_corr <- subset(cor_df, abs(Freq) > 0.7 & Var1 != Var2)

#Number of pairs with correlation above 0.7 after removal
print(nrow(high_corr))


monthly_2003 <- monthly_2003[, !(names(monthly_2003) %in% remove_vars)]



monthly_yield <- tenyr_yield %>% complete(date = seq(min(date), max(date), by = "day")) %>% fill(everything(), .direction = "down")

monthly_yield <- monthly_yield %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(across(-date, ~mean(.x, na.rm = TRUE)))

save(monthly_2003, monthly_yield, file = "Data/cleaned_monthly_data.RData")


data <- monthly_2003[, c("month", "tenyr_yield")]

# Extract year and month
data <- data %>%
  mutate(year = as.numeric(format(month, "%Y")),
         month = as.numeric(format(month, "%m")))

# Checking to see if there is any seasonality
ggplot(data, aes(x = month, y = tenyr_yield, color = as.factor(year), group = year)) +
  geom_line() +
  labs(title = "Ten-Year Yield Performance by Month for Each Year",
       x = "Month",
       y = "Ten-Year Yield",
       color = "Year") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  theme_minimal()


tenyr_yield_ts <- ts(monthly_2003$tenyr_yield, start = c(2003, 1), frequency = 12)

#Checking the 10 year yield autocorrelation with the autocorrelation function
acf(tenyr_yield_ts, main = "Autocorrelation Function of Ten-Year Yield", lag.max = 60)

pacf(tenyr_yield_ts, main = "Partial Autocorrelation Function of Ten-Year Yield", lag.max = 60)


