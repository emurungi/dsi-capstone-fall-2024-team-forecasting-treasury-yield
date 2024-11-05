set.seed(42566)
load("Data/cleaned_monthly_data.RData")

library(bsts)
library(ggplot2)
library(dplyr)
library(tidymodels)
library(MLmetrics)
library(lubridate)
library(Metrics)
library(caret)


monthly_1985$yield_5yr_ahead <- lead(monthly_1985$tenyr_yield, 60)

monthly_1985 <- na.omit(monthly_1985)

cv_splits <- rolling_origin(data = monthly_1985, initial = 60, assess = 61, cumulative = TRUE)

all_preds <- c()
all_actuals <- c()

for (split in cv_splits$splits) {
  train_data <- analysis(split)
  test_data <- assessment(split)
  
  y_train <- train_data$yield_5yr_ahead

  ss <- AddDynamicRegression(list(), y_train ~ GDP_based_recession_indicator + umich_inflation_excpectation + 
                               equity_market_volatility + yen_dollar_exchange_rate + velocity_of_m1_money_stock +
                             `30yr_yield` + industrial_production_consumer_goods + yield_stdev,
                             data = train_data)

  model <- bsts(y_train, state.specification = ss, niter = 4000)
  
  
  y_test <- test_data$yield_5yr_ahead 
  pred <- predict(model, newdata = test_data, horizon = length(y_test)) 
  all_preds <- c(all_preds, as.vector(pred$median)[61])
  all_actuals <- c(all_actuals, y_test[61])
}


# Create a data frame for plotting
results <- data.frame(
  date = monthly_1985$month[121:418] + years(5),
  actual = all_actuals,
  predicted = all_preds
)

# Plot the results
ggplot(results, aes(x = date)) +
  geom_line(aes(y = actual, color = "Actual")) +
  geom_line(aes(y = predicted, color = "Predicted")) +
  labs(x = "Date", y = "10-Year Yield", title = "Actual vs Predicted 10-Year Yield") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal()


# Root Mean Squared Error (RMSE)
rmse <- RMSE(y_pred = all_preds, y_true = all_actuals)
print(paste("RMSE: ", rmse))

# R-squared (R²)
r2 <- R2(all_preds,all_actuals)
print(paste("R²: ", r2))

# Mean Absolute Percentage Error (MAPE)
mape <- MAPE(y_pred = all_preds, y_true = all_actuals) * 100
print(paste("MAPE: ", mape))




cv_splits <- rolling_origin(data = monthly_1985, initial = 60, assess = 61, cumulative = FALSE)

all_preds2 <- c()
all_actuals2 <- c()

for (split in cv_splits$splits) {
  train_data <- analysis(split)
  test_data <- assessment(split)
  
  y_train <- train_data$yield_5yr_ahead
  
  ss <- AddDynamicRegression(list(), y_train ~ GDP_based_recession_indicator + umich_inflation_excpectation + 
                               equity_market_volatility + yen_dollar_exchange_rate + velocity_of_m1_money_stock +
                               `30yr_yield` + industrial_production_consumer_goods + yield_stdev,
                             data = train_data)
  
  model <- bsts(y_train, state.specification = ss, niter = 4000)
  
  
  y_test <- test_data$yield_5yr_ahead 
  pred <- predict(model, newdata = test_data, horizon = length(y_test)) 
  all_preds2 <- c(all_preds2, as.vector(pred$median)[61])
  all_actuals2 <- c(all_actuals2, y_test[61])
}


# Create a data frame for plotting
results2 <- data.frame(
  date = monthly_1985$month[121:418] + years(5),
  actual = all_actuals2,
  predicted = all_preds2
)

# Plot the results
ggplot(results2, aes(x = date)) +
  geom_line(aes(y = actual, color = "Actual")) +
  geom_line(aes(y = predicted, color = "Predicted")) +
  labs(x = "Date", y = "10-Year Yield", title = "Actual vs Predicted 10-Year Yield") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal()


# Root Mean Squared Error (RMSE)
rmse <- RMSE(all_preds2, all_actuals2)
print(paste("RMSE: ", rmse))

# R-squared (R²)
r2 <- R2(all_preds2, all_actuals2)
print(paste("R²: ", r2))

# Mean Absolute Percentage Error (MAPE)
mape <- MAPE(y_pred = all_preds2, y_true = all_actuals2) * 100
print(paste("MAPE: ", mape))
