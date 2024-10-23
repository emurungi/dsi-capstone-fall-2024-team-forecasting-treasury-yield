set.seed(37826)

load("Data/cleaned_monthly_data.RData")

library(dplyr)
library(forecast)
library(glmnet)
library(lubridate)
library(Metrics)
library(caret)
library(rsample)
library(caret)
library(MLmetrics)
library(vars)
library(MTS)

#####Basic ARIMA####

tenyr_yield_ts <- ts(monthly_2003$tenyr_yield, start = c(2003, 1), frequency = 12)

# Define the length of the train data
train_length <- length(tenyr_yield_ts) - 60  # Assuming monthly data and 5 years for test

# Split the data
train_data <- window(tenyr_yield_ts, end = c(2019, 10))  # Assuming data ends in 2024
test_data <- window(tenyr_yield_ts, start = c(2019, 11))

# Fit ARIMA model on training data
arima_model <- auto.arima(train_data)

# Forecast 5 years out
arima_forecast <- forecast(arima_model, h = 60)

# Plot the forecast against actual test data
plot(arima_forecast, main = "ARIMA Forecast vs Actuals")
lines(test_data, col = 'red')

# Calculate accuracy metrics
#INCORRECT FIX THIS
accuracy(arima_forecast, test_data)


start_date <- c(2015, 1)  # January 2015
end_date <- c(2019, 12)   # December 2019 (5 years back from 2024 end)
forecast_horizon <- 60    # 5 years ahead, assuming monthly data

rolling_forecasts <- list()

for (i in seq(start_date[1] + (start_date[2] - 1) / 12, end_date[1] + (end_date[2] - 1) / 12, by = 1/12)) {
  # Define the training window
  train_window <- window(tenyr_yield_ts, end = c(floor(i), round(12 * (i - floor(i)) + 1)))
  
  # Fit ARIMA model
  arima_model <- auto.arima(train_window)
  
  # Forecast 5 years out
  arima_forecast <- forecast(arima_model, h = forecast_horizon)
  
  # Store the forecast
  rolling_forecasts[[paste(floor(i), round(12 * (i - floor(i)) + 1), sep = "_")]] <- arima_forecast
}

# Plot each forecast against actual data
plot(tenyr_yield_ts, main = "Rolling 5-Year Forecasts")
for (i in names(rolling_forecasts)) {
  lines(rolling_forecasts[[i]]$mean, col = "blue")
}


####Naive LASSO####

#Mainly for feature selection


X <- monthly_1985[, names(monthly_1985) != "tenyr_yield"]

X$future_date <- X$month + years(5)

merged_data <- X %>% inner_join(monthly_yield, by = c("future_date" = "month"))

merged_data <- merged_data[, -which(names(merged_data) == "future_date")]

#we'll do a 75-25 train-test split for this

#This train test split is incorrect
#Should do more like a window where we take a number of points and validate on the next point
#Called is next step ahead cross validation

#Also talked about what we should use as our evaluation metrics
#Liked using R squared and RMSE

train_size <- round(nrow(merged_data) * 0.75)

train_data <- merged_data[1:train_size, ]
test_data <- merged_data[-(1:train_size), ]

y_train <- train_data$tenyr_yield
X_train <- as.matrix(train_data[, -which(names(train_data) %in% c("month", "tenyr_yield"))])

y_test <- test_data$tenyr_yield
X_test <- as.matrix(test_data[, -which(names(test_data) %in% c("month", "tenyr_yield"))])


lasso_model <- cv.glmnet(X_train, y_train, aplha = 1, lambda = seq(0.01, 1, by = 0.01))
best_lambda <- lasso_model$lambda.min

coef_matrix <- coef(lasso_model, s = best_lambda)
coef_df <- as.data.frame(as.matrix(coef_matrix))
coef_df <- data.frame(feature = rownames(coef_df), coefficient = coef_df[, 1])

# Make predictions for the test period
test_preds <- predict(lasso_model, s = best_lambda, newx = X_test)

# Compare with actual test data
results <- data.frame(date = test_data$month, actual = y_test, predicted = test_preds)

# Assuming 'results' is your dataframe with date, actual, and predicted values
ggplot(results, aes(x = date + years(5))) +
  geom_line(aes(y = actual, color = "Actual")) +
  geom_line(aes(y = s1, color = "Predicted")) +
  labs(title = "Predicted vs Actual Ten-Year Yield",
       x = "Date",
       y = "Ten-Year Yield",
       color = "Legend") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal()


train_preds <- predict(lasso_model, s = best_lambda, newx = X_train)

train_rmse <- RMSE(y_train, train_preds)
train_mape <- MAPE(y_train, train_preds)
train_r2 <- R2(train_preds, y_train)

test_rmse <- RMSE(y_test, test_preds)
test_mape <- MAPE(y_test, test_preds)
test_r2 <- R2(test_preds, y_test)

print(paste("Train RMSE:", train_rmse))
print(paste("Train MAPE:", train_mape))
print(paste("Train R-Squared:", train_r2))
print(paste("Test RMSE:", test_rmse))
print(paste("Test MAPE:", test_mape))
print(paste("Test R-Squared:", test_r2))




####LASSO Using time-series cross validation####
#With expanding window#

monthly_2003$yield_5yr_ahead <- lead(monthly_2003$tenyr_yield, 60)

monthly_2003 <- na.omit(monthly_2003)

cv_splits <- rolling_origin(data = monthly_2003, initial = 60, assess = 61, cumulative = TRUE)

all_preds <- c()
all_actuals <- c()
all_lambdas <- c()

for (split in cv_splits$splits) {
  train_data <- analysis(split)
  test_data <- assessment(split)
  
  #Change test_data so it's only the last n points 
  #(i.e. we want to try and predict after our test data otherwise
  #it will learn the future trend from training data)
  test_data <- tail(test_data, n=1)
  
  X_train <- as.matrix(train_data[, -which(names(train_data) %in% c("month", "yield_5yr_ahead"))])
  y_train <- train_data$yield_5yr_ahead
  
  X_test <- as.matrix(test_data[, -which(names(test_data) %in% c("month", "yield_5yr_ahead"))])
  y_test <- test_data$yield_5yr_ahead
  
  lasso_model <- cv.glmnet(X_train, y_train, alpha = 1)
  
  preds <- predict(lasso_model, newx = X_test, s = "lambda.min")
  
  best_lambda <- lasso_model$lambda.min
  
  all_preds <- c(all_preds, preds)
  
  all_actuals <- c(all_actuals, y_test)
  
  all_lambdas <- c(all_lambdas, best_lambda)
}

results <- data.frame(
  month = monthly_2003$month[121:202] + years(5), 
  actual = all_actuals,
  predicted = all_preds
)

ggplot(results, aes(x = month)) +
  geom_line(aes(y = actual, color = "Actual")) +
  geom_line(aes(y = predicted, color = "Predicted")) +
  labs(title = "Actual vs Predicted Yields Over Time",
       x = "Month",
       y = "Yield") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal()


print(paste("Total RMSE: ", RMSE(all_preds, all_actuals)))
print(paste("Total MAPE: ", MAPE(all_preds, all_actuals) * 100, "%"))
print(paste("Total R-Squared: ", R2(all_preds, all_actuals)))

pre_2021 <- results %>% filter(month < as.Date('2021-01-01'))

print(paste("Total RMSE: ", RMSE(pre_2021$predicted, pre_2021$actual)))
print(paste("Total MAPE: ", MAPE(pre_2021$predicted, pre_2021$actual) * 100, "%"))
print(paste("Total R-Squared: ", R2(pre_2021$predicted, pre_2021$actual)))


ggplot() +
  geom_line(data = results, aes(x = month, y = predicted, color = "Predicted")) +
  geom_line(data = monthly_yield, aes(x = month, y = tenyr_yield, color = "Actual")) +
  labs(title = "Actual vs Predicted Yields Over Time",
       x = "Month",
       y = "Yield") +
  scale_color_manual(values = c("Predicted" = "red", "Actual" = "black")) +
  xlim(c(as.Date('2003-01-01'), as.Date('2025-01-01'))) +
  theme_minimal()

####LASSO Using time-series cross validation####
#With rolling window# (same window size)


cv_splits <- rolling_origin(data = monthly_2003, initial = 48, assess = 61, cumulative = FALSE)

all_preds <- c()
all_actuals <- c()
all_lambdas <- c()

for (split in cv_splits$splits) {
  train_data <- analysis(split)
  test_data <- assessment(split)
  
  test_data <- tail(test_data, n=1)
  
  X_train <- as.matrix(train_data[, -which(names(train_data) %in% c("month", "yield_5yr_ahead"))])
  y_train <- train_data$yield_5yr_ahead
  
  X_test <- as.matrix(test_data[, -which(names(test_data) %in% c("month", "yield_5yr_ahead"))])
  y_test <- test_data$yield_5yr_ahead
  
  lasso_model <- cv.glmnet(X_train, y_train, alpha = 1)
  
  preds <- predict(lasso_model, newx = X_test, s = "lambda.min")
  
  best_lambda <- lasso_model$lambda.min
  
  all_preds <- c(all_preds, preds)
  
  all_actuals <- c(all_actuals, y_test)
  
  all_lambdas <- c(all_lambdas, best_lambda)
}

results <- data.frame(
  month = monthly_2003$month[109:202] + years(5),
  actual = all_actuals,
  predicted = all_preds
)

ggplot(results, aes(x = month)) +
  geom_line(aes(y = actual, color = "Actual")) +
  geom_line(aes(y = predicted, color = "Predicted")) +
  labs(title = "Actual vs Predicted Yields Over Time",
       x = "Month",
       y = "Yield") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal()


print(paste("Total RMSE: ", RMSE(all_preds, all_actuals)))
print(paste("Total MAPE: ", MAPE(all_preds, all_actuals) * 100, "%"))
print(paste("Total R-Squared: ", R2(all_preds, all_actuals)))

post_2021 <- results %>% filter(month >= as.Date('2021-01-01'))

print(paste("Total RMSE: ", RMSE(post_2021$predicted, post_2021$actual)))
print(paste("Total MAPE: ", MAPE(post_2021$predicted, post_2021$actual) * 100, "%"))
print(paste("Total R-Squared: ", R2(post_2021$predicted, post_2021$actual)))

#By far the best R-squared with window at 48
#Best MAPE and RMSE at window with 60
#This means this model is able to explain the variance the best but 
#highly sensitive to outliers and we might want to try and reduce outliers
#Will see if standardizing the data has any impact






ggplot() +
  geom_line(data = results, aes(x = month, y = predicted, color = "Predicted")) +
  geom_line(data = monthly_yield, aes(x = month, y = tenyr_yield, color = "Actual")) +
  labs(title = "Actual vs Predicted Yields Over Time",
       x = "Month",
       y = "Yield") +
  scale_color_manual(values = c("Predicted" = "red", "Actual" = "black")) +
  xlim(c(as.Date('2003-01-01'), as.Date('2025-01-01'))) +
  theme_minimal()

#tried standardizing, didn't help#







####LASSO Using time-series cross validation####
#With expanding window#

monthly_1985$yield_5yr_ahead <- lead(monthly_1985$tenyr_yield, 60)

monthly_1985 <- na.omit(monthly_1985)

library(rsample)
library(caret)
library(MLmetrics)

cv_splits <- rolling_origin(data = monthly_1985, initial = 60, assess = 61, cumulative = TRUE)

all_preds <- c()
all_actuals <- c()
all_lambdas <- c()

for (split in cv_splits$splits) {
  train_data <- analysis(split)
  test_data <- assessment(split)
  
  #Change test_data so it's only the last n points 
  #(i.e. we want to try and predict after our test data otherwise
  #it will learn the future trend from training data)
  test_data <- tail(test_data, n=1)
  
  X_train <- as.matrix(train_data[, -which(names(train_data) %in% c("month", "yield_5yr_ahead"))])
  y_train <- train_data$yield_5yr_ahead
  
  X_test <- as.matrix(test_data[, -which(names(test_data) %in% c("month", "yield_5yr_ahead"))])
  y_test <- test_data$yield_5yr_ahead
  
  lasso_model <- cv.glmnet(X_train, y_train, alpha = 1)
  
  preds <- predict(lasso_model, newx = X_test, s = "lambda.min")
  
  best_lambda <- lasso_model$lambda.min
  
  all_preds <- c(all_preds, preds)
  
  all_actuals <- c(all_actuals, y_test)
  
  all_lambdas <- c(all_lambdas, best_lambda)
}

results <- data.frame(
  month = monthly_1985$month[121:418] + years(5),
  actual = all_actuals,
  predicted = all_preds
)

ggplot(results, aes(x = month)) +
  geom_line(aes(y = actual, color = "Actual")) +
  geom_line(aes(y = predicted, color = "Predicted")) +
  labs(title = "Actual vs Predicted Yields Over Time",
       x = "Month",
       y = "Yield") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal()


print(paste("Total RMSE: ", RMSE(all_preds, all_actuals)))
print(paste("Total MAPE: ", MAPE(all_preds, all_actuals) * 100, "%"))
print(paste("Total R-Squared: ", R2(all_preds, all_actuals)))


ggplot() +
  geom_line(data = results, aes(x = month, y = predicted, color = "Predicted")) +
  geom_line(data = monthly_yield, aes(x = month, y = tenyr_yield, color = "Actual")) +
  labs(title = "Actual vs Predicted Yields Over Time",
       x = "Month",
       y = "Yield") +
  scale_color_manual(values = c("Predicted" = "red", "Actual" = "black")) +
  xlim(c(as.Date('1985-01-01'), as.Date('2025-01-01'))) +
  theme_minimal()



####LASSO Using time-series cross validation####
#With rolling window# (same window size)


cv_splits <- rolling_origin(data = monthly_1985, initial = 48, assess = 61, cumulative = FALSE)

all_preds <- c()
all_actuals <- c()
all_lambdas <- c()

for (split in cv_splits$splits) {
  train_data <- analysis(split)
  test_data <- assessment(split)
  
  test_data <- tail(test_data, n=1)
  
  X_train <- as.matrix(train_data[, -which(names(train_data) %in% c("month", "yield_5yr_ahead"))])
  y_train <- train_data$yield_5yr_ahead
  
  X_test <- as.matrix(test_data[, -which(names(test_data) %in% c("month", "yield_5yr_ahead"))])
  y_test <- test_data$yield_5yr_ahead
  
  lasso_model <- cv.glmnet(X_train, y_train, alpha = 1)
  
  preds <- predict(lasso_model, newx = X_test, s = "lambda.min")
  
  best_lambda <- lasso_model$lambda.min
  
  all_preds <- c(all_preds, preds)
  
  all_actuals <- c(all_actuals, y_test)
  
  all_lambdas <- c(all_lambdas, best_lambda)
}

results <- data.frame(
  month = monthly_1985$month[109:418] + years(5),
  actual = all_actuals,
  predicted = all_preds
)

ggplot(results, aes(x = month)) +
  geom_line(aes(y = actual, color = "Actual")) +
  geom_line(aes(y = predicted, color = "Predicted")) +
  labs(title = "Actual vs Predicted Yields Over Time",
       x = "Month",
       y = "Yield") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal()


print(paste("Total RMSE: ", RMSE(all_preds, all_actuals)))
print(paste("Total MAPE: ", MAPE(all_preds, all_actuals) * 100, "%"))
print(paste("Total R-Squared: ", R2(all_preds, all_actuals)))

post_2021 <- results %>% filter(month >= as.Date('2021-01-01'))

print(paste("Total RMSE: ", RMSE(post_2021$predicted, post_2021$actual)))
print(paste("Total MAPE: ", MAPE(post_2021$predicted, post_2021$actual) * 100, "%"))
print(paste("Total R-Squared: ", R2(post_2021$predicted, post_2021$actual)))




####VAR####

train <- monthly_1985 %>% filter(month < as.Date("2019-01-01"))
train <- as.matrix(train[, -which(names(train) %in% c("month"))])
var_model <- vars::VAR(train, p=1)

forecasts <- predict(var_model, n.ahead = 60)

# Extract forecast for tenyr_yield
tenyr_yield_forecast <- forecasts$fcst$tenyr_yield

forecast_dates <- seq.Date(from = as.Date("2019-01-01"), by = "month", length.out = 60)
forecast_df <- data.frame(month = forecast_dates, predicted = tenyr_yield_forecast[, "fcst"], lower = tenyr_yield_forecast[, "lower"], upper = tenyr_yield_forecast[, "upper"])

combined_df <- monthly_1985 %>% full_join(forecast_df, by = "month")

ggplot(combined_df, aes(x = month)) +
  geom_line(aes(y = tenyr_yield, color = "Actual")) +
  geom_line(aes(y = predicted, color = "Predicted")) +
  geom_line(aes(y = lower, color = "bounds")) + 
  geom_line(aes(y = upper, color = "bounds")) +
  labs(title = "Actual vs Predicted Ten-Year Yields",
       x = "Month",
       y = "Ten-Year Yield") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red", "bounds" = "black")) +
  theme_minimal()


  
