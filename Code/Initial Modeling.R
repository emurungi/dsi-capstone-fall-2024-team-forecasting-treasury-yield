load("Data/cleaned_monthly_data.RData")

library(forecast)


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
accuracy(arima_forecast, test_data)


# Assuming tenyr_yield_ts is your time series data
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


####LASSO####
library(glmnet)


X <- monthly_2003[, names(monthly_2003) != "tenyr_yield"]

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


lasso_model <- cv.glmnet(X_train, y_train, aplha = 1)
best_lambda <- lasso_model$lambda.min

coef_matrix <- coef(lasso_model, s = best_lambda)
coef_df <- as.data.frame(as.matrix(coef_matrix))
coef_df <- data.frame(feature = rownames(coef_df), coefficient = coef_df[, 1])

# Make predictions for the test period
test_preds <- predict(lasso_model, s = best_lambda, newx = X_test)

# Compare with actual test data
results <- data.frame(date = test_data$month, actual = y_test, predicted = future_prediction)

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

library(Metrics)
library(caret)

train_preds <- predict(lasso_model, s = best_lambda, newx = X_train)

train_rmse <- rmse(y_train, train_preds)
train_mape <- mape(y_train, train_preds)
train_r2 <- R2(train_preds, y_train)

test_rmse <- rmse(y_test, test_preds)
test_mape <- mape(y_test, test_preds)
test_r2 <- R2(test_preds, y_test)

print(paste("Train RMSE:", train_rmse))
print(paste("Train MAPE:", train_mape))
print(paste("Train R-Squared:", train_r2))
print(paste("Test RMSE:", test_rmse))
print(paste("Test MAPE:", test_mape))
print(paste("Test R-Squared:", test_r2))


# Combine the actual and predicted values
full_data <- data.frame(
  date = c(train_data$month, test_data$month),
  actual = c(y_train, y_test),
  predicted = c(train_preds, test_preds)
)
full_data$date <- full_data$date + years(5)

ggplot() + 
  geom_line(data = monthly_yield, aes(x = month, y = tenyr_yield, color = "Actual")) + 
  geom_line(data = full_data, aes(x = date, y = predicted, color = "Predicted"), linetype = "dashed") + 
  labs(title = "Actual vs Predicted Ten-Year Yield",
       x = "Date",
       y = "Ten-Year Yield",
       color = "Legend") + 
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) + 
  theme_minimal()


ggplot(full_data, aes(x = date)) +
  geom_line(aes(y = actual, color = "Actual")) +
  geom_line(aes(y = predicted, color = "Predicted"), linetype = "dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2020-09-01")), linetype = "dashed", color = "black")
  labs(title = "Actual vs Predicted Ten-Year Yield",
       x = "Date",
       y = "Ten-Year Yield",
       color = "Legend") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal()

  

####Fine-Tuning LASSO####
  
new_lasso_model <- cv.glmnet(X_train, y_train, alpha = 1, lambda = seq(0.015, 10, length = 100), nfolds = 10)
best_lambda <- new_lasso_model$lambda.min
  
coef_matrix <- coef(new_lasso_model, s = best_lambda)
coef_df <- as.data.frame(as.matrix(coef_matrix))
coef_df <- data.frame(feature = rownames(coef_df), coefficient = coef_df[, 1])
  
test_preds <- predict(new_lasso_model, s = best_lambda, newx = X_test)
  
results <- data.frame(date = test_data$month, actual = y_test, predicted = test_preds)
  

ggplot(results, aes(x = date + years(5))) +
  geom_line(aes(y = actual, color = "Actual")) +
  geom_line(aes(y = s1, color = "Predicted")) +
  labs(title = "Predicted vs Actual Ten-Year Yield",
       x = "Date",
       y = "Ten-Year Yield",
       color = "Legend") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal()

#Training this way just favors a lower and lower lambda
#Want a validation set where we train on the training data and then
#minimize the loss/error on the validation set

#Does this have data leakage? It probably does because it includes the 10 year yield


#Maybe use a smaller time frame like 1 year or two years to see if this model has predictive value in short
#term since we're testing on a period that is drastically different from training data
#But can't include this covid shock in training data because it is within the last 5 years


