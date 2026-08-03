[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/m5cwIV43)
# Data Science Capstone & Ethics (ENGI E4800)

# Forecasting the 10-Year U.S. Treasury Yield

**Columbia University Data Science Institute Capstone — Fall 2024**

**Authors:** Max Mason and Emmanuel Mwebaze

This project investigates the use of statistical and machine learning methods to forecast the **10-year U.S. Treasury yield five years ahead**. The objective is to identify macroeconomic variables that provide information about long-horizon Treasury yields and evaluate forecasting approaches capable of adapting to changing economic regimes.

**[View the Final Project Report](./Final_Project_Report.pdf)**

## Project Overview

Long-term Treasury yields play an important role in financing, investment, and liability management decisions for corporations, governments, pension funds, insurers, and other market participants. While much of the existing yield-forecasting literature focuses on the term structure itself, this project explores whether a broader set of macroeconomic and financial variables can improve long-horizon forecasts of the 10-year U.S. Treasury yield.

The analysis focuses on a 60-month forecast horizon and uses monthly economic and financial data spanning multiple economic regimes.

## Data

Data was collected primarily from:

- Federal Reserve Economic Data (FRED)
- U.S. Congressional Budget Office (CBO)
- U.S. Department of the Treasury

The underlying datasets include interest rates, inflation measures, recession indicators, monetary variables, fiscal data, and other macroeconomic indicators.

Data of different frequencies was standardized to a monthly frequency while preserving point-in-time information. Daily observations were aggregated to monthly measures, while lower-frequency economic data was carried forward to the corresponding monthly observations.

## Methodology

### Exploratory Time-Series Analysis

The 10-year Treasury yield was examined using:

- Autocorrelation Function (ACF)
- Partial Autocorrelation Function (PACF)
- Augmented Dickey-Fuller (ADF) tests
- Correlation analysis and feature selection

The analysis found strong raw autocorrelation in Treasury yields, but substantially weaker partial autocorrelation beyond the first few lags, motivating the inclusion of macroeconomic predictors for the five-year forecasting problem.

### Feature Engineering

The project constructs a monthly modeling dataset from economic variables reported at different frequencies. Additional features were derived from higher-frequency Treasury data, and highly correlated predictors were removed to reduce multicollinearity.

LASSO was also used for variable selection and model interpretation. Important predictors identified during the analysis included measures related to:

- Recession conditions
- Producer price inflation
- Consumer inflation expectations
- Federal Reserve balance-sheet activity

### Time-Series Validation

Because conventional random train-test splitting would introduce look-ahead bias, several temporally ordered validation approaches were evaluated:

1. Naive chronological train-test split
2. Expanding-window validation
3. Sliding-window validation
4. Nested sliding-window validation

The nested sliding-window framework was designed to capture longer-term patterns while allowing the models to adapt to more recent economic conditions.

## Models

Several statistical and machine learning approaches were evaluated:

- LASSO Regression — regularized regression used for prediction and feature selection
- Bayesian Structural Time Series (BSTS) — probabilistic time-series modeling incorporating trend and regression components
- Gated Recurrent Unit (GRU) — recurrent neural network designed to capture temporal dependencies
- Feed-Forward Neural Network to CDF — reformulation of the forecasting problem as a threshold-based classification problem to increase the effective training sample

## Results

Model performance was evaluated below:

| Model | Validation Window | RMSE | MAPE |
|---|---|---:|---:|
| GRU (low autocorrelation) | Nested | **1.084** | **31.53%** |
| GRU (autocorrelated) | Sliding | 1.265 | 40.53% |
| BSTS | Sliding | 1.420 | 65.95% |
| Feed-Forward Network to CDF | Sliding | 1.589 | 66.45% |
| LASSO | Sliding | 4.290 | 129.70% |

The best-performing specification was the GRU using nested-window sampling, after modifying the training procedure to reduce the tendency of the model to reproduce lagged yield patterns.

## Key Findings

The analysis suggests that:

- Past Treasury yields alone provide limited information for forecasting yields at a five-year horizon despite their strong raw autocorrelation.
- Macroeconomic variables contain useful information for long-horizon Treasury yield prediction.
- Inflation, recession indicators, inflation expectations, and Federal Reserve balance-sheet measures emerged as important predictors.
- The choice of time-series validation methodology materially affects model performance.
- Sliding and nested sliding-window approaches were better suited to changing economic regimes than static or expanding training windows.
- GRU-based models produced the strongest forecasting performance among the approaches tested, although controlling overfitting and autocorrelation in predictions remains an important challenge.

## Future Work

Potential extensions include:

- Incorporating Treasury futures and other market-implied expectations
- Expanding the model to multiple points along the Treasury yield curve
- Exploring multivariate Bayesian time-series models
- Testing Transformer-based forecasting architectures
- Developing more robust methods for handling structural breaks and economic regime changes
---
   
