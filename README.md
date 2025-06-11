# Food Price Analysis

## Overview
This repository contains R scripts and datasets for a comprehensive analysis of food price trends across various countries, with a focus on Nigeria. The project leverages data from the World Bank's food price datasets to perform exploratory data analysis (EDA), time series forecasting, clustering, regression modeling, anomaly detection, correlation analysis, and the development of a custom food price index. The primary datasets used are:

- `WLD_RTP_details_2023-10-02.csv`: Contains detailed food price metrics.
- `WLD_RTFP_country_2023-10-02.csv`: Includes country-specific inflation data over time.

## Features
- **Exploratory Data Analysis (EDA)**: Visualize inflation trends and summarize statistics.
- **Time Series Forecasting**: Predict future inflation rates using ARIMA models.
- **Clustering**: Group countries based on food price characteristics using K-means.
- **Regression Analysis**: Model the relationship between price increases and other variables.
- **Anomaly Detection**: Identify unusual spikes or drops in food prices.
- **Correlation Analysis**: Explore relationships between food price metrics.
- **Custom Food Price Index**: Develop a composite index combining inflation and volatility.

## Setup Instructions

### Prerequisites
- R (version 4.0 or later)
- Git (for version control)
- The following R packages:
  - `dplyr`
  - `readr`
  - `ggplot2`
  - `anomalize`
  - `tibbletime`
  - `tseries`
  - `forecast`
  - `cluster`
  - `factoextra`
  - `randomForest`
  - `tidyr`
  - `imputeTS`
  - `lubridate`
