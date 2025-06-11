library(dplyr)
library(readr)
library(ggplot2)
library(anomalize)
library(tibbletime)


# 1. Exploratory Data Analysis (EDA) of Food Price Inflation
# Objective: Analyze and visualize inflation trends across countries (focus = Nigeria)
# Load inflation data
inflation_data <- read.csv("C:\\Users\\DELL\\Documents\\favoured\\datasets\\WLD_RTFP_country_2023-10-02.csv")

# Clean inflation_data and convert date
inflation_data <- inflation_data %>%
  mutate(
    Inflation = as.numeric(Inflation),
    date = as.Date(date, format = "%Y-%m-%d")  # Adjust date format as needed
  ) %>%
  drop_na()

# Summary statistics
summary_stats <- inflation_data %>%
  group_by(country) %>%
  summarise(mean_inflation = mean(Inflation, na.rm = TRUE),
            sd_inflation = sd(Inflation, na.rm = TRUE))

# Plot inflation trend for a specific country
ggplot(inflation_data %>% filter(country == "Nigeria"), aes(x = date, y = Inflation)) +
  geom_line() +
  labs(title = "Inflation Trend in Nigeria", x = "Date", y = "Inflation (%)")

# 2. Time Series Forecasting of Inflation
# Objective: Forecast future inflation rates for a selected country.
library(forecast)
library(tseries)
library(lubridate)

# Filter data for a specific country
nga_data <- inflation_data %>%
  filter(country == "Nigeria")

# Create time series object
ts_data <- ts(nga_data$Inflation, 
              start = c(year(min(nga_data$date)), month(min(nga_data$date))), 
              frequency = 12)

# Fit ARIMA model
model <- auto.arima(ts_data)

# Forecast next 12 months
forecast_values <- forecast(model, h = 12)

# Plot forecast
plot(forecast_values)

# 3. Clustering Countries by Food Price Metrics
# Objective: Group countries based on food price characteristics.
library(cluster)
library(factoextra)

# Load product details data
product_data <- read.csv("C:\\Users\\DELL\\Documents\\favoured\\datasets\\WLD_RTP_details_2023-10-02.csv")

product_data <- product_data %>%
  mutate(total_food_price_increase_since_start_date = parse_number(total_food_price_increase_since_start_date),
         average_annualized_food_inflation = parse_number(average_annualized_food_inflation),
         maximum_food_drawdown = parse_number(maximum_food_drawdown),
         average_annualized_food_volatility = parse_number(average_annualized_food_volatility),
         data_coverage_food = parse_number(gsub("%", "", data_coverage_food)),
         data_coverage_previous_12_months_food = parse_number(gsub("%", "", data_coverage_previous_12_months_food)))

product_data <- product_data %>% drop_na()

# Select relevant features
features <- product_data %>% select(average_annualized_food_inflation, maximum_food_drawdown)

# Standardize features
scaled_features <- scale(features)

# Perform K-means clustering
set.seed(123)
kmeans_result <- kmeans(scaled_features, centers = 3)

# Visualize clusters
fviz_cluster(kmeans_result, data = scaled_features)

# 4. Regression Analysis of Price Increase
# Objective: Model the relationship between food price increase and other variables.
model <- lm(total_food_price_increase_since_start_date ~ number_of_markets_modeled + number_of_food_items + average_annualized_food_inflation, data = product_data)

# Summary of the model
summary(model)

# Plot regression line
ggplot(product_data, aes(x = average_annualized_food_inflation, y = total_food_price_increase_since_start_date)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Inflation vs. Total Price Increase", x = "Annualized Inflation (%)", y = "Total Price Increase (%)")

# Load libraries
library(randomForest)

# Prepare data for Random Forest
data_for_rf <- product_data %>%
  select(
    total_food_price_increase_since_start_date,
    number_of_markets_modeled,
    number_of_markets_covered,
    number_of_food_items,
    data_coverage_food,
    data_coverage_previous_12_months_food,
    average_annualized_food_inflation,
    maximum_food_drawdown,
    average_annualized_food_volatility,
    average_monthly_food_price_correlation_between_markets,
    average_annual_food_price_correlation_between_markets,
    index_confidence_score
  )

# Fit Random Forest model
set.seed(123)
rf_model <- randomForest(
  total_food_price_increase_since_start_date ~ .,
  data = data_for_rf,
  ntree = 500,
  mtry = 4,
  importance = TRUE
)

# Evaluate model
print(rf_model)
varImpPlot(rf_model)


# 5. Anomaly Detection in Price Data
# Objective: Identify unusual spikes or drops in food prices.
library(anomalize)
library(tibble)

# Filter data for a specific country
nga_inflation <- inflation_data %>% filter(country == "Nigeria") %>% select(date, Inflation)

# Convert to tsibble
nga_tibble <- as_tibble(nga_inflation, index = date)

# Summary of the tibble
summary(nga_tibble)

# Check for NA values in the Inflation column
sum(is.na(nga_tibble$Inflation))

nga_tibble <- nga_tibble %>% drop_na(Inflation)

# Detect anomalies
anomalies <- nga_tibble %>%
  time_decompose(Inflation) %>%
  anomalize(remainder) %>%
  time_recompose()

# Plot anomalies
plot_anomalies(anomalies)


# 6. Correlation Analysis Between Food Items
# Objective: Analyze correlations between different food items' prices.
library(corrplot)

# Select numeric variables for correlation
correlation_data <- product_data %>%
  select(
    total_food_price_increase_since_start_date,
    average_annualized_food_inflation,
    maximum_food_drawdown,
    average_annualized_food_volatility,
    number_of_markets_modeled,
    number_of_markets_covered,
    number_of_food_items,
    data_coverage_food,
    data_coverage_previous_12_months_food,
    average_monthly_food_price_correlation_between_markets,
    average_annual_food_price_correlation_between_markets,
    index_confidence_score
  )

# Compute correlation matrix
cor_matrix <- cor(correlation_data)

# Visualize correlation matrix
corrplot(cor_matrix, method = "circle", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, diag = FALSE)

# Significance Test
cor.test(~ average_annualized_food_inflation + total_food_price_increase_since_start_date, data = correlation_data)


# 7. Custom Food Price Index Development
# Objective: Create a composite index for food prices in a country.
# Food price index analysis
price_summary <- product_data %>%
  group_by(country) %>%
  summarise(
    mean_price_increase = mean(total_food_price_increase_since_start_date, na.rm = TRUE),
    mean_inflation = mean(average_annualized_food_inflation, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(price_summary, aes(x = reorder(country, mean_price_increase), y = mean_price_increase)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Average Food Price Increase by Country", x = "Country", y = "Mean Price Increase (%)") +
  theme_minimal()

# Custom index
product_data <- product_data %>%
  mutate(custom_food_index = (average_annualized_food_inflation + (1 - average_annualized_food_volatility)) / 2)

ggplot(product_data, aes(x = country, y = custom_food_index)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  coord_flip() +
  labs(title = "Custom Food Price Index by Country", x = "Country", y = "Index Value") +
  theme_minimal()

# Merging with inflation_data
combined_data <- if ("country" %in% colnames(inflation_data)) {
  inflation_data %>%
    left_join(product_data %>% select(country, custom_food_index), by = "country") %>%
    mutate(custom_food_index = ifelse(is.na(custom_food_index), mean(custom_food_index, na.rm = TRUE), custom_food_index))
} else {
  inflation_data
}

# Ensure time series structure
ts_data <- combined_data %>%
  arrange(date) %>%
  as_tbl_time(index = date)  # Convert to tbl_time for anomalize

str(ts_data)
summary(ts_data$date)
length(unique(ts_data$date))  # Check for duplicates

ts_data <- combined_data %>%
  complete(date = seq.Date(min(date, na.rm = TRUE), max(date, na.rm = TRUE), by = "month"), 
           fill = list(Inflation = NA, custom_food_index = NA)) %>%
  arrange(date) %>%
  as_tbl_time(index = date) %>%
  mutate(Inflation = na_interpolation(Inflation),  # Impute missing Inflation
         custom_food_index = na_interpolation(custom_food_index))  # Impute missing index


# Time series decomposition and anomaly detection
anomalies <- ts_data %>%
  time_decompose(Inflation, frequency = "1 month", trend = "6 months") %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  filter(!is.na(remainder))

# Plot anomalies
plot_anomalies(anomalies)

# Plot inflation over time
ggplot(combined_data, aes(x = date, y = Inflation, color = country)) +
  geom_line() +
  labs(title = "Inflation Over Time", x = "Date", y = "Inflation (%)") +
  theme_minimal()