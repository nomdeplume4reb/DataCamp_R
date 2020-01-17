#source: https://data.humdata.org/dataset/wfp-food-prices#
library("dplyr")
library("lubridate")
library("ggplot2")
library("magrittr")
library("forecast")

wfp <- read.csv("wfpvam_foodprices.csv")

wfp_rwa <- wfp %>% 
  select(adm0_name, adm1_name, mkt_name, cm_name, mp_month, mp_year, mp_price) %>%
  filter(adm0_name == "Rwanda")

# Take a glimpse at the contents
glimpse(wfp_rwa)

# Rename the columns to be more informative
wfp_rwa_renamed <- rename(wfp_rwa, country = adm0_name, region = adm1_name, market = mkt_name,
                                "commodity_kg" = cm_name , month = mp_month, year = mp_year, price_rwf = mp_price)

# Check the result
glimpse(wfp_rwa_renamed)

# List of food items
food_items <- list("Bananas - Retail", "Beans (dry) - Retail",
                   "Bread - Retail", "Cassava - Retail", "Cheese - Retail", 
                   "Chili (red) - Retail", "Coffee - Retail", "Dates - Retail", 
                   "Eggs - Retail", "Fish - Retail", "Maize - Retail",
                   "Meat (beef) - Retail", "Meat (chicken) - Retail", "Milk - Retail",
                   "Oranges (big size) - Retail", "Passion fruit - Retail",
                   "Peas (dry) - Retail", "Plantains - Retail", "Potatoes (Irish) - Retail",
                   "Rice - Retail", "Rice (local) - Retail", "Sorghum - Retail",
                   "Sugar - Retail", "Tomatoes - Retail")

# Convert year and month to Date
rwa_cleaned <- wfp_rwa_renamed %>% 
  mutate(date = ymd(paste(year, month, "01"))) %>%
  select(-year, -month)

# See the result
str(rwa_cleaned)

#view commodity factor levels
rwa_cleaned$commodity_kg %>% 
  sapply(levels)

# Draw a line plot of price vs. date grouped by market
# for data before 2016
rwa_cleaned %>%
  filter(date < "2016-01-01", commodity_kg == "Potatoes (Irish) - Retail") %>%
  ggplot(aes(x=date, y=price_rwf, group = market))+
  geom_line(alpha=.2)+
  ggtitle("Potato prices over time")
# 
# # Wrap this code into a function
plot_price_vs_time <- function(commodity){
  rwa_cleaned %>%
    filter(date < "2016-01-01", commodity_kg == commodity) %>%
    ggplot(aes(date, price_rwf, group = market)) +
    geom_line(alpha = 0.2) +
    ggtitle(paste(commodity, "price over time"))
}
# Try the function on the pea data
plot_price_vs_time("Peas (fresh) - Retail")
 
# Group by date, and calculate the median price
potato_prices_summarized <- rwa_cleaned %>%
  filter(date < "2016-01-01", commodity_kg == "Potatoes (Irish) - Retail") %>%
  group_by(date) %>%
  summarize(median_price_rwf = median(price_rwf))

# See the result
print(potato_prices_summarized)

# Extract a time series
potato_time_series <- potato_prices_summarized %$%
  ts(
    median_price_rwf,
    start = c(year(min(date)), month(min(date))),
    end   = c(year(max(date)), month(max(date))),
    frequency = 12
  )

# See the result
print(potato_time_series)
# 
# Wrap this code into a function
create_price_time_series <- function(commodity) {
  prices_summarized <- rwa_cleaned %>%
    filter(date < "2016-01-01", commodity_kg == commodity) %>%
    group_by(date) %>%
    summarize(median_price_rwf = median(price_rwf))

  time_series <- prices_summarized %$%
    ts(
      median_price_rwf,
      start = c(year(min(date)), month(min(date))),
      end   = c(year(max(date)), month(max(date))),
      frequency = 12
    )

}


# Try the function on the pea data
pea_time_series <- create_price_time_series("Peas (fresh) - Retail")
pea_time_series
 
# Forecast the potato time series
potato_price_forecast <- forecast(potato_time_series)

# View itp
print(potato_price_forecast)

# Plot the forecast
autoplot(potato_price_forecast, main='Potato Price Forecast')

# Wrap the code into a function
plot_price_forecast <- function(time_series, commodity){
  price_forecast <- forecast(time_series)
  autoplot(price_forecast, main = paste(commodity, "price forecast"))
}

# Try the function on the pea data
plot_price_forecast(pea_time_series, "Peas (fresh) - Retail")

# Choose dry beans as the commodity
commodity <- "Beans (dry) - Retail"

# Plot price vs. time
plot_price_vs_time(commodity)

# Create a price time series
bean_time_series <- create_price_time_series(commodity)

# Plot the price forecast
plot_price_forecast(bean_time_series, commodity)