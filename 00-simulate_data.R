#### Preamble ####
# Purpose: Simulates data from Open Data Toronto
# Author: Dingshuo Li
# Date: 21 September 2024 
# Contact: dawson.li@mail.utoronto.ca
# License: None
# Pre-requisites: None
# Any other information needed? None


#### Workspace setup ####
library(tidyverse)

#### Simulate data ####
set.seed(304)

# Set the number of random dates you want to generate
number_of_dates <- 200

# Define the date and time
start_date <- as.Date("2018-01-03")
end_date <- as.Date("2023-12-30")

#simulate the data
data <- 
  tibble(
    dates = as.Date(
      runif(
        n = number_of_dates,
        min = as.numeric(start_date),
        max = as.numeric(end_date)
      ),
      origin = "1970-01-01"
    ),
    number_of_hate_crime = rpois(n = number_of_dates, lambda = 1)  # Adjust lambda as needed
  ) %>%
  mutate(
    occurrence_year = year(dates),  
    location_type = sample(c("Commercial", "Residential", "Public Park", 
                             "Religious Place", "Educational Institution"), 
                           number_of_dates, replace = TRUE),
    primary_offence = sample(c("Assault", "Mischief", "Theft", "Fraud", 
                               "Wilful Promotion of Hatred"), 
                             number_of_dates, replace = TRUE),
    arrest_made = sample(c("YES", "NO"), number_of_dates, replace = TRUE, prob = c(0.4, 0.6)),
    hour = sample(0:24, number_of_dates, replace = TRUE)  
  )

#### Write CSV ####
write_csv(data, file = "data/raw_data/simulated_data.csv")


#### key test for simulated data ####


t_test <- t.test(hour ~ arrest_made, data = data)
print(t_test)

anova_test <- aov(hour ~ location_type, data = data)
print(summary(anova_test))

expected_start_date <- as.Date("2018-01-03")
expected_end_date <- as.Date("2023-12-30")
test_date_range <- min(data$dates) >= expected_start_date & max(data$dates) <= expected_end_date
print(test_date_range)
