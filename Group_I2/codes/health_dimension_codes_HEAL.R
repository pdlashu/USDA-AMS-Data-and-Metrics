library(tidyverse)
library(openxlsx)
library(writexl)
library(readxl)


file_path <- "C:/Users/ab01235/OneDrive - University of Georgia/Desktop/Local data visualization/data visualtization/raw_data.xlsx"

data <- read_excel(file_path)

texas <- filter(data, state == "Texas")
calif <- filter(data, state == "California")

view(data)

#creating US_avg as new column

data_w <- data %>% 
  mutate(us_avg_obs = 41.9,
         us_avg_diab = 8.2,
         us_avg_insecurity = 0.105,
         us_avg_fast_food = 0.78,
         us_avg_direct_sales = 5.81712652
         )

data_w <- data_w %>%
  mutate(median_fast_food = ifelse(is.na(us_avg_fast_food), median(us_avg_fast_food, na.rm = TRUE), us_avg_fast_food))

view(data_w)

#standardizing the data
data_w <- data_w %>% 
  mutate(std_avg_obs = 100 * exp(log(0.5) * adult_obesity_pct/us_avg_obs),
         std_avg_diab = 100 * exp(log(0.5) * adult_diabetes/us_avg_obs),
         std_avg_insecurity = 100 * exp(log(0.5) * food_insecurity_rate/us_avg_insecurity ),
         std_avg_fastfood = 100 * exp(log(0.5) * fast_food_restaurant_per_1000/us_avg_fast_food))

data_w <- data_w %>% 
  mutate(health_mean = (std_avg_obs+ std_avg_diab+ std_avg_fastfood + std_avg_insecurity)/4)
         

file_path1 <- file_path <- "C:/Users/ab01235/OneDrive - University of Georgia/Desktop/Local data visualization/data visualtization/output.xlsx"

write.xlsx(data_w, file_path1)

