# Library
library(ggplot2)
library(dplyr)

# --- Yarmouth ---
# Temp data
ym_temp <- read.csv("data/weatherstats_yarmouth_daily_v2.csv")
ym_temp[,1] <- as.Date(ym_temp[,1])
head(ym_temp)

# No missing date in temp data
time_diff <- diff(ym_temp$date, unit="days")
unique(time_diff)

# Water Surge data
ym_data <- read.table("data/Yarmouth Water Surge.txt",
                      header = FALSE, sep = " ", quote = "\"", fill = TRUE, strip.white = TRUE)

colnames(ym_data) <- c("Time", "water_level", "Year", "water_surge")
ym_data <- ym_data[-1,]
ym_data[,1] <- as.POSIXct(ym_data[,1])
ym_data[,2] <- as.numeric(ym_data[,2])
ym_data[,3] <- as.numeric(ym_data[,3])

# Print the first few rows of the data
head(ym_data)

# Grouping hourly to daily data, extracting min, max and avg for water level and water surge
daily_summary_ym <- ym_data %>%
  mutate(date = as.Date(Time)) %>%
  group_by(date) %>%
  summarise(
    max_water_surge = max(water_surge),
    min_water_surge = min(water_surge),
    avg_water_surge = mean(water_surge),
    max_water_level = max(water_level),
    min_water_level = min(water_level),
    avg_water_level = mean(water_level),
  )

head(daily_summary_ym)

# Inner join will map date that including in both two tables (water surge and temp data)
new_df_ym <- inner_join(ym_temp, daily_summary_ym, by = "date")
head(new_df_ym)

# -- Check time interval of data --
# Begining time interval: 1956-05-01
head(sort(new_df_ym$date))

# Ending of time: 2024-01-01
tail(sort(new_df_ym$date))

# Check unique value of difference in data: 1
unique(diff(sort(new_df_ym$date)))

# --- Halifax ---
# Temp data
halifax_temp <- read.csv("data/weatherstats_halifaxairport_daily_v2.csv")
halifax_temp[,1] <- as.Date(halifax_temp[,1])
head(halifax_temp)

# No missing date in temp data
time_diff <- diff(halifax_temp$date, unit="days")
unique(time_diff)

# Water Surge data
halifax_data <- read.table("data/Halifax Water Surge.txt",
                           header = FALSE, sep = " ", quote = "\"", fill = TRUE, strip.white = TRUE)
colnames(halifax_data) <- c("Time", "water_level", "Year", "water_surge")
halifax_data <- halifax_data[-1,]
halifax_data[, 1] <- as.POSIXct(halifax_data[, 1], format = "%Y-%m-%d %H:%M:%S")
halifax_data[, 2] <- as.numeric(halifax_data[, 2])
halifax_data[, 3] <- as.numeric(halifax_data[, 3])

# Print the first few rows of the data
head(halifax_data)

# Grouping hourly to daily data, extracting min, max and avg for water level and water surge
daily_summary_halifax <- halifax_data %>%
  mutate(date = as.Date(Time)) %>%
  group_by(date) %>%
  summarise(
    max_water_surge = max(water_surge),
    min_water_surge = min(water_surge),
    avg_water_surge = mean(water_surge),
    max_water_level = max(water_level),
    min_water_level = min(water_level),
    avg_water_level = mean(water_level),
  )

head(daily_summary_halifax)

# Inner join will map date that including in both two tables (water surge and temp data)
new_df_halifax <- inner_join(halifax_temp, daily_summary_halifax, by = "date")
head(new_df_halifax)

# Beginning time interval: 1952-12-31
head(sort(new_df_halifax$date))

# Ending of time: 2014-08-13
tail(sort(new_df_halifax$date))

# Check unique value of difference in data: 1
unique(diff(sort(new_df_halifax$date)))

# --- Combine all data of 2 locations ---
# Generate a column to store location name
new_df_halifax$location = "Halifax"
new_df_ym$location = "Yarmouth"

# Extracted relevant columns
extract_col_name <- c("date", "location",
                      "max_water_level", "avg_water_level", "min_water_level",
                      "max_water_surge", "avg_water_surge", "min_water_surge",
                      "max_temperature", "avg_temperature", "min_temperature",
                      "max_wind_speed", "avg_wind_speed", "min_wind_speed",
                      "rain")

combination_df <- rbind(new_df_halifax[extract_col_name], new_df_ym[extract_col_name])
combination_df <- combination_df[nrow(combination_df):1,] # order data by time
head(combination_df)

# Save data
write.csv(combination_df, "data/combination_df.csv")
