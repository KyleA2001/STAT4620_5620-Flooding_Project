
#Reading in the excel sheet with both Halifax and Yarmouth observations for all the variables:
all.data = read.csv("Data/combination_df.csv")

head(all.data)
dim(all.data)

#Number of daily observations for Halifax
length(which(all.data$location=="Halifax"))
#Number of daily observations for Yarmouth
length(which(all.data$location=="Yarmouth"))

#Ensure Date column is in Date format (modify if your date column has a different name)
all.data$date = as.Date(all.data$date)

#Identifying location specific missingness.
#For easier analysis separate the table by location.
halifax = subset(all.data, all.data$location == "Halifax")
#dim(halifax)
#Number of missing observations in the time interval observed.
#This will be the same for max/min  storm surge and avg/max/min water levels.
length(which(is.na(halifax$avg_water_surge)))

yarmouth = subset(all.data, all.data$location == "Yarmouth")
#dim(yarmouth)
#Number of missing observations in the time interval observed.
length(which(is.na(yarmouth$avg_water_surge)))

#Removing the rows where  storm surge is NA in Halifax
halifax2 =  halifax[!is.na(halifax$avg_water_surge), ]
#Identifying the starting and ending timestamps.
print(min(halifax2$date))
print(max(halifax2$date))
nrow(halifax2)
#Removing the rows where  storm surge is NA in Yarmouth
yarmouth2 = yarmouth[!is.na(yarmouth$avg_water_surge), ]
#Identifying the starting and ending timestamps.
print(min(yarmouth2$date))
print(max(yarmouth2$date))
nrow(yarmouth2)

#Going back and removing all avg_water surge NA rows and accordingly identifying missingness in the other variables.
all.data2 = all.data[!is.na(all.data$avg_water_surge), ]

#Checking for NAs in other other variables.
#Temperature
length(which(is.na(all.data2$max_temperature)))
length(which(is.na(all.data2$avg_temperature)))
length(which(is.na(all.data2$min_temperature)))
#No missingness in temperature for both locations.
#Wind Speed
length(which(is.na(halifax2$max_wind_speed))) #No missingness in max.wind.speed for Yarmouth
length(which(is.na(halifax2$avg_wind_speed))) #No missingness in avg.wind.speed for Yarmouth
length(which(is.na(halifax2$min_wind_speed))) #No missingness in min.wind.speed for Yarmouth

#Rai
length(which(is.na(halifax2$rain))) #109 decreased to 92 after removing NA  storm surge
length(which(is.na(yarmouth2$rain))) #83 decreased to 81 after removing NA  storm surge

#Identifying missingness with respect to storm surge for Halifax.

library(ggplot2)
# Plot time series with missing values highlighted
ggplot(halifax, aes(x = date, y = avg_water_surge)) +
  geom_line(na.rm = TRUE) +  # Plot the time series, ignoring NA values
  geom_point(data = halifax[is.na(halifax$avg_water_surge), ],
             aes(x = date, y = 0), color = "red", size = 3) +  # Mark missing values
  labs(title = "Halifax Time Series of Average  storm surge with Missing Data",
       x = "Date",
       y = "Average  storm surge") +
  theme_minimal()

#NA rows removed.
ggplot(halifax2, aes(x = date, y = avg_water_surge)) +
  geom_line(na.rm = TRUE) +  # Plot the time series, ignoring NA values
  geom_point(data = halifax2[is.na(halifax2$avg_water_surge), ],
             aes(x = date, y = 0), color = "red", size = 3) +  # Mark missing values
  labs(title = "Halifax Time Series of Average  storm surge (NA Removed)",
       x = "Date",
       y = "Average  storm surge") +
  theme_minimal()

#Identifying missingness with respect to  storm surge for Yarmouth.

# Plot time series with missing values highlighted
ggplot(yarmouth, aes(x = date, y = avg_water_surge)) +
  geom_line(na.rm = TRUE) +  # Plot the time series, ignoring NA values
  geom_point(data = yarmouth[is.na(yarmouth$avg_water_surge), ],
             aes(x = date, y = 0), color = "red", size = 3) +  # Mark missing values
  labs(title = "Yarmouth Time Series of Average  storm surge with Missing Data",
       x = "Date",
       y = "Average  storm surge") +
  theme_minimal()

#NA rows removed.
ggplot(yarmouth2, aes(x = date, y = avg_water_surge)) +
  geom_line(na.rm = TRUE) +  # Plot the time series, ignoring NA values
  labs(title = "Yarmouth Time Series of Average  storm surge (NA Removed)",
       x = "Date",
       y = "Average  storm surge") +
  theme_minimal()

