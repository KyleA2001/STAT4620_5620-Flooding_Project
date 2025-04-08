source("R/df_linear_interpolation_fn.R")
source("R/find_missing_interval_fn.R")
source("R/linear_interpolation_fn.R")

final_df = read.csv("data/combination_df.csv")

# This will be the same for max/min water surge and avg/max/min water levels.
halifax = subset(final_df, final_df$location == "Halifax")
# Number of missing observations in the time interval observed.
length(which(is.na(halifax$avg_water_surge)))

yarmouth = subset(final_df, final_df$location == "Yarmouth")
# Number of missing observations in the time interval observed.
length(which(is.na(yarmouth$avg_water_surge)))

# --- Find missing index ---
# Halifax
start_missing_index <- c()
end_missing_index <- c()
missing_water_surge_halifax <- which(is.na(halifax$avg_water_surge))
start_missing_index[1] <- missing_water_surge_halifax[1]

for (i in 2:(length(missing_water_surge_halifax))){
  if (missing_water_surge_halifax[i]-missing_water_surge_halifax[i-1] > 1){
    start_missing_index <- c(start_missing_index, missing_water_surge_halifax[i])
    end_missing_index <- c(end_missing_index, missing_water_surge_halifax[i-1])
  }
}
end_missing_index <- c(end_missing_index, missing_water_surge_halifax[length(missing_water_surge_halifax)])
missing_period <- end_missing_index - start_missing_index + 1

halifax_missingness_index <- as.data.frame(cbind(start_missing_index, end_missing_index, missing_period))
head(halifax_missingness_index, 10)

sort(halifax_missingness_index$missing_period, decreasing=TRUE)

# Yarmouth
start_missing_index <- c()
end_missing_index <- c()
missing_water_surge_yarmouth <- which(is.na(yarmouth$avg_water_surge))
start_missing_index[1] <- missing_water_surge_yarmouth[1]

for (i in 2:(length(missing_water_surge_yarmouth))){
  if (missing_water_surge_yarmouth[i]-missing_water_surge_yarmouth[i-1] > 1){
    start_missing_index <- c(start_missing_index, missing_water_surge_yarmouth[i])
    end_missing_index <- c(end_missing_index, missing_water_surge_yarmouth[i-1])
  }
}
end_missing_index <- c(end_missing_index, missing_water_surge_yarmouth[length(missing_water_surge_yarmouth)])
missing_period <- end_missing_index - start_missing_index + 1

yarmouth_missingness_index <- as.data.frame(cbind(start_missing_index, end_missing_index, missing_period))
head(yarmouth_missingness_index, 10)

sort(yarmouth_missingness_index$missing_period, decreasing=TRUE)

which(is.na(yarmouth$max_wind_speed))

# Yarmouth
length(which(is.na(yarmouth$avg_water_surge)))
length(which(is.na(yarmouth$avg_wind_speed)))
length(which(is.na(yarmouth$rain)))
length(which(is.na(yarmouth$avg_temperature)))

yarmouth_df_interpolation <- linear_interpolation_df(df=yarmouth,
                                                     col_names=c("rain",
                                                                 "max_wind_speed", "avg_wind_speed", "min_wind_speed",
                                                                 "max_temperature", "avg_temperature", "min_temperature"),
                                                     cut_off=15)

# Halifax
length(which(is.na(halifax$avg_water_surge)))
length(which(is.na(halifax$avg_wind_speed)))
length(which(is.na(halifax$rain)))
length(which(is.na(halifax$avg_temperature)))

halifax_df_interpolation <- linear_interpolation_df(df=halifax,
                                                    col_names=c("rain",
                                                                "max_wind_speed", "avg_wind_speed", "min_wind_speed",
                                                                "max_temperature", "avg_temperature", "min_temperature"),
                                                    cut_off=15)

new_halifax_df_interpolation = na.omit(halifax_df_interpolation)
new_yarmouth_df_interpolation = na.omit(yarmouth_df_interpolation)

new_df_linear_interpolation <- rbind(new_halifax_df_interpolation, new_yarmouth_df_interpolation)
head(new_df_linear_interpolation)

png("visualization/ts_storm_surge.png", height=800, width=1200)
par(mfrow=c(2, 1))
plot(x=as.Date(halifax_df_interpolation$date), y=halifax_df_interpolation$avg_water_surge, type="l", main="Halifax Storm Surge", xlab="date", ylab="storm surge level (m)")
plot(x=as.Date(yarmouth_df_interpolation$date), y=yarmouth_df_interpolation$avg_water_surge, , type="l", main="Yarmouth Storm Surge", xlab="date", ylab="storm surge level (m)")
dev.off()

png("visualization/ts_windspeed.png", height=800, width=1200)
par(mfrow=c(2, 1))
plot(x=as.Date(halifax_df_interpolation$date), y=halifax_df_interpolation$avg_wind_speed, type="l", main="Halifax Wind Speed", xlab="date", ylab="km/h")
plot(x=as.Date(yarmouth_df_interpolation$date), y=yarmouth_df_interpolation$avg_wind_speed, , type="l", main="Yarmouth Wind Speed", xlab="date", ylab="km/h")
dev.off()

png("visualization/ts_temp.png", height=800, width=1200)
par(mfrow=c(2, 1))
plot(x=as.Date(halifax_df_interpolation$date), y=halifax_df_interpolation$avg_temperature, type="l", main="Halifax Temperature", xlab="date", ylab="C")
plot(x=as.Date(yarmouth_df_interpolation$date), y=yarmouth_df_interpolation$avg_temperature, , type="l", main="Yarmouth Temperature", xlab="date", ylab="C")
dev.off()

png("visualization/ts_rain.png", height=800, width=1200)
par(mfrow=c(2, 1))
plot(x=as.Date(halifax_df_interpolation$date), y=halifax_df_interpolation$rain, type="l", main="Halifax Rain", xlab="date", ylab="mm")
plot(x=as.Date(yarmouth_df_interpolation$date), y=yarmouth_df_interpolation$rain, , type="l", main="Yarmouth Rain", xlab="date", ylab="mm")
dev.off()

png("visualization/ts_halifax.png", height=1200, width=1200)
par(mfrow=c(4, 1))
plot(x=as.Date(halifax_df_interpolation$date), y=halifax_df_interpolation$avg_water_surge, type="l", main="Halifax Storm Surge", xlab="date", ylab="storm surge level (m)")
plot(x=as.Date(halifax_df_interpolation$date), y=halifax_df_interpolation$avg_wind_speed, type="l", main="Halifax Wind Speed", xlab="date", ylab="km/h")
plot(x=as.Date(halifax_df_interpolation$date), y=halifax_df_interpolation$rain, type="l", main="Halifax Rain", xlab="date", ylab="mm")
plot(x=as.Date(halifax_df_interpolation$date), y=halifax_df_interpolation$avg_temperature, type="l", main="Halifax Temperature", xlab="date", ylab="C")
dev.off()

png("visualization/ts_yarmouth.png", height=1200, width=1200)
par(mfrow=c(4, 1))
plot(x=as.Date(yarmouth_df_interpolation$date), y=yarmouth_df_interpolation$avg_water_surge, , type="l", main="Yarmouth Storm Surge", xlab="date", ylab="storm surge level (m)")
plot(x=as.Date(yarmouth_df_interpolation$date), y=yarmouth_df_interpolation$avg_wind_speed, , type="l", main="Yarmouth Wind Speed", xlab="date", ylab="km/h")
plot(x=as.Date(yarmouth_df_interpolation$date), y=yarmouth_df_interpolation$rain, , type="l", main="Yarmouth Rain", xlab="date", ylab="mm")
plot(x=as.Date(yarmouth_df_interpolation$date), y=yarmouth_df_interpolation$avg_temperature, , type="l", main="Yarmouth Temperature", xlab="date", ylab="C")
dev.off()

write.csv(new_df_linear_interpolation, "data/linear_interpolation_df.csv")
