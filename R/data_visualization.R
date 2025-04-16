linear_interpolation_df = read.csv("data/linear_interpolation_df.csv")

# This will be the same for max/min storm surge and avg/max/min water levels.
halifax = subset(linear_interpolation_df, linear_interpolation_df$location == "Halifax")
yarmouth = subset(linear_interpolation_df, linear_interpolation_df$location == "Yarmouth")

# Halifax
png("visualization/ts_halifax.png", width=900, height=1200)
par(mfrow=c(4, 1))
plot(as.Date(halifax$date), halifax$avg_water_surge, type="l", xlab="Date", ylab="Storm Surge Level (m)")
plot(as.Date(halifax$date), halifax$avg_wind_speed, type="l", xlab="Date", ylab="Wind Speed (km/h)")
plot(as.Date(halifax$date), halifax$rain, type="l", xlab="Date", ylab="Rain (mm)")
plot(as.Date(halifax$date), halifax$avg_temperature, type="l", xlab="Date", ylab="Temperature (°C)")
dev.off()

# Yarmouth
png("visualization/ts_yarmouth.png", width=900, height=1200)
par(mfrow=c(4, 1))
plot(as.Date(yarmouth$date), yarmouth$avg_water_surge, type="l", xlab="Date", ylab="Storm Surge Level")
plot(as.Date(yarmouth$date), yarmouth$avg_wind_speed, type="l", xlab="Date", ylab="Wind Speed (km/h)")
plot(as.Date(yarmouth$date), yarmouth$rain, type="l", xlab="Date", ylab="Rain (mm)")
plot(as.Date(yarmouth$date), yarmouth$avg_temperature, type="l", xlab="Date", ylab="Temperature (°C)")
dev.off()
