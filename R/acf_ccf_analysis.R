linear_interpolation_df = read.csv("data/linear_interpolation_df.csv")

# This will be the same for max/min water surge and avg/max/min water levels.
halifax = subset(linear_interpolation_df, linear_interpolation_df$location == "Halifax")
yarmouth = subset(linear_interpolation_df, linear_interpolation_df$location == "Yarmouth")

# ---- HALIFAX ----
par(mfrow=c(1, 1))
plot(x=as.Date(halifax$date[2:length(halifax$date)]), diff(as.Date(halifax$date)),
     xlab="date", ylab="number of missingness", main="All data")

# Convert the 'date' column to Date format (if not already done)
halifax$date <- as.Date(halifax$date)

# Check gap of missing value
table(diff(as.Date(halifax$date)))

# --- ACF ---
png("visualization/acf_halifax.png", height=800, width=1200)
par(mfrow=c(2, 2))
# Storm surge: lag 1, acf = 0.6860053
hf_acf_surge = acf(halifax$avg_water_surge, lag.max = 50, main="Halifax-ACF-Water Surge")
# Wind: lag 1, acf = 0.32414053
hf_acf_wind = acf(halifax$avg_wind_speed, lag.max = 50, main="Halifax-ACF-Wind Speed")
# Temp: lag 1, acf = 0.9303815
hf_acf_temp = acf(halifax$avg_temperature, lag.max = 50, main="Halifax-ACF-Temperature")
# Rain: lag 1, acf = 0.08731215
hf_acf_rain = acf(halifax$rain, lag.max = 50, main="Halifax-ACF-Rain")
dev.off()

# --- CCF ---
png("visualization/ccf_halifax.png", width=850, height=1200)
par(mfrow=c(3, 1))
# Wind: x[t] = y[t] with ccf = 0.212548522
hf_ccf_surge_wind = ccf(halifax$avg_water_surge, halifax$avg_wind_speed, lag.max = 50, plot=F)
plot(hf_ccf_surge_wind$lag[hf_ccf_surge_wind$lag>=0], hf_ccf_surge_wind$acf[hf_ccf_surge_wind$lag>=0],
     main="Halifax P1-CCF-Water Surge vs Wind Speed", xlab="lag", ylab="CCF", type="h")
abline(h=0)

# Temp: x[t+3] = y[t] with ccf = -0.2058019
hf_ccf_surge_temp = ccf(halifax$avg_water_surge, halifax$avg_temperature, lag.max = 50, plot=F)
plot(hf_ccf_surge_temp$lag[hf_ccf_surge_temp$lag>=0], hf_ccf_surge_temp$acf[hf_ccf_surge_temp$lag>=0],
     main="Halifax P1-CCF-Water Surge vs Temp", xlab="lag", ylab="CCF", type="h")
abline(h=0)

# Rain: x[t] = y[t] with ccf =  0.184461933
hf_ccf_surge_rain = ccf(halifax$avg_water_surge, halifax$rain, lag.max = 50, plot=F)
plot(hf_ccf_surge_rain$lag[hf_ccf_surge_rain$lag>=0], hf_ccf_surge_rain$acf[hf_ccf_surge_rain$lag>=0],
     main="Halifax P1-CCF-Water Surge vs Rain", xlab="lag", ylab="CCF", type="h")
abline(h=0)
dev.off()

# ---- YARMOUTH ----
par(mfrow=c(1, 1))
plot(x=as.Date(yarmouth$date[2:length(yarmouth$date)]), diff(as.Date(yarmouth$date)),
     xlab="date", ylab="number of missingness", main="All data")

# Convert the 'date' column to Date format (if not already done)
yarmouth$date <- as.Date(yarmouth$date)

# Check gap of missing value
table(diff(as.Date(yarmouth$date)))

# --- ACF ---
png("visualization/acf_yarmouth.png", height=800, width=1200)
par(mfrow=c(2, 2))
# Storm surge: lag 1, acf = 0.7050030
ym_acf_surge = acf(yarmouth$avg_water_surge, lag.max = 50, main="Yarmouth-ACF-Water Surge")
# Wind: lag 1, acf = 0.3990786
ym_acf_wind = acf(yarmouth$avg_wind_speed, lag.max = 50, main="Yarmouth-ACF-Wind Speed")
# Temp: lag 1, acf = 0.9245579
ym_acf_temp = acf(yarmouth$avg_temperature, lag.max = 50, main="Yarmouth-ACF-Temperature")
# Rain: lag 1, acf = 0.0644192864
ym_acf_rain = acf(yarmouth$rain, lag.max = 50, main="Yarmouth-ACF-Rain")
dev.off()

# --- CCF ---
png("visualization/ccf_yarmouth.png", width=850, height=1200)
par(mfrow=c(3, 1))
# Wind: x[t] = y[t] with ccf = 0.164177964
ym_ccf_surge_wind = ccf(yarmouth$avg_water_surge, yarmouth$avg_wind_speed, lag.max = 50, plot=F)
plot(ym_ccf_surge_wind$lag[ym_ccf_surge_wind$lag>=0], ym_ccf_surge_wind$acf[ym_ccf_surge_wind$lag>=0],
     main="Halifax P1-CCF-Water Surge vs Wind Speed", xlab="lag", ylab="CCF", type="h")
abline(h=0)

# Temp: x[t+3] = y[t] with ccf = -0.06642735
ym_ccf_surge_temp = ccf(yarmouth$avg_water_surge, yarmouth$avg_temperature, lag.max = 50, plot=F)
plot(ym_ccf_surge_temp$lag[ym_ccf_surge_temp$lag>=0], ym_ccf_surge_temp$acf[ym_ccf_surge_temp$lag>=0],
     main="Halifax P1-CCF-Water Surge vs Temp", xlab="lag", ylab="CCF", type="h")
abline(h=0)

# Rain: x[t] = y[t] with ccf =  0.163015998
ym_ccf_surge_rain = ccf(yarmouth$avg_water_surge, yarmouth$rain, lag.max = 50, plot=F)
plot(ym_ccf_surge_rain$lag[ym_ccf_surge_rain$lag>=0], ym_ccf_surge_rain$acf[ym_ccf_surge_rain$lag>=0],
     main="Halifax P1-CCF-Water Surge vs Rain", xlab="lag", ylab="CCF", type="h")
abline(h=0)
dev.off()
