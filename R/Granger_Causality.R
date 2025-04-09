library(lmtest)

linear_interpolation_df = read.csv("data/linear_interpolation_df.csv")

# This will be the same for max/min water surge and avg/max/min water levels.
new_halifax_df_interpolation = subset(linear_interpolation_df, linear_interpolation_df$location == "Halifax")
new_yarmouth_df_interpolation = subset(linear_interpolation_df, linear_interpolation_df$location == "Yarmouth")

halifax_var <- new_halifax_df_interpolation[, c("avg_water_surge", "avg_temperature", "avg_wind_speed", "rain")]
yarmouth_var <- new_halifax_df_interpolation[, c("avg_water_surge", "avg_temperature", "avg_wind_speed", "rain")]

grangertest(avg_water_surge ~ avg_wind_speed, order = 1, data = na.omit(halifax_var))
grangertest(avg_water_surge ~ avg_temperature, order = 1, data = na.omit(halifax_var))
grangertest(avg_water_surge ~ rain, order = 1, data = na.omit(halifax_var))

grangertest(avg_water_surge ~ avg_wind_speed, order = 1, data = na.omit(yarmouth_var))
grangertest(avg_water_surge ~ avg_temperature, order = 1, data = na.omit(yarmouth_var))
grangertest(avg_water_surge ~ rain, order = 1, data = na.omit(yarmouth_var))
