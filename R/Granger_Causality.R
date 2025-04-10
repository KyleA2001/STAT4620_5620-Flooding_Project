library(lmtest)
library(vars)
library(tseries)

linear_interpolation_df = read.csv("data/linear_interpolation_df.csv")

# This will be the same for max/min water surge and avg/max/min water levels.
new_halifax_df_interpolation = subset(linear_interpolation_df, linear_interpolation_df$location == "Halifax")
new_yarmouth_df_interpolation = subset(linear_interpolation_df, linear_interpolation_df$location == "Yarmouth")

halifax_var <- new_halifax_df_interpolation[, c("avg_water_surge", "avg_temperature", "avg_wind_speed", "rain")]
yarmouth_var <- new_yarmouth_df_interpolation[, c("avg_water_surge", "avg_temperature", "avg_wind_speed", "rain")]

grangertest(avg_water_surge ~ avg_wind_speed, order = 1, data = na.omit(halifax_var))
grangertest(avg_water_surge ~ avg_temperature, order = 1, data = na.omit(halifax_var))
grangertest(avg_water_surge ~ rain, order = 1, data = na.omit(halifax_var))

grangertest(avg_water_surge ~ avg_wind_speed, order = 1, data = na.omit(yarmouth_var))
grangertest(avg_water_surge ~ avg_temperature, order = 1, data = na.omit(yarmouth_var))
grangertest(avg_water_surge ~ rain, order = 1, data = na.omit(yarmouth_var))

# ---- HALIFAX ----
halifax_var_model <- VAR(halifax_var, p = 1, type = "none")
# Granger causality H0: avg_temperature avg_wind_speed rain do not Granger-cause avg_water_surge
# p-value < 2.2e-16 => Reject H0
causality(halifax_var_model, c('avg_wind_speed', 'avg_temperature', 'rain'))

halifax_var_model_wind <- VAR(halifax_var[, c("avg_water_surge", "avg_wind_speed")], p = 1, type = "none")
# Granger causality H0: avg_wind_speed do not Granger-cause avg_water_surge
# p-value < 2.2e-16 => Reject H0
causality(halifax_var_model_wind, 'avg_wind_speed')

halifax_var_model_temp <- VAR(halifax_var[, c("avg_water_surge", "avg_temperature")], p = 1, type = "none")
# Granger causality H0: avg_temperature do not Granger-cause avg_water_surge
# p-value = 4.144e-06 => Reject H0
causality(halifax_var_model_temp, 'avg_temperature')

halifax_var_model_rain <- VAR(halifax_var[, c("avg_water_surge", "rain")], p = 1, type = "none")
# Granger causality H0: rain do not Granger-cause avg_water_surge
# p-value = 0.1648 => Do not reject H0
causality(halifax_var_model_rain, 'rain')

# ---- YARMOUTH ----
yarmouth_var_model <- VAR(yarmouth_var, p = 1, type = "none")
# Granger causality H0: avg_temperature avg_wind_speed rain do not Granger-cause avg_water_surge
# p-value < 2.2e-16 => Reject H0
causality(yarmouth_var_model, c('avg_wind_speed', 'avg_temperature', 'rain'))

yarmouth_var_model_wind <- VAR(yarmouth_var[, c("avg_water_surge", "avg_wind_speed")], p = 1, type = "none")
# Granger causality H0: avg_wind_speed do not Granger-cause avg_water_surge
# p-value < 2.2e-16 => Reject H0
causality(yarmouth_var_model_wind, 'avg_wind_speed')

yarmouth_var_model_temp <- VAR(yarmouth_var[, c("avg_water_surge", "avg_temperature")], p = 1, type = "none")
# Granger causality H0: avg_temperature do not Granger-cause avg_water_surge
# p-value = 1.792e-07 => Reject H0
causality(yarmouth_var_model_temp, 'avg_temperature')

yarmouth_var_model_rain <- VAR(yarmouth_var[, c("avg_water_surge", "rain")], p = 1, type = "none")
# Granger causality H0: rain do not Granger-cause avg_water_surge
# p-value = 3.113e-10 => Reject H0
causality(yarmouth_var_model_rain, 'rain')
